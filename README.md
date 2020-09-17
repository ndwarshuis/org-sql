# Org-SQL ![Github Workflow Status](https://img.shields.io/github/workflow/status/ndwarshuis/org-sql/CI) ![MELPA VERSION](https://melpa.org/packages/org-sql-badge.svg)

This package converts org-mode files to Structured Query Language (SQL) and
stores them in a database, which can then be used for comprehensive data
analysis and visualization.

# Installation

Download the package from MELPA

```
M-x package-install RET org-sql RET
```

Alternatively, clone this repository into your config directory

``` sh
git clone git@github.com:ndwarshuis/org-sql.git ~/config/path/org-sql/
```

Once obtained, add the package to `load-path` and require it

``` emacs-lisp
(add-to-list 'load-path "~/config/path/org-sql/")
(require 'org-sql)
```

One can also use `use-package` to automate this entire process

``` emacs-lisp
(use-package org-sql
  :ensure t
  :config
  ;; add config options here...
  )
```

## Dependencies

### Emacs packages

- org-ml.el
- dash.el
- s.el

### Databases

Only the programs for your desired implementation are required:

- sqlite3
- postgres (specifically createdb, dropdb, psql)

# Configuration

## General Behavior

- `org-sql-db-config`: a list describing the database implementation to use and
  how to connect to it (see this variable's help page for more details)
- `org-sql-files`: list of org files to insert into the database
- `org-sql-debug`: turn on SQL transaction debug output in the message buffer

## Database Storage

Options following the pattern `org-sql-exclude-X` or `org-sql-excluded-X`
dictate what not to store in the database. By default all these variables are
nil (include everything). See the help page for each of these for further
details.

# Usage

## Initializing

Run `org-sql-user-reset`. This will create a new database and initialize it with
the default schema. It will also delete an existing database before creating the
new one if it exists.

## Updating

Run `org-sql-user-update`. This will synchronize the database with all files as
indicated in `org-sql-files` by first checking if the file is in the database
and inserting it if not. If the file is already present, it will check the md5
to assess if updates are needed. Note that for any file in the database,
changing even one character in the file on disk will trigger an deletion of the
file in the database followed by an insertion of the *entire* org file.

This may take several seconds/minutes if inserting many files depending on the
speed of your device (particularly IO) and the size/number of files. This
operation will also block Emacs until complete.

## Removing all data

Run `org-sql-user-clear-all`. This will clear all data but leave the schema.

# Limitations

## OS support

This has currently only been tested on Linux and will likely break on Windows
(it may work on MacOS). Support for other operating systems is planned for
future releases.

## Logbook entries

Due to a limitation in `org-ml` (used as the backend for parsing org files here)
logbooks will only be detected and parsed if `org-log-into-drawer` is non-nil
(which means logbook entries go into a separate drawer, `LOGBOOK` by default).
Even when this limitation is fixed, there is no guarantee that logbook entries
without a drawer will be parsed correctly (since these are just free clock
entries and plain-lists in the headline section with everything else). For this
reason, it is recommended to set `org-log-into-drawer` to non-nil anyways.

Additionally, `org-ml` will not pay attention to file-level logbook settings (eg
`#+STARTUP: nologdrawer`) or subtree settings (eg the `LOG_INTO_DRAWER`
property) and as such `org-ml` will only parse subtrees and store logbooks
according to `org-log-into-drawer`.

## Logbook header types

`org-ml` parse logbook entries by trying to match their headings using
`org-log-note-headings`. There is no way to set this variable on a per-file or
per-tree basis, so all parsed file will be treated the same according to this
variable.

# Database Layout

## General design features

- with the exception of a few types, the schema are identical between all
  database implementations (SQLite, Postgres, etc)
- all foreign keys are set with `UPDATE CASCADE` and `DELETE CASCADE`
- all time values are stores as unixtime (integers in seconds)

## Schema

### files

Each row stores metadata for one tracked org file

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x |  |  | TEXT / TEXT | path to the org file |
| md5 |  |  |  | TEXT / TEXT | md5 checksum of the org file |
| size |  |  |  | INTEGER / INTEGER | size of the org file in bytes |

### headlines

Each row stores one headline in a given org file and its metadata

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - files |  | TEXT / TEXT | path to file containing the headline |
| headline_offset | x |  |  | INTEGER / INTEGER | file offset of the headline's first character |
| headline_text |  |  |  | TEXT / TEXT | raw text of the headline |
| keyword |  |  | x | TEXT / TEXT | the TODO state keyword |
| effort |  |  | x | INTEGER / INTEGER | the value of the Effort property in minutes |
| priority |  |  | x | TEXT / TEXT | character value of the priority |
| is_archived |  |  |  | INTEGER / BOOLEAN | true if the headline has an archive tag |
| is_commented |  |  |  | INTEGER / BOOLEAN | true if the headline has a comment keyword |
| content |  |  | x | TEXT / TEXT | the headline contents |

### headline_closures

Each row stores the ancestor and depth of a headline relationship (eg closure table)

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines, file_path - headlines |  | TEXT / TEXT | path to the file containing this headline |
| headline_offset | x | headline_offset - headlines |  | INTEGER / INTEGER | offset of this headline |
| parent_offset | x | headline_offset - headlines |  | INTEGER / INTEGER | offset of this headline's parent |
| depth |  |  | x | INTEGER / INTEGER | levels between this headline and the referred parent |

### timestamps

Each row stores one timestamp

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | TEXT / TEXT | path to the file containing this timestamp |
| headline_offset |  | headline_offset - headlines |  | INTEGER / INTEGER | offset of the headline containing this timestamp |
| timestamp_offset | x |  |  | INTEGER / INTEGER | offset of this timestamp |
| raw_value |  |  |  | TEXT / TEXT | text representation of this timestamp |
| is_active |  |  |  | INTEGER / BOOLEAN | true if the timestamp is active |
| warning_type |  |  | x | TEXT / ENUM | warning type of this timestamp (`all`, or `first`) |
| warning_value |  |  | x | INTEGER / INTEGER | warning shift of this timestamp |
| warning_unit |  |  | x | TEXT / ENUM | warning unit of this timestamp  (`hour`, `day`, `week`, `month`, or `year`) |
| repeat_type |  |  | x | TEXT / ENUM | repeater type of this timestamp (`catch-up`, `restart`, or `cumulate`) |
| repeat_value |  |  | x | INTEGER / INTEGER | repeater shift of this timestamp |
| repeat_unit |  |  | x | TEXT / ENUM | repeater unit of this timestamp (`hour`, `day`, `week`, `month`, or `year`) |
| time_start |  |  |  | INTEGER / INTEGER | the start time (or only time) of this timestamp |
| time_end |  |  | x | INTEGER / INTEGER | the end time of this timestamp |
| start_is_long |  |  |  | INTEGER / BOOLEAN | true if the start time is in long format |
| end_is_long |  |  | x | INTEGER / BOOLEAN | true if the end time is in long format |

### planning_entries

Each row stores the metadata for headline planning timestamps.

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - timestamps |  | TEXT / TEXT | path to the file containing the entry |
| headline_offset | x |  |  | INTEGER / INTEGER | file offset of the headline with this tag |
| planning_type | x |  |  | TEXT / ENUM | the type of this planning entry (`closed`, `scheduled`, or `deadline`) |
| timestamp_offset |  | timestamp_offset - timestamps |  | INTEGER / INTEGER | file offset of this entries timestamp |

### file_tags

Each row stores one tag at the file level

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - files |  | TEXT / TEXT | path to the file containing the tag |
| tag | x |  |  | TEXT / TEXT | the text value of this tag |

### headline_tags

Each row stores one tag

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | TEXT / TEXT | path to the file containing the tag |
| headline_offset | x | headline_offset - headlines |  | INTEGER / INTEGER | file offset of the headline with this tag |
| tag | x |  |  | TEXT / TEXT | the text value of this tag |
| is_inherited | x |  |  | INTEGER / BOOLEAN | true if this tag is from the ITAGS property |

### properties

Each row stores one property

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - files |  | TEXT / TEXT | path to the file containing this property |
| property_offset | x |  |  | INTEGER / INTEGER | file offset of this property in the org file |
| key_text |  |  |  | TEXT / TEXT | this property's key |
| val_text |  |  |  | TEXT / TEXT | this property's value |

### file_properties

Each row stores a property at the file level

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - files, file_path - properties |  | TEXT / TEXT | path to file containin the property |
| property_offset | x | property_offset - properties |  | INTEGER / INTEGER | file offset of this property in the org file |

### headline_properties

Each row stores a property at the headline level

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | TEXT / TEXT | path to file containin the property |
| property_offset | x |  |  | INTEGER / INTEGER | file offset of this property in the org file |
| headline_offset |  | headline_offset - headlines |  | INTEGER / INTEGER | file offset of the headline with this property |

### clocks

Each row stores one clock entry

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | TEXT / TEXT | path to the file containing this clock |
| headline_offset |  | headline_offset - headlines |  | INTEGER / INTEGER | offset of the headline with this clock |
| clock_offset | x |  |  | INTEGER / INTEGER | file offset of this clock |
| time_start |  |  | x | INTEGER / INTEGER | timestamp for the start of this clock |
| time_end |  |  | x | INTEGER / INTEGER | timestamp for the end of this clock |
| clock_note |  |  | x | TEXT / TEXT | the note entry beneath this clock |

### logbook_entries

Each row stores one logbook entry (except for clocks)

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | TEXT / TEXT | path to the file containing this entry |
| headline_offset |  | headline_offset - headlines |  | INTEGER / INTEGER | offset of the headline with this entry |
| entry_offset | x |  |  | INTEGER / INTEGER | offset of this logbook entry |
| entry_type |  |  | x | TEXT / TEXT | type of this entry (see `org-log-note-headlines`) |
| time_logged |  |  | x | INTEGER / INTEGER | timestamp for when this entry was taken |
| header |  |  | x | TEXT / TEXT | the first line of this entry (usually standardized) |
| note |  |  | x | TEXT / TEXT | the text of this entry underneath the header |

### state_changes

Each row stores additional metadata for a state change logbook entry

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - logbook_entries |  | TEXT / TEXT | path to the file containing this entry |
| entry_offset | x | entry_offset - logbook_entries |  | INTEGER / INTEGER | offset of the logbook entry for this state change |
| state_old |  |  |  | TEXT / TEXT | former todo state keyword |
| state_new |  |  |  | TEXT / TEXT | updated todo state keyword |

### planning_changes

Each row stores additional metadata for a planning change logbook entry

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - timestamps, file_path - logbook_entries |  | TEXT / TEXT | path to the file containing this entry |
| entry_offset | x | entry_offset - logbook_entries |  | INTEGER / INTEGER | offset of the logbook entry for this planning change |
| timestamp_offset |  | timestamp_offset - timestamps |  | INTEGER / INTEGER | offset of the former timestamp |

### links

Each rows stores one link

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type (SQLite / Postgres) | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | TEXT / TEXT | path to the file containing this link |
| headline_offset |  | headline_offset - headlines |  | INTEGER / INTEGER | offset of the headline with this link |
| link_offset | x |  |  | INTEGER / INTEGER | file offset of this link |
| link_path |  |  |  | TEXT / TEXT | target of this link (eg url, file path, etc) |
| link_text |  |  | x | TEXT / TEXT | text of this link |
| link_type |  |  |  | TEXT / TEXT | type of this link (eg http, mu4e, file, etc) |

<!-- 1.0.1 -->

# Contributing

Contributions welcome! But please take advantage of the testing environment
(especially when contributing to code which directly interacts with the database
servers).

## Dependencies

In addition to all required dependencies above:

- cask
- make
- docker
- docker-compose

## Emacs setup

Install all emacs dependencies:

``` sh
cask install --dev
```

## Test environment setup

Except for SQLite, the each database for testing is encoded and set up using
`docker-compose` (see the included `docker-compose.yml` file). These are
necessary to run the stateful tests above.

To set up the environment, start the docker-daemon (may require sudo):

``` sh
docker-compose up -d
```

To shut down the environment:

``` sh
docker-compose down
```

## Running tests

Tests are divided into stateless (pure functions, don't rely on external
database implementations) and stateful (impure, interacts with the database
and/or files on disk).

Run all stateless tests:

```
make stateless
```

Run all stateful tests:

```
make stateful
```

Compile code and run stateful and stateless tests:

```
make compile
```

Run all tests using both interpreted and compiled code:

```
make test
```

## Building documentation

To generate documentation from the readme template:

```
make docs
```

# Changelog

## 1.0.1

- various bugfixes

## 1.0.0 (relative to previous unversioned release)

- use `org-ml` to simplify code
- add support for postgres
- use subprocesses for SQL interaction instead of built-in Emacs SQL comint mode
- various performance improvements
- add tests (stateless and stateful)
- total rewrite of the schema:
   - moved/added file tags, file properties, headline closures, headline tags,
     headline properties, and planning entries as seperate tables
   - no longer defer foreign keys
   - use enum where fixed data types are expected (not sqlite)

# Acknowledgements

The idea for this is based on ![John Kitchin's](http://kitchingroup.cheme.cmu.edu/blog/2017/01/03/Find-stuff-in-org-mode-anywhere/)
implementation, which uses `emacsql` as the SQL backend.
