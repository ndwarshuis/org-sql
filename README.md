# Org-SQL

This is a SQL backend for Emacs Org-Mode. It scans through text files formatted
in org-mode, parses them, and adds key information such as todo keywords,
timestamps, and links to a relational database. For now only SQLite is
supported.

# Motivation and Goals

Despite the fact that Emacs is the (second?) greatest text editor of all time,
it is not a good data analysis platform, and neither is Lisp a good data
analysis language. This is the strong-suite of other languages such as Python
and R which have powerful data manipulation packages (`dplyr` and `pandas`) as
well as specialized presentation platforms (`shiny` and `dash`). The common
thread between these data analysis tools is a tabular data storage system, for
which SQL is the most common language.

Therefore, the primary goal of `org-sql` is to provide a link between the
text-based universe of Emacs / Org-mode and the table-based universe of data
analysis platforms.

A common use case for org-mode is a daily planner. Within this use case, some
questions that can easily be answered with a SQL-backed approach:
- How much time do I spend doing pre-planned work? (track how much time is spent
  clocking)
- How well do I estimate how long tasks take? (compare effort and clocked time)
- Which types of tasks do I concentrate on the most? (tag entries and sort based
  on effort and clocking)
- How indecisive am I? (track how many times schedule or deadline timestamps are
  changed)
- How much do I overplan? (track number of canceled tasks)
- How much do I delegate (track properties indicating which people are working
  on tasks)
- How many outstanding tasks/projects do I have? (count keywords and tags on
  headlines)

There are other uses for an Org-mode SQL database. If one has many org files
scattered throughout their filesystem, a database is an easy way to aggregate
key information such as links or timestamps. Or if one primary uses org-mode for
taking notes, it could be a way to aggregate and analyze meeting minutes.

Of course, these could all be done directly in Org-mode with Lisp code (indeed
there are already built-in functions for reporting aggregated effort and
clock-time). But why do that when one could analyze all org files by making a
descriptive dashboard with a relatively few lines of R or Python code?

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

# Configuration

## General Behavior

- `org-sql-sqlite-path`: the path to the sqlite database to be created
- `org-sql-files`: list of org files to insert into database
- `org-sql-pragma`: pragma to use for new connections (useful for performance
  tuning)
- `org-sql-buffer`: the name of the buffer for the SQLite connection
- `org-sql-debug`: turn on SQL transaction debug output in the message buffer

## Database Storage

These options control what data gets stored in the database, and are useful to minimize the size of the database as well as the time it takes to update:
- `org-sql-ignored-properties`: list of properties to ignore
- `org-sql-ignored-tags`: list of tags to ignore
- `org-sql-ignored-link-types`: list link types to ignore (eg mu4e, file)
- `org-sql-included-healine-planning-types`: planning types (eg `:closed`,
  `:scheduled`) to include
- `org-sql-included-contents-timestamp-types`: type of timestamps (eg `active`,
  `inactive`) to include
- `org-sql-included-logbook-types`: types of logbook entries to include (eg
  `note`, `reschedule`, etc)
- `org-sql-use-tag-inheritance`: add inherited tags to the database
- `org-sql-store-clocks`: whether to include clocks (nil implies no clock notes
  are desired either)
- `org-sql-store-clock-notes`: whether to include clock notes

## Logbooks

Much of the extracted data from `org-sql` pertains to logbook entries, and there
are a number of settings that effect how this data is generated in org files and
how it may be parsed reliably.

Firstly, one needs to set the relevant `org-mode` variables in order to capture
logging information. Please refer to the documentation in `org-mode` itself for
their meaning:
- `org-log-done`
- `org-log-reschedule`
- `org-log-redeadline`
- `org-log-note-clock-out`
- `org-log-refile`
- `org-log-repeat`
- `org-todo-keywords` (in this one can set which todo keywords changes are
  logged)

Obtaining the above information for the database assumes that
`org-log-note-headings` is left at its default value. This limitation may be
surpassed in the future.

Additionally, for best results it is recommended that all logbook entries be
contained in their own drawer. This means that `org-log-into-drawer` should be
set to `LOGBOOK` and `org-clock-into-drawer` should be set to `t` (which means
clocks go into a drawer with hardcoded name `LOGBOOK`). Without these settings,
`org-sql` needs to guess where the logbook entries are based on location and
pattern matching, which is not totally reliable.

# Usage

## Initializing

Run `org-sql-user-reset`. This will create a new database and initialize it with
the default schema. It will also delete an existing database before creating the
new one if it exists in `org-sql-sqlite-path`.


## Updating

Run `org-sql-user-update`. This will synchronize the database with all files as
indicated in `org-sql-files` by first checking if the file is in the database
and inserting it if not. If the file is already present, it will check the md5
to assess if updates are needed. This function will insert the *entire* content
of any org file that is either new or changed.

Note that the database will take several seconds to minutes if inserting many
files depending on the speed of your device (particularly IO) and the
size/number of files. This operation will also block Emacs until complete.

## Clearing all data

Run `org-sql-user-clear-all`. This will clear all data but leave the schema.

# Database Layout

The database is arranged by files at the top level and by `org-mode` features
moving down to child tables. Primary keys are foreign keys are marked with P and
F in parens respecively. All dates are converted into unix time integers before
entering into the database.

## Hierarchy

The databases are arranged as follows according to their foreign key contraints:

- files
  - headlines
    - tags
    - properties
    - clocking
    - logbook
      - state_changes
      - planning_changes

## Schema

### files

Each row stores metadata for one tracked org file

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x |  |  | text | path to the org file |
| md5 |  |  |  | text | md5 checksum of the org file |
| size |  |  |  | integer | size of the org file in bytes |

### headlines

Each row stores one headline in a given org file and its metadata

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - files |  | text | path to file containin the headline |
| headline_offset | x |  |  | integer | file offset of the headline's first character |
| tree_path |  |  | x | text | outline tree path of the headline |
| headline_text |  |  |  | text | raw text of the headline |
| keyword |  |  | x | text | the TODO state keyword |
| effort |  |  | x | integer | the value of the Effort property in minutes |
| scheduled_offset |  |  | x | integer | file offset of the SCHEDULED timestamp |
| deadline_offset |  |  | x | integer | file offset of the DEADLINE timestamp |
| closed_offset |  |  | x | integer | file offset of the CLOSED timestamp |
| priority |  |  | x | char | character value of the priority |
| archived |  |  | x | boolean | true if the headline has an archive tag |
| commented |  |  | x | boolean | true if the headline has a comment keyword |
| content |  |  | x | text | the headline contents (currently unused) |

### tags

Each row stores one tag

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | text | path to the file containing the tag |
| headline_offset | x | headline_offset - headlines |  | integer | file offset of the headline with this tag |
| tag | x |  |  | text | the text value of this tag |
| inherited | x |  |  | boolean | true if this tag is inherited |

### properties

Each row stores one property

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | text | path to the file containing this property |
| headline_offset |  | headline_offset - headlines | x | integer | file offset of the headline with this property |
| property_offset | x |  |  | integer | file offset of this property in the org file |
| key_text |  |  |  | text | this property's key |
| val_text |  |  |  | text | this property's value |
| inherited |  |  | x | boolean | true if this property is inherited (currently unused) |

### clocks

Each row stores one clock entry

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | text | path to the file containing this clock |
| headline_offset |  | headline_offset - headlines | x | integer | offset of the headline with this clock |
| clock_offset | x |  |  | integer | file offset of this clock |
| time_start |  |  | x | integer | timestamp for the start of this clock |
| time_end |  |  | x | integer | timestamp for the end of this clock |
| clock_note |  |  | x | text | the note entry beneath this clock |

### logbook_entries

Each row stores one logbook entry (except for clocks)

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | text | path to the file containing this entry |
| headline_offset |  | headline_offset - headlines | x | integer | offset of the headline with this entry |
| entry_offset | x |  |  | integer | offset of this logbook entry |
| entry_type |  |  | x | text | type of this entry (see `org-log-note-headlines`) |
| time_logged |  |  | x | integer | timestamp for when this entry was taken |
| header |  |  | x | text | the first line of this entry (usually standardized) |
| note |  |  | x | text | the text of this entry underneath the header |

### state_changes

Each row stores additional metadata for a state change logbook entry

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - logbook_entries |  | text | path to the file containing this entry |
| entry_offset | x | entry_offset - logbook_entries |  | integer | offset of the logbook entry for this state change |
| state_old |  |  |  | text | former todo state keyword |
| state_new |  |  |  | text | updated todo state keyword |

### planning_changes

Each row stores additional metadata for a planning change logbook entry

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - timestamps, file_path - logbook_entries |  | text | path to the file containing this entry |
| entry_offset | x | entry_offset - logbook_entries |  | integer | offset of the logbook entry for this planning change |
| timestamp_offset |  | timestamp_offset - timestamps |  | integer | offset of the former timestamp |

### links

Each rows stores one link

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | text | path to the file containing this link |
| headline_offset |  | headline_offset - headlines | x | integer | offset of the headline with this link |
| link_offset | x |  |  | integer | file offset of this link |
| link_path |  |  | x | text | target of this link (eg url, file path, etc) |
| link_text |  |  | x | text | text of this link |
| link_type |  |  | x | text | type of this link (eg http, mu4e, file, etc) |

### timestamps

Each row stores one timestamp

| Column | Is Primary | Foreign Keys (parent - table) | NULL Allowed | Type | Description |
|  -  |  -  |  -  |  -  |  -  |  -  |
| file_path | x | file_path - headlines |  | text | path to the file containing this timestamp |
| headline_offset |  | headline_offset - headlines | x | integer | offset of the headline containing this timestamp |
| timestamp_offset | x |  |  | integer | offset of this timestamp |
| raw_value |  |  |  | text | text representation of this timestamp |
| type |  |  | x | text | type of this timestamp (`active`, or `inactive`) |
| warning_type |  |  | x | text | warning type of this timestamp (`all`, or `first`) |
| warning_value |  |  | x | integer | warning shift of this timestamp |
| warning_unit |  |  | x | text | warning unit of this timestamp  (`hour`, `day`, `week`, `month`, or `year`) |
| repeat_type |  |  | x | text | repeater type of this timestamp (`catch-up`, `restart`, or `cumulate`) |
| repeat_value |  |  | x | integer | repeater shift of this timestamp |
| repeat_unit |  |  | x | text | repeater unit of this timestamp (`hour`, `day`, `week`, `month`, or `year`) |
| time |  |  |  | integer | the start time (or only time) of this timestamp |
| time_end |  |  | x | integer | the end time of this timestamp |
| resolution |  |  | x | text | the format of the starting time (`day`, or `minute`) |
| resolution_end |  |  | x | text | for the format of the ending time (`day`, or `minute`) |

<!-- 0.0.1 -->

# Changelog

## 1.0.0 (relative to previous unversioned release)

- use `org-ml` to simplify code
- use shell commands and temp scripts for SQL interaction instead of built-in
  Emacs SQL comint mode
- various performance improvements
- make entries for `closed`, `scheduled`, and `deadline` in `headlines` table
  and remove the `planning_type` entry in `timestamp` table

# Acknowledgements

The idea for this is based on ![John Kitchin's](http://kitchingroup.cheme.cmu.edu/blog/2017/01/03/Find-stuff-in-org-mode-anywhere/)
implementation, which uses `emacsql` as the SQL backend.
