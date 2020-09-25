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

## Logbook Parsing

`org-sql` will parse logbook entries by trying to match their headings using
`org-log-note-headings`. Org-mode provides no way to set this variable on a
per-file basis, so `org-sql` provides the variable 
`org-sql-log-note-headings-overrides` which is an alist matching a file path to
the desired variation of `org-log-note-headlings`.

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

## Logbook drawer variables

`org-ml` will currently not pay attention to file-level logbook settings (eg
`#+STARTUP: nologdrawer`) or subtree settings (eg the `LOG_INTO_DRAWER`
property) and as such `org-ml` will only parse subtrees and store logbooks
according to `org-log-into-drawer`.

## Logbook and clock entries

### Short version

Set `org-clock-into-drawer` and `org-log-into-drawer` to be distinct drawers,
which will guarantee that both logbook entries and clocks are isolated from each
other and the rest of the headline which will enable 100% accurate parsing.

### Long version

Org-mode stores these two types of metadata near the top of each header.

Clocks look like this (with an optional closing note):

```
* headline
CLOCK: [2020-01-01 Thu 00:00]--[2020-01-01 Thu 01:00] => 01:00
- clock out note (optional)
```

Logbook entries are more complicated and follow the patterns described in
`org-log-note-headings`. These are some examples:

```
* DONE headline
- CLOSING NOTE [2020-01-01 Thu 00:00] \\
  here is a closing note
- Note taken on [2020-01-01 Thu 00:00] \\
  here is an arbitrary note
- Refiled on [2020-01-01 Thu 00:00] \\
```

These may or may not be stored in drawers, and the consequences are as follows:

- If clocks and logbook entries are put in different drawers, parsing will be
efficient and 100% accurate since the logbook drawer can be assumed to be just a
plain list where each item will match a pattern in `org-log-note-headings` and
can be classified into the correct SQL table from there. Likewise, the clock
drawer can be assumed to only contain clocks and maybe singleton plain-lists
where each plain list corresponds to a clock note.

- If clocks and logbook entries are in the same drawer, any plain-list item after
a clock cannot automatically be assumed to be a clock note. In this cases the
item will be matched against `org-log-note-headings` and if it matches none of
the patterns it will be assumed to be a clock note. For this most part, this is
likely to still be accurate but is no longer guaranteed.

- Accuracy may be poor if logbook entries and/or clocks are not put in drawers
at all; this is because org-mode has no way to define the ending point of "the
logbook and clock entries" in this case. If there is a newline after the clock
and logbook entries, the same pitfalls of storing them in the same drawer are
still applicable. However, if the logbook is followed immediately by another
plain-list, there is no way to determine if these logbook items are part of the
logbook or are clock notes. Additionally, performance will be worse than either
of the above cases.

# Database Layout

## General design features

- with the exception of a few types, the schema are identical between all
  database implementations (SQLite, Postgres, etc)
- all foreign keys are set with `UPDATE CASCADE` and `DELETE CASCADE`
- all time values are stores as unixtime (integers in seconds)

## Schema

[[ schema-docs ]]

<!-- [[ version ]] -->

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

## 1.0.2

- added `org-sql-log-note-headings-overrides`

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
