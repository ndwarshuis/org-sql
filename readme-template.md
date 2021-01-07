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

## Logbook variables

This library uses the function `org-ml-headline-get-supercontents` from `org-ml`
in order to determine which components of a headline belong to "the logbook."
Knowing how this function works is not necessary to use `org-sql`, but the
relevent point is that it takes a plist variable representing the logbook
config, and this plist only has keys that correspond to `org-log-into-drawer`,
`org-clock-into-drawer`, and `org-log-note-clock-out`. `org-sql` transparently
understands these variables and their file-level and property-level equivalents
where applicable, so logbooks will be parsed correctly assuming that their
configuration matches the correspondingly scoped variables.

While this should cover the vast array of use cases, `org-sql` does not support
any other variables that may change the way the logbook is defined, including
`org-log-state-notes-insert-after-drawers` and `org-log-note-headings`. Setting
these to anything other than their defaults may break `org-sql`'s ability to
properly parse logbooks (particulary the latter).

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

To set up the environment, start the docker-daemon (may require sudo). Note the
`-V` flag, which is to ensure the database is fresh each time:

``` sh
docker-compose up -d -V
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

# Acknowledgements

The idea for this is based on ![John Kitchin's](http://kitchingroup.cheme.cmu.edu/blog/2017/01/03/Find-stuff-in-org-mode-anywhere/)
implementation, which uses `emacsql` as the SQL backend.
