# Org-SQL ![Github Workflow Status](https://img.shields.io/github/workflow/status/ndwarshuis/org-sql/CI) ![MELPA VERSION](https://melpa.org/packages/org-sql-badge.svg)

This package converts org-mode files to Structured Query Language (SQL) and
stores them in a database, which can then be used for comprehensive data
analysis and visualization. Supports SQLite, PostgreSQL, MySQL/MariaDB, and
Microsoft SQL-Server.

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

### Database Clients

Only the client binary for your desired implementation are required (ensure they
are in your PATH):

- sqlite3
- psql (PostgreSQL)
- mysql (MariaDB/MySQL)
- sqlcmd (SQL-Server)

### Database Servers

The following databases servers are supported and tested:

- PostgreSQL 12
- MariaDB 10.5
- MySQL 5.6
- SQL-Server 2019-CU8

Many versions besides these will likely work; these are simply those that are in
the testing suite.

# Configuration

## General Behavior

- `org-sql-files`: list of org files to sync with the database
- `org-sql-async`: turn on to spawn the database client process asynchronously
  (and hence not block Emacs while the client is updating the database)
- `org-sql-debug`: turn on SQL transaction debug output in the message buffer

## Database Storage

Options following the pattern `org-sql-exclude-X` or `org-sql-excluded-X`
dictate what not to store in the database. By default all these variables are
nil (include everything). See the help page for each of these for further
details.

## Database Connection

The database connection is controlled by `org-sql-db-config`. This is where one
would choose the database client (and server connection if applicable) as well
as database specific behavior. The format for this variable is like `(DB-TYPE
[KEY VAL] [[KEY VAL] ...])` where `DB-TYPE` is the type of database to use and
the `KEY-VAL` pairs are options for that database.

`DB-TYPE` is one of `sqlite`, `pgsql`, `mysql`, or `sqlserver` (a symbol).

An explanation of the `KEY-VAL` pairs is below.

### General Guidelines

While the docstring of `org-sql-db-config` is a good reference, the following
are some sane guidelines for each database configuration.

#### SQLite

This is by far the simplest and only requires the `:path` key (the path to the
file where the database will be stored).

#### All Databases Except SQLite

The only required key for these is `:database` which is the database name to use.

Any needs not met by the the more specific keys below can be covered using the
`:args` and `:env` keys. The former is a list of additional arguments to send to
the client command, and the latter is a list of 2-membered lists like `(VAR
VAL)` which sets the environmental values with which the client command will
run. Consult the documentation for the client command (eg `psql`, `mysql`, or
`sqlcmd`) for which arguments and environemtal variables make sense.

#### Postgres

Likely one would set the `:hostname` key unless using the localhost.

From here many other options are possible. A simple setup (eg one using a
straightforward docker deployment) might define a username, password, and port
(denoted by the `:username`, `:password`, and `:port` keys respectively). If the
database stores other data alongside that from `org-sql`, one can create a
schema specifically for `org-sql` set the `:schema` key with the name of this
schema.

To prevent leaking a password in plain text, one can use a `.pgpass` file as
normally used with the `psql` command, or set the `:pass-file` key to the path
of the password file. More advanced setups can utilize the `.pg_service` file as
normal, or set the `:service-file` key to the desired path to the service file.

As an additional performance optimization, set the `:unlogged` key as t to use
unlogged tables. This may significantly boost performance, particularly for
functions in `org-sql` that do bulk inserts (eg `org-sql-user-push` and
`org-sql-push-to-db`). The tradeoff is data loss if the database crashes during
a transaction, which may not be a terrible cost if the org-files denoted by
`org-sql-files` are more permanent than the database itself. NOTE: this only
sets the unlogged property on the tables that `org-sql` uses; no other tables
will be changed.

#### MySQL/MariaDB

Likely one would also set the `:hostname` key unless using the localhost.

Similar to Postgres, a simple setup might define a username, password, and port
(denoted by the `:username`, `:password`, and `:port` keys respectively). Unlike
Postgres, one might also need to set the `:args` key with `"--protocol=TCP"` if
using a TCP connection (see above for explanation of `:args`).

To prevent leaking a password in plain text, one can use an options file as
normally used with the `mysql` command (eg `.my.cnf`), as well as any other
connection parameters in the place of keys. If the options file is in a
non-default location, set it with the `:defaults-file` key. A similar key exists
for the defaults-extra file (`:defaults-extra-file`).

#### SQL Server

Likely one would set the `:server` key to denote the instance of the server
to use (eg `"tcp:example.com,1443"`). Note that this takes the place of the
`:hostname`/`:port` keys for MySQL and Postgres 

Specify the username and password using the keys `:username` and `password`
respectively. If the database stores other data alongside that from `org-sql`,
one can create a schema specifically for `org-sql` set the `:schema` key with
the name of this schema.

To prevent hardcoding the password in Emacs code, one can set the `"SQLCMDINI"`
environmental variable in the `:env` key (see above) to the path of a startup
file which sets the password using the `"SQLCMDPASSWORD"` environmental
variable.

### Running custom SQL

In addition to the database-specific keys above, there are four keys which can
be used to execute arbitrary SQL commands (called 'hooks') on the `org-sql`
database (within permissions obviously). This might be useful for setting up
triggers, adding additional indexes, defining and/or running procedures, etc.

The keys are:
- `:post-init-hooks`: run after the commands `org-sql-init-db`,
  `org-sql-reset-db` and `org-sql-user-reset`
- `:post-update-hooks`: run after the commands `org-sql-push-to-db` and
  `org-sql-user-push`.
- `:post-clear-hooks`: run after the commands `org-sql-clear-db` and
  `org-sql-user-clear-all`.
- `:pre-reset-hooks`: run before the commands `org-sql-reset-db` and
  `org-sql-user-reset`.

The value of each hook key is a list of 2-membered lists, where each 2-membered
list is one 'hook'. The car of each hook is one of `sql`, `sql+`, `file`, or
`file+`. The `sql(+)` keys denote that the cadr of the hook is a SQL statement
to be executed. The `file(+)` keys denote that the cadr is a path to a SQL file
to be executated.

The difference between the `+` and non-`+` versions of these is that the former
will be run inside the transaction of the SQL statements belonging to the
function modified by the key. Note that all the functions listed above perform
their job by sending a single transaction with `BEGIN` and `COMMIT` (and `+`
means that the extra SQL command will be inside the `BEGIN/COMMIT` block).

## Database Preparation

Since `org-sql` cannot assume it has superuser access to your database and/or
filesystem, external configuration will be necessary in almost all cases before
running any commands with this package.

### SQLite

The only configuration necessary is to ensure that the path denoted by `:path`
is writable to the same user running emacs.

### Postgres and SQL-Server

The database server as well as the database itself (eg the database defined by
the `:database` key) must already exist. Additionally, there must be a role
defined that `org-sql` can use for the connection. The schema defined by
`:schema` itself must also already exist (or the `public` schema if `:schema` is
not given should exist in the case of Postgres), and the role to be used by
`org-sql` must have authorization to create tables and insert/delete rows from
those tables on this schema.

### MySQL/MariaDB

The database server must already exist and the database defined by the
`:database` key must also already exist. The user used by `org-sql` to connect
must have permissions to create tables and insert/delete data from said tables.

# Usage

## Initializing

Run `org-sql-user-reset`. This will create a new database and initialize it with
the default table layout. It will also delete an existing database before
creating the new one if it exists.

## Updating

Run `org-sql-user-push`. This will synchronize the database with all files as
indicated in `org-sql-files` by first checking if the file is in the database
and inserting it if not. Any renamed files will also be updated. If the contents
of a file are changed, the entire file is deleted from the database and
reinserted. Files with identical contents are only stored once (with the
exception of the file paths and attributes that point to the identical files).

This may take several seconds/minutes if inserting many files depending on the
speed of your device (particularly IO) and the size/number of files. This
operation will also block Emacs until complete. Even if `org-sql-async` is t,
Emacs will still block for all computation internal to Emacs (getting the
org-element trees and converting them to SQL statements that will sync their
contents).

If performance/blocking is a concern, the best way to improve update speeds is
to use many small org files rather than a few big ones. Because the only
efficient way to 'update' a file is to delete and reinsert it into the database,
changing one character in a large file will cause that entire file to be
inserted.

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

Generally, the database is arranged from the perspective of the 'org-tree' (each
with a unique MD5 hash). Any org-tree might exists in identical files.
Therefore, the toplevel table in the database stores hashes for org-trees and
this is referenced by a child table which stores the file paths for the
org-tree. This might seem backwards, but if the file paths were in the parent
table rather than the org-trees, data for any identical org-trees would need to
be duplicated.

## General design features

- With the exception of a few types, the layouts are identical between all
  database implementations
- All foreign keys are set with `DELETE CASCADE`
- All time values are stores as unixtime (integers in seconds)
- No triggers or indexes (outside of the primary keys) are created by `org-sql`

## Entity Relationship Diagrams

[MySQL/MariaDB](doc/erd-mysql.pdf) 

[PostgreSQL](doc/erd-postgres.pdf) 

[SQLite](doc/erd-sqlite.pdf) 

[SQL Server](doc/erd-sql-server.pdf) 

## Table Descriptions

See [here](doc/table-descriptions.md) for
a description of each table and its columns.

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
- erd

## Emacs setup

Install all emacs dependencies:

``` sh
cask install --dev
```

## Test environment setup

Except for SQLite, the each database for testing is encoded and set up using
`docker-compose` (see the included `docker-compose.yml` file). These are
necessary to run the stateful tests above.

To set up the environment, start the docker-daemon (may require sudo).

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

To generate documentation:

```
make docs
```

# Acknowledgements

The idea for this is based on [John Kitchin's](http://kitchingroup.cheme.cmu.edu/blog/2017/01/03/Find-stuff-in-org-mode-anywhere/)
implementation, which uses `emacsql` as the SQL backend.
