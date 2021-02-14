# Org-SQL ![Github Workflow Status](https://img.shields.io/github/workflow/status/ndwarshuis/org-sql/CI) ![MELPA VERSION](https://melpa.org/packages/org-sql-badge.svg)

This package converts org-mode files to Structured Query Language (SQL) and
stores them in a database, which can then be used for comprehensive data
analysis and visualization. Supports SQLite, PostgreSQL, MySQL/MariaDB, and
Microsoft SQL-Server.

# Upcoming breaking changes for 2.x.x release

* The `file_path` column will be replaced by the `md5` column in all tables
  except the `files` table, `file_path` will be removed from the `files` table,
  and another table will be created to map `file_path` to `md5` (exact table
  names subject to change). This is necessary because the current assumption is
  that all files denoted by different `file_paths` are unique, which will make
  updating impossible and will duplicate data in the unlikely but conceivable
  case where there are identical org files inserted into the database.
  Performance will also likely be improved since we can use fixed-length primary
  keys in all cases and file renames will be much simpler to update in the
  database (no more `ON UPDATE CASCADE`)
* The `warning` and `repeater` columns in the `timestamps` table will be moved
  to their own separate tables. This will clean up the `timestamps` table as
  these columns are very sparely populated anyways (mostly `NULL`s) and will
  make adding future timestamp decorators such as habits much easier.

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

Only Emacs 27.1+ has been tested

### Emacs packages

- org-ml.el
- dash.el
- s.el
- f.el

### Database Clients

Only the client binary for your desired implementation are required (ensure they
are in your PATH):

- sqlite3
- psql (PostgreSQL)
- mysql (MariaDB/MySQL)
- sqlcmd (SQL-Server)

### Database Servers

The following databases servers/versions are supported and tested:

- PostgreSQL (13, 12, 11, 10, 9)
- MariaDB (10.5, 10.4, 10.3, 10.2)
- MySQL (8.0, 5.6)
- SQL-Server (2019, 2017)

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
as database-specific behavior. The format for this variable is like `(DB-TYPE
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

Most other needs should be satisfied by the database-specific keys in each
subsection below. If your configuration requires more than this, the `:args`
and `:env` key exists as catchall keys. The former is a list of additional
arguments to send to the client command, and the latter is a list of 2-membered
lists like `(VAR VAL)` which sets the environmental values with which the client
command will run. Consult the documentation for the client command (eg `psql`,
`mysql`, or `sqlcmd`) for which arguments and environemtal variables make sense.

#### Postgres

Likely one would set the `:hostname` key unless using the localhost.

From here many other options are possible. A simple setup (eg one using a
straightforward docker deployment) might define a username, password, and port
(denoted by the `:username`, `:password`, and `:port` keys respectively). If the
database stores other data alongside that from `org-sql`, one can create a
schema specifically for `org-sql` and set the `:schema` key with the name of
this schema.

To prevent leaking a password in plain text, one can use a `.pgpass` file as
normally used with the `psql` command, or set the `:pass-file` key to the path
of the password file. More advanced setups can utilize the `.pg_service` file as
normal, or set the `:service-file` key to the desired path to the service file.

As an additional performance optimization, set the `:unlogged` key as t to use
unlogged tables. This may significantly boost performance, particularly for
functions in `org-sql` that do bulk inserts (eg `org-sql-user-push` and
`org-sql-push-to-db`). The tradeoff is data loss if the database crashes during
a transaction, which may be acceptable if the org-files denoted by
`org-sql-files` are more permanent than the database itself. NOTE: this only
sets the unlogged property on the tables that `org-sql` uses; no other tables
will be changed.

#### MySQL/MariaDB

Likely one would set the `:hostname` key unless using the localhost.

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
`:hostname`/`:port` keys for MySQL and Postgres.

Specify the username and password using the keys `:username` and `password`
respectively. If the database stores other data alongside that from `org-sql`,
one can create a schema specifically for `org-sql` and set the `:schema` key
with the name of this schema.

To prevent hardcoding the password in Emacs code, one can set the `"SQLCMDINI"`
environmental variable in the `:env` key (see above) to the path of a startup
file which sets the password using the `"SQLCMDPASSWORD"` environmental
variable.

## Database Preparation

Since `org-sql` cannot assume it has superuser access to your database and/or
filesystem, external configuration will be necessary in many cases before
running any commands with this package.

### SQLite

The only configuration necessary is to ensure that the path denoted by `:path`
is writable to the same user running emacs.

### Postgres and SQL-Server

The database server as well as the database itself (eg the database defined by
the `:database` key) must already exist. Additionally, there must be a role
defined that `org-sql` can use for the connection. If `:schema` is non-nil, the
schema defined by this key must already exist. If it is undefined, `org-sql`
will use the default schema (`public` for Postgres and usually `dbo` for
SQL-Server). In any case, the role to be used by `org-sql` must have
authorization to create tables and insert/delete rows from those tables on the
configured schema.

See init files for [Postgres](test/docker/postgres/init/org_sql.sql) and 
[SQL-Server](test/docker/sql-server/setup.sql) for bare-bones examples.

### MySQL/MariaDB

The database server must already exist and the database defined by the
`:database` key must also already exist. The user used by `org-sql` to connect
must have permissions to create tables and insert/delete data from said tables.

See the init file for [MariaDB](test/docker/mariadb/init/org_sql.sql) for 
bare-bones example.

## Database Customization

`org-sql` by default will only create tables (with pimary and foreign keys) and
insert/delete data in these tables. If you want to do anything beyond this such
as creating additional indexes, adding triggers, defining and calling
procedures, etc, one can do so through 'hooks'. These are variables that hold
additional SQL statements that will be run along with the functions in 
`org-sql`.

These variables are:
- `org-sql-post-init-hooks`: run after `org-sql-init-db`
- `org-sql-post-push-hooks`: run after `org-sql-push-to-db`
- `org-sql-post-clear-hooks`: run after `org-sql-clear-db` 
- `org-sql-pre-reset-hooks`: run before `org-sql-reset-db`

See the docstrings of these variables for how to define the custom SQL
statements and how to control their execution.

# Usage

## Interactive Functions

The following functions can be invoked using `M-x` and should cover the simple
use case of creating a database and syncing org files to it.

### Initializing

Run `org-sql-user-init`. In the case of SQLite, this will create a new database
file. In all cases this will create the tables associated with `org-sql`.

### Updating

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

### Removing all data

Run `org-sql-user-clear-all`. This will clear all data but leave the schema.

### Resetting

Run `org-sql-user-reset`. This will drop all tables associated with `org-sql`.
In the case of SQLite, this will also delete the database file.

### Debugging

The interactive functions above will print a "success" message if the client
command returns an exit code of 0. While a non-zero exit code almost certainly
means something went wrong, **the transaction may still have failed even if the
client returned 0.** If running a command seems to have no effect on the
database, set `org-sql-debug` to t and run the command again. This will print
any additional output given by the client (which are configured when called by
`org-sql` to print errors to stdout/stderr) and will likely explain what went
wrong.

Additionally, the command `org-sql-dump-push-transaction` will print the
transaction used by the `org-sql-push-to-db` and `org-sql-user-push` commands.


## Public API

`org-sql` exposes the following public functions for interacting with the
database beyond the use cases covered by the above interactive functions:

- Table-level Operations
  - `org-sql-create-tables`
  - `org-sql-drop-tables`
  - `org-sql-list-tables`
- Database-level Operations
  - `org-sql-create-db`
  - `org-sql-drop-db`
  - `org-sql-db-exists`
- Init/Teardown Operations
  - `org-sql-init-db`
  - `org-sql-reset-db`
- Data-level operations
  - `org-sql-dump-table`
  - `org-sql-push-to-db`
  - `org-sql-clear-db`
- Other SQL Commands
  - `org-sql-send-sql`

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

- All foreign keys are set with `DELETE CASCADE`
- All time values are stores as unixtime (integers in seconds)
- No triggers or indexes (outside of the primary keys) are created by `org-sql`

## Entity Relationship Diagrams

The table layouts for each implementation are more or less identical; the only
differences are the types.

[MySQL/MariaDB](doc/erd/erd-mysql.png) 

[PostgreSQL](doc/erd/erd-postgres.png) 

[SQLite](doc/erd/erd-sqlite.png) 

[SQL Server](doc/erd/erd-sql-server.png) 

## Table Descriptions

See [here](doc/table-descriptions.md) for
a description of each table and its columns.

# Contributing

Contributions welcome! But please take advantage of the testing environment
(especially when contributing to code which directly interacts with the database
servers).

## Dependencies

In addition to all required dependencies above:

- [cask](https://github.com/cask/cask)
- docker
- docker-compose
- [erd](https://github.com/BurntSushi/erd)
- make


## Emacs setup

Install all emacs dependencies:

``` sh
cask install --dev
```

## Test environment setup

Except for SQLite, the each database for testing is encoded and set up using
`docker-compose` (see the included `docker-compose.yml` and
`docker-compose.override.yml` files). These are
necessary to run the stateful tests above.

To set up the environment, start the docker-daemon (may require sudo).

``` sh
docker-compose up -d -V
```

To shut down the environment:

``` sh
docker-compose down
```

### Dockerfile/Docker-compose Layout

Customization of the `docker-compose` files should not be necessary except when
adding a new database for testing (or a new version). The 'base' docker images
are defined using [Dockerfiles](test/docker), which in turn are built with SQL
initialization scripts (which are necessary to test the containers with minimal
privileges). Each Dockerfile has an overridable `IMAGE` argument whose default
is set to the latest version of the container to pull. Note that MariaDB and
MySQL are assumed to share the exact same container configuration, and thus they
share the same Dockerfile.

The `docker-compose` configuration is split between `docker-compose.yml` and
`docker-compose.override.yml`, with the latter defining the ports (every
container is bound to a separate port loosely matching its version) and `IMAGE`
and the former defining everything else.

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
