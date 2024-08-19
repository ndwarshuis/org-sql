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

Only Emacs 29.3 has been tested. It will probably work for others.

### Emacs packages

- org-ml.el (5.8.8)
- dash.el (2.19.1)
- s.el (1.13)
- f.el (0.20.0)

Versions indicated those that have been tested. Others may work but are not
guaranteed.

As of version 5.8.8, org-ml.el requires org 9.6.x to work. 9.7.x and later
*will* break.

### Database Clients

Only the client binary for your desired implementation are required (ensure they
are in your PATH):

- sqlite3 
- psql (PostgreSQL)
- mysql (MariaDB/MySQL)
- sqlcmd (SQL-Server)

See the conda environment file at `env-XX.Y.yml` (where XX.Y corresponds to the
emacs version) for the exact versions of the each client used for testing. These
were provided with the following packages:

- sqlite (for sqlite)
- postgresql (for PostgreSQL)
- mysqlclient (for MariaDB and MySQL)
- go-sqlcmd (for SQL-Server)


### Database Servers

The following databases servers/versions are supported and tested:

- PostgreSQL (16, 15, 14, 13)
- MariaDB (11.4, 10.6, 10.5)
- MySQL (8.4, 8.0)
- SQL-Server (2022, 2019, 2017)

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

### Inserting data

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

### Pulling data out

If you make changes in the database, run `org-sql-user-pull` to obtain the
current database state. This will return a list where each member has the file
path and its corresponding org-tree. Each org-tree can then be converted to a
string using `org-ml-to-string` from the `org-ml` library.

For now this will pull all the contents of the database. Fine-grained query
control is planned for a future release.

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
- Data-level Operations
  - `org-sql-dump-table`
  - `org-sql-push-to-db`
  - `org-sql-pull-from-db`
  - `org-sql-clear-db`
- Other SQL Commands
  - `org-sql-send-sql`

# Limitations

## OS support

This has currently only been tested on Linux and will likely break on Windows
(it may work on MacOS). Support for other operating systems is planned for
future releases.

## Logbook variables

The structure of the logbook (eg the the thing that holds clocks, notes, state
changes, etc under a given headline) is determined by several variables.
`org-sql` understands these three:
- `org-log-into-drawer`
- `org-clock-into-drawer`
- `org-log-note-clock-out`

These variables (may not be exhaustive) are **not** understood:
- `org-log-state-notes-insert-after-drawers`
- `org-log-note-headings`.

The reason for this scope of support is due to `org-ml`, the library on which
`org-sql` depends to parse org-mode syntax.

### Inserting data

When inserting data (`org-sql-push-to-db`), the three supported variables above
will be used to determine what a valid logbook *should* look like. File-level
(eg defined with the `#+PROPERTY` keyword) and headline-level (eg defined in a
`PROPERTIES` drawer) values of these variables are also understood.

This has two consequences:
1) modifications to any unsupported variable that change the logbook may result
   in an unrecognizeable logbook that will not be inserted into the proper
   tables (most likely it will end up in the `headlines` table under the 
   `contents` column)
2) manual edits to the logbook that are out of sync with how it would normally
   be produced using the given variables will result in a similar situation as
   (1)
   
### Pulling data

When pulling data (`org-sql-pull-from-db`), the three supported variables are
used to reassemble the logbook data from the database into org syntax. Any
unsupported variables will simply be ignored.

Unlike `org-sql-push-to-db`, the pull mechanism corrently only considers the
global value of the three supported variables. Support for file- and
headline-level values is planned for a future release.

# Database Layout

## General design features

- All foreign keys are set with `DELETE CASCADE`
- All time values are store as unixtime (integers in seconds)
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

## Reproducible Environment

The entire development environment is designed to be self-contained and
reproducible. All binaries (including emacs itself) are specified in a conda
environment, the databases are specified in a docker-compose file, and the
dependencies for emacs are specified in a `straight.el` profile in
`.emacs/XX.Y/straight/versions/default.el`.

The only prerequisites for running this are a working conda and docker-compose
installation.

It isn't stricly necessary to use this, but doing so ensures that all tests run
in a standardized manner across all machines and therefore minimizes 'bit rot
bugs'.

### Conda dependencies

Assuming a working mamba installation, install all binary dependencies and
activate:

``` sh
mamba env create -f env-XX.Y.yml
conda activate env-XX.Y.yml
```

### Emacs dependencies

Run the following

``` sh
export LD_PRELOAD=/usr/lib/libc_malloc_debug.so 
make install
```

Note, the LD_PRELOAD setting is necessary if one gets and error about undefined
symbols for malloc_set_state. This is necessary for all `make ...` commands.

### Database setup

Except for SQLite, the each database for testing is encoded and set up using
`docker-compose` (see the included `docker-compose.yml` file). These are
necessary to run the stateful tests (see below).

To set up the environment, start the docker-daemon (may require sudo).

``` sh
docker-compose up -d -V
```

Add `--build` to rebuild images if altered (see below).

To shut down the environment:

``` sh
docker-compose down
```

#### Dockerfile/Docker-compose Layout

Customization of the `docker-compose` files should not be necessary except when
adding a new database for testing (or a new version). The 'base' docker images
are defined using [Dockerfiles](test/docker), which in turn are built with SQL
initialization scripts (which are necessary to test the containers with minimal
privileges). Each Dockerfile has an overridable `IMAGE` argument whose default
is set to the latest version of the container to pull. Note that MariaDB and
MySQL are assumed to share the exact same container configuration, and thus they
share the same Dockerfile.

### Running tests

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

### Building documentation

To generate documentation:

```
make docs
```

requires [erd](https://github.com/BurntSushi/erd) (which is unfortunately not in
conda).

## Interactive development

To use Emacs to edit the code of `org-sql`, one has several options, from most
to least reliable.

### Emacs from conda

Simply activate the conda environment set up from above and run emacs from the
shell. This will have all the required dependencies, but also won't have your
personal setup.

### Personal emacs with straight dependencies

More clunkily, if one wants/needs the exact versions of each emacs package, one
can copy the hashes from `emacs.d/XX.Y/straight/versions/default.el` into their
own straight config (or make a new profile).

### Personal emacs config with cask

Install [cask](https://github.com/cask/cask) and run the following (without the
conda env activated):

``` sh
cask install
```

Run emacs as normal, and activate the conda environment to run the tests.

# Acknowledgements

The idea for this is based on [John
Kitchin's](http://kitchingroup.cheme.cmu.edu/blog/2017/01/03/Find-stuff-in-org-mode-anywhere/)
implementation, which uses `emacsql` as the SQL backend.
