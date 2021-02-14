# Changelog

## 2.0.0

- new features
  - added support for SQL-Server, MySQL/MariaDB, and added testing for all
    current versions of listed databases
  - added entity relationship diagrams to documentation
  - added hooks (eg `org-sql-post-push-hook` and related) to allow custom SQL
    commands to be run
  - `:args` and `:env` options to extend the client program call as needed
  - comprehensive public API for manipulating the database (in addition to the
    'interactive' functions)
  - statistics cookies are now tracked in the `headlines` table
- performance improvements
  - option to spawn asynchronous client process (`org-sql-async`)
  - optional `:unlogged` tables for Postgres
  - now use one-line bulk INSERT syntax
- changes (all of which are **breaking changes**)
  - switched role of `file_hash` and `file_path`; the former now has a
    one-to-many relationship with the latter which allows identical org-files to
    exist in the database where they couldn't before
  - all tables now use surrogate keys, which is much faster (possibly two orders
    of magnitude) and doesn't require buffer-specific information to make a
    primary key which would be unknown for new rows
  - `files` table renamed to `outlines` and `file_metadata` table
    added
  - `md5` column has been renamed to `outline_hash`
  - `file_properties` was removed (it was redundant)
  - `headline_id` removed from `planning_entries` table
  - warning and repeater information in the `timestamps` table has been split
    off into separate tables
  - removed `ON UPDATE CASCADE` from all tables (no longer needed)
  - `org-sql-user-update` was renamed to `org-sql-user-push`...to make room for
    a `-pull` ;)

## 1.1.0

- use latest logbook/contents code from `org-ml`

## 1.0.3

- fixed execution paths and temp file path
  (![jarifuri](https://github.com/jarifuri))

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
