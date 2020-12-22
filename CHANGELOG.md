# Changelog

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
