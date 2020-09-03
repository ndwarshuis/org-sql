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

[[ schema-docs ]]

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
