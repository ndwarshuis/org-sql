# Table Descriptions

## outlines

Each row stores the hash, size, and toplevel section for an org file (here called an `outline`). Note that if there are identical org files, only one `outline` will be stored in the database (as determined by the unique hash) and the paths shared the outline will be reflected in the `file_metadata` table.

| Column | Description |
|  -  |  -  |
| outline_hash | hash (MD5) of this org outline |
| outline_size | number of characters of the org outline |
| outline_lines | number of lines in the org file |
| outline_preamble | the content before the first headline |

## file_metadata

Each row stores filesystem metadata for one tracked org file.

| Column | Description |
|  -  |  -  |
| file_path | path to org file |
| outline_hash | hash (MD5) of the org outline with this path |
| file_uid | UID of the file |
| file_gid | GID of the file |
| file_modification_time | time of the file's last modification |
| file_attr_change_time | time of the file's last attribute change |
| file_modes | permission mode bits for the file |

## headlines

Each row stores one headline in a given org outline.

| Column | Description |
|  -  |  -  |
| headline_id | id of this headline |
| outline_hash | hash (MD5) of the org outline with this headline |
| headline_text | raw text of the headline without leading stars or tags |
| level | the level of this headline |
| headline_index | the order of this headline relative to its neighbors |
| keyword | the TODO state keyword |
| effort | the value of the `Effort` property in minutes |
| priority | character value of the priority |
| stats_cookie_type | type of the statistics cookie (the `[n/d]` or `[p%]` at the end of some headlines) (`fraction`, or `percent`) |
| stats_cookie_value | value of the statistics cookie (between 0 and 1) |
| is_archived | TRUE if the headline has an ARCHIVE tag |
| is_commented | TRUE if the headline has a COMMENT keyword |
| content | the headline contents (everything after the planning entries, property-drawer, and/or logbook) |

## headline_closures

Each row stores the ancestor and depth of a headline relationship. All headlines will have a 0-depth entry in which `parent_id` and `headline_id` are equal.

| Column | Description |
|  -  |  -  |
| headline_id | id of this headline |
| parent_id | id of this headline's parent |
| depth | levels between this headline and the referred parent |

## timestamps

Each row stores one timestamp. Any timestamps in this table that are not referenced in other tables are part of the headlines's contents (the part after the logbook).

| Column | Description |
|  -  |  -  |
| timestamp_id | id of this timestamp |
| headline_id | id of the headline for this timestamp |
| raw_value | text representation of this timestamp |
| is_active | true if the timestamp is active |
| time_start | the start time (or only time) of this timestamp |
| time_end | the end time of this timestamp |
| start_is_long | true if the start time is in long format (eg `[YYYY-MM-DD DOW HH:MM]` vs `[YYYY-MM-DD DOW]`) |
| end_is_long | true if the end time is in long format (see `start_is_long`) |

## timestamp_warnings

Each row stores the warning component for a timestamp.

| Column | Description |
|  -  |  -  |
| timestamp_id | id of the timestamp for this warning |
| warning_value | shift of this warning |
| warning_unit | unit of this warning (`hour`, `day`, `week`, `month`, or `year`) |
| warning_type | type of this warning (`all`, or `first`) |

## timestamp_repeaters

Each row stores the repeater component for a timestamp. If the repeater also has a habit appended to it, this will be stored as well.

| Column | Description |
|  -  |  -  |
| timestamp_id | id of the timestamp for this repeater |
| repeater_value | shift of this repeater |
| repeater_unit | unit of this repeater (`hour`, `day`, `week`, `month`, or `year`) |
| repeater_type | type of this repeater (`catch-up`, `restart`, or `cumulate`) |
| habit_value | shift of this repeater's habit |
| habit_unit | unit of this repeaters habit (`hour`, `day`, `week`, `month`, or `year`) |

## planning_entries

Each row denotes a timestamp which is a planning entry (eg `DEADLINE`, `SCHEDULED`, or `CLOSED`).

| Column | Description |
|  -  |  -  |
| timestamp_id | id of the timestamp for this planning entry |
| planning_type | the type of this planning entry (`closed`, `scheduled`, or `deadline`) |

## file_tags

Each row stores one tag denoted by the `#+FILETAGS` keyword

| Column | Description |
|  -  |  -  |
| outline_hash | hash (MD5) of the org outline with this tag |
| tag | the text value of this tag |

## headline_tags

Each row stores one tag attached to a headline. This includes tags actively attached to a headlines as well as those in the `ARCHIVE_ITAGS` property within archive files. The `is_inherited` field will only be TRUE for the latter.

| Column | Description |
|  -  |  -  |
| headline_id | id of the headline for this tag |
| tag | the text value of this tag |
| is_inherited | TRUE if this tag is from the `ARCHIVE_ITAGS` property |

## properties

Each row stores one property. Note this includes properties under headlines as well as properties defined at the file-level using `#+PROPERTY`.

| Column | Description |
|  -  |  -  |
| outline_hash | hash (MD5) of the org outline with this property |
| property_id | id of this property |
| key_text | this property's key |
| val_text | this property's value |

## headline_properties

Each row stores a property under a headline.

| Column | Description |
|  -  |  -  |
| headline_id | id of the headline for this property |
| property_id | id of this property |

## clocks

Each row stores one clock entry.

| Column | Description |
|  -  |  -  |
| clock_id | id of this clock |
| headline_id | id of the headline for this clock |
| time_start | timestamp for the start of this clock |
| time_end | timestamp for the end of this clock |
| clock_note | the note entry beneath this clock |

## logbook_entries

Each row stores one logbook entry (except for clocks). Note that the possible values of `entry_type` depend on `org-log-note-headlines`. By default, the possible types are: `reschedule`, `delschedule`, `redeadline`, `deldeadline`, `state`, `done`, `note`, and `refile`. Note that while `clock-out` is also a default type in `org-log-note-headings` but this is already covered by the `clock_note` column in the `clocks` table and thus won't be stored in this table.

| Column | Description |
|  -  |  -  |
| entry_id | id of this entry |
| headline_id | id of the headline for this logbook entry |
| entry_type | type of this entry |
| time_logged | timestamp for when this entry was taken |
| header | the first line of this entry (usually standardized) |
| note | the text underneath the header of this entry  |

## state_changes

Each row stores the new and old states for logbook entries of type `state`.

| Column | Description |
|  -  |  -  |
| entry_id | id of the entry for this state change |
| state_old | former todo state keyword |
| state_new | updated todo state keyword |

## planning_changes

Each row stores the former timestamp for logbook entries with type `reschedule`, `delschedule`, `redeadline`, and `deldeadline`.

| Column | Description |
|  -  |  -  |
| entry_id | id of the entry for this planning change |
| timestamp_id | id of the former timestamp |

## links

Each row stores one link.

| Column | Description |
|  -  |  -  |
| link_id | id of this link |
| headline_id | id of the headline for this link |
| link_path | target of this link (eg url, file path, etc) |
| link_text | text of this link that isn't part of the path |
| link_type | type of this link (eg http, mu4e, file, etc) |

Version: 3.0.1