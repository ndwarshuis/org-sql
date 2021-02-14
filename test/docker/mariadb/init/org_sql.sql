CREATE DATABASE org_sql;
USE org_sql;

CREATE USER 'org_sql' IDENTIFIED BY 'org_sql';
-- MySQL needs to REFERENCES permission for foreign keys, MariaDB apparently
-- ignores it
GRANT CREATE, DROP, DELETE, INSERT, REFERENCES, SELECT ON org_sql.* TO 'org_sql';
