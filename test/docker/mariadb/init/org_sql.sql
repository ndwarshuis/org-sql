CREATE DATABASE org_sql;
USE org_sql;

CREATE USER org_sql IDENTIFIED BY 'org_sql'@'locahost';
GRANT CREATE, DROP, DELETE, INSERT, SELECT ON 'org_sql' TO 'org_sql'@'localhost';
