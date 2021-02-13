CREATE DATABASE org_sql;
\connect org_sql;

-- org_sql db "admin" (not root but has the power to create objects in this db)
CREATE ROLE org_sql WITH LOGIN PASSWORD 'org_sql';

-- create alt schema to test
CREATE SCHEMA nonpublic AUTHORIZATION org_sql;
GRANT ALL ON SCHEMA nonpublic TO org_sql;
