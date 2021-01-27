CREATE DATABASE org_sql;
\connect org_sql;

-- org_sql db "admin" (not root but has the power to create objects in this db)
CREATE ROLE org_sql WITH LOGIN PASSWORD 'org_sql';
CREATE SCHEMA nonpublic AUTHORIZATION org_sql;
GRANT ALL ON SCHEMA nonpublic TO org_sql;

CREATE SCHEMA restapi AUTHORIZATION org_sql;
GRANT ALL ON SCHEMA restapi TO org_sql;

-- separate role for the anon REST API
CREATE ROLE web_anon NOLOGIN;
GRANT USAGE ON SCHEMA restapi TO web_anon;
ALTER DEFAULT PRIVILEGES
  FOR ROLE org_sql
  IN SCHEMA restapi
  GRANT SELECT ON TABLES TO web_anon;

-- separate role for authenticated REST API
CREATE ROLE restuser NOLOGIN;
GRANT USAGE ON SCHEMA restapi TO restuser;
ALTER DEFAULT PRIVILEGES
  FOR ROLE org_sql
  IN SCHEMA restapi
  GRANT ALL ON TABLES TO restuser;

CREATE ROLE authenticator NOINHERIT LOGIN PASSWORD 'authenticator';
GRANT restuser TO authenticator;
GRANT web_anon TO authenticator;
