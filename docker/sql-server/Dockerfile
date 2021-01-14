FROM mcr.microsoft.com/mssql/server:2019-CU8-ubuntu-16.04

## Start the sql-server docker image with an init script to set up the test db

USER root
COPY . /usr/src/app
RUN chmod +x /usr/src/app/init-db.sh
USER mssql

ENTRYPOINT /bin/bash /usr/src/app/entrypoint.sh
