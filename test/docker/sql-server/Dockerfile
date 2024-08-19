ARG IMAGE=mcr.microsoft.com/mssql/server:2022-CU11-ubuntu-22.04
FROM $IMAGE

## Start the sql-server docker image with an init script to set up the test db

USER root
COPY . /usr/src/app
RUN chmod +x /usr/src/app/init-db.sh

# for some reason the 2017 image doesn't have the mssql user
# USER mssql

ENTRYPOINT /bin/bash /usr/src/app/entrypoint.sh
