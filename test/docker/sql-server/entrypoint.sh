#start SQL Server, start the script to create the DB and import the data, start the app
set -m
/opt/mssql/bin/sqlservr & /usr/src/app/init-db.sh
fg
