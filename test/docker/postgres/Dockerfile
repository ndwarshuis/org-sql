ARG IMAGE=postgres:16.4-alpine
FROM $IMAGE

COPY ./init/* /docker-entrypoint-initdb.d/
