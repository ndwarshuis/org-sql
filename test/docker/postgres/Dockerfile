ARG IMAGE=postgres:13.2-alpine
FROM $IMAGE

COPY ./init/* /docker-entrypoint-initdb.d/
