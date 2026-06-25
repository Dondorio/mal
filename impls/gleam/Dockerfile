FROM alpine:3.24.1
MAINTAINER Dondorio <dondadecahedron@gmail.com>
LABEL org.opencontainers.image.source=https://github.com/kanaka/mal
LABEL org.opencontainers.image.description="mal test container: gleam"

##########################################################
# General requirements for testing or common across many
# implementations
##########################################################

# Required for running tests
RUN apk add --no-cache make python3 bash
RUN ln -fs /usr/bin/python3 /usr/local/bin/python

RUN mkdir -p /mal
WORKDIR /mal

##########################################################
# Specific implementation requirements
##########################################################

RUN apk add --no-cache gleam erlang28 rebar3 nodejs npm
RUN mkdir -p /.cache
RUN chmod -R 777 /.cache
RUN mkdir -p /.npm
RUN chmod -R 777 /.npm
