#!/usr/bin/env bash

#stop execution if any of the commands fails
set -e

#rebar3 ct

## workaround for travis, see this issue for more details:
##     https://github.com/erlang/rebar3/issues/1778
rebar3 ct --readable=false

rebar3 dialyzer
