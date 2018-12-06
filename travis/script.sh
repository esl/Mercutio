#!/usr/bin/env bash

#stop execution if any of the commands fails
set -e

rebar3 ct
rebar3 dialyzer
