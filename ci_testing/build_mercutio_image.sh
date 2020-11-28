#!/usr/bin/env bash

otp_vsn="${1:-22.3.4}"
echo "ERLANG/OTP '${otp_vsn}'"

docker build --force-rm --build-arg "otp_vsn=${otp_vsn}" -t mercutio ./
