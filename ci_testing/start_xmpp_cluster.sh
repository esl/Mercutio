#!/usr/bin/env bash

####################################################################
## start MIM cluster
####################################################################
docker network create mim_cluster

mim_config_file="$(pwd)/ci_testing/config_files/ejabberd.cfg"

docker run --rm -dt -h mongooseim-1 --name mongooseim-1 \
           -v "$mim_config_file":/member/ejabberd.cfg   \
           --network=mim_cluster -p 5222:5222           \
           mongooseim/mongooseim:3.1.1

docker run --rm -dt -h mongooseim-2 --name mongooseim-2 \
           -v "$mim_config_file":/member/ejabberd.cfg   \
           --network=mim_cluster                        \
           mongooseim/mongooseim:3.1.1

sleep 60

####################################################################
## start mercutio
####################################################################
mercutio_config_file="$(pwd)/ci_testing/config_files/mercutio.cfg"
docker run --rm -dt --name mercutio                 \
           -v "$mercutio_config_file":/mercutio.cfg \
           -e "MERCUTIO_CONFIG=/mercutio.cfg"       \
           --network=mim_cluster                    \
           mercutio
