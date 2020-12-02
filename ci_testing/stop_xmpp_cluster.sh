#!/usr/bin/env bash

docker stop mercutio mongooseim-1 mongooseim-2
docker network prune -f
