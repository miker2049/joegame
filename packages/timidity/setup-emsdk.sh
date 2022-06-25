#!/usr/bin/env bash

EMSDK_VERSION="1.40.1"
./emsdk/emsdk install $EMSDK_VERSION
./emsdk/emsdk activate $EMSDK_VERSION
source "./emsdk/emsdk_env.sh"
echo $EMSDK_VERSION
