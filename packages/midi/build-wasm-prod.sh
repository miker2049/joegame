#!/usr/bin/env bash
pushd $(dirname $0)
EMSDK_VERSION="1.40.1"
EMSDK_DIR=../../vendor/emsdk
EMSDK_BIN=$EMSDK_DIR/emsdk
$EMSDK_BIN install $EMSDK_VERSION
$EMSDK_BIN activate $EMSDK_VERSION

source "../../vendor/emsdk/emsdk_env.sh"

EMCC_OPTIONS="\
    -s SINGLE_FILE=1 \
    -s BINARYEN_ASYNC_COMPILATION=0 \
    -s ALLOW_MEMORY_GROWTH=1 \
    -s EXPORTED_FUNCTIONS=@exports.json \
    -s EXPORTED_RUNTIME_METHODS=@exports-runtime.json \
    -s WASM=1 \
    -s ENVIRONMENT=web,worker,shell \
    -s MODULARIZE=1 \
    -s EXPORT_NAME=JoegameSynth"
OPT="-O3"
INCLUDES="-I ./TinySoundFont"
OUT="-o libsynth.js"
SOURCE_FILES="src/synth.c"

echo $SOURCE_FILES
emcc $EMCC_OPTIONS $OPT $INCLUDES $OUT $SOURCE_FILES
