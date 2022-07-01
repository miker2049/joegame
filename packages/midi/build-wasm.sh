#!/usr/bin/env bash
# set -e
pushd $(dirname $0)
EMSDK_VERSION="2.0.21"
EMSDK_DIR=../../vendor/emsdk
EMSDK_BIN=$EMSDK_DIR/emsdk
$EMSDK_BIN install $EMSDK_VERSION
$EMSDK_BIN activate $EMSDK_VERSION
source "../../vendor/emsdk/emsdk_env.sh"

mkdir -p build
cd build
emcmake cmake ..
emmake make
cp compile_commands.json ..
cd ..

EMCC_OPTIONS="\
    -s SINGLE_FILE=1 \
    -s BINARYEN_ASYNC_COMPILATION=1 \
    -s ALLOW_MEMORY_GROWTH=1 \
    -s EXPORTED_FUNCTIONS=@exports.json \
    -s WASM=1 \
    -s ENVIRONMENT=web,worker \
    -s MODULARIZE=1 \
    -Oz"
OUT="-o libsynth.js"
SOURCE_FILES="build/libjoegame-midi-wasm.a"

echo $SOURCE_FILES
emcc $EMCC_OPTIONS $OPT $INCLUDES $OUT $SOURCE_FILES
rm -rf ./build
