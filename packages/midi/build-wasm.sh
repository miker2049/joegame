#!/usr/bin/env bash
# set -e
pushd $(dirname $0)
EMSDK_VERSION="3.1.12"
EMSDK_DIR=../../vendor/emsdk
EMSDK_BIN=$EMSDK_DIR/emsdk
$EMSDK_BIN install $EMSDK_VERSION
$EMSDK_BIN activate $EMSDK_VERSION
source "../../vendor/emsdk/emsdk_env.sh"

# generate compile commands
cmake -B buildt -S . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
cp buildt/compile_commands.json .

# gen emscripten prepared lib
emcmake cmake -B build -S .
emmake make -C ./build

# final linking/generation
EMCC_OPTIONS="\
    -s SINGLE_FILE=1 \
    -s BINARYEN_ASYNC_COMPILATION=1 \
    -s ALLOW_MEMORY_GROWTH=1 \
    -s EXPORTED_FUNCTIONS=@exports.json \
    -s WASM=1 \
    -Oz \
    -s ENVIRONMENT=web,worker \
    -s MODULARIZE=1 "
OUT="-o libsynth.js"
SOURCE_FILES="build/libsynth.a"

emcc $EMCC_OPTIONS $OPT $INCLUDES $OUT $SOURCE_FILES

# clean up
rm -rf ./buildt ./build
