#!/usr/bin/env bash
# Build the package
# set -e

export EM_CACHE=./.em_cache
pushd $(dirname $0)
EMSDK_VERSION="1.40.1"
EMSDK_DIR=../../../vendor/emsdk
EMSDK_BIN=emsdk
$EMSDK_BIN install $EMSDK_VERSION
$EMSDK_BIN activate $EMSDK_VERSION
# source "../../../vendor/emsdk/emsdk_env.sh"
# echo $EMSDK_VERSION
# Compile the libtimidity C codebase to JavaScript with emscripten
BUILD_FLAGS="-s ALLOW_MEMORY_GROWTH=1 -s MODULARIZE=1 -s EXPORT_NAME=LibTimidity -s EXPORTED_FUNCTIONS=@exports.json \
    -s EXPORTED_RUNTIME_METHODS=@exports-runtime.json -s FORCE_FILESYSTEM=1 -s SINGLE_FILE=1 -s BINARYEN_ASYNC_COMPILATION=0 "

# Maximize optimization options for smallest file size
OPTIMIZE_FLAGS_PROD="-Oz -sENVIRONMENT='web,worker' "
OPTIMIZE_FLAGS_DEBUG=" -DTIMIDITY_DEBUG -sENVIRONMENT='shell,web,worker' "

emcc -v -o ../wasm/libtimidity.js $OPTIMIZE_FLAGS_PROD $BUILD_FLAGS ../libtimidity/src/*.c
emcc -o ../wasm/libtimidity.debug.js $OPTIMIZE_FLAGS_DEBUG $BUILD_FLAGS ../libtimidity/src/*.c
popd

# Include the freepats config in the published package so `brfs` can inline it
# # cp node_modules/freepats/freepats.cfg freepats.cfg
# # cp patches/gravis.cfg src/gravis.cfg

# # Enhance the source maps for libtimidity.debug.js
# # node tools/enhance-source-map.js
