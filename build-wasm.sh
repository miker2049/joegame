#!/usr/bin/env sh

EMCC_OPTIONS="\
    -s SINGLE_FILE=1 \
    -s BINARYEN_ASYNC_COMPILATION=0 \
    -s ALLOW_MEMORY_GROWTH=1 \
    -s EXPORTED_FUNCTIONS=@exports.json \
    -s WASM=1 \
    -s ENVIRONMENT=web,worker,shell \
    -s MODULARIZE=1 \
    -s ASSERTIONS=2 \
    -s SAFE_HEAP=1 \
    -s SAFE_HEAP_LOG=1 \
    -s EXPORT_NAME=JoegameSynth"
# OPT="-O3"
INCLUDES="-I ./TinySoundfont"
OUT="-o libsynth.js"
SOURCE_FILES="src/synth.c"

echo $SOURCE_FILES
emcc $EMCC_OPTIONS $OPT $INCLUDES $OUT $SOURCE_FILES
