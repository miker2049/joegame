#!/usr/bin/env sh
EMCC_OPTIONS="\
    -s SINGLE_FILE=1 \
    -s BINARYEN_ASYNC_COMPILATION=0 \
    -s ALLOW_MEMORY_GROWTH=1 \
    -s EXPORTED_FUNCTIONS=@exports.json \
    -s WASM=1 \
    -s EXPORTED_RUNTIME_METHODS=@exports-runtime.json \
    -s ENVIRONMENT=web,worker,shell \
    -s MODULARIZE=1 \
    -s ASSERTIONS=1 \
    -s SAFE_HEAP=1 \
    -s SAFE_HEAP_LOG=1 \
    -s EXPORT_NAME=Fluid"
INCLUDES="-I ./Fluidlite/include/ -I ./Fluidlite/stb/ -I ./Fluidlite/src"
OPT="-gsource-map"
# PRES="--pre-js=./js/pre.js --post-js=./js/post.js"
 # PRES="--pre-js=./js/pre.js"
OUT="-o libfluidlite.js"
DEFINES="-DSF3_SUPPORT=2 -DNDEBUG -DSTB_VORBIS_NO_STDIO --no-entry -DFLUID_BUFSIZE=128"
SOURCE_FILES="Fluidlite/src/fluid_chan.c \
Fluidlite/src/fluid_chorus.c \
Fluidlite/src/fluid_conv.c \
Fluidlite/src/fluid_defsfont.c \
Fluidlite/src/fluid_dsp_float.c \
Fluidlite/src/fluid_gen.c \
Fluidlite/src/fluid_hash.c \
Fluidlite/src/fluid_init.c \
Fluidlite/src/fluid_list.c \
Fluidlite/src/fluid_mod.c \
Fluidlite/src/fluid_ramsfont.c \
Fluidlite/src/fluid_rev.c \
Fluidlite/src/fluid_settings.c \
Fluidlite/src/fluid_synth.c \
Fluidlite/src/fluid_sys.c \
Fluidlite/src/fluid_tuning.c \
Fluidlite/src/fluid_voice.c \
Fluidlite/stb/stb_vorbis.c \
src/fluid_webinterface.c"
EM_COMPILER_WRAPPER=compiledb

echo $SOURCE_FILES
emcc -MJ compile_commands.json $EMCC_OPTIONS $OPT $DEFINES $PRES $INCLUDES $OUT $SOURCE_FILES
