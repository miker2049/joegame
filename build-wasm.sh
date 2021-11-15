#!/usr/bin/env sh

EMCC_OPTIONS="-s ALLOW_MEMORY_GROWTH=1 -O2 -s EXPORTED_FUNCTIONS=@exports.json -o libfluidlite.wasm \
    -s STANDALONE_WASM -s EXPORTED_RUNTIME_METHODS=@exports-runtime.json -s ENVIRONMENT=web,worker -s MODULARIZE=1 -s EXPORT_NAME=fluid"
INCLUDES="-I ./Fluidlite/include/ -I ./Fluidlite/stb/ --profiling-funcs"
DEFINES="-DSF3_SUPPORT=2 -DNDEBUG -DSTB_VORBIS_NO_STDIO --no-entry"
SOURCE_FILES="Fluidlite/src/fluid_chan.c \
Fluidlite/src/fluid_chorus.c
Fluidlite/src/fluid_conv.c
Fluidlite/src/fluid_defsfont.c
Fluidlite/src/fluid_dsp_float.c
Fluidlite/src/fluid_gen.c
Fluidlite/src/fluid_hash.c
Fluidlite/src/fluid_init.c
Fluidlite/src/fluid_list.c
Fluidlite/src/fluid_mod.c
Fluidlite/src/fluid_ramsfont.c
Fluidlite/src/fluid_rev.c
Fluidlite/src/fluid_settings.c
Fluidlite/src/fluid_synth.c
Fluidlite/src/fluid_sys.c
Fluidlite/src/fluid_tuning.c
Fluidlite/src/fluid_voice.c
Fluidlite/stb/stb_vorbis.c"
echo $SOURCE_FILES
emcc $EMCC_OPTIONS $DEFINES $INCLUDES $SOURCE_FILES
