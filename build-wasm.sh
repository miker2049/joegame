#!/usr/bin/env sh

EMCC_OPTIONS="-s ALLOW_MEMORY_GROWTH=1 -O2 -s EXPORTED_FUNCTIONS=@exports.json -o libfluidlite.js -s ENVIRONMENT=web,worker -s MODULARIZE=1 -s EXPORT_NAME=fluid"
INCLUDES="-I ./include/ -I ./stb/ --profiling-funcs"
DEFINES="-DSF3_SUPPORT=2 -DNDEBUG -DSTB_VORBIS_NO_STDIO -DHAVE_STDIO_H=0"
SOURCE_FILES="src/fluid_chan.c \
src/fluid_chorus.c
src/fluid_conv.c
src/fluid_defsfont.c
src/fluid_dsp_float.c
src/fluid_gen.c
src/fluid_hash.c
src/fluid_init.c
src/fluid_list.c
src/fluid_mod.c
src/fluid_ramsfont.c
src/fluid_rev.c
src/fluid_settings.c
src/fluid_synth.c
src/fluid_sys.c
src/fluid_tuning.c
src/fluid_voice.c
stb/stb_vorbis.c"
echo $SOURCE_FILES
emcc $EMCC_OPTIONS $DEFINES $INCLUDES $SOURCE_FILES
