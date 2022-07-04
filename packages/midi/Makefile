##
# joegame wasm midi
#
# @file
# @version 0.1

sargs := $(sargs) -s SINGLE_FILE=1
sargs := $(sargs) -s BINARYEN_ASYNC_COMPILATION=1
sargs := $(sargs) -s ALLOW_MEMORY_GROWTH=1
sargs := $(sargs) -s EXPORTED_FUNCTIONS=@exports.json
sargs := $(sargs) -s EXPORTED_RUNTIME_METHODS=UTF8ToString
sargs := $(sargs) -s WASM=1
sargs := $(sargs) -s ENVIRONMENT=web,worker
sargs := $(sargs) -s MODULARIZE=1

EMSDK_VERSION:="3.1.12"
EMSDK_DIR:=../../vendor/emsdk
EMSDK_BIN:=$(EMSDK_DIR)/emsdk

mini := -Oz

all: libsfsynth.js compile_commands.json dist/synth-worklet.js

init_emsdk:
	$(EMSDK_BIN) install $(EMSDK_VERSION)
	$(EMSDK_BIN) activate $(EMSDK_VERSION)
	source "../../vendor/emsdk/emsdk_env.sh"

libsfsynth.js: sfsynth.c init_emsdk
	emcc -MJ cfrag.json -o $@ -I ./TinySoundFont $(mini) $(sargs) $<

dist/synth-worklet.js: libsfsynth.js src/worklet/worklet.ts
	pnpm esbuild --external:@types/audioworklet --minify --bundle --outfile=$@ $<

cfrag.json: libsfsynth.js ;

*.c:
	echo $@

*.ts:
	echo $@

compile_commands.json: cfrag.json
	echo '[' > $@
	cat $< | sed 's/,$$//' >> $@
	echo ']' >> $@

clean:
	rm cfrag.json libsfsynth.js compile_commands.json dist/synth-worklet.js

# end
