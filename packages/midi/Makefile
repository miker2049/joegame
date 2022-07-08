##
# joegame wasm midi
#
# @file
# @version 0.1

SHELL := /bin/bash

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

all: compile_commands.json dist/synth-worklet.js dist/midi.js

init_emsdk:
	$(EMSDK_BIN) install $(EMSDK_VERSION)
	$(EMSDK_BIN) activate $(EMSDK_VERSION)

.ONESHELL:
libsfsynth.js: sfsynth.c init_emsdk
	. "../../vendor/emsdk/emsdk_env.sh"
	emcc -MJ cfrag.json -o $@ -I ./TinySoundFont $(mini) $(sargs) $<

dist/synth-worklet.js:  src/worklet/worklet.ts libsfsynth.js
	mkdir -p $(@D)
	pnpm tsc -P src/worklet/tsconfig.json --noEmit
	pnpm esbuild --tsconfig=src/worklet/tsconfig.json --minify --bundle --outfile=$@ $<

dist/midi.js:  src/client/midi.ts
	mkdir -p $(@D)
	pnpm tsc -P src/client/tsconfig.json
	pnpm esbuild --format=esm --minify --bundle --outfile=$@ $<

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
	rm -rf cfrag.json libsfsynth.js compile_commands.json dist/

# end
