##
# joegame wasm midi
#
# @file
# @version 0.1

#SHELL := /bin/zsh

sargs := $(sargs) -s SINGLE_FILE=1
sargs := $(sargs) -s BINARYEN_ASYNC_COMPILATION=1
sargs := $(sargs) -s ALLOW_MEMORY_GROWTH=1
sargs := $(sargs) -s EXPORTED_FUNCTIONS=@exports.json
sargs := $(sargs) -s EXPORTED_RUNTIME_METHODS=UTF8ToString
sargs := $(sargs) -s WASM=1
sargs := $(sargs) -s ENVIRONMENT=web,worker
sargs := $(sargs) -s MODULARIZE=1

# EMSDK_VERSION:="3.1.12"
# EMSDK_DIR:=../../vendor/emsdk
# EMSDK_BIN:=$(EMSDK_DIR)/emsdk
export EM_CACHE = ./.em_cache

mini := -Oz

all: compile_commands.json dist/synth-worklet.js dist/midi.js

ESBUILD = ./node_modules/.bin/esbuild
TSC = ./node_modules/.bin/tsc

$(ESBUILD) $(TSC) &:
	pnpm i --filter=midi

.ONESHELL:
libsfsynth.js: sfsynth.c
	emcc -MJ cfrag.json -o $@ -I ./TinySoundFont $(mini) $(sargs) $<

dist/synth-worklet.js:  src/worklet/worklet.ts libsfsynth.js $(TSC) $(ESBUILD)
	mkdir -p $(@D)
	$(TSC) -P src/worklet/tsconfig.json --noEmit
	$(ESBUILD) --tsconfig=src/worklet/tsconfig.json --minify --bundle --outfile=$@ $<

dist/midi.js:  src/client/midi.ts $(TSC) $(ESBUILD)
	mkdir -p $(@D)
	$(TSC) -P src/client/tsconfig.json
	$(ESBUILD) --format=esm --minify --bundle --outfile=$@ $<

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
