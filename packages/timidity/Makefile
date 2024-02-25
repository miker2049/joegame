##
# timidity
#
# @file
# @version 0.1


all: dist/worklet-bundle.js dist/player-bundle.js
srcfiles := $(shell find src -iname "*.js")

ROLLUP := pnpm rollup

wasm/libtimidity.debug.js wasm/libtimidity.js: $(wildcard libtimidity/src/*.c)
	./tools/build.sh

dist/worklet-bundle.js dist/player-bundle.js: $(srcfiles) wasm/libtimidity.js wasm/libtimidity.debug.js
	$(ROLLUP) -c rollup.bundle.js
# end
