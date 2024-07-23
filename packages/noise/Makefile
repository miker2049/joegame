##
# noise
#
# @file
# @version 0.1

export EMCC_CFLAGS = ${NIX_CFLAGS_COMPILE}

all: wasm libs

wasm: noise.js
libs: libspooky.so libsimplex.so libfastnoiselite.so

EMCC := emcc

spooky.wasm: SpookyV2.cpp
	emcc --no-entry $< -o $@ -s WASM=1 -sWASM_BIGINT -s STANDALONE_WASM -Oz -s EXPORTED_FUNCTIONS="['_spooky_64', '_spooky_128', '_malloc', '_free']"

simplex.wasm: SimplexNoise.cpp
	emcc --no-entry $< -o $@ -s WASM=1 -s STANDALONE_WASM -Oz -s EXPORTED_FUNCTIONS="['_simplex']"

libsimplex.so: SimplexNoise.cpp
	gcc -c -Wall -Werror -fpic $< -o SimplexNoise.o
	gcc -shared SimplexNoise.o -o $@

libspooky.so: SpookyV2.cpp
	gcc -c -Wall -fpic $< -o spooky.o
	gcc -shared spooky.o -o $@


fastnoiselite.wasm: fastnoise.c
	emcc --no-entry $< -o $@ -s WASM=1 -s STANDALONE_WASM -Oz -s EXPORTED_FUNCTIONS="['_fnlCreateState', '_fnlGetNoise2D', '_fnlGetNoise3D', '_fnlDomainWarp2D', '_fnlDomainWarp3D']"

libfastnoiselite.so: fastnoise.c
	gcc -c -Wall -Werror -fpic $< -o fastnoise.o
	gcc -shared fastnoise.o -o $@

spooky-test: spookytest2.cpp SpookyV2.cpp
	g++ -o $@ $< SpookyV2.cpp

noise.d.ts: noise.ts
	pnpm tsc

noise.js: noise.ts simplex.wasm spooky.wasm fastnoiselite.wasm noise.d.ts perlin.ts
	pnpm esbuild --bundle --format=esm --loader:.wasm=binary $< > $@ && echo 0

libxxhash.so: xxHash/libxxhash.so
	ln -s $(shell readlink -f $<) $@

xxHash/libxxhash.so:
	make -C ./xxHash

.PHONY: clean
clean:
	make -C ./xxHash clean
	rm -f simplex.wasm spooky.wasm noise.js noise.d.ts spooky.o SimplexNoise.o libsimplex.so libspooky.so libxxhash.so xxHash/libxxhash.so libfastnoiselite.so fastnoiselite.wasm
# end
