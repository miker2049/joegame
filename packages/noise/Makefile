##
# noise
#
# @file
# @version 0.1

export EM_CACHE = ./.em_cache

all: wasm libs

wasm: noise.js
libs: libspooky.so libsimplex.so libxxhash.so

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

spooky-test: spookytest2.cpp SpookyV2.cpp
	g++ -o $@ $< SpookyV2.cpp

noise.d.ts: noise.ts
	pnpm tsc

noise.js: noise.ts simplex.wasm spooky.wasm noise.d.ts perlin.ts
	pnpm esbuild --bundle --format=esm --loader:.wasm=binary $< > $@ && echo 0

libxxhash.so: xxHash/libxxhash.so
	ln -s $(shell readlink -f $<) .

xxHash/libxxhash.so:
	make -C ./xxHash

.PHONY: clean
clean:
	make -C ./xxHash clean
	rm -f simplex.wasm spooky.wasm noise.js noise.d.ts spooky.o SimplexNoise.o libsimplex.so libspooky.so libxxhash.so xxHash/libxxhash.so
# end
