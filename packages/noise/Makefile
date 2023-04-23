##
# noise
#
# @file
# @version 0.1

export EM_CACHE = ./.em_cache
spooky.wasm: spooky/SpookyV2.cpp
	emcc --no-entry $< -o $@ -s WASM=1 -sWASM_BIGINT -s STANDALONE_WASM -Oz -s EXPORTED_FUNCTIONS="['_spooky_64', '_spooky_128', '_malloc', '_free']"

simplex.wasm: SimplexNoise/src/SimplexNoise.cpp
	emcc --no-entry $< -o $@ -s WASM=1 -s STANDALONE_WASM -Oz -s EXPORTED_FUNCTIONS="['_simplex']"

noise.d.ts: noise.ts
	pnpm tsc --emitDeclarationOnly --declaration --outfile $@ $<

noise.js: noise.ts simplex.wasm spooky.wasm noise.d.ts
	pnpm esbuild --minify --bundle --format=esm --loader:.wasm=binary $< > $@

.PHONY: clean
clean:
	rm -f simplex.wasm spooky.wasm noise.js noise.d.ts
# end
