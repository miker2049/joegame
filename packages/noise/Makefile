##
# noise
#
# @file
# @version 0.1

export EM_CACHE = ./.em_cache
spooky.wasm: spooky/SpookyV2.cpp
	emcc --no-entry $< -o $@ -s WASM=1 -sWASM_BIGINT -s STANDALONE_WASM -Oz -s EXPORTED_FUNCTIONS="['_spooky_64', '_spooky_128', '_malloc', '_free']"


# end
