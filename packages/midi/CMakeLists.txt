cmake_minimum_required(VERSION 3.10)

# set the project name and version
project(synth VERSION 0.1)
file(GLOB SRC_FILES src/*.c)

add_library(synth src/synth.c src/smf.c src/smf_decode.c src/smf_load.c src/smf_tempo.c src/smf_save.c)

target_include_directories(synth PUBLIC TinySoundFont/)
target_include_directories(synth PUBLIC c-vector/)
target_include_directories(synth PUBLIC libsmf/src/)
target_include_directories(synth PUBLIC ../../vendor/emsdk/upstream/emscripten/cache/sysroot/include/)




target_link_libraries(synth m)
