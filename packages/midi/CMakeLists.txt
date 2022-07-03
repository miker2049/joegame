cmake_minimum_required(VERSION 3.10)

# set the project name and version
project(synth VERSION 0.1)
add_library(synth src/synth.c)

target_include_directories(synth PUBLIC TinySoundFont/)
target_include_directories(synth PUBLIC ../../vendor/emsdk/upstream/emscripten/cache/sysroot/include/)

find_package(PkgConfig REQUIRED)
pkg_check_modules(deps REQUIRED IMPORTED_TARGET glib-2.0)
target_link_libraries(target PkgConfig::deps)


target_link_libraries(synth m)
