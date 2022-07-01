cmake_minimum_required(VERSION 3.10)

# set the project name and version
project(joegame-midi-wasm VERSION 0.1)
add_library(joegame-midi-wasm src/synth.c)
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sSINGLE_FILE=1")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sBINARYEN_ASYNC_COMPILATION=0")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sALLOW_MEMORY_GROWTH=1")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sEXPORTED_FUNCTIONS=@exports.json")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sWASM=1")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sENVIRONMENT=web,worker,shell")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sMODULARIZE=1")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sASSERTIONS=2")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sSAFE_HEAP=1")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sDEMANGLE_SUPPORT=1")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -sEXPORT_NAME=JoegameSynth")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Oz")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} --pre-js ./js/pre.js")
# set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -o cmakelibsynth.js")

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# set_target_properties(joegame-midi-wasm PROPERTIES LINK_FLAGS "
#     -s SINGLE_FILE=1 \
#     -s BINARYEN_ASYNC_COMPILATION=0 \
#     -s ALLOW_MEMORY_GROWTH=1 \
#     -s EXPORTED_FUNCTIONS=@exports.json \
#     -s WASM=1 \
#     -s ENVIRONMENT=web,worker,shell \
#     -s MODULARIZE=1 \
#     -s ASSERTIONS=2 \
#     -s SAFE_HEAP=1 \
#     -s DEMANGLE_SUPPORT=1 \
#     -s EXPORT_NAME=JoegameSynth \
#     -Oz \
#     -pre-js ./js/pre.js \
#     -o cmakesyunt.js
# ")
target_include_directories(joegame-midi-wasm PUBLIC TinySoundFont/)
target_link_libraries(joegame-midi-wasm m)
