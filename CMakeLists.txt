cmake_minimum_required(VERSION 3.22)
project(PersistentDAG LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# directories
set(_INC_DIR ${PROJECT_SOURCE_DIR}/inc)
set(_SRC_DIR ${PROJECT_SOURCE_DIR}/src)
set(_VectorTree_INC_DIR ${PROJECT_SOURCE_DIR}/../VectorTree/inc)
set(_GTEST_INC_DIR /usr/src/googletest/googletest)
set(_GTEST_LIB_DIR /usr/src/googletest/googletest/build/lib)
set(_BENCKMARK_INC_DIR /usr/src/googlebenchmark/benchmark/include/benchmark)
set(_BENCKMARK_LIB_DIR /usr/src/googlebenchmark/benchmark/build/lib)

set(_INC_DIR ${_INC_DIR} PARENT_SCOPE)
set(_SRC_DIR ${_INC_DIR} PARENT_SCOPE)
set(_VectorTree_INC_DIR ${_VectorTree_INC_DIR} PARENT_SCOPE)
set(_GTEST_INC_DIR ${_GTEST_INC_DIR} PARENT_SCOPE)
set(_GTEST_LIB_DIR ${_GTEST_LIB_DIR} PARENT_SCOPE)
set(_BENCKMARK_INC_DIR ${_BENCKMARK_INC_DIR} PARENT_SCOPE)
set(_BENCKMARK_LIB_DIR ${_BENCKMARK_LIB_DIR} PARENT_SCOPE)

# subdirectories
add_subdirectory(src)
add_subdirectory(test)
add_subdirectory(benchmark)

#config
set(PersistentDAG_VERSION_MAJOR 1)
set(PersistentDAG_VERSION_MINOR 0)

configure_file(
  config/PersistentDAG_Config.h.in
  config/PersistentDAG_Config.h
)
