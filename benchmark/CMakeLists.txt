find_package(benchmark REQUIRED)

file(GLOB HEADER_BENCHMARK *.h)
file(GLOB SORCE_BENCHMARK *.cpp)

add_executable(
  benchmark.out
  ${HEADER_BENCHMARK}
  ${SORCE_BENCHMARK}
)
target_include_directories(
  benchmark.out
  PRIVATE ${_VectorTree_INC_DIR}
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${_BENCKMARK_INC_DIR}
)
link_directories(${_VectorTree_LIB_DIR})
target_link_libraries(
  benchmark.out
  PRIVATE PersistentDAG
  benchmark
  benchmark_main
  pthread
)
