find_package(GTest REQUIRED)

file(GLOB HEADER_TEST *.h)
file(GLOB SORCE_TEST *.cpp)

add_executable(
  test.out
  ${HEADER_TEST}
  ${SORCE_TEST}
)
target_include_directories(
  test.out
  PRIVATE ${_VectorTree_INC_DIR}
  PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}
  PRIVATE ${_GTEST_INC_DIR}
)
link_directories(${_GTEST_LIB_DIR})
target_link_libraries(
  test.out
  PRIVATE PersistentDAG
  libgtest.a
  libgtest_main.a
  pthread
)

enable_testing()
gtest_discover_tests(test.out)
