add_library(PersistentDAG INTERFACE)
target_include_directories(
  PersistentDAG
  INTERFACE ${_VectorTree_INC_DIR}
  INTERFACE ${_INC_DIR}
)
set_target_properties(
  PersistentDAG
  PROPERTIES
  ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
  LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
)
