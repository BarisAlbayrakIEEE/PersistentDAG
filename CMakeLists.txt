cmake_minimum_required (VERSION 3.8)
project (PersistentDAG)
set (PersistentDAG_VERSION_MAJOR 1)
set (PersistentDAG_VERSION_MINOR 0)

# set the c++ standard
set(CMAKE_CXX_STANDARD 20)

# Include directories
set (
	_INCLUDE_DIRECTORIES
	"${PROJECT_SOURCE_DIR}/source"
)

# Library directories
#set (
#	_LIBRARY_DIRECTORIES 
#	"${PROJECT_SOURCE_DIR}/SharedLibraries"
#)

# Header files
set (
	_HEADER_FILES
	"${PROJECT_BINARY_DIR}/PersistentDAG_Config.h"
	"${PROJECT_SOURCE_DIR}/read_only_queue_wrapper.h"
	"${PROJECT_SOURCE_DIR}/VectorTree.h"
	"${PROJECT_SOURCE_DIR}/PersistentDAG_1.h"
	"${PROJECT_SOURCE_DIR}/PersistentDAG_2.h"
)	

# Source files
set (
	_SOURCE_FILES
	"${PROJECT_SOURCE_DIR}/PersistentDAG.cpp" 
)

# Create the config header file in the binary tree
configure_file (
	"${PROJECT_SOURCE_DIR}/PersistentDAG_Config.h.in"
	"${PROJECT_BINARY_DIR}/PersistentDAG_Config.h"
)

# Add the include directories
# Add also PROJECT_BINARY_DIR to the include directories for Config.h
include_directories (
	${_INCLUDE_DIRECTORIES}
	"${PROJECT_BINARY_DIR}"
)

# Add the library directories
#link_directories (
#	${_LIBRARY_DIRECTORIES}
#)

# Create the static library
#add_library (
#	PersistentDAG 
#	${_SOURCE_FILES}
#)

# Create the executable
add_executable(
	PersistentDAG 
	${_SOURCE_FILES}
)

# Link the libraries
#target_link_libraries(
#	PersistentDAG
#	${_STATIC_DEPENDENCIES}
#)

# Set the output directories (DEBUG and RELEASE)
set_target_properties(
	PersistentDAG
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib"
    LIBRARY_OUTPUT_DIRECTORY "${PROJECT_BINARY_DIR}/lib"
)
