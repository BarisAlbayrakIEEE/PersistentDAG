# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.22

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/administrator/Documents/Work/Kod/cpp/VectorTree

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/administrator/Documents/Work/Kod/cpp/VectorTree/build

# Include any dependencies generated for this target.
include src/CMakeFiles/sil.out.dir/depend.make
# Include any dependencies generated by the compiler for this target.
include src/CMakeFiles/sil.out.dir/compiler_depend.make

# Include the progress variables for this target.
include src/CMakeFiles/sil.out.dir/progress.make

# Include the compile flags for this target's objects.
include src/CMakeFiles/sil.out.dir/flags.make

src/CMakeFiles/sil.out.dir/sil.cpp.o: src/CMakeFiles/sil.out.dir/flags.make
src/CMakeFiles/sil.out.dir/sil.cpp.o: ../src/sil.cpp
src/CMakeFiles/sil.out.dir/sil.cpp.o: src/CMakeFiles/sil.out.dir/compiler_depend.ts
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/administrator/Documents/Work/Kod/cpp/VectorTree/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object src/CMakeFiles/sil.out.dir/sil.cpp.o"
	cd /home/administrator/Documents/Work/Kod/cpp/VectorTree/build/src && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -MD -MT src/CMakeFiles/sil.out.dir/sil.cpp.o -MF CMakeFiles/sil.out.dir/sil.cpp.o.d -o CMakeFiles/sil.out.dir/sil.cpp.o -c /home/administrator/Documents/Work/Kod/cpp/VectorTree/src/sil.cpp

src/CMakeFiles/sil.out.dir/sil.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/sil.out.dir/sil.cpp.i"
	cd /home/administrator/Documents/Work/Kod/cpp/VectorTree/build/src && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/administrator/Documents/Work/Kod/cpp/VectorTree/src/sil.cpp > CMakeFiles/sil.out.dir/sil.cpp.i

src/CMakeFiles/sil.out.dir/sil.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/sil.out.dir/sil.cpp.s"
	cd /home/administrator/Documents/Work/Kod/cpp/VectorTree/build/src && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/administrator/Documents/Work/Kod/cpp/VectorTree/src/sil.cpp -o CMakeFiles/sil.out.dir/sil.cpp.s

# Object files for target sil.out
sil_out_OBJECTS = \
"CMakeFiles/sil.out.dir/sil.cpp.o"

# External object files for target sil.out
sil_out_EXTERNAL_OBJECTS =

bin/sil.out: src/CMakeFiles/sil.out.dir/sil.cpp.o
bin/sil.out: src/CMakeFiles/sil.out.dir/build.make
bin/sil.out: src/CMakeFiles/sil.out.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/administrator/Documents/Work/Kod/cpp/VectorTree/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable ../bin/sil.out"
	cd /home/administrator/Documents/Work/Kod/cpp/VectorTree/build/src && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/sil.out.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
src/CMakeFiles/sil.out.dir/build: bin/sil.out
.PHONY : src/CMakeFiles/sil.out.dir/build

src/CMakeFiles/sil.out.dir/clean:
	cd /home/administrator/Documents/Work/Kod/cpp/VectorTree/build/src && $(CMAKE_COMMAND) -P CMakeFiles/sil.out.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/sil.out.dir/clean

src/CMakeFiles/sil.out.dir/depend:
	cd /home/administrator/Documents/Work/Kod/cpp/VectorTree/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/administrator/Documents/Work/Kod/cpp/VectorTree /home/administrator/Documents/Work/Kod/cpp/VectorTree/src /home/administrator/Documents/Work/Kod/cpp/VectorTree/build /home/administrator/Documents/Work/Kod/cpp/VectorTree/build/src /home/administrator/Documents/Work/Kod/cpp/VectorTree/build/src/CMakeFiles/sil.out.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/CMakeFiles/sil.out.dir/depend

