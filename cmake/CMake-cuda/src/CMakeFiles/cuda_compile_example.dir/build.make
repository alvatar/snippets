# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 2.6

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canoncical targets will work.
.SUFFIXES:

# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list

# Suppress display of executed commands.
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
RM = /usr/bin/cmake -E remove -f

# The program to use to edit the cache.
CMAKE_EDIT_COMMAND = /usr/bin/ccmake

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/alvaro/CMake-cuda

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/alvaro/CMake-cuda

# Include any dependencies generated for this target.
include src/CMakeFiles/cuda_compile_example.dir/depend.make

# Include the progress variables for this target.
include src/CMakeFiles/cuda_compile_example.dir/progress.make

# Include the compile flags for this target's objects.
include src/CMakeFiles/cuda_compile_example.dir/flags.make

src/CMakeFiles/cuda_compile_example.dir/main.o: src/CMakeFiles/cuda_compile_example.dir/flags.make
src/CMakeFiles/cuda_compile_example.dir/main.o: src/main.cc
	$(CMAKE_COMMAND) -E cmake_progress_report /home/alvaro/CMake-cuda/CMakeFiles $(CMAKE_PROGRESS_1)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building CXX object src/CMakeFiles/cuda_compile_example.dir/main.o"
	cd /home/alvaro/CMake-cuda/src && /usr/bin/c++   $(CXX_DEFINES) $(CXX_FLAGS) -o CMakeFiles/cuda_compile_example.dir/main.o -c /home/alvaro/CMake-cuda/src/main.cc

src/CMakeFiles/cuda_compile_example.dir/main.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/cuda_compile_example.dir/main.i"
	cd /home/alvaro/CMake-cuda/src && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -E /home/alvaro/CMake-cuda/src/main.cc > CMakeFiles/cuda_compile_example.dir/main.i

src/CMakeFiles/cuda_compile_example.dir/main.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/cuda_compile_example.dir/main.s"
	cd /home/alvaro/CMake-cuda/src && /usr/bin/c++  $(CXX_DEFINES) $(CXX_FLAGS) -S /home/alvaro/CMake-cuda/src/main.cc -o CMakeFiles/cuda_compile_example.dir/main.s

src/CMakeFiles/cuda_compile_example.dir/main.o.requires:
.PHONY : src/CMakeFiles/cuda_compile_example.dir/main.o.requires

src/CMakeFiles/cuda_compile_example.dir/main.o.provides: src/CMakeFiles/cuda_compile_example.dir/main.o.requires
	$(MAKE) -f src/CMakeFiles/cuda_compile_example.dir/build.make src/CMakeFiles/cuda_compile_example.dir/main.o.provides.build
.PHONY : src/CMakeFiles/cuda_compile_example.dir/main.o.provides

src/CMakeFiles/cuda_compile_example.dir/main.o.provides.build: src/CMakeFiles/cuda_compile_example.dir/main.o
.PHONY : src/CMakeFiles/cuda_compile_example.dir/main.o.provides.build

# Object files for target cuda_compile_example
cuda_compile_example_OBJECTS = \
"CMakeFiles/cuda_compile_example.dir/main.o"

# External object files for target cuda_compile_example
cuda_compile_example_EXTERNAL_OBJECTS =

src/cuda_compile_example: src/CMakeFiles/cuda_compile_example.dir/main.o
src/cuda_compile_example: /opt/cuda/lib/libcudart.so
src/cuda_compile_example: src/CMakeFiles/cuda_compile_example.dir/build.make
src/cuda_compile_example: src/CMakeFiles/cuda_compile_example.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --red --bold "Linking CXX executable cuda_compile_example"
	cd /home/alvaro/CMake-cuda/src && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/cuda_compile_example.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
src/CMakeFiles/cuda_compile_example.dir/build: src/cuda_compile_example
.PHONY : src/CMakeFiles/cuda_compile_example.dir/build

src/CMakeFiles/cuda_compile_example.dir/requires: src/CMakeFiles/cuda_compile_example.dir/main.o.requires
.PHONY : src/CMakeFiles/cuda_compile_example.dir/requires

src/CMakeFiles/cuda_compile_example.dir/clean:
	cd /home/alvaro/CMake-cuda/src && $(CMAKE_COMMAND) -P CMakeFiles/cuda_compile_example.dir/cmake_clean.cmake
.PHONY : src/CMakeFiles/cuda_compile_example.dir/clean

src/CMakeFiles/cuda_compile_example.dir/depend:
	cd /home/alvaro/CMake-cuda && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/alvaro/CMake-cuda /home/alvaro/CMake-cuda/src /home/alvaro/CMake-cuda /home/alvaro/CMake-cuda/src /home/alvaro/CMake-cuda/src/CMakeFiles/cuda_compile_example.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : src/CMakeFiles/cuda_compile_example.dir/depend

