cmake_minimum_required(VERSION 2.8)
project (VM)
enable_language (Fortran)
message("CMAKE_Fortran_COMPILER_INIT = ${CMAKE_Fortran_COMPILER_INIT}")
message("CMAKE_Fortran_COMPILER_FULLPATH = ${CMAKE_Fortran_COMPILER_FULLPATH}")
message("CMAKE_Fortran_COMPILER = ${CMAKE_Fortran_COMPILER}")
message("CMAKE_Fortran_FLAGS = ${CMAKE_Fortran_FLAGS}")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")

add_subdirectory(src)
