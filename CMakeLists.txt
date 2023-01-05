cmake_minimum_required(VERSION 3.20)
project(fastconv Fortran)

enable_language(Fortran)
enable_language(C)

set(CMAKE_Fortran_FLAGS "-std=f2018")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -funsafe-math-optimizations -fno-protect-parens")
set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O3 -funsafe-math-optimizations -fno-protect-parens -g")
set(CMAKE_Fortran_FLAGS_DEBUG "-Og -fcheck=all -g")

set(CMAKE_C_FLAGS_RELEASE "-O3 -funsafe-math-optimizations")
set(CMAKE_C_FLAGS_RELWITHDEBINFO "-O3 -funsafe-math-optimizations -g")
set(CMAKE_C_FLAGS_DEBUG "-Og -g")

add_compile_options("-march=native")
add_compile_options("-Wall" "-Wextra")

add_subdirectory(utils)
add_subdirectory(conv)

link_libraries(utils)
link_libraries(conv)

add_compile_options("-cpp" "-ffree-line-length-none")

add_executable(test_conv1d test/test_conv1d.f90)
add_executable(test_conv2d test/test_conv2d.f90)

install(TARGETS test_conv1d test_conv2d)