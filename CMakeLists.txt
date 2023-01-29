cmake_minimum_required(VERSION 3.20)
project(fastconv)

enable_language(Fortran)
enable_language(C)

set(IS_RELEASE $<NOT:$<CONFIG:Debug>>)
set(IS_DEBUG $<CONFIG:Debug>)
set(IS_FORTRAN $<COMPILE_LANGUAGE:Fortran>)
set(IS_FORTRAN_DEBUG $<AND:${IS_DEBUG},${IS_FORTRAN}>)
set(IS_FORTRAN_RELEASE $<AND:${IS_RELEASE},${IS_FORTRAN}>)

add_compile_options($<${IS_RELEASE}:-O3>)
add_compile_options($<${IS_RELEASE}:-funsafe-math-optimizations>)
add_compile_options($<${IS_FORTRAN_RELEASE}:-fno-protect-parens>)

add_compile_options($<${IS_DEBUG}:-Og>)
add_compile_options($<${IS_DEBUG}:-DDEBUG>)
add_compile_options($<${IS_DEBUG}:-DCHECKS>)
add_compile_options($<${IS_FORTRAN_DEBUG}:-fcheck=all>)

add_compile_options($<${IS_FORTRAN}:-fimplicit-none>)
add_compile_options($<${IS_FORTRAN}:-ffree-line-length-none>)
add_compile_options($<${IS_FORTRAN}:-fcoarray=single>)
add_compile_options($<${IS_FORTRAN}:-cpp>)

add_compile_options(-march=native)
add_compile_options(-Wall -Wextra)

add_subdirectory(utils)
add_subdirectory(conv)

link_libraries(utils)
link_libraries(conv)


add_executable(test_conv1d test/test_conv1d.f90)
add_executable(test_conv2d test/test_conv2d.f90)

install(TARGETS test_conv1d test_conv2d)