cmake_minimum_required(VERSION 3.20)
project(fastconv LANGUAGES Fortran C)

set(IS_RELEASE $<NOT:$<CONFIG:Debug>>)
set(IS_DEBUG $<CONFIG:Debug>)
set(IS_FORTRAN $<COMPILE_LANGUAGE:Fortran>)
set(IS_FORTRAN_DEBUG $<AND:${IS_DEBUG},${IS_FORTRAN}>)
set(IS_FORTRAN_RELEASE $<AND:${IS_RELEASE},${IS_FORTRAN}>)

add_compile_options(
    $<${IS_RELEASE}:-funsafe-math-optimizations>
    $<${IS_RELEASE}:-fno-semantic-interposition>
    $<${IS_RELEASE}:-fallow-store-data-races>
    $<${IS_FORTRAN_RELEASE}:-fno-protect-parens>
)

add_compile_options(
    $<${IS_DEBUG}:-Og>
    $<${IS_FORTRAN_DEBUG}:-fcheck=all>
    $<${IS_FORTRAN_DEBUG}:-Warray-temporaries>
    -Wall -Wextra
)

add_compile_options(
    $<${IS_FORTRAN}:-fimplicit-none>
    $<${IS_FORTRAN}:-ffree-line-length-none>
    $<${IS_FORTRAN}:-cpp>
)

add_compile_options(-march=native)

add_subdirectory(utils)
add_subdirectory(conv)

link_libraries(utils)
link_libraries(conv)


add_executable(test_conv1d test/test_conv1d.f90)
add_executable(test_conv2d test/test_conv2d.f90)

install(TARGETS test_conv1d test_conv2d)