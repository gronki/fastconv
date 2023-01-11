cmake_minimum_required(VERSION 3.20)
project(fastconv Fortran)

enable_language(Fortran)
enable_language(C)

add_compile_options(-Wall -Wextra)
set(RELEASE $<OR:$<CONFIG:Release>,$<CONFIG:RelWithDebInfo>>)
set(IS_FORTRAN $<COMPILE_LANGUAGE:Fortran>)
add_compile_options($<${RELEASE}:-O3>)
add_compile_options($<${RELEASE}:-funsafe-math-optimizations>)
add_compile_options($<${RELEASE}:-fno-semantic-interposition>)
add_compile_options($<${RELEASE}:-fallow-store-data-races>)
add_compile_options($<$<AND:${RELEASE},${IS_FORTRAN}>:-fno-protect-parens>)
add_compile_options($<$<AND:$<CONFIG:Debug>,${IS_FORTRAN}>:-fcheck=all>)
add_compile_options($<$<CONFIG:Debug>:-Og>)
add_compile_options($<${IS_FORTRAN}:-ffree-line-length-none>)
add_compile_options($<${IS_FORTRAN}:-fimplicit-none>)
add_compile_options($<${IS_FORTRAN}:-fcoarray=single>)
add_compile_options($<${IS_FORTRAN}:-cpp>)
add_compile_options(-march=native)

add_subdirectory(utils)
add_subdirectory(conv)

link_libraries(utils)
link_libraries(conv)

add_executable(test_conv1d test/test_conv1d.f90)
add_executable(test_conv2d test/test_conv2d.f90)

install(TARGETS test_conv1d test_conv2d)