cmake_minimum_required(VERSION 3.16)
project(fastconv LANGUAGES Fortran)
enable_testing()

file(GLOB_RECURSE SRC_SOURCES CONFIGURE_DEPENDS
    src/*.f90
    src/*.F90
    src/*.c
)

add_library(fastconv_static STATIC ${SRC_SOURCES})
target_link_libraries(fastconv_static PUBLIC yaftree_static)

set_target_properties(fastconv_static PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod
)

target_include_directories(fastconv_static PUBLIC
    ${CMAKE_CURRENT_BINARY_DIR}/mod
)


#file(GLOB TEST_SOURCES CONFIGURE_DEPENDS test/*.f90 test/*.F90)
#
#foreach(test_src ${TEST_SOURCES})
#    get_filename_component(test_name ${test_src} NAME_WE)
#    add_executable(${test_name} ${test_src} test/utils/assert.f90)
#    target_link_libraries(${test_name} PRIVATE fastconv_static)
#    add_test(NAME ${test_name} COMMAND ${test_name})
#endforeach()
