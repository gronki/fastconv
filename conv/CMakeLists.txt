add_library(conv)

set_property(TARGET conv 
    PROPERTY Fortran_MODULE_DIRECTORY 
    "${CMAKE_CURRENT_BINARY_DIR}/modules")
target_include_directories(conv PUBLIC 
    "${CMAKE_CURRENT_BINARY_DIR}/modules")

add_subdirectory(base)
add_subdirectory(conv1d)
add_subdirectory(conv2d)
