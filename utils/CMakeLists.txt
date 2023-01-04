add_library(utils OBJECT)

set_property(
    TARGET utils 
    PROPERTY Fortran_MODULE_DIRECTORY 
    "${CMAKE_CURRENT_BINARY_DIR}/modules"
)

target_include_directories(
    utils 
    PUBLIC 
    "${CMAKE_CURRENT_BINARY_DIR}/modules"
)

target_sources(
    utils 
    PRIVATE 
    testing.f90
)
