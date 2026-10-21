cmake_minimum_required(VERSION 3.22)
# Numeric consumers need neither the IR nor the generator executable.
if(NOT TARGET fortgen_runtime)
    add_library(fortgen_runtime STATIC
        ../src/fortgen_precision.f90 ../src/fortgen_storage.f90)
    add_library(fortgen::runtime ALIAS fortgen_runtime)
    set_target_properties(fortgen_runtime PROPERTIES
        Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
        POSITION_INDEPENDENT_CODE ON)
    target_include_directories(fortgen_runtime PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
        $<INSTALL_INTERFACE:include>)
    install(TARGETS fortgen_runtime EXPORT fortgenTargets ARCHIVE DESTINATION lib)
    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include/
        DESTINATION include FILES_MATCHING PATTERN "*.mod")
endif()
