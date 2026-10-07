cmake_minimum_required(VERSION 3.22)

project(fortgen LANGUAGES Fortran)

include(GNUInstallDirs)

set(CMAKE_Fortran_STANDARD 2018)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/include)

set(FORTGEN_IS_STANDALONE OFF)
if(CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    set(FORTGEN_IS_STANDALONE ON)
endif()
option(FORTGEN_BUILD_TESTING "Build FortGen tests" ${FORTGEN_IS_STANDALONE})
option(FORTGEN_BUILD_CUDA_TESTING "Build the opt-in CUDA device oracle" OFF)
if(FORTGEN_BUILD_CUDA_TESTING AND NOT FORTGEN_BUILD_TESTING)
    message(FATAL_ERROR "FORTGEN_BUILD_CUDA_TESTING requires FORTGEN_BUILD_TESTING")
endif()

add_library(fortgen)
add_library(fortgen::fortgen ALIAS fortgen)
target_sources(fortgen PRIVATE
    src/fortgen_string.f90
    src/fortgen_buffer.f90
    src/fortgen_layout.f90
    src/fortgen_banner.f90
    src/fortgen_names.f90
    src/fortgen_precision.f90
    src/fortgen_kernel_target.f90
    src/fortgen_kernel_ir.f90
    src/fortgen_functions.f90
    src/fortgen_kernel_emit.f90
    src/fortgen_ir_text.f90
)
target_include_directories(fortgen PUBLIC
    $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)

add_executable(fortgen-codegen app/fortgen_codegen.f90)
target_link_libraries(fortgen-codegen PRIVATE fortgen)

if(FORTGEN_BUILD_TESTING)
    enable_testing()
    add_executable(test_kernel_ir test/test_kernel_ir.f90)
    target_link_libraries(test_kernel_ir PRIVATE fortgen)
    add_test(NAME kernel_ir COMMAND test_kernel_ir)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/behavior_kernel.f90
        COMMAND $<TARGET_FILE:fortgen-codegen> fortran
            ${CMAKE_CURRENT_SOURCE_DIR}/test/behavior_kernel.fgir
            ${CMAKE_CURRENT_BINARY_DIR}/behavior_kernel.f90
        DEPENDS fortgen-codegen test/behavior_kernel.fgir
        VERBATIM)
    configure_file(test/test_compiled_kernel.f90.in
        ${CMAKE_CURRENT_BINARY_DIR}/test_compiled_kernel.f90 COPYONLY)
    add_executable(test_compiled_kernel
        ${CMAKE_CURRENT_BINARY_DIR}/test_compiled_kernel.f90
        ${CMAKE_CURRENT_BINARY_DIR}/behavior_kernel.f90)
    add_test(NAME compiled_kernel COMMAND test_compiled_kernel)
    if(FORTGEN_BUILD_CUDA_TESTING)
        enable_language(CUDA)
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/behavior_kernel.cuh
            COMMAND $<TARGET_FILE:fortgen-codegen> cuda
                ${CMAKE_CURRENT_SOURCE_DIR}/test/device/fixture.fgir
                ${CMAKE_CURRENT_BINARY_DIR}/behavior_kernel.cuh
            DEPENDS fortgen-codegen test/device/fixture.fgir
            VERBATIM)
        add_executable(test_cuda_kernel
            test/device/test_cuda_kernel.cu
            ${CMAKE_CURRENT_BINARY_DIR}/behavior_kernel.cuh)
        target_include_directories(test_cuda_kernel PRIVATE
            ${CMAKE_CURRENT_BINARY_DIR})
        set_target_properties(test_cuda_kernel PROPERTIES
            CUDA_STANDARD 17 CUDA_STANDARD_REQUIRED ON)
        add_test(NAME cuda_kernel COMMAND test_cuda_kernel)
    endif()
endif()

install(TARGETS fortgen fortgen-codegen
    EXPORT fortgenTargets
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}/
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    FILES_MATCHING PATTERN "*.mod")
install(EXPORT fortgenTargets
    FILE fortgenTargets.cmake
    NAMESPACE fortgen::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortgen)
