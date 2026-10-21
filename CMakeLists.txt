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

add_subdirectory(runtime)

add_library(fortgen)
target_link_libraries(fortgen PUBLIC fortgen_runtime)
add_library(fortgen::fortgen ALIAS fortgen)
target_sources(fortgen PRIVATE
    src/fortgen_expr_ir.f90
    src/fortgen_string.f90
    src/fortgen_buffer.f90
    src/fortgen_layout.f90
    src/fortgen_banner.f90
    src/fortgen_names.f90
    src/fortgen_precision_emit.f90
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
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/reciprocal_kernel.f90
        COMMAND $<TARGET_FILE:fortgen-codegen> fortran
            ${CMAKE_CURRENT_SOURCE_DIR}/test/reciprocal_kernel.fgir
            ${CMAKE_CURRENT_BINARY_DIR}/reciprocal_kernel.f90
        COMMAND ${CMAKE_COMMAND} -E compare_files
            ${CMAKE_CURRENT_BINARY_DIR}/reciprocal_kernel.f90
            ${CMAKE_CURRENT_SOURCE_DIR}/test/reciprocal_kernel.f90
        DEPENDS fortgen-codegen test/reciprocal_kernel.fgir
            test/reciprocal_kernel.f90
        VERBATIM)
    add_executable(test_reciprocal_kernel test/test_reciprocal_kernel.f90
        ${CMAKE_CURRENT_BINARY_DIR}/reciprocal_kernel.f90)
    add_test(NAME reciprocal_kernel COMMAND test_reciprocal_kernel)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/linewrap_kernel.f90
        COMMAND $<TARGET_FILE:fortgen-codegen> fortran
            ${CMAKE_CURRENT_SOURCE_DIR}/test/linewrap_kernel.fgir
            ${CMAKE_CURRENT_BINARY_DIR}/linewrap_kernel.f90
        COMMAND ${CMAKE_COMMAND} -E compare_files
            ${CMAKE_CURRENT_BINARY_DIR}/linewrap_kernel.f90
            ${CMAKE_CURRENT_SOURCE_DIR}/test/linewrap_kernel.f90
        DEPENDS fortgen-codegen test/linewrap_kernel.fgir
            test/linewrap_kernel.f90
        VERBATIM)
    add_executable(test_linewrap_kernel test/test_linewrap_kernel.f90
        ${CMAKE_CURRENT_BINARY_DIR}/linewrap_kernel.f90)
    target_link_libraries(test_linewrap_kernel PRIVATE fortgen)
    if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        target_compile_options(test_linewrap_kernel PRIVATE
            -ffree-line-length-132 -Werror=line-truncation)
    endif()
    add_test(NAME linewrap_kernel COMMAND test_linewrap_kernel)
    add_executable(generate_temporary_name_fixture
        test/generate_temporary_name_fixture.f90)
    target_link_libraries(generate_temporary_name_fixture PRIVATE fortgen)
    add_custom_command(
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/temporary_name_kernel.f90
        COMMAND $<TARGET_FILE:generate_temporary_name_fixture>
            ${CMAKE_CURRENT_BINARY_DIR}/temporary_name_kernel.f90
        COMMAND ${CMAKE_COMMAND} -E compare_files
            ${CMAKE_CURRENT_BINARY_DIR}/temporary_name_kernel.f90
            ${CMAKE_CURRENT_SOURCE_DIR}/test/temporary_name_kernel.f90
        DEPENDS generate_temporary_name_fixture
            test/temporary_name_kernel.f90
        VERBATIM)
    add_executable(test_temporary_names test/test_temporary_names.f90
        ${CMAKE_CURRENT_BINARY_DIR}/temporary_name_kernel.f90)
    if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        target_compile_options(test_temporary_names PRIVATE
            -std=f2018 -ffree-line-length-132 -Werror=line-truncation)
    endif()
    add_test(NAME temporary_name_validation COMMAND generate_temporary_name_fixture
        ${CMAKE_CURRENT_BINARY_DIR}/temporary_names_checked.f90)
    add_test(NAME temporary_names COMMAND test_temporary_names)
    add_executable(test_python_emitter test/test_python_emitter.f90)
    target_link_libraries(test_python_emitter PRIVATE fortgen)
    add_test(NAME python_emitter COMMAND test_python_emitter
        ${CMAKE_CURRENT_SOURCE_DIR}/test/python_kernel.fgir)
    find_package(Python3 COMPONENTS Interpreter)
    if(Python3_Interpreter_FOUND)
        add_test(NAME python_kernel
            COMMAND ${Python3_EXECUTABLE}
                ${CMAKE_CURRENT_SOURCE_DIR}/test/test_python_kernel.py
                $<TARGET_FILE:fortgen-codegen>
                ${CMAKE_CURRENT_SOURCE_DIR}/test/python_kernel.fgir)
        set_tests_properties(python_kernel PROPERTIES SKIP_RETURN_CODE 77)
    endif()
    foreach(precision real64 real32 mixed)
        configure_file(test/device/batch.fgir.in
            ${CMAKE_CURRENT_BINARY_DIR}/batch_${precision}.fgir @ONLY)
        add_test(NAME cuda_batch_emit_${precision} COMMAND fortgen-codegen cuda-batch
            ${CMAKE_CURRENT_BINARY_DIR}/batch_${precision}.fgir
            ${CMAKE_CURRENT_BINARY_DIR}/batch_emit_${precision}.cuh)
    endforeach()
    if(FORTGEN_BUILD_CUDA_TESTING)
        if(NOT CMAKE_CUDA_ARCHITECTURES)
            message(FATAL_ERROR "Set an explicit CMAKE_CUDA_ARCHITECTURES list")
        endif()
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
        foreach(precision real64 real32 mixed)
            configure_file(test/device/batch.fgir.in
                ${CMAKE_CURRENT_BINARY_DIR}/batch_${precision}.fgir @ONLY)
            add_custom_command(
                OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/batch_${precision}.cuh
                COMMAND $<TARGET_FILE:fortgen-codegen> cuda-batch
                    ${CMAKE_CURRENT_BINARY_DIR}/batch_${precision}.fgir
                    ${CMAKE_CURRENT_BINARY_DIR}/batch_${precision}.cuh
                DEPENDS fortgen-codegen
                    ${CMAKE_CURRENT_BINARY_DIR}/batch_${precision}.fgir VERBATIM)
        endforeach()
        add_executable(test_cuda_batch test/device/test_cuda_batch.cu
            ${CMAKE_CURRENT_BINARY_DIR}/batch_real64.cuh
            ${CMAKE_CURRENT_BINARY_DIR}/batch_real32.cuh
            ${CMAKE_CURRENT_BINARY_DIR}/batch_mixed.cuh)
        target_include_directories(test_cuda_batch PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
        set_target_properties(test_cuda_batch PROPERTIES
            CUDA_STANDARD 17 CUDA_STANDARD_REQUIRED ON)
        add_test(NAME cuda_batch COMMAND test_cuda_batch)
        add_custom_command(
            OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/reciprocal_kernel.cuh
            COMMAND $<TARGET_FILE:fortgen-codegen> cuda
                ${CMAKE_CURRENT_SOURCE_DIR}/test/reciprocal_kernel.fgir
                ${CMAKE_CURRENT_BINARY_DIR}/reciprocal_kernel.cuh
            DEPENDS fortgen-codegen test/reciprocal_kernel.fgir
            VERBATIM)
        add_executable(test_cuda_reciprocal test/device/test_cuda_reciprocal.cu
            ${CMAKE_CURRENT_BINARY_DIR}/reciprocal_kernel.cuh)
        target_include_directories(test_cuda_reciprocal PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
        set_target_properties(test_cuda_reciprocal PROPERTIES
            CUDA_STANDARD 17 CUDA_STANDARD_REQUIRED ON)
        add_test(NAME cuda_reciprocal COMMAND test_cuda_reciprocal)
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

if(FORTGEN_BUILD_TESTING)
    add_executable(test_precision_storage test/test_precision_storage.f90)
    target_link_libraries(test_precision_storage PRIVATE fortgen)
    add_test(NAME precision_storage COMMAND test_precision_storage)
    add_executable(generate_precision_fixture test/generate_precision_fixture.f90)
    target_link_libraries(generate_precision_fixture PRIVATE fortgen)
    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/precision_family.f90
        COMMAND generate_precision_fixture
            ${CMAKE_CURRENT_SOURCE_DIR}/test/precision_fixture.fgir
            ${CMAKE_CURRENT_BINARY_DIR}/precision_family.f90
        COMMAND ${CMAKE_COMMAND} -E compare_files
            ${CMAKE_CURRENT_BINARY_DIR}/precision_family.f90
            ${CMAKE_CURRENT_SOURCE_DIR}/test/precision_family.f90
        DEPENDS generate_precision_fixture test/precision_fixture.fgir
            test/precision_family.f90 VERBATIM)
    add_executable(test_precision_kernel test/test_precision_kernel.f90
        ${CMAKE_CURRENT_BINARY_DIR}/precision_family.f90)
    target_link_libraries(test_precision_kernel PRIVATE fortgen)
    add_test(NAME precision_kernel COMMAND test_precision_kernel)
endif()

if(FORTGEN_BUILD_TESTING)
    add_executable(test_expr_ir test/test_expr_ir.f90)
    target_link_libraries(test_expr_ir PRIVATE fortgen)
    add_test(NAME test_expr_ir COMMAND test_expr_ir)
endif()
if(FORTGEN_BUILD_TESTING AND Python3_Interpreter_FOUND)
    add_test(NAME test_expr_adapter_differential
        COMMAND ${Python3_EXECUTABLE} ${CMAKE_CURRENT_SOURCE_DIR}/test/test_expr_adapter.py
            $<TARGET_FILE:test_expr_ir>)
    set_tests_properties(test_expr_adapter_differential PROPERTIES
        SKIP_RETURN_CODE 77 ENVIRONMENT "PYTHONPATH=${CMAKE_CURRENT_SOURCE_DIR}/python")
endif()
