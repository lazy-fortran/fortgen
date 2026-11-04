# Emit every Horner case kernel, compile it and compare with a dense oracle.
add_executable(generate_horner_suite test/horner/generate_horner_suite.f90)
target_link_libraries(generate_horner_suite PRIVATE fortgen)
set(horner_dir ${CMAKE_CURRENT_BINARY_DIR}/horner)
file(MAKE_DIRECTORY ${horner_dir})
add_custom_command(
    OUTPUT ${horner_dir}/horner_kernels.f90 ${horner_dir}/horner_cases.inc
    COMMAND generate_horner_suite fortran ${horner_dir}/horner_kernels.f90
        ${horner_dir}/horner_cases.inc
    DEPENDS generate_horner_suite VERBATIM)
add_executable(test_horner ${horner_dir}/horner_kernels.f90
    test/horner/test_horner.f90)
target_include_directories(test_horner PRIVATE ${horner_dir})
set_source_files_properties(test/horner/test_horner.f90 PROPERTIES
    OBJECT_DEPENDS ${horner_dir}/horner_cases.inc)
add_test(NAME horner COMMAND test_horner)
add_executable(generate_horner_storage test/horner/generate_horner_storage.f90)
target_link_libraries(generate_horner_storage PRIVATE fortgen)
add_custom_command(
    OUTPUT ${horner_dir}/horner_storage.f90
    COMMAND generate_horner_storage fortran ${horner_dir}/horner_storage.f90
    DEPENDS generate_horner_storage VERBATIM)
add_executable(test_horner_storage ${horner_dir}/horner_storage.f90
    test/horner/test_horner_storage.f90)
add_test(NAME horner_storage COMMAND test_horner_storage)
if(FORTGEN_BUILD_CUDA_TESTING)
    add_custom_command(
        OUTPUT ${horner_dir}/horner_storage.cuh
        COMMAND generate_horner_storage cuda ${horner_dir}/horner_storage.cuh
        DEPENDS generate_horner_storage VERBATIM)
    add_executable(test_cuda_horner_storage test/horner/test_horner_storage.cu
        ${horner_dir}/horner_storage.cuh)
    target_include_directories(test_cuda_horner_storage PRIVATE ${horner_dir})
    set_target_properties(test_cuda_horner_storage PROPERTIES
        CUDA_STANDARD 17 CUDA_STANDARD_REQUIRED ON)
    add_test(NAME cuda_horner_storage COMMAND test_cuda_horner_storage)
    add_custom_command(
        OUTPUT ${horner_dir}/horner_kernels.cuh ${horner_dir}/horner_cases.cuh
        COMMAND generate_horner_suite cuda ${horner_dir}/horner_kernels.cuh
            ${horner_dir}/horner_cases.cuh
        DEPENDS generate_horner_suite VERBATIM)
    add_executable(test_cuda_horner test/horner/test_horner.cu
        ${horner_dir}/horner_kernels.cuh ${horner_dir}/horner_cases.cuh)
    target_include_directories(test_cuda_horner PRIVATE ${horner_dir})
    set_target_properties(test_cuda_horner PROPERTIES
        CUDA_STANDARD 17 CUDA_STANDARD_REQUIRED ON)
    add_test(NAME cuda_horner COMMAND test_cuda_horner)
endif()
