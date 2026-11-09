# Emit every Lagrange point kernel, compile it and compare with a dense oracle.
add_executable(generate_lagrange_suite test/lagrange/generate_lagrange_suite.f90)
target_link_libraries(generate_lagrange_suite PRIVATE fortgen)
set(lagrange_dir ${CMAKE_CURRENT_BINARY_DIR}/lagrange)
file(MAKE_DIRECTORY ${lagrange_dir})
add_custom_command(
    OUTPUT ${lagrange_dir}/lagrange_kernels.f90 ${lagrange_dir}/lagrange_cases.inc
    COMMAND generate_lagrange_suite ${lagrange_dir}/lagrange_kernels.f90
        ${lagrange_dir}/lagrange_cases.inc
    DEPENDS generate_lagrange_suite VERBATIM)
add_executable(test_lagrange ${lagrange_dir}/lagrange_kernels.f90
    test/lagrange/test_lagrange.f90)
target_include_directories(test_lagrange PRIVATE ${lagrange_dir})
set_source_files_properties(test/lagrange/test_lagrange.f90 PROPERTIES
    OBJECT_DEPENDS ${lagrange_dir}/lagrange_cases.inc)
add_test(NAME lagrange COMMAND test_lagrange)
