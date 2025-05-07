cmake_minimum_required(VERSION 3.10)
project(mudpack LANGUAGES Fortran VERSION 1.0.0)

enable_language(Fortran)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} \
-O3 \
-fbounds-check \
-finit-real=inf \
-ffpe-trap=invalid,zero \
-fdefault-real-8")

# Create the library target
add_library(mudpack_sp SHARED
        # Include modules
        src/c_com2dcr.f90
        src/c_fmud2cr.f90
        src/c_fmud3sp.f90
        src/c_imud2cr.f90
        src/c_imud3sp.f90
        src/c_mud3spc.f90
        src/c_pde2com.f90
        src/c_pde3com.f90

        # Shared modules to avoid circular dependencies
        src/intfaces.f90
        # Main modules
        src/mud3sp.f90
        src/mudcom.f90
        # Main program
        src/mudpack.f90)

# Set include directories for build and install
target_include_directories(mudpack_sp
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
)

# Set up export functionality
include(GNUInstallDirs)
install(TARGETS mudpack_sp
    EXPORT mudpack_sp-targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

# Install export target
install(EXPORT mudpack_sp-targets
    FILE mudpack_sp-targets.cmake
    NAMESPACE mudpack::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/mudpack_sp
)

# Create and install config file
include(CMakePackageConfigHelpers)
configure_package_config_file(
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake/mudpack_sp-config.cmake.in
    ${CMAKE_CURRENT_BINARY_DIR}/mudpack_sp-config.cmake
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/mudpack_sp
)
write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/mudpack_sp-config-version.cmake
    VERSION ${PROJECT_VERSION}
    COMPATIBILITY SameMajorVersion
)
install(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/mudpack_sp-config.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/mudpack_sp-config-version.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/mudpack_sp
)
