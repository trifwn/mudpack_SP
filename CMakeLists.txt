cmake_minimum_required(VERSION 3.10)
project(mudpack LANGUAGES Fortran VERSION 1.0.0)

enable_language(Fortran)

# Configure compiler-specific flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    # GNU Fortran compiler flags
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} \
        -O3 \
        -fbounds-check \
        -finit-real=inf \
        -ffpe-trap=invalid,zero \
        -fdefault-real-8"
    )
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    # Intel Fortran compiler flags
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} \
        -O3 \
        -check bounds \
        -fpe0 \
        -real-size 64"
    )
else()
    message(WARNING "Unsupported Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}. Using default flags.")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3")
endif()

# Create the library target
add_library(mudpack_sp SHARED
        src/intfaces.f90
        # Include modules
        src/c_com2dcr.f90
        src/c_fmud2cr.f90
        src/c_fmud3sp.f90
        src/c_imud2cr.f90
        src/c_imud3sp.f90
        src/c_mud3spc.f90
        src/c_pde2com.f90
        src/c_pde3com.f90

        # Main modules
        src/mud3sp.f90
        src/mudcom.f90
)

target_include_directories(mudpack_sp PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
    $<INSTALL_INTERFACE:include>
)
set_property(TARGET mudpack_sp PROPERTY Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
set_property(TARGET mudpack_sp PROPERTY VERSION ${PROJECT_VERSION})

# GLOBAL needed for use by parent projects
add_library(mudpack_sp::mudpack_sp INTERFACE IMPORTED GLOBAL)
target_link_libraries(mudpack_sp::mudpack_sp INTERFACE mudpack_sp)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include)
# avoid race condition when used by parent projects

# SET CMAKE_INSTALL_LIBDIR AND CMAKE_INSTALL_INCLUDEDIR IF NOT SET
if(NOT CMAKE_INSTALL_LIBDIR)
    set(CMAKE_INSTALL_LIBDIR lib)
endif()
if(NOT CMAKE_INSTALL_INCLUDEDIR)
    set(CMAKE_INSTALL_INCLUDEDIR include)
endif()

# Install the library and headers
install(TARGETS mudpack_sp EXPORT mudpack_sp-targets
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/include/mudpack_sp.mod 
        OPTIONAL DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
