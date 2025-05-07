cmake_minimum_required(VERSION 3.10)
project(mudpack Fortran)

enable_language(Fortran)
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} \
-O3 \
-fbounds-check \
-finit-real=inf \
-ffpe-trap=invalid,zero \
-fdefault-real-8")

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
# target_link_libraries(mudpack PUBLIC types)
