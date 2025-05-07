## Modern Fortran Refactoring of MUDPACK SP Package (SPAG v8, F90)

This repository provides a **clean Fortran 90** refactoring of the **SP (Separable) package** from the original **MUDPACK** library, developed by the National Center for Atmospheric Research (NCAR). The goal is to preserve the numerical behavior of the SP routines while modernizing code structure, improving readability, and ensuring compatibility with contemporary Fortran compilers.

### About MUDPACK SP

* **MUDPACK** is a collection of Fortran subroutines for solving elliptic partial differential equations using multigrid techniques on rectangular grids.
* The **SP package** targets separable, constant-coefficient elliptic problems in 1D, 2D, and 3D Cartesian domains with homogeneous boundary conditions.

### Refactoring Highlights

* Fully converted to Fortran 90 with `MODULE`s and explicit interfaces
* Modern naming conventions and consistent formatting
* Modular code layout for easier maintenance and extension
* No changes to underlying algorithms; numerical results are identical to the original

### Repository Contents

```
src/            # Fortran 90 source files for SP routines
tests/          # Not Ported yet
build/          # Build scripts and Makefile
README.md       # This file
```

### Prerequisites

* A Fortran 90–compatible compiler (e.g., gfortran, ifort)
* GNU Make (or compatible build tool)

### Building

```bash
mkdir -p build && cd build
cmake ..
make
```

### Usage

After building, link the compiled library into your application:

```fortran
use mudpack_sp
! ... call solver routines as documented in the module interfaces
```

### Testing

Run the provided test suite to verify correctness:

```bash
cd build
make test
```

### License

This refactored code is released under the [MIT License](LICENSE).
