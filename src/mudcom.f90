!*==SWK2.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

!
!     file mudcom.f
!
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!     *                                                               *
!     *                  copyright (c) 2008 by UCAR                   *
!     *                                                               *
!     *       University Corporation for Atmospheric Research         *
!     *                                                               *
!     *                      all rights reserved                      *
!     *                                                               *
!     *                     MUDPACK  version 5.0.1                    *
!     *                                                               *
!     *                 A Fortran Package of Multigrid                *
!     *                                                               *
!     *                Subroutines and Example Programs               *
!     *                                                               *
!     *      for Solving Elliptic Partial Differential Equations      *
!     *                                                               *
!     *                             by                                *
!     *                                                               *
!     *                         John Adams                            *
!     *                                                               *
!     *                             of                                *
!     *                                                               *
!     *         the National Center for Atmospheric Research          *
!     *                                                               *
!     *                Boulder, Colorado  (80307)  U.S.A.             *
!     *                                                               *
!     *                   which is sponsored by                       *
!     *                                                               *
!     *              the National Science Foundation                  *
!     *                                                               *
!     * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! ... purpose
!
!     mudcom.f is a file containing subroutines called by some or all
!     of the real two- and three-dimensional mudpack solvers.  It must
!     be loaded with any real mudpack solver.
!
SUBROUTINE swk2(Nfx, Nfy, Phif, Rhsf, Phi, Rhs)
!
!     set phif,rhsf input in arrays which include
!     virtual boundaries for phi (for all 2-d real codes)
!
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nfx
   INTEGER, INTENT(IN) :: Nfy
   REAL, INTENT(IN), DIMENSION(Nfx, Nfy) :: Phif
   REAL, INTENT(IN), DIMENSION(Nfx, Nfy) :: Rhsf
   REAL, INTENT(OUT), DIMENSION(0:Nfx + 1, 0:Nfy + 1) :: Phi
   REAL, INTENT(OUT), DIMENSION(Nfx, Nfy) :: Rhs
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, j
!
! End of declarations rewritten by SPAG
!
   DO j = 1, Nfy
      DO i = 1, Nfx
         Phi(i, j) = Phif(i, j)
         Rhs(i, j) = Rhsf(i, j)
      END DO
   END DO
!
!     set virtual boundaries in phi to zero
!
   DO j = 0, Nfy + 1
      Phi(0, j) = 0.0
      Phi(Nfx + 1, j) = 0.0
   END DO
   DO i = 0, Nfx + 1
      Phi(i, 0) = 0.0
      Phi(i, Nfy + 1) = 0.0
   END DO
END SUBROUTINE swk2
!*==TRSFC2.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE trsfc2(Nx, Ny, Phi, Rhs, Ncx, Ncy, Phic, Rhsc)
!
!     transfer fine grid to coarse grid
!
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nx
   INTEGER, INTENT(IN) :: Ny
   INTEGER, INTENT(IN) :: Ncx
   INTEGER, INTENT(IN) :: Ncy
   REAL, INTENT(IN), DIMENSION(0:Nx + 1, 0:Ny + 1) :: Phi
   REAL, INTENT(IN), DIMENSION(Nx, Ny) :: Rhs
   REAL, INTENT(OUT), DIMENSION(0:Ncx + 1, 0:Ncy + 1) :: Phic
   REAL, INTENT(OUT), DIMENSION(Ncx, Ncy) :: Rhsc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ic, j, jc
!
! End of declarations rewritten by SPAG
!
!
!     set virtual boundaries in phic to zero
!
   DO jc = 0, Ncy + 1
      Phic(0, jc) = 0.0
      Phic(Ncx + 1, jc) = 0.0
   END DO
   DO ic = 0, Ncx + 1
      Phic(ic, 0) = 0.0
      Phic(ic, Ncy + 1) = 0.0
   END DO
   IF (Ncx < Nx .AND. Ncy < Ny) THEN
!
!     coarsening in both x and y
!
      DO jc = 1, Ncy
         j = jc + jc - 1
         DO ic = 1, Ncx
            i = ic + ic - 1
            Phic(ic, jc) = Phi(i, j)
            Rhsc(ic, jc) = Rhs(i, j)
         END DO
      END DO
   ELSEIF (Ncx < Nx .AND. Ncy == Ny) THEN
!
!     coarsening in x only
!
      DO jc = 1, Ncy
         j = jc
         DO ic = 1, Ncx
            i = ic + ic - 1
            Phic(ic, jc) = Phi(i, j)
            Rhsc(ic, jc) = Rhs(i, j)
         END DO
      END DO
   ELSE
!
!     coarsening in y only
!
      DO jc = 1, Ncy
         j = jc + jc - 1
         DO ic = 1, Ncx
            i = ic
            Phic(ic, jc) = Phi(i, j)
            Rhsc(ic, jc) = Rhs(i, j)
         END DO
      END DO
   END IF
END SUBROUTINE trsfc2
!*==RES2.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE res2(Nx, Ny, Resf, Ncx, Ncy, Rhsc, Nxa, Nxb, Nyc, Nyd)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nx
   INTEGER, INTENT(IN) :: Ny
   INTEGER, INTENT(IN) :: Ncx
   INTEGER, INTENT(IN) :: Ncy
   REAL, INTENT(IN), DIMENSION(Nx, Ny) :: Resf
   REAL, INTENT(OUT), DIMENSION(Ncx, Ncy) :: Rhsc
   INTEGER, INTENT(IN) :: Nxa
   INTEGER, INTENT(IN) :: Nxb
   INTEGER, INTENT(IN) :: Nyc
   INTEGER, INTENT(IN) :: Nyd
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ic, im1, ip1, ix, j, jc, jm1, jp1, jy
!
! End of declarations rewritten by SPAG
!
!
!     restrict fine grid residual in resf to coarse grid in rhsc
!     using full weighting for all real 2d codes
!
!
!     set x,y coarsening integer subscript scales
!
   ix = 1
   IF (Ncx == Nx) ix = 0
   jy = 1
   IF (Ncy == Ny) jy = 0
!
!     restrict on interior
!
   IF (Ncy < Ny .AND. Ncx < Nx) THEN
!
!     coarsening in both directions
!
!$OMP PARALLEL DO PRIVATE(i,j,ic,jc), SHARED(resf,rhsc,ncx,ncy)
      DO jc = 2, Ncy - 1
         j = jc + jc - 1
         DO ic = 2, Ncx - 1
            i = ic + ic - 1
            Rhsc(ic,jc) = (Resf(i-1,j-1)+Resf(i+1,j-1)+Resf(i-1,j+1)+Resf(i+1,j+1)                                                 &
                        & + 2.*(Resf(i - 1, j) + Resf(i + 1, j) + Resf(i, j - 1) + Resf(i, j + 1)) + 4.*Resf(i, j))*.0625
         END DO
      END DO
   ELSEIF (Ncy == Ny) THEN
!
!     no coarsening in y but coarsening in x
!
!$OMP PARALLEL DO PRIVATE(i,j,ic,jc), SHARED(resf,rhsc,ncx,ncy)
      DO jc = 2, Ncy - 1
         j = jc
         DO ic = 2, Ncx - 1
            i = ic + ic - 1
            Rhsc(ic,jc) = (Resf(i-1,j-1)+Resf(i+1,j-1)+Resf(i-1,j+1)+Resf(i+1,j+1)                                                 &
                        & + 2.*(Resf(i - 1, j) + Resf(i + 1, j) + Resf(i, j - 1) + Resf(i, j + 1)) + 4.*Resf(i, j))*.0625
         END DO
      END DO
   ELSE
!
!     no coarsening in x but coarsening in y
!
!$OMP PARALLEL DO PRIVATE(i,j,ic,jc), SHARED(resf,rhsc,ncx,ncy)
      DO jc = 2, Ncy - 1
         j = jc + jc - 1
         DO ic = 2, Ncx - 1
            i = ic
            Rhsc(ic,jc) = (Resf(i-1,j-1)+Resf(i+1,j-1)+Resf(i-1,j+1)+Resf(i+1,j+1)                                                 &
                        & + 2.*(Resf(i - 1, j) + Resf(i + 1, j) + Resf(i, j - 1) + Resf(i, j + 1)) + 4.*Resf(i, j))*.0625
         END DO
      END DO
   END IF
!
!     set residual on boundaries
!
   DO jc = 1, Ncy, Ncy - 1
!
!     y=yc,yd boundaries
!
      j = jc + jy*(jc - 1)
      jm1 = max0(j - 1, 2)
      jp1 = min0(j + 1, Ny - 1)
      IF (j == 1 .AND. Nyc == 0) jm1 = Ny - 1
      IF (j == Ny .AND. Nyc == 0) jp1 = 2
!
!     y=yc,yd and x=xa,xb cornors
!
      DO ic = 1, Ncx, Ncx - 1
         i = ic + ix*(ic - 1)
         im1 = max0(i - 1, 2)
         ip1 = min0(i + 1, Nx - 1)
         IF (i == 1 .AND. Nxa == 0) im1 = Nx - 1
         IF (i == Nx .AND. Nxa == 0) ip1 = 2
         Rhsc(ic,jc) = (Resf(im1,jm1)+Resf(ip1,jm1)+Resf(im1,jp1)+Resf(ip1,jp1)+2.*(Resf(im1,j)+Resf(ip1,j)+Resf(i,jm1)+Resf(i,jp1)&
                     & ) + 4.*Resf(i, j))*.0625
      END DO
!
!     set y=yc,yd interior edges
!
      DO ic = 2, Ncx - 1
         i = ic + ix*(ic - 1)
         Rhsc(ic,jc) = (Resf(i-1,jm1)+Resf(i+1,jm1)+Resf(i-1,jp1)+Resf(i+1,jp1)+2.*(Resf(i-1,j)+Resf(i+1,j)+Resf(i,jm1)+Resf(i,jp1)&
                     & ) + 4.*Resf(i, j))*.0625
      END DO
   END DO
!
!     set x=xa,xb interior edges
!
   DO ic = 1, Ncx, Ncx - 1
      i = ic + ix*(ic - 1)
      im1 = max0(i - 1, 2)
      ip1 = min0(i + 1, Nx - 1)
      IF (i == 1 .AND. Nxa == 0) im1 = Nx - 1
      IF (i == Nx .AND. Nxa == 0) ip1 = 2
      DO jc = 2, Ncy - 1
         j = jc + jy*(jc - 1)
         Rhsc(ic,jc) = (Resf(im1,j-1)+Resf(ip1,j-1)+Resf(im1,j+1)+Resf(ip1,j+1)+2.*(Resf(im1,j)+Resf(ip1,j)+Resf(i,j-1)+Resf(i,j+1)&
                     & ) + 4.*Resf(i, j))*.0625
      END DO
   END DO
!
!     set coarse grid residual zero on specified boundaries
!
   IF (Nxa == 1) THEN
      DO jc = 1, Ncy
         Rhsc(1, jc) = 0.0
      END DO
   END IF
   IF (Nxb == 1) THEN
      DO jc = 1, Ncy
         Rhsc(Ncx, jc) = 0.0
      END DO
   END IF
   IF (Nyc == 1) THEN
      DO ic = 1, Ncx
         Rhsc(ic, 1) = 0.0
      END DO
   END IF
   IF (Nyd == 1) THEN
      DO ic = 1, Ncx
         Rhsc(ic, Ncy) = 0.0
      END DO
   END IF
END SUBROUTINE res2
!*==PROLON2.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!
!     prolon2 modified from rgrd2u 11/20/97
!
SUBROUTINE prolon2(Ncx, Ncy, P, Nx, Ny, Q, Nxa, Nxb, Nyc, Nyd, Intpol)
   USE S_PROLON1
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ncx
   INTEGER, INTENT(IN) :: Ncy
   INTEGER :: Nx
   INTEGER, INTENT(IN) :: Ny
   REAL, DIMENSION(0:Ncx + 1, 0:Ncy + 1) :: P
   REAL, INTENT(INOUT), DIMENSION(0:Nx + 1, 0:Ny + 1) :: Q
   INTEGER :: Nxa
   INTEGER :: Nxb
   INTEGER, INTENT(IN) :: Nyc
   INTEGER, INTENT(IN) :: Nyd
   INTEGER :: Intpol
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ifn, ist, j, jc, jfn, joddfn, joddst, jst
!
! End of declarations rewritten by SPAG
!
   ist = 1
   ifn = Nx
   jst = 1
   jfn = Ny
   joddst = 1
   joddfn = Ny
   IF (Nxa == 1) ist = 2
   IF (Nxb == 1) ifn = Nx - 1
   IF (Nyc == 1) THEN
      jst = 2
      joddst = 3
   END IF
   IF (Nyd == 1) THEN
      jfn = Ny - 1
      joddfn = Ny - 2
   END IF
   IF (Intpol == 1 .OR. Ncy < 4) THEN
!
!     linearly interpolate in y
!
      IF (Ncy < Ny) THEN
!
!     ncy grid is an every other point subset of ny grid
!     set odd j lines interpolating in x and then set even
!     j lines by averaging odd j lines
!
         DO j = joddst, joddfn, 2
            jc = j/2 + 1
            CALL prolon1(Ncx, P(0, jc), Nx, Q(0, j), Nxa, Nxb, Intpol)
         END DO
         DO j = 2, jfn, 2
            DO i = ist, ifn
               Q(i, j) = 0.5*(Q(i, j - 1) + Q(i, j + 1))
            END DO
         END DO
!
!     set periodic virtual boundaries if necessary
!
         IF (Nyc == 0) THEN
            DO i = ist, ifn
               Q(i, 0) = Q(i, Ny - 1)
               Q(i, Ny + 1) = Q(i, 2)
            END DO
         END IF
         RETURN
      ELSE
!
!     ncy grid equals ny grid so interpolate in x only
!
         DO j = jst, jfn
            jc = j
            CALL prolon1(Ncx, P(0, jc), Nx, Q(0, j), Nxa, Nxb, Intpol)
         END DO
!
!     set periodic virtual boundaries if necessary
!
         IF (Nyc == 0) THEN
            DO i = ist, ifn
               Q(i, 0) = Q(i, Ny - 1)
               Q(i, Ny + 1) = Q(i, 2)
            END DO
         END IF
         RETURN
      END IF
!
!     cubically interpolate in y
!
   ELSEIF (Ncy < Ny) THEN
!
!     set every other point of ny grid by interpolating in x
!
      DO j = joddst, joddfn, 2
         jc = j/2 + 1
         CALL prolon1(Ncx, P(0, jc), Nx, Q(0, j), Nxa, Nxb, Intpol)
      END DO
!
!     set deep interior of ny grid using values just
!     generated and symmetric cubic interpolation in y
!
      DO j = 4, Ny - 3, 2
         DO i = ist, ifn
            Q(i, j) = (-Q(i, j - 3) + 9.*(Q(i, j - 1) + Q(i, j + 1)) - Q(i, j + 3))*.0625
         END DO
      END DO
!
!     interpolate from q at j=2 and j=ny-1
!
      IF (Nyc /= 0) THEN
!
!     asymmetric formula near nonperiodic y boundaries
!
         DO i = ist, ifn
            Q(i, 2) = (5.*Q(i, 1) + 15.*Q(i, 3) - 5.*Q(i, 5) + Q(i, 7))*.0625
            Q(i, Ny - 1) = (5.*Q(i, Ny) + 15.*Q(i, Ny - 2) - 5.*Q(i, Ny - 4) + Q(i, Ny - 6))*.0625
         END DO
      ELSE
!
!     periodicity in y alows symmetric formula near bndys
!
         DO i = ist, ifn
            Q(i, 2) = (-Q(i, Ny - 2) + 9.*(Q(i, 1) + Q(i, 3)) - Q(i, 5))*.0625
            Q(i, Ny - 1) = (-Q(i, Ny - 4) + 9.*(Q(i, Ny - 2) + Q(i, Ny)) - Q(i, 3))*.0625
            Q(i, Ny + 1) = Q(i, 2)
            Q(i, 0) = Q(i, Ny - 1)
         END DO
      END IF
      RETURN
   ELSE
!
!     ncy grid is equals ny grid so interpolate in x only
!
      DO j = jst, jfn
         jc = j
         CALL prolon1(Ncx, P(0, jc), Nx, Q(0, j), Nxa, Nxb, Intpol)
      END DO
!
!     set periodic virtual boundaries if necessary
!
      IF (Nyc == 0) THEN
         DO i = ist, ifn
            Q(i, 0) = Q(i, Ny - 1)
            Q(i, Ny + 1) = Q(i, 2)
         END DO
      END IF
      RETURN
   END IF
END SUBROUTINE prolon2
!*==PROLON1.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

!
!     11/20/97  modification of rgrd1u.f for mudpack
!
SUBROUTINE prolon1(Ncx, P, Nx, Q, Nxa, Nxb, Intpol)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Ncx
   INTEGER, INTENT(IN) :: Nx
   REAL, INTENT(IN), DIMENSION(0:Ncx + 1) :: P
   REAL, INTENT(INOUT), DIMENSION(0:Nx + 1) :: Q
   INTEGER, INTENT(IN) :: Nxa
   INTEGER, INTENT(IN) :: Nxb
   INTEGER, INTENT(IN) :: Intpol
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ic, ifn, ioddfn, ioddst, ist
!
! End of declarations rewritten by SPAG
!
   ist = 1
   ioddst = 1
   ifn = Nx
   ioddfn = Nx
   IF (Nxa == 1) THEN
      ist = 2
      ioddst = 3
   END IF
   IF (Nxb == 1) THEN
      ifn = Nx - 1
      ioddfn = Nx - 2
   END IF
   IF (Intpol == 1 .OR. Ncx < 4) THEN
!
!     linear interpolation in x
!
      IF (Ncx < Nx) THEN
!
!     every other point of nx grid is ncx grid
!
         DO i = ioddst, ioddfn, 2
            ic = (i + 1)/2
            Q(i) = P(ic)
         END DO
         DO i = 2, ifn, 2
            Q(i) = 0.5*(Q(i - 1) + Q(i + 1))
         END DO
      ELSE
!
!     nx grid equals ncx grid
!
         DO i = ist, ifn
            Q(i) = P(i)
         END DO
      END IF
!
!     set virtual end points if periodic
!
      IF (Nxa == 0) THEN
         Q(0) = Q(Nx - 1)
         Q(Nx + 1) = Q(2)
      END IF
      RETURN
!
!     cubic interpolation in x
!
   ELSEIF (Ncx < Nx) THEN
      DO i = ioddst, ioddfn, 2
         ic = (i + 1)/2
         Q(i) = P(ic)
      END DO
!
!      set deep interior with symmetric formula
!
      DO i = 4, Nx - 3, 2
         Q(i) = (-Q(i - 3) + 9.*(Q(i - 1) + Q(i + 1)) - Q(i + 3))*.0625
      END DO
!
!     interpolate from q at i=2 and i=nx-1
!
      IF (Nxa /= 0) THEN
!
!     asymmetric formula near nonperiodic bndys
!
         Q(2) = (5.*Q(1) + 15.*Q(3) - 5.*Q(5) + Q(7))*.0625
         Q(Nx - 1) = (5.*Q(Nx) + 15.*Q(Nx - 2) - 5.*Q(Nx - 4) + Q(Nx - 6))*.0625
      ELSE
!
!     periodicity in x alows symmetric formula near bndys
!
         Q(2) = (-Q(Nx - 2) + 9.*(Q(1) + Q(3)) - Q(5))*.0625
         Q(Nx - 1) = (-Q(Nx - 4) + 9.*(Q(Nx - 2) + Q(Nx)) - Q(3))*.0625
         Q(Nx + 1) = Q(2)
         Q(0) = Q(Nx - 1)
      END IF
      RETURN
   ELSE
!
!     ncx grid equals nx grid
!
      DO i = ist, ifn
         Q(i) = P(i)
      END DO
      IF (Nxa == 0) THEN
         Q(0) = Q(Nx - 1)
         Q(Nx + 1) = Q(2)
      END IF
      RETURN
   END IF
END SUBROUTINE prolon1
!*==COR2.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE cor2(Nx, Ny, Phif, Ncx, Ncy, Phic, Nxa, Nxb, Nyc, Nyd, Intpol, Phcor)
!
!     add coarse grid correction in phic to fine grid approximation
!     in phif using linear or cubic interpolation
!
   USE S_PROLON2
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nx
   INTEGER :: Ny
   INTEGER :: Ncx
   INTEGER :: Ncy
   REAL, INTENT(INOUT), DIMENSION(0:Nx + 1, 0:Ny + 1) :: Phif
   REAL, DIMENSION(0:Ncx + 1, 0:Ncy + 1) :: Phic
   INTEGER :: Nxa
   INTEGER :: Nxb
   INTEGER :: Nyc
   INTEGER :: Nyd
   INTEGER :: Intpol
   REAL, INTENT(INOUT), DIMENSION(0:Nx + 1, 0:Ny + 1) :: Phcor
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ifn, ist, j, jfn, jst
!
! End of declarations rewritten by SPAG
!
   DO j = 0, Ny + 1
      DO i = 0, Nx + 1
         Phcor(i, j) = 0.0
      END DO
   END DO
!
!     lift correction in phic to fine grid in phcor
!
   CALL prolon2(Ncx, Ncy, Phic, Nx, Ny, Phcor, Nxa, Nxb, Nyc, Nyd, Intpol)
!
!     add correction in phcor to phif on nonspecified boundaries
!
   ist = 1
   ifn = Nx
   jst = 1
   jfn = Ny
   IF (Nxa == 1) ist = 2
   IF (Nxb == 1) ifn = Nx - 1
   IF (Nyc == 1) jst = 2
   IF (Nyd == 1) jfn = Ny - 1
   DO j = jst, jfn
      DO i = ist, ifn
         Phif(i, j) = Phif(i, j) + Phcor(i, j)
      END DO
   END DO
!
!     add periodic points if necessary
!
   IF (Nyc == 0) THEN
      DO i = ist, ifn
         Phif(i, 0) = Phif(i, Ny - 1)
         Phif(i, Ny + 1) = Phif(i, 2)
      END DO
   END IF
   IF (Nxa == 0) THEN
      DO j = jst, jfn
         Phif(0, j) = Phif(Nx - 1, j)
         Phif(Nx + 1, j) = Phif(2, j)
      END DO
   END IF
END SUBROUTINE cor2
!*==PDE2.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE pde2(Nx, Ny, U, I, J, Ux3, Ux4, Uy3, Uy4, Nxa, Nyc)
   USE C_PDE2COM
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nx
   INTEGER, INTENT(IN) :: Ny
   REAL, INTENT(IN), DIMENSION(Nx, Ny) :: U
   INTEGER, INTENT(IN) :: I
   INTEGER, INTENT(IN) :: J
   REAL, INTENT(OUT) :: Ux3
   REAL, INTENT(OUT) :: Ux4
   REAL, INTENT(OUT) :: Uy3
   REAL, INTENT(OUT) :: Uy4
   INTEGER, INTENT(IN) :: Nxa
   INTEGER, INTENT(IN) :: Nyc
!
! End of declarations rewritten by SPAG
!
!
!     use second order approximation in u to estimate (second order)
!     third and fourth partial derivatives in the x and y direction
!     non-symmetric difference formula (derived from the  routine
!     finpdf,findif) are used at and one point in from mixed boundaries.
!
   IF (Nxa /= 0) THEN
!
!     nonperiodic in x
!
      IF (I > 2 .AND. I < Nx - 1) THEN
         Ux3 = (-U(I - 2, J) + 2.0*U(I - 1, J) - 2.0*U(I + 1, J) + U(I + 2, J))/Tdlx3
         Ux4 = (U(I - 2, J) - 4.0*U(I - 1, J) + 6.0*U(I, J) - 4.0*U(I + 1, J) + U(I + 2, J))/Dlx4
      ELSEIF (I == 1) THEN
         Ux3 = (-5.0*U(1, J) + 18.0*U(2, J) - 24.0*U(3, J) + 14.0*U(4, J) - 3.0*U(5, J))/Tdlx3
         Ux4 = (3.0*U(1, J) - 14.0*U(2, J) + 26.0*U(3, J) - 24.0*U(4, J) + 11.0*U(5, J) - 2.0*U(6, J))/Dlx4
      ELSEIF (I == 2) THEN
         Ux3 = (-3.0*U(1, J) + 10.0*U(2, J) - 12.0*U(3, J) + 6.0*U(4, J) - U(5, J))/Tdlx3
         Ux4 = (2.0*U(1, J) - 9.0*U(2, J) + 16.0*U(3, J) - 14.0*U(4, J) + 6.0*U(5, J) - U(6, J))/Dlx4
      ELSEIF (I == Nx - 1) THEN
         Ux3 = (U(Nx - 4, J) - 6.0*U(Nx - 3, J) + 12.0*U(Nx - 2, J) - 10.0*U(Nx - 1, J) + 3.0*U(Nx, J))/Tdlx3
         Ux4 = (-U(Nx - 5, J) + 6.0*U(Nx - 4, J) - 14.0*U(Nx - 3, J) + 16.0*U(Nx - 2, J) - 9.0*U(Nx - 1, J) + 2.0*U(Nx, J))/Dlx4
      ELSEIF (I == Nx) THEN
         Ux3 = (3.0*U(Nx - 4, J) - 14.0*U(Nx - 3, J) + 24.0*U(Nx - 2, J) - 18.0*U(Nx - 1, J) + 5.0*U(Nx, J))/Tdlx3
       Ux4 = (-2.0*U(Nx - 5, J) + 11.0*U(Nx - 4, J) - 24.0*U(Nx - 3, J) + 26.0*U(Nx - 2, J) - 14.0*U(Nx - 1, J) + 3.0*U(Nx, J))/Dlx4
      END IF
!
!     periodic in x
!
   ELSEIF (I > 2 .AND. I < Nx - 1) THEN
      Ux3 = (-U(I - 2, J) + 2.0*U(I - 1, J) - 2.0*U(I + 1, J) + U(I + 2, J))/Tdlx3
      Ux4 = (U(I - 2, J) - 4.0*U(I - 1, J) + 6.0*U(I, J) - 4.0*U(I + 1, J) + U(I + 2, J))/Dlx4
   ELSEIF (I == 1) THEN
      Ux3 = (-U(Nx - 2, J) + 2.0*U(Nx - 1, J) - 2.0*U(2, J) + U(3, J))/Tdlx3
      Ux4 = (U(Nx - 2, J) - 4.0*U(Nx - 1, J) + 6.0*U(1, J) - 4.0*U(2, J) + U(3, J))/Dlx4
   ELSEIF (I == 2) THEN
      Ux3 = (-U(Nx - 1, J) + 2.0*U(1, J) - 2.0*U(3, J) + U(4, J))/(Tdlx3)
      Ux4 = (U(Nx - 1, J) - 4.0*U(1, J) + 6.0*U(2, J) - 4.0*U(3, J) + U(4, J))/Dlx4
   ELSEIF (I == Nx - 1) THEN
      Ux3 = (-U(Nx - 3, J) + 2.0*U(Nx - 2, J) - 2.0*U(1, J) + U(2, J))/Tdlx3
      Ux4 = (U(Nx - 3, J) - 4.0*U(Nx - 2, J) + 6.0*U(Nx - 1, J) - 4.0*U(1, J) + U(2, J))/Dlx4
   ELSEIF (I == Nx) THEN
      Ux3 = (-U(Nx - 2, J) + 2.0*U(Nx - 1, J) - 2.0*U(2, J) + U(3, J))/Tdlx3
      Ux4 = (U(Nx - 2, J) - 4.0*U(Nx - 1, J) + 6.0*U(Nx, J) - 4.0*U(2, J) + U(3, J))/Dlx4
   END IF
!
!     y partial derivatives
!
   IF (Nyc /= 0) THEN
!
!     not periodic in y
!
      IF (J > 2 .AND. J < Ny - 1) THEN
         Uy3 = (-U(I, J - 2) + 2.0*U(I, J - 1) - 2.0*U(I, J + 1) + U(I, J + 2))/Tdly3
         Uy4 = (U(I, J - 2) - 4.0*U(I, J - 1) + 6.0*U(I, J) - 4.0*U(I, J + 1) + U(I, J + 2))/Dly4
      ELSEIF (J == 1) THEN
         Uy3 = (-5.0*U(I, 1) + 18.0*U(I, 2) - 24.0*U(I, 3) + 14.0*U(I, 4) - 3.0*U(I, 5))/Tdly3
         Uy4 = (3.0*U(I, 1) - 14.0*U(I, 2) + 26.0*U(I, 3) - 24.0*U(I, 4) + 11.0*U(I, 5) - 2.0*U(I, 6))/Dly4
      ELSEIF (J == 2) THEN
         Uy3 = (-3.0*U(I, 1) + 10.0*U(I, 2) - 12.0*U(I, 3) + 6.0*U(I, 4) - U(I, 5))/Tdly3
         Uy4 = (2.0*U(I, 1) - 9.0*U(I, 2) + 16.0*U(I, 3) - 14.0*U(I, 4) + 6.0*U(I, 5) - U(I, 6))/Dly4
      ELSEIF (J == Ny - 1) THEN
         Uy3 = (U(I, Ny - 4) - 6.0*U(I, Ny - 3) + 12.0*U(I, Ny - 2) - 10.0*U(I, Ny - 1) + 3.0*U(I, Ny))/Tdly3
         Uy4 = (-U(I, Ny - 5) + 6.0*U(I, Ny - 4) - 14.0*U(I, Ny - 3) + 16.0*U(I, Ny - 2) - 9.0*U(I, Ny - 1) + 2.0*U(I, Ny))/Dly4
      ELSEIF (J == Ny) THEN
         Uy3 = (3.0*U(I, Ny - 4) - 14.0*U(I, Ny - 3) + 24.0*U(I, Ny - 2) - 18.0*U(I, Ny - 1) + 5.0*U(I, Ny))/Tdly3
       Uy4 = (-2.0*U(I, Ny - 5) + 11.0*U(I, Ny - 4) - 24.0*U(I, Ny - 3) + 26.0*U(I, Ny - 2) - 14.0*U(I, Ny - 1) + 3.0*U(I, Ny))/Dly4
      END IF
!
!     periodic in y
!
   ELSEIF (J > 2 .AND. J < Ny - 1) THEN
      Uy3 = (-U(I, J - 2) + 2.0*U(I, J - 1) - 2.0*U(I, J + 1) + U(I, J + 2))/Tdly3
      Uy4 = (U(I, J - 2) - 4.0*U(I, J - 1) + 6.0*U(I, J) - 4.0*U(I, J + 1) + U(I, J + 2))/Dly4
   ELSEIF (J == 1) THEN
      Uy3 = (-U(I, Ny - 2) + 2.0*U(I, Ny - 1) - 2.0*U(I, 2) + U(I, 3))/Tdly3
      Uy4 = (U(I, Ny - 2) - 4.0*U(I, Ny - 1) + 6.0*U(I, 1) - 4.0*U(I, 2) + U(I, 3))/Dly4
   ELSEIF (J == 2) THEN
      Uy3 = (-U(I, Ny - 1) + 2.0*U(I, 1) - 2.0*U(I, 3) + U(I, 4))/(Tdly3)
      Uy4 = (U(I, Ny - 1) - 4.0*U(I, 1) + 6.0*U(I, 2) - 4.0*U(I, 3) + U(I, 4))/Dly4
   ELSEIF (J == Ny - 1) THEN
      Uy3 = (-U(I, Ny - 3) + 2.0*U(I, Ny - 2) - 2.0*U(I, 1) + U(I, 2))/Tdly3
      Uy4 = (U(I, Ny - 3) - 4.0*U(I, Ny - 2) + 6.0*U(I, Ny - 1) - 4.0*U(I, 1) + U(I, 2))/Dly4
   ELSEIF (J == Ny) THEN
      Uy3 = (-U(I, Ny - 2) + 2.0*U(I, Ny - 1) - 2.0*U(I, 2) + U(I, 3))/Tdly3
      Uy4 = (U(I, Ny - 2) - 4.0*U(I, Ny - 1) + 6.0*U(I, Ny) - 4.0*U(I, 2) + U(I, 3))/Dly4
   END IF
END SUBROUTINE pde2
!*==SWK3.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE swk3(Nfx, Nfy, Nfz, Phif, Rhsf, Phi, Rhs)
!
!     set phif,rhsf input in arrays which include
!     virtual boundaries for phi (for all 2-d real codes)
!
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nfx
   INTEGER, INTENT(IN) :: Nfy
   INTEGER, INTENT(IN) :: Nfz
   REAL, INTENT(IN), DIMENSION(Nfx, Nfy, Nfz) :: Phif
   REAL, INTENT(IN), DIMENSION(Nfx, Nfy, Nfz) :: Rhsf
   REAL, INTENT(OUT), DIMENSION(0:Nfx + 1, 0:Nfy + 1, 0:Nfz + 1) :: Phi
   REAL, INTENT(OUT), DIMENSION(Nfx, Nfy, Nfz) :: Rhs
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, j, k
!
! End of declarations rewritten by SPAG
!
   DO k = 1, Nfz
      DO j = 1, Nfy
         DO i = 1, Nfx
            Phi(i, j, k) = Phif(i, j, k)
            Rhs(i, j, k) = Rhsf(i, j, k)
         END DO
      END DO
   END DO
!
!     set virtual boundaries in phi to zero
!
   DO k = 0, Nfz + 1
      DO j = 0, Nfy + 1
         Phi(0, j, k) = 0.0
         Phi(Nfx + 1, j, k) = 0.0
      END DO
   END DO
   DO k = 0, Nfz + 1
      DO i = 0, Nfx + 1
         Phi(i, 0, k) = 0.0
         Phi(i, Nfy + 1, k) = 0.0
      END DO
   END DO
   DO j = 0, Nfy + 1
      DO i = 0, Nfx + 1
         Phi(i, j, 0) = 0.0
         Phi(i, j, Nfz + 1) = 0.0
      END DO
   END DO
END SUBROUTINE swk3
!*==TRSFC3.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE trsfc3(Nx, Ny, Nz, Phi, Rhs, Ncx, Ncy, Ncz, Phic, Rhsc)
!
!     transfer fine grid to coarse grid
!
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nx
   INTEGER, INTENT(IN) :: Ny
   INTEGER, INTENT(IN) :: Nz
   INTEGER, INTENT(IN) :: Ncx
   INTEGER, INTENT(IN) :: Ncy
   INTEGER, INTENT(IN) :: Ncz
   REAL, INTENT(IN), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Phi
   REAL, INTENT(IN), DIMENSION(Nx, Ny, Nz) :: Rhs
   REAL, INTENT(OUT), DIMENSION(0:Ncx + 1, 0:Ncy + 1, 0:Ncz + 1) :: Phic
   REAL, INTENT(OUT), DIMENSION(Ncx, Ncy, Ncz) :: Rhsc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ic, ix, j, jc, jy, k, kc, kz
!
! End of declarations rewritten by SPAG
!
!
!     set virtual boundaries in phic to zero
!
   DO kc = 0, Ncz + 1
      DO jc = 0, Ncy + 1
         Phic(0, jc, kc) = 0.0
         Phic(Ncx + 1, jc, kc) = 0.0
      END DO
   END DO
   DO kc = 0, Ncz + 1
      DO ic = 0, Ncx + 1
         Phic(ic, 0, kc) = 0.0
         Phic(ic, Ncy + 1, kc) = 0.0
      END DO
   END DO
   DO jc = 0, Ncy + 1
      DO ic = 0, Ncx + 1
         Phic(ic, jc, 0) = 0.0
         Phic(ic, jc, Ncz + 1) = 0.0
      END DO
   END DO
   IF (Ncx < Nx .AND. Ncy < Ny .AND. Ncz < Nz) THEN
!
!     coarsening in x,y,z (usually the case?)
!
      DO kc = 1, Ncz
         k = kc + kc - 1
         DO jc = 1, Ncy
            j = jc + jc - 1
            DO ic = 1, Ncx
               i = ic + ic - 1
               Phic(ic, jc, kc) = Phi(i, j, k)
               Rhsc(ic, jc, kc) = Rhs(i, j, k)
            END DO
         END DO
      END DO
   ELSE
!
!     no coarsening in at least one dimension
!
      ix = 1
      IF (Ncx == Nx) ix = 0
      jy = 1
      IF (Ncy == Ny) jy = 0
      kz = 1
      IF (Ncz == Nz) kz = 0

      DO kc = 1, Ncz
         k = kc + kz*(kc - 1)
         DO jc = 1, Ncy
            j = jc + jy*(jc - 1)
            DO ic = 1, Ncx
               i = ic + ix*(ic - 1)
               Phic(ic, jc, kc) = Phi(i, j, k)
               Rhsc(ic, jc, kc) = Rhs(i, j, k)
            END DO
         END DO
      END DO
   END IF
END SUBROUTINE trsfc3
!*==RES3.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE res3(Nx, Ny, Nz, Resf, Ncx, Ncy, Ncz, Rhsc, Nxa, Nxb, Nyc, Nyd, Nze, Nzf)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nx
   INTEGER, INTENT(IN) :: Ny
   INTEGER, INTENT(IN) :: Nz
   INTEGER, INTENT(IN) :: Ncx
   INTEGER, INTENT(IN) :: Ncy
   INTEGER, INTENT(IN) :: Ncz
   REAL, INTENT(IN), DIMENSION(Nx, Ny, Nz) :: Resf
   REAL, INTENT(OUT), DIMENSION(Ncx, Ncy, Ncz) :: Rhsc
   INTEGER, INTENT(IN) :: Nxa
   INTEGER, INTENT(IN) :: Nxb
   INTEGER, INTENT(IN) :: Nyc
   INTEGER, INTENT(IN) :: Nyd
   INTEGER, INTENT(IN) :: Nze
   INTEGER, INTENT(IN) :: Nzf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ic, im1, ip1, ix, j, jc, jm1, jp1, jy, k, kc, km1, kp1, kz
   REAL :: rk, rm, rp
!
! End of declarations rewritten by SPAG
!
!
!     restrict fine grid residual in resf to coarse grid in rhsc
!     using full weighting
!
!
!     set x,y,z coarsening integer subscript scales
!
   ix = 1
   IF (Ncx == Nx) ix = 0
   jy = 1
   IF (Ncy == Ny) jy = 0
   kz = 1
   IF (Ncz == Nz) kz = 0
!
!     restrict on interior
!
   IF (Ncz < Nz .AND. Ncy < Ny .AND. Ncx < Nx) THEN
!
!     coarsening in x,y,z
!
!$OMP PARALLEL DO PRIVATE(i,j,k,ic,jc,kc,rm,rk,rp)
!$OMP+SHARED(resf,rhsc,ncx,ncy,ncz)
      DO kc = 2, Ncz - 1
         k = kc + kc - 1
         DO jc = 2, Ncy - 1
            j = jc + jc - 1
            DO ic = 2, Ncx - 1
               i = ic + ic - 1
!
!     weight on k-1,k,k+1 z planes in rm,rk,rp
!
               rm = (Resf(i-1,j-1,k-1)+Resf(i+1,j-1,k-1)+Resf(i-1,j+1,k-1)+Resf(i+1,j+1,k-1)                                       &
& + 2.*(Resf(i - 1, j, k - 1) + Resf(i + 1, j, k - 1) + Resf(i, j - 1, k - 1) + Resf(i, j + 1, k - 1)) + 4.*Resf(i, j, k - 1))*.0625
               rk = (Resf(i-1,j-1,k)+Resf(i+1,j-1,k)+Resf(i-1,j+1,k)+Resf(i+1,j+1,k)+2.*(Resf(i-1,j,k)+Resf(i+1,j,k)+Resf(i,j-1,k) &
                  & + Resf(i, j + 1, k)) + 4.*Resf(i, j, k))*.0625
               rp = (Resf(i-1,j-1,k+1)+Resf(i+1,j-1,k+1)+Resf(i-1,j+1,k+1)+Resf(i+1,j+1,k+1)                                       &
& + 2.*(Resf(i - 1, j, k + 1) + Resf(i + 1, j, k + 1) + Resf(i, j - 1, k + 1) + Resf(i, j + 1, k + 1)) + 4.*Resf(i, j, k + 1))*.0625
!
!     weight in z direction for final result
!
               Rhsc(ic, jc, kc) = 0.25*(rm + 2.*rk + rp)
            END DO
         END DO
      END DO
   ELSE
!
!     allow for noncoarsening in any of x,y,z
!
!$OMP PARALLEL DO PRIVATE(i,j,k,ic,jc,kc,rm,rk,rp)
!$OMP+SHARED(ix,jy,kz,resf,rhsc,ncx,ncy,ncz)
      DO kc = 2, Ncz - 1
         k = kc + kz*(kc - 1)
         DO jc = 2, Ncy - 1
            j = jc + jy*(jc - 1)
            DO ic = 2, Ncx - 1
               i = ic + ix*(ic - 1)
!
!     weight on k-1,k,k+1 z planes in rm,rk,rp
!
               rm = (Resf(i-1,j-1,k-1)+Resf(i+1,j-1,k-1)+Resf(i-1,j+1,k-1)+Resf(i+1,j+1,k-1)                                       &
& + 2.*(Resf(i - 1, j, k - 1) + Resf(i + 1, j, k - 1) + Resf(i, j - 1, k - 1) + Resf(i, j + 1, k - 1)) + 4.*Resf(i, j, k - 1))*.0625
               rk = (Resf(i-1,j-1,k)+Resf(i+1,j-1,k)+Resf(i-1,j+1,k)+Resf(i+1,j+1,k)+2.*(Resf(i-1,j,k)+Resf(i+1,j,k)+Resf(i,j-1,k) &
                  & + Resf(i, j + 1, k)) + 4.*Resf(i, j, k))*.0625
               rp = (Resf(i-1,j-1,k+1)+Resf(i+1,j-1,k+1)+Resf(i-1,j+1,k+1)+Resf(i+1,j+1,k+1)                                       &
& + 2.*(Resf(i - 1, j, k + 1) + Resf(i + 1, j, k + 1) + Resf(i, j - 1, k + 1) + Resf(i, j + 1, k + 1)) + 4.*Resf(i, j, k + 1))*.0625
!
!     weight in z direction for final result
!
               Rhsc(ic, jc, kc) = 0.25*(rm + 2.*rk + rp)
            END DO
         END DO
      END DO
   END IF
!
!     set residual on boundaries
!
   DO ic = 1, Ncx, Ncx - 1
!
!     x=xa and x=xb
!
      i = ic + ix*(ic - 1)
      im1 = max0(i - 1, 2)
      ip1 = min0(i + 1, Nx - 1)
      IF (i == 1 .AND. Nxa == 0) im1 = Nx - 1
      IF (i == Nx .AND. Nxb == 0) ip1 = 2
!
!    (y,z) interior
!
!$OMP PARALLEL DO PRIVATE(j,k,jc,kc,rm,rk,rp)
!$OMP+SHARED(kz,jy,ic,im1,i,ip1,resf,rhsc,ncy,ncz)
      DO kc = 2, Ncz - 1
         k = kc + kz*(kc - 1)
         DO jc = 2, Ncy - 1
            j = jc + jy*(jc - 1)
            rm = (Resf(im1,j-1,k-1)+Resf(ip1,j-1,k-1)+Resf(im1,j+1,k-1)+Resf(ip1,j+1,k-1)                                          &
    & + 2.*(Resf(im1, j, k - 1) + Resf(ip1, j, k - 1) + Resf(i, j - 1, k - 1) + Resf(i, j + 1, k - 1)) + 4.*Resf(i, j, k - 1))*.0625
            rk = (Resf(im1,j-1,k)+Resf(ip1,j-1,k)+Resf(im1,j+1,k)+Resf(ip1,j+1,k)+2.*(Resf(im1,j,k)+Resf(ip1,j,k)+Resf(i,j-1,k)    &
               & + Resf(i, j + 1, k)) + 4.*Resf(i, j, k))*.0625
            rp = (Resf(im1,j-1,k+1)+Resf(ip1,j-1,k+1)+Resf(im1,j+1,k+1)+Resf(ip1,j+1,k+1)                                          &
    & + 2.*(Resf(im1, j, k + 1) + Resf(ip1, j, k + 1) + Resf(i, j - 1, k + 1) + Resf(i, j + 1, k + 1)) + 4.*Resf(i, j, k + 1))*.0625
            Rhsc(ic, jc, kc) = 0.25*(rm + 2.*rk + rp)
         END DO
      END DO
!
!     x=xa,xb and y=yc,yd interior edges
!
      DO jc = 1, Ncy, Ncy - 1
         j = jc + jy*(jc - 1)
         jm1 = max0(j - 1, 2)
         jp1 = min0(j + 1, Ny - 1)
         IF (j == 1 .AND. Nyc == 0) jm1 = Ny - 1
         IF (j == Ny .AND. Nyc == 0) jp1 = 2
         DO kc = 2, Ncz - 1
            k = kc + kz*(kc - 1)
            rm = (Resf(im1,jm1,k-1)+Resf(ip1,jm1,k-1)+Resf(im1,jp1,k-1)+Resf(ip1,jp1,k-1)                                          &
        & + 2.*(Resf(im1, j, k - 1) + Resf(ip1, j, k - 1) + Resf(i, jm1, k - 1) + Resf(i, jp1, k - 1)) + 4.*Resf(i, j, k - 1))*.0625
            rk = (Resf(im1,jm1,k)+Resf(ip1,jm1,k)+Resf(im1,jp1,k)+Resf(ip1,jp1,k)+2.*(Resf(im1,j,k)+Resf(ip1,j,k)+Resf(i,jm1,k)    &
               & + Resf(i, jp1, k)) + 4.*Resf(i, j, k))*.0625
            rp = (Resf(im1,jm1,k+1)+Resf(ip1,jm1,k+1)+Resf(im1,jp1,k+1)+Resf(ip1,jp1,k+1)                                          &
        & + 2.*(Resf(im1, j, k + 1) + Resf(ip1, j, k + 1) + Resf(i, jm1, k + 1) + Resf(i, jp1, k + 1)) + 4.*Resf(i, j, k + 1))*.0625
            Rhsc(ic, jc, kc) = 0.25*(rm + 2.*rk + rp)
         END DO
!     x=xa,xb; y=yc,yd; z=ze,zf cornors
         DO kc = 1, Ncz, Ncz - 1
            k = kc + kz*(kc - 1)
            km1 = max0(k - 1, 2)
            kp1 = min0(k + 1, Nz - 1)
            IF (k == 1 .AND. Nze == 0) km1 = Nz - 1
            IF (k == Nz .AND. Nzf == 0) kp1 = 2
            rm = (Resf(im1,jm1,km1)+Resf(ip1,jm1,km1)+Resf(im1,jp1,km1)+Resf(ip1,jp1,km1)                                          &
               & + 2.*(Resf(im1, j, km1) + Resf(ip1, j, km1) + Resf(i, jm1, km1) + Resf(i, jp1, km1)) + 4.*Resf(i, j, km1))*.0625
            rk = (Resf(im1,jm1,k)+Resf(ip1,jm1,k)+Resf(im1,jp1,k)+Resf(ip1,jp1,k)+2.*(Resf(im1,j,k)+Resf(ip1,j,k)+Resf(i,jm1,k)    &
               & + Resf(i, jp1, k)) + 4.*Resf(i, j, k))*.0625
            rp = (Resf(im1,jm1,kp1)+Resf(ip1,jm1,kp1)+Resf(im1,jp1,kp1)+Resf(ip1,jp1,kp1)                                          &
               & + 2.*(Resf(im1, j, kp1) + Resf(ip1, j, kp1) + Resf(i, jm1, kp1) + Resf(i, jp1, kp1)) + 4.*Resf(i, j, kp1))*.0625
            Rhsc(ic, jc, kc) = 0.25*(rm + 2.*rk + rp)
         END DO
      END DO
!
!      x=xa,xb and z=ze,zf edges
!
      DO kc = 1, Ncz, Ncz - 1
         k = kc + kz*(kc - 1)
         km1 = max0(k - 1, 2)
         kp1 = min0(k + 1, Nz - 1)
         IF (k == 1 .AND. Nze == 0) km1 = Nz - 1
         IF (k == Nz .AND. Nzf == 0) kp1 = 2
         DO jc = 2, Ncy - 1
            j = jc + jy*(jc - 1)
            rm = (Resf(im1,j-1,km1)+Resf(ip1,j-1,km1)+Resf(im1,j+1,km1)+Resf(ip1,j+1,km1)                                          &
              & + 2.*(Resf(im1, j, km1) + Resf(ip1, j, km1) + Resf(i, j - 1, km1) + Resf(i, j + 1, km1)) + 4.*Resf(i, j, km1))*.0625
            rk = (Resf(im1,j-1,k)+Resf(ip1,j-1,k)+Resf(im1,j+1,k)+Resf(ip1,j+1,k)+2.*(Resf(im1,j,k)+Resf(ip1,j,k)+Resf(i,j-1,k)    &
               & + Resf(i, j + 1, k)) + 4.*Resf(i, j, k))*.0625
            rp = (Resf(im1,j-1,kp1)+Resf(ip1,j-1,kp1)+Resf(im1,j+1,kp1)+Resf(ip1,j+1,kp1)                                          &
              & + 2.*(Resf(im1, j, kp1) + Resf(ip1, j, kp1) + Resf(i, j - 1, kp1) + Resf(i, j + 1, kp1)) + 4.*Resf(i, j, kp1))*.0625
            Rhsc(ic, jc, kc) = 0.25*(rm + 2.*rk + rp)
         END DO
      END DO
   END DO
!
!     y boundaries y=yc and y=yd
!
   DO jc = 1, Ncy, Ncy - 1
      j = jc + jy*(jc - 1)
      jm1 = max0(j - 1, 2)
      jp1 = min0(j + 1, Ny - 1)
      IF (j == 1 .AND. Nyc == 0) jm1 = Ny - 1
      IF (j == Ny .AND. Nyd == 0) jp1 = 2
!
!     (x,z) interior
!
!$OMP PARALLEL DO PRIVATE(i,k,ic,kc,rm,rk,rp)
!$OMP+SHARED(ix,kz,jc,jm1,j,jp1,resf,rhsc,ncx,ncz)
      DO kc = 2, Ncz - 1
         k = kc + kz*(kc - 1)
         DO ic = 2, Ncx - 1
            i = ic + ix*(ic - 1)
            rm = (Resf(i-1,jm1,k-1)+Resf(i+1,jm1,k-1)+Resf(i-1,jp1,k-1)+Resf(i+1,jp1,k-1)                                          &
    & + 2.*(Resf(i - 1, j, k - 1) + Resf(i + 1, j, k - 1) + Resf(i, jm1, k - 1) + Resf(i, jp1, k - 1)) + 4.*Resf(i, j, k - 1))*.0625
            rk = (Resf(i-1,jm1,k)+Resf(i+1,jm1,k)+Resf(i-1,jp1,k)+Resf(i+1,jp1,k)+2.*(Resf(i-1,j,k)+Resf(i+1,j,k)+Resf(i,jm1,k)    &
               & + Resf(i, jp1, k)) + 4.*Resf(i, j, k))*.0625
            rp = (Resf(i-1,jm1,k+1)+Resf(i+1,jm1,k+1)+Resf(i-1,jp1,k+1)+Resf(i+1,jp1,k+1)                                          &
    & + 2.*(Resf(i - 1, j, k + 1) + Resf(i + 1, j, k + 1) + Resf(i, jm1, k + 1) + Resf(i, jp1, k + 1)) + 4.*Resf(i, j, k + 1))*.0625
            Rhsc(ic, jc, kc) = 0.25*(rm + 2.*rk + rp)
         END DO
      END DO
!
!     y=yc,yd and z=ze,zf edges
!
      DO kc = 1, Ncz, Ncz - 1
         k = kc + kz*(kc - 1)
         km1 = max0(k - 1, 2)
         kp1 = min0(k + 1, Nz - 1)
         IF (k == 1 .AND. Nze == 0) km1 = Nz - 1
         IF (k == Nz .AND. Nzf == 0) kp1 = 2
!
!     interior in x
!
         DO ic = 2, Ncx - 1
            i = ic + ix*(ic - 1)
            rm = (Resf(i-1,jm1,km1)+Resf(i+1,jm1,km1)+Resf(i-1,jp1,km1)+Resf(i+1,jp1,km1)                                          &
              & + 2.*(Resf(i - 1, j, km1) + Resf(i + 1, j, km1) + Resf(i, jm1, km1) + Resf(i, jp1, km1)) + 4.*Resf(i, j, km1))*.0625
            rk = (Resf(i-1,jm1,k)+Resf(i+1,jm1,k)+Resf(i-1,jp1,k)+Resf(i+1,jp1,k)+2.*(Resf(i-1,j,k)+Resf(i+1,j,k)+Resf(i,jm1,k)    &
               & + Resf(i, jp1, k)) + 4.*Resf(i, j, k))*.0625
            rp = (Resf(i-1,jm1,kp1)+Resf(i+1,jm1,kp1)+Resf(i-1,jp1,kp1)+Resf(i+1,jp1,kp1)                                          &
              & + 2.*(Resf(i - 1, j, kp1) + Resf(i + 1, j, kp1) + Resf(i, jm1, kp1) + Resf(i, jp1, kp1)) + 4.*Resf(i, j, kp1))*.0625
            Rhsc(ic, jc, kc) = 0.25*(rm + 2.*rk + rp)
         END DO
      END DO
   END DO
!
!     z=ze,zf boundaries
!
   DO kc = 1, Ncz, Ncz - 1
      k = kc + kz*(kc - 1)
      km1 = max0(k - 1, 2)
      kp1 = min0(k + 1, Nz - 1)
      IF (k == 1 .AND. Nze == 0) km1 = Nz - 1
      IF (k == Nz .AND. Nzf == 0) kp1 = 2
!
!     (x,y) interior
!
!$OMP PARALLEL DO PRIVATE(i,j,ic,jc,rm,rk,rp)
!$OMP+SHARED(ix,jy,kc,km1,k,kp1,resf,rhsc,ncx,ncz)
      DO jc = 2, Ncy - 1
         j = jc + jy*(jc - 1)
         DO ic = 2, Ncx - 1
            i = ic + ix*(ic - 1)
            rm = (Resf(i-1,j-1,km1)+Resf(i+1,j-1,km1)+Resf(i-1,j+1,km1)+Resf(i+1,j+1,km1)                                          &
          & + 2.*(Resf(i - 1, j, km1) + Resf(i + 1, j, km1) + Resf(i, j - 1, km1) + Resf(i, j + 1, km1)) + 4.*Resf(i, j, km1))*.0625
            rk = (Resf(i-1,j-1,k)+Resf(i+1,j-1,k)+Resf(i-1,j+1,k)+Resf(i+1,j+1,k)+2.*(Resf(i-1,j,k)+Resf(i+1,j,k)+Resf(i,j-1,k)    &
               & + Resf(i, j + 1, k)) + 4.*Resf(i, j, k))*.0625
            rp = (Resf(i-1,j-1,kp1)+Resf(i+1,j-1,kp1)+Resf(i-1,j+1,kp1)+Resf(i+1,j+1,kp1)                                          &
          & + 2.*(Resf(i - 1, j, kp1) + Resf(i + 1, j, kp1) + Resf(i, j - 1, kp1) + Resf(i, j + 1, kp1)) + 4.*Resf(i, j, kp1))*.0625
            Rhsc(ic, jc, kc) = 0.25*(rm + 2.*rk + rp)
         END DO
      END DO
   END DO
!
!     set coarse grid residual to zero at specified boundaries
!
   IF (Nxa == 1) THEN
      ic = 1
      DO kc = 1, Ncz
         DO jc = 1, Ncy
            Rhsc(ic, jc, kc) = 0.0
         END DO
      END DO
   END IF
   IF (Nxb == 1) THEN
      ic = Ncx
      DO kc = 1, Ncz
         DO jc = 1, Ncy
            Rhsc(ic, jc, kc) = 0.0
         END DO
      END DO
   END IF
   IF (Nyc == 1) THEN
      jc = 1
      DO kc = 1, Ncz
         DO ic = 1, Ncx
            Rhsc(ic, jc, kc) = 0.0
         END DO
      END DO
   END IF
   IF (Nyd == 1) THEN
      jc = Ncy
      DO kc = 1, Ncz
         DO ic = 1, Ncx
            Rhsc(ic, jc, kc) = 0.0
         END DO
      END DO
   END IF
   IF (Nze == 1) THEN
      kc = 1
      DO jc = 1, Ncy
         DO ic = 1, Ncx
            Rhsc(ic, jc, kc) = 0.0
         END DO
      END DO
   END IF
   IF (Nzf == 1) THEN
      kc = Ncz
      DO jc = 1, Ncy
         DO ic = 1, Ncx
            Rhsc(ic, jc, kc) = 0.0
         END DO
      END DO
   END IF
END SUBROUTINE res3
!*==PROLON3.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

!
!     prolon3 modified from prolon2 11/25/97
!
SUBROUTINE prolon3(Ncx, Ncy, Ncz, P, Nx, Ny, Nz, Q, Nxa, Nxb, Nyc, Nyd, Nze, Nzf, Intpol)
   USE S_PROLON2
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Ncx
   INTEGER :: Ncy
   INTEGER, INTENT(IN) :: Ncz
   INTEGER :: Nx
   INTEGER :: Ny
   INTEGER, INTENT(IN) :: Nz
   REAL, DIMENSION(0:Ncx + 1, 0:Ncy + 1, 0:Ncz + 1) :: P
   REAL, INTENT(INOUT), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Q
   INTEGER :: Nxa
   INTEGER :: Nxb
   INTEGER :: Nyc
   INTEGER :: Nyd
   INTEGER, INTENT(IN) :: Nze
   INTEGER, INTENT(IN) :: Nzf
   INTEGER :: Intpol
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ifn, ist, j, jfn, jst, k, kc, kfn, koddfn, koddst, kst
!
! End of declarations rewritten by SPAG
!
   ist = 1
   ifn = Nx
   jst = 1
   jfn = Ny
   kst = 1
   kfn = Nz
   koddst = 1
   koddfn = Nz
   IF (Nxa == 1) ist = 2
   IF (Nxb == 1) ifn = Nx - 1
   IF (Nyc == 1) jst = 2
   IF (Nyd == 1) jfn = Ny - 1
   IF (Nze == 1) THEN
      kst = 2
      koddst = 3
   END IF
   IF (Nzf == 1) THEN
      kfn = Nz - 1
      koddfn = Nz - 2
   END IF
   IF (Intpol == 1 .OR. Ncz < 4) THEN
!
!     linearly interpolate in z
!
      IF (Ncz < Nz) THEN
!
!     ncz grid is an every other point subset of nz grid
!     set odd k planes interpolating in x&y and then set even
!     k planes by averaging odd k planes
!
         DO k = koddst, koddfn, 2
            kc = k/2 + 1
            CALL prolon2(Ncx, Ncy, P(0, 0, kc), Nx, Ny, Q(0, 0, k), Nxa, Nxb, Nyc, Nyd, Intpol)
         END DO
         DO k = 2, kfn, 2
            DO j = jst, jfn
               DO i = ist, ifn
                  Q(i, j, k) = 0.5*(Q(i, j, k - 1) + Q(i, j, k + 1))
               END DO
            END DO
         END DO
!
!     set periodic virtual boundaries if necessary
!
         IF (Nze == 0) THEN
            DO j = jst, jfn
               DO i = ist, ifn
                  Q(i, j, 0) = Q(i, j, Nz - 1)
                  Q(i, j, Nz + 1) = Q(i, j, 2)
               END DO
            END DO
         END IF
         RETURN
      ELSE
!
!     ncz grid is equals nz grid so interpolate in x&y only
!
         DO k = kst, kfn
            kc = k
            CALL prolon2(Ncx, Ncy, P(0, 0, kc), Nx, Ny, Q(0, 0, k), Nxa, Nxb, Nyc, Nyd, Intpol)
         END DO
!
!     set periodic virtual boundaries if necessary
!
         IF (Nze == 0) THEN
            DO j = jst, jfn
               DO i = ist, ifn
                  Q(i, j, 0) = Q(i, j, Nz - 1)
                  Q(i, j, Nz + 1) = Q(i, j, 2)
               END DO
            END DO
         END IF
         RETURN
      END IF
!
!     cubically interpolate in z
!
   ELSEIF (Ncz < Nz) THEN
!
!     set every other point of nz grid by interpolating in x&y
!
      DO k = koddst, koddfn, 2
         kc = k/2 + 1
         CALL prolon2(Ncx, Ncy, P(0, 0, kc), Nx, Ny, Q(0, 0, k), Nxa, Nxb, Nyc, Nyd, Intpol)
      END DO
!
!     set deep interior of nz grid using values just
!     generated and symmetric cubic interpolation in z
!
      DO k = 4, Nz - 3, 2
         DO j = jst, jfn
            DO i = ist, ifn
               Q(i, j, k) = (-Q(i, j, k - 3) + 9.*(Q(i, j, k - 1) + Q(i, j, k + 1)) - Q(i, j, k + 3))*.0625
            END DO
         END DO
      END DO
!
!     interpolate from q at k=2 and k=nz-1
!
      IF (Nze /= 0) THEN
!
!     asymmetric formula near nonperiodic z boundaries
!
         DO j = jst, jfn
            DO i = ist, ifn
               Q(i, j, 2) = (5.*Q(i, j, 1) + 15.*Q(i, j, 3) - 5.*Q(i, j, 5) + Q(i, j, 7))*.0625
               Q(i, j, Nz - 1) = (5.*Q(i, j, Nz) + 15.*Q(i, j, Nz - 2) - 5.*Q(i, j, Nz - 4) + Q(i, j, Nz - 6))*.0625
            END DO
         END DO
      ELSE
!
!     periodicity in y alows symmetric formula near bndys
!
         DO j = jst, jfn
            DO i = ist, ifn
               Q(i, j, 2) = (-Q(i, j, Nz - 2) + 9.*(Q(i, j, 1) + Q(i, j, 3)) - Q(i, j, 5))*.0625
               Q(i, j, Nz - 1) = (-Q(i, j, Nz - 4) + 9.*(Q(i, j, Nz - 2) + Q(i, j, Nz)) - Q(i, j, 3))*.0625
               Q(i, j, Nz + 1) = Q(i, j, 2)
               Q(i, j, 0) = Q(i, j, Nz - 1)
            END DO
         END DO
      END IF
      RETURN
   ELSE
!
!     ncz grid is equals nx grid so interpolate in x&y only
!
      DO k = kst, kfn
         kc = k
         CALL prolon2(Ncx, Ncy, P(0, 0, kc), Nx, Ny, Q(0, 0, k), Nxa, Nxb, Nyc, Nyd, Intpol)
      END DO
!
!     set periodic virtual boundaries if necessary
!
      IF (Nze == 0) THEN
         DO j = jst, jfn
            DO i = ist, ifn
               Q(i, j, 0) = Q(i, j, Nz - 1)
               Q(i, j, Nz + 1) = Q(i, j, 2)
            END DO
         END DO
      END IF
      RETURN
   END IF
END SUBROUTINE prolon3
!*==COR3.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE cor3(Nx, Ny, Nz, Phif, Ncx, Ncy, Ncz, Phic, Nxa, Nxb, Nyc, Nyd, Nze, Nzf, Intpol, Phcor)
   USE S_PROLON3
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nx
   INTEGER :: Ny
   INTEGER :: Nz
   INTEGER :: Ncx
   INTEGER :: Ncy
   INTEGER :: Ncz
   REAL, INTENT(INOUT), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Phif
   REAL, DIMENSION(0:Ncx + 1, 0:Ncy + 1, 0:Ncz + 1) :: Phic
   INTEGER :: Nxa
   INTEGER :: Nxb
   INTEGER :: Nyc
   INTEGER :: Nyd
   INTEGER :: Nze
   INTEGER :: Nzf
   INTEGER :: Intpol
   REAL, INTENT(INOUT), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Phcor
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ifn, ist, j, jfn, jst, k, kfn, kst
!
! End of declarations rewritten by SPAG
!
!
!     add coarse grid correction in phic to fine grid approximation
!     in phif using linear or cubic interpolation
!
   DO k = 0, Nz + 1
      DO j = 0, Ny + 1
         DO i = 0, Nx + 1
            Phcor(i, j, k) = 0.0
         END DO
      END DO
   END DO
!
!     lift correction in phic to fine grid in phcor
!
   CALL prolon3(Ncx, Ncy, Ncz, Phic, Nx, Ny, Nz, Phcor, Nxa, Nxb, Nyc, Nyd, Nze, Nzf, Intpol)
!
!     add correction in phcor to phif on nonspecified boundaries
!
   ist = 1
   ifn = Nx
   jst = 1
   jfn = Ny
   kst = 1
   kfn = Nz
   IF (Nxa == 1) ist = 2
   IF (Nxb == 1) ifn = Nx - 1
   IF (Nyc == 1) jst = 2
   IF (Nyd == 1) jfn = Ny - 1
   IF (Nze == 1) kst = 2
   IF (Nzf == 1) kfn = Nz - 1
   DO k = kst, kfn
      DO j = jst, jfn
         DO i = ist, ifn
            Phif(i, j, k) = Phif(i, j, k) + Phcor(i, j, k)
         END DO
      END DO
   END DO
!
!     add periodic points if necessary
!
   IF (Nze == 0) THEN
      DO j = jst, jfn
         DO i = ist, ifn
            Phif(i, j, 0) = Phif(i, j, Nz - 1)
            Phif(i, j, Nz + 1) = Phif(i, j, 2)
         END DO
      END DO
   END IF
   IF (Nyc == 0) THEN
      DO k = kst, kfn
         DO i = ist, ifn
            Phif(i, 0, k) = Phif(i, Ny - 1, k)
            Phif(i, Ny + 1, k) = Phif(i, 2, k)
         END DO
      END DO
   END IF
   IF (Nxa == 0) THEN
      DO k = kst, kfn
         DO j = jst, jfn
            Phif(0, j, k) = Phif(Nx - 1, j, k)
            Phif(Nx + 1, j, k) = Phif(2, j, k)
         END DO
      END DO
   END IF
END SUBROUTINE cor3
!*==PER3VB.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE per3vb(Nx, Ny, Nz, Phi, Nxa, Nyc, Nze)
!
!     set virtual periodic boundaries from interior values
!     in three dimensions (for all 3-d solvers)
!
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nx
   INTEGER, INTENT(IN) :: Ny
   INTEGER, INTENT(IN) :: Nz
   REAL, INTENT(INOUT), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Phi
   INTEGER, INTENT(IN) :: Nxa
   INTEGER, INTENT(IN) :: Nyc
   INTEGER, INTENT(IN) :: Nze
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, j, k
!
! End of declarations rewritten by SPAG
!
   IF (Nxa == 0) THEN
      DO k = 1, Nz
         DO j = 1, Ny
            Phi(0, j, k) = Phi(Nx - 1, j, k)
            Phi(Nx, j, k) = Phi(1, j, k)
            Phi(Nx + 1, j, k) = Phi(2, j, k)
         END DO
      END DO
   END IF
   IF (Nyc == 0) THEN
      DO k = 1, Nz
         DO i = 1, Nx
            Phi(i, 0, k) = Phi(i, Ny - 1, k)
            Phi(i, Ny, k) = Phi(i, 1, k)
            Phi(i, Ny + 1, k) = Phi(i, 2, k)
         END DO
      END DO
   END IF
   IF (Nze == 0) THEN
      DO j = 1, Ny
         DO i = 1, Nx
            Phi(i, j, 0) = Phi(i, j, Nz - 1)
            Phi(i, j, Nz) = Phi(i, j, 1)
            Phi(i, j, Nz + 1) = Phi(i, j, 2)
         END DO
      END DO
   END IF
END SUBROUTINE per3vb
!*==PDE2CR.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE pde2cr(Nx, Ny, U, I, J, Ux3y, Uxy3, Ux2y2)
!
!     compute mixed partial derivative approximations
!
   USE C_COM2DCR
   USE C_FMUD2CR
   USE C_IMUD2CR
   USE C_PDE2COM
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nx
   INTEGER, INTENT(IN) :: Ny
   REAL, INTENT(IN), DIMENSION(Nx, Ny) :: U
   INTEGER, INTENT(IN) :: I
   INTEGER, INTENT(IN) :: J
   REAL, INTENT(OUT) :: Ux3y
   REAL, INTENT(OUT) :: Uxy3
   REAL, INTENT(OUT) :: Ux2y2
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: m1, m2, m3, m4, n1, n2, n3, n4
!
! End of declarations rewritten by SPAG
!
   n1 = Ny - 1
   n2 = Ny - 2
   n3 = Ny - 3
   n4 = Ny - 4
   m1 = Nx - 1
   m2 = Nx - 2
   m3 = Nx - 3
   m4 = Nx - 4

   IF (I == 1) THEN

      IF ((J > 2 .AND. J < Ny - 1)) THEN
!     x=xa, yinterior
         Ux3y = (5*U(1,J-1)-18*U(2,J-1)+24*U(3,J-1)-14*U(4,J-1)+3*U(5,J-1)-5*U(1,J+1)+18*U(2,J+1)-24*U(3,J+1)+14*U(4,J+1)          &
              & - 3*U(5, J + 1))/Dxxxy4
         Uxy3 = (3*U(1,J-2)-4*U(2,J-2)+U(3,J-2)-6*U(1,J-1)+8*U(2,J-1)-2*U(3,J-1)+6*U(1,J+1)-8*U(2,J+1)+2*U(3,J+1)-3*U(1,J+2)       &
              & + 4*U(2, J + 2) - U(3, J + 2))/Dxyyy4
      ELSEIF (J == 1) THEN
!     (xa,yc)
         Ux3y = (15*U(1,1)-54*U(2,1)+72*U(3,1)-42*U(4,1)+9*U(5,1)-20*U(1,2)+72*U(2,2)-96*U(3,2)+56*U(4,2)-12*U(5,2)+5*U(1,3)       &
              & - 18*U(2, 3) + 24*U(3, 3) - 14*U(4, 3) + 3*U(5, 3))/Dxxxy4
         Uxy3 = (15*U(1,1)-20*U(2,1)+5*U(3,1)-54*U(1,2)+72*U(2,2)-18*U(3,2)+72*U(1,3)-96*U(2,3)+24*U(3,3)-42*U(1,4)+56*U(2,4)      &
              & - 14*U(3, 4) + 9*U(1, 5) - 12*U(2, 5) + 3*U(3, 5))/Dxyyy4
         Ux2y2 = (4*U(1,1)-10*U(2,1)+8*U(3,1)-2*U(4,1)-10*U(1,2)+25*U(2,2)-20*U(3,2)+5*U(4,2)+8*U(1,3)-20*U(2,3)+16*U(3,3)-4*U(4,3)&
               & - 2*U(1, 4) + 5*U(2, 4) - 4*U(3, 4) + U(4, 4))/Dxxyy
      ELSEIF (J == 2) THEN
!     (xa,yc+dly)
         Ux3y = (5*U(1,1)-18*U(2,1)+24*U(3,1)-14*U(4,1)+3*U(5,1)-5*U(1,3)+18*U(2,3)-24*U(3,3)+14*U(4,3)-3*U(5,3))/Dxxxy4
         Uxy3 = (9*U(1,1)-12*U(2,1)+3*U(3,1)-30*U(1,2)+40*U(2,2)-10*U(3,2)+36*U(1,3)-48*U(2,3)+12*U(3,3)-18*U(1,4)+24*U(2,4)       &
              & - 6*U(3, 4) + 3*U(1, 5) - 4*U(2, 5) + U(3, 5))/Dxyyy4
      ELSEIF (J == Ny - 1) THEN
!     x=xa,y=yd-dly
         Ux3y = (5*U(1,J-1)-18*U(2,J-1)+24*U(3,J-1)-14*U(4,J-1)+3*U(5,J-1)-5*U(1,J+1)+18*U(2,J+1)-24*U(3,J+1)+14*U(4,J+1)          &
              & - 3*U(5, J + 1))
         Uxy3 = (5*U(1,n2)-18*U(2,n2)+24*U(3,n2)-14*U(4,n2)+3*U(5,n2)-5*U(1,Ny)+18*U(2,Ny)-24*U(3,Ny)+14*U(4,Ny)-3*U(5,Ny))/Dxyyy4
      ELSEIF (J == Ny) THEN
!     x=xa, y=yd
         Ux3y = (-5*U(1,n2)+18*U(2,n2)-24*U(3,n2)+14*U(4,n2)-3*U(5,n2)+20*U(1,n1)-72*U(2,n1)+96*U(3,n1)-56*U(4,n1)+12*U(5,n1)      &
              & - 15*U(1, Ny) + 54*U(2, Ny) - 72*U(3, Ny) + 42*U(4, Ny) - 9*U(5, Ny))/Dxxxy4
         Uxy3 = (-9*U(1,n4)+12*U(2,n4)-3*U(3,n4)+42*U(1,n3)-56*U(2,n3)+14*U(3,n3)-72*U(1,n2)+96*U(2,n2)-24*U(3,n2)+54*U(1,n1)      &
              & - 72*U(2, n1) + 18*U(3, n1) - 15*U(1, Ny) + 20*U(2, Ny) - 5*U(3, Ny))/Dxyyy4
         Ux2y2 = (-2*U(1,n3)+5*U(2,n3)-4*U(3,n3)+U(4,n3)+8*U(1,n2)-20*U(2,n2)+16*U(3,n2)-4*U(4,n2)-10*U(1,n1)+25*U(2,n1)-20*U(3,n1)&
               & + 5*U(4, n1) + 4*U(1, Ny) - 10*U(2, Ny) + 8*U(3, Ny) - 2*U(4, Ny))/Dxxyy
      END IF

   ELSEIF (I == 2) THEN

      IF ((J > 2 .AND. J < Ny - 1)) THEN
!     x=xa+dlx, y interior
         Ux3y = (3*U(1,J-1)-10*U(2,J-1)+12*U(3,J-1)-6*U(4,J-1)+U(5,J-1)-3*U(1,J+1)+10*U(2,J+1)-12*U(3,J+1)+6*U(4,J+1)-U(5,J+1))    &
              & /Dxxxy4
         Uxy3 = (U(1,J-2)-U(3,J-2)-2*U(1,J-1)+2*U(3,J-1)+2*U(1,J+1)-2*U(3,J+1)-U(1,J+2)+U(3,J+2))/Dxyyy4
      ELSEIF (J == 1) THEN
!     x=xa+dlx, y=yc
         Ux3y = (9*U(1,1)-30*U(2,1)+36*U(3,1)-18*U(4,1)+3*U(5,1)-12*U(1,2)+40*U(2,2)-48*U(3,2)+24*U(4,2)-4*U(5,2)+3*U(1,3)         &
              & - 10*U(2, 3) + 12*U(3, 3) - 6*U(4, 3) + U(5, 3))/Dxxxy4
         Uxy3 = (5*U(1,1)-5*U(3,1)-18*U(1,2)+18*U(3,2)+24*U(1,3)-24*U(3,3)-14*U(1,4)+14*U(3,4)+3*U(1,5)-3*U(3,5))/Dxyyy4
      ELSEIF (J == 2) THEN
!     at x=xa+dlx,y=yc+dly
         Ux3y = (3*U(1,1)-10*U(2,1)+12*U(3,1)-6*U(4,1)+U(5,1)-3*U(1,3)+10*U(2,3)-12*U(3,3)+6*U(4,3)-U(5,3))/Dxxxy4
         Uxy3 = (3*U(1,1)-3*U(3,1)-10*U(1,2)+10*U(3,2)+12*U(1,3)-12*U(3,3)-6*U(1,4)+6*U(3,4)+U(1,5)-U(3,5))/Dxyyy4
      ELSEIF (J == Ny - 1) THEN
!     x=xa+dlx,y=yd-dly
         Ux3y = (3*U(1,n2)-10*U(2,n2)+12*U(3,n2)-6*U(4,n2)+U(5,n2)-3*U(1,Ny)+10*U(2,Ny)-12*U(3,Ny)+6*U(4,Ny)-U(5,Ny))/Dxxxy4
         Uxy3 = (-U(1,n4)+U(3,n4)+6*U(1,n3)-6*U(3,n3)-12*U(1,n2)+12*U(3,n2)+10*U(1,n1)-10*U(3,n1)-3*U(1,Ny)+3*U(3,Ny))/Dxyyy4
      ELSEIF (J == Ny) THEN
!     at x=xa+dlx,y=yd
         Ux3y = (-3*U(1,n2)+10*U(2,n2)-12*U(3,n2)+6*U(4,n2)-U(5,n2)+12*U(1,n1)-40*U(2,n1)+48*U(3,n1)-24*U(4,n1)+4*U(5,n1)-9*U(1,Ny)&
              & + 30*U(2, Ny) - 36*U(3, Ny) + 18*U(4, Ny) - 3*U(5, Ny))/Dxxxy4
         Uxy3 = (-3*U(1,n4)+3*U(3,n4)+14*U(1,n3)-14*U(3,n3)-24*U(1,n2)+24*U(3,n2)+18*U(1,n1)-18*U(3,n1)-5*U(1,Ny)+5*U(3,Ny))/Dxyyy4
      END IF

   ELSEIF (I > 2 .AND. I < Nx - 1) THEN

      IF (J == 1) THEN
!     y=yc,x interior
         Ux3y = (3.0*U(I-2,1)-6.0*U(I-1,1)+6.0*U(I+1,1)-3.0*U(I+2,1)-4.0*U(I-2,2)+8.0*U(I-1,2)-8.0*U(I+1,2)+4.0*U(I+2,2)+U(I-2,3)  &
              & - 2.0*U(I - 1, 3) + 2.0*U(I + 1, 3) - U(I + 2, 3))/Dxxxy4
         Uxy3 = (5.0*U(I-1,1)-5.0*U(I+1,1)-18.0*U(I-1,2)+18.0*U(I+1,2)+24.0*U(I-1,3)-24.0*U(I+1,3)-14.0*U(I-1,4)+14.0*U(I+1,4)     &
              & + 3.0*U(I - 1, 5) - 3.0*U(I + 1, 5))/Dxyyy4
      ELSEIF (J == 2) THEN
!     y=yc+dly,x interior
         Ux3y = (U(I-2,1)-2.0*U(I-1,1)+2.0*U(I+1,1)-U(I+2,1)-U(I-2,3)+2.0*U(I-1,3)-2.0*U(I+1,3)+U(I+2,3))/Dxxxy4
         Uxy3 = (U(I-1,1)-U(I+1,1)-2.0*U(I-1,2)+2.0*U(I+1,2)+2.0*U(I-1,4)-2.0*U(I+1,4)-U(I-1,5)+U(I+1,5))/Dxyyy4
      ELSEIF (J == Ny - 1) THEN
!     y=yd-dly, x interior
         Ux3y = (U(I-2,n2)-2.0*U(I-1,n2)+2.0*U(I+1,n2)-U(I+2,n2)-U(I-2,Ny)+2.0*U(I-1,Ny)-2.0*U(I+1,Ny)+U(I+2,Ny))/Dxxxy4
         Uxy3 = (-U(I-1,n4)+U(I+1,n4)+6.0*U(I-1,n3)-6.0*U(I+1,n3)-12.0*U(I-1,n2)+12.0*U(I+1,n2)+10.0*U(I-1,n1)-10.0*U(I+1,n1)      &
              & - 3.0*U(I - 1, Ny) + 3.0*U(I + 1, Ny))/Dxyyy4
      ELSEIF (J == Ny) THEN
!     at y=yd, x interior
         Ux3y = (-U(I-2,n2)+2.0*U(I-1,n2)-2.0*U(I+1,n2)+U(I+2,n2)+4.0*U(I-2,n1)-8.0*U(I-1,n1)+8.0*U(I+1,n1)-4.0*U(I+2,n1)          &
              & - 3.0*U(I - 2, Ny) + 6.0*U(I - 1, Ny) - 6.0*U(I + 1, Ny) + 3.0*U(I + 2, Ny))/Dxxxy4
         Uxy3 = (-3.0*U(I-1,n4)+3.0*U(I+1,n4)+14.0*U(I-1,n3)-14.0*U(I+1,n3)-24.0*U(I-1,n2)+24.0*U(I+1,n2)+18.0*U(I-1,n1)           &
              & - 18.0*U(I + 1, n1) - 5.0*U(I - 1, Ny) + 5.0*U(I + 1, Ny))/Dxyyy4
      END IF

   ELSEIF (I == Nx - 1) THEN

      IF ((J > 2 .AND. J < Ny - 1)) THEN
!     x=xb-dlx,y interior
         Ux3y = (-U(m4,J-1)+6.*U(m3,J-1)-12.*U(m2,J-1)+10.*U(m1,J-1)-3.*U(Nx,J-1)+U(m4,J+1)-6.*U(m3,J+1)+12.*U(m2,J+1)             &
              & - 10.*U(m1, J + 1) + 3.*U(Nx, J + 1))/Dxxxy4
         Uxy3 = (U(m2,J-2)-U(Nx,J-2)-2.*U(m2,J-1)+2.*U(Nx,J-1)+2.*U(m2,J+1)-2.*U(Nx,J+1)-U(m2,J+2)+U(Nx,J+2))/Dxyyy4
      ELSEIF (J == 1) THEN
!     at x=xb-dlx, y=yc
         Ux3y = (-3.0*U(m4,1)+18.0*U(m3,1)-36.0*U(m2,1)+30.0*U(m1,1)-9.0*U(Nx,1)+4.0*U(m4,2)-24.0*U(m3,2)+48.0*U(m2,2)-40.0*U(m1,2)&
              & + 12.0*U(Nx, 2) - U(m4, 3) + 6.0*U(m3, 3) - 12.0*U(m2, 3) + 10.0*U(m1, 3) - 3.0*U(Nx, 3))/Dxxxy4
         Uxy3 = (5.0*U(m2,1)-5.0*U(Nx,1)-18.0*U(m2,2)+18.0*U(Nx,2)+24.0*U(m2,3)-24.0*U(Nx,3)-14.0*U(m2,4)+14.0*U(Nx,4)+3.0*U(m2,5) &
              & - 3.0*U(Nx, 5))/Dxyyy4
      ELSEIF (J == 2) THEN
!     x=xb-dlx,y=yc+dly
         Ux3y = (-U(m4,1)+6.0*U(m3,1)-12.0*U(m2,1)+10.*U(m1,1)-3.*U(Nx,1)+U(m4,3)-6.0*U(m3,3)+12.0*U(m2,3)-10.*U(m1,3)+3.*U(Nx,3)) &
              & /Dxxxy4
         Uxy3 = (3.0*U(m2,1)-3.*U(Nx,1)-10.*U(m2,2)+10.*U(Nx,2)+12.*U(m2,3)-12.*U(Nx,3)-6.*U(m2,4)+6.*U(Nx,4)+U(m2,5)-U(Nx,5))     &
              & /Dxyyy4
      ELSEIF (J == Ny - 1) THEN
!     at x=xb-dlx,y=yd-dly
         Ux3y = (-U(m4,n2)+6.*U(m3,n2)-12.*U(m2,n2)+10.*U(m1,n2)-3.*U(Nx,n2)+U(m4,Ny)-6.*U(m3,Ny)+12.*U(m2,Ny)-10.*U(m1,Ny)        &
              & + 3.*U(Nx, Ny))/Dxxxy4
         Uxy3 = (-U(m2,n4)+U(Nx,n4)+6*U(m2,n3)-6.*U(Nx,n3)-12.*U(m2,n2)+12.*U(Nx,n2)+10.*U(m2,n1)-10.*U(Nx,n1)-3.*U(m2,Ny)         &
              & + 3.*U(Nx, Ny))/Dxyyy4
      ELSEIF (J == Ny) THEN
!     at x=xb.dlx,y=yd
         Ux3y = (U(m4,n2)-6.*U(m3,n2)+12.*U(m2,n2)-10.*U(m1,n2)+3.*U(Nx,n2)-4.*U(m4,n1)+24.*U(m3,n1)-48.*U(m2,n1)+40.*U(m1,n1)     &
              & - 12.*U(Nx, n1) + 3.*U(m4, Ny) - 18.*U(m3, Ny) + 36.*U(m2, Ny) - 30.*U(m1, Ny) + 9.*U(Nx, Ny))/Dxxxy4
         Uxy3 = (-3.*U(m2,n4)+3.*U(Nx,n4)+14.*U(m2,n3)-14.*U(Nx,n3)-24.*U(m2,n2)+24.*U(Nx,n2)+18.*U(m2,n1)-18.*U(Nx,n1)-5.*U(m2,Ny)&
              & + 5.*U(Nx, Ny))/Dxyyy4
      END IF

   ELSEIF (I == Nx) THEN

      IF ((J > 2 .AND. J < Ny - 1)) THEN
!     x=xb,y interior
         Ux3y = (-3.*U(m4,J-1)+14.*U(m3,J-1)-24.*U(m2,J-1)+18.*U(m1,J-1)-5.*U(Nx,J-1)+3.*U(m4,J+1)-14.*U(m3,J+1)+24.*U(m2,J+1)     &
              & - 18.*U(m1, J + 1) + 5.*U(Nx, J + 1))/Dxxxy4
         Uxy3 = (-U(m2,J-2)+4.*U(m1,J-2)-3.*U(Nx,J-2)+2.*U(m2,J-1)-8.*U(m1,J-1)+6.*U(Nx,J-1)-2.*U(m2,J+1)+8.*U(m1,J+1)-6.*U(Nx,J+1)&
              & + U(m2, J + 2) - 4.*U(m1, J + 2) + 3.*U(Nx, J + 2))/Dxyyy4
      ELSEIF (J == 1) THEN
!     x=xb,y=yc
         Ux3y = (-9.*U(m4,1)+42.*U(m3,1)-72.*U(m2,1)+54.*U(m1,1)-15.*U(Nx,1)+12.*U(m4,2)-56.*U(m3,2)+96.*U(m2,2)-72.*U(m1,2)       &
              & + 20.*U(Nx, 2) - 3.*U(m4, 3) + 14.*U(m3, 3) - 24.*U(m2, 3) + 18.*U(m1, 3) - 5.*U(Nx, 3))/Dxxxy4
         Uxy3 = (-5.*U(m2,1)+20.*U(m1,1)-15.*U(Nx,1)+18.*U(m2,2)-72.*U(m1,2)+54.*U(Nx,2)-24.*U(m2,3)+96.*U(m1,3)-72.*U(Nx,3)       &
              & + 14.*U(m2, 4) - 56.*U(m1, 4) + 42.*U(Nx, 4) - 3.*U(m2, 5) + 12.*U(m1, 5) - 9.*U(Nx, 5))/Dxyyy4
         Ux2y2 = (-2.*U(m3,1)+8.*U(m2,1)-10.*U(m1,1)+4.*U(Nx,1)+5.*U(m3,2)-20.*U(m2,2)+25.*U(m1,2)-10.*U(Nx,2)-4.*U(m3,3)          &
               & + 16.*U(m2, 3) - 20.*U(m1, 3) + 8.*U(Nx, 3) + U(m3, 4) - 4.*U(m2, 4) + 5.*U(m1, 4) - 2.*U(Nx, 4))/Dxxyy
      ELSEIF (J == 2) THEN
!     x=xb,y=yc+dly
         Ux3y = (-3.*U(m4,1)+14.*U(m3,1)-24.*U(m2,1)+18.*U(m1,1)-5.*U(Nx,1)+3.*U(m4,3)-14.*U(m3,3)+24.*U(m2,3)-18.*U(m1,3)         &
              & + 5.*U(Nx, 3))/Dxxxy4
         Uxy3 = (-3.*U(m2,1)+12.*U(m1,1)-9.*U(Nx,1)+10.*U(m2,2)-40.*U(m1,2)+30.*U(Nx,2)-12.*U(m2,3)+48.*U(m1,3)-36.*U(Nx,3)        &
              & + 6.*U(m2, 4) - 24.*U(m1, 4) + 18.*U(Nx, 4) - U(m2, 5) + 4.*U(m1, 5) - 3.*U(Nx, 5))/Dxyyy4
      ELSEIF (J == Ny - 1) THEN
!     x=xb,y=yd-dly
         Ux3y = (-3.*U(m4,n2)+14.*U(m3,n2)-24.*U(m2,n2)+18.*U(m1,n2)-5.*U(Nx,n2)+3.*U(m4,Ny)-14.*U(m3,Ny)+24.*U(m2,Ny)-18.*U(m1,Ny)&
              & + 5.*U(Nx, Ny))/Dxxxy4
         Uxy3 = (U(m2,n4)-4.*U(m1,n4)+3.*U(Nx,n4)-6.*U(m2,n3)+24.*U(m1,n3)-18.*U(Nx,n3)+12.*U(m2,n2)-48.*U(m1,n2)+36.*U(Nx,n2)     &
              & - 10.*U(m2, n1) + 40.*U(m1, n1) - 30.*U(Nx, n1) + 3.*U(m2, Ny) - 12.*U(m1, Ny) + 9.*U(Nx, Ny))/Dxyyy4
      ELSEIF (J == Ny) THEN
!     x=xb,y=yd
         Ux3y = (3.*U(m4,n2)-14.*U(m3,n2)+24.*U(m2,n2)-18.*U(m1,n2)+5.*U(Nx,n2)-12.*U(m4,n1)+56.*U(m3,n1)-96.*U(m2,n1)+72.*U(m1,n1)&
              & - 20.*U(Nx, n1) + 9.*U(m4, Ny) - 42.*U(m3, Ny) + 72.*U(m2, Ny) - 54.*U(m1, Ny) + 15.*U(Nx, Ny))/Dxxxy4
         Uxy3 = (3.*U(m2,n4)-12.*U(m1,n4)+9.*U(Nx,n4)-14.*U(m2,n3)+56.*U(m1,n3)-42.*U(Nx,n3)+24.*U(m2,n2)-96.*U(m1,n2)+72.*U(Nx,n2)&
              & - 18.*U(m2, n1) + 72.*U(m1, n1) - 54.*U(Nx, n1) + 5.*U(m2, Ny) - 20.*U(m1, Ny) + 15.*U(Nx, Ny))/Dxyyy4
         Ux2y2 = (U(m3,n3)-4.*U(m2,n3)+5.*U(m1,n3)-2.*U(Nx,n3)-4.*U(m3,n2)+16.*U(m2,n2)-20.*U(m1,n2)+8.*U(Nx,n2)+5.0*U(m3,n1)      &
               & - 20.*U(m2, n1) + 25.*U(m1, n1) - 10.*U(Nx, n1) - 2.*U(m3, Ny) + 8.*U(m2, Ny) - 10.*U(m1, Ny) + 4.*U(Nx, Ny))/Dxxyy
      END IF

   END IF

END SUBROUTINE pde2cr
!*==PDE3.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
SUBROUTINE pde3(Nx, Ny, Nz, U, I, J, K, Ux3, Ux4, Uy3, Uy4, Uz3, Uz4, Nxa, Nyc, Nze)
!
!     estimate third and fourth partial derivatives in x,y,z
!
   USE C_PDE3COM
   USE S_P3DE2
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nx
   INTEGER :: Ny
   INTEGER, INTENT(IN) :: Nz
   REAL, DIMENSION(Nx, Ny, Nz) :: U
   INTEGER :: I
   INTEGER :: J
   INTEGER, INTENT(IN) :: K
   REAL :: Ux3
   REAL :: Ux4
   REAL :: Uy3
   REAL :: Uy4
   REAL, INTENT(OUT) :: Uz3
   REAL, INTENT(OUT) :: Uz4
   INTEGER :: Nxa
   INTEGER :: Nyc
   INTEGER, INTENT(IN) :: Nze
!
! End of declarations rewritten by SPAG
!
!
!     x,y partial derivatives
!
   CALL p3de2(Nx, Ny, U(1, 1, K), I, J, Ux3, Ux4, Uy3, Uy4, Nxa, Nyc)
!
!     z partial derivatives
!
   IF (Nze /= 0) THEN
!
!     nonperiodic in z
!
      IF (K > 2 .AND. K < Nz - 1) THEN
         Uz3 = (-U(I, J, K - 2) + 2.0*U(I, J, K - 1) - 2.0*U(I, J, K + 1) + U(I, J, K + 2))/Tdlz3
         Uz4 = (U(I, J, K - 2) - 4.0*U(I, J, K - 1) + 6.0*U(I, J, K) - 4.0*U(I, J, K + 1) + U(I, J, K + 2))/Dlz4
      ELSEIF (K == 1) THEN
         Uz3 = (-5.0*U(I, J, 1) + 18.0*U(I, J, 2) - 24.0*U(I, J, 3) + 14.0*U(I, J, 4) - 3.0*U(I, J, 5))/Tdlz3
         Uz4 = (3.0*U(I, J, 1) - 14.0*U(I, J, 2) + 26.0*U(I, J, 3) - 24.0*U(I, J, 4) + 11.0*U(I, J, 5) - 2.0*U(I, J, 6))/Dlz4
      ELSEIF (K == 2) THEN
         Uz3 = (-3.0*U(I, J, 1) + 10.0*U(I, J, 2) - 12.0*U(I, J, 3) + 6.0*U(I, J, 4) - U(I, J, 5))/Tdlz3
         Uz4 = (2.0*U(I, J, 1) - 9.0*U(I, J, 2) + 16.0*U(I, J, 3) - 14.0*U(I, J, 4) + 6.0*U(I, J, 5) - U(I, J, 6))/Dlz4
      ELSEIF (K == Nz - 1) THEN
         Uz3 = (U(I, J, Nz - 4) - 6.0*U(I, J, Nz - 3) + 12.0*U(I, J, Nz - 2) - 10.0*U(I, J, Nz - 1) + 3.0*U(I, J, Nz))/Tdlz3
         Uz4 = (-U(I,J,Nz-5)+6.0*U(I,J,Nz-4)-14.0*U(I,J,Nz-3)+16.0*U(I,J,Nz-2)-9.0*U(I,J,Nz-1)+2.0*U(I,J,Nz))/Dlz4
      ELSEIF (K == Nz) THEN
         Uz3 = (3.0*U(I, J, Nz - 4) - 14.0*U(I, J, Nz - 3) + 24.0*U(I, J, Nz - 2) - 18.0*U(I, J, Nz - 1) + 5.0*U(I, J, Nz))/Tdlz3
         Uz4 = (-2.0*U(I,J,Nz-5)+11.0*U(I,J,Nz-4)-24.0*U(I,J,Nz-3)+26.0*U(I,J,Nz-2)-14.0*U(I,J,Nz-1)+3.0*U(I,J,Nz))/Dlz4
      END IF
!
!     periodic in z so use symmetric formula even "near" z boundaies
!
   ELSEIF (K > 2 .AND. K < Nz - 1) THEN
      Uz3 = (-U(I, J, K - 2) + 2.0*U(I, J, K - 1) - 2.0*U(I, J, K + 1) + U(I, J, K + 2))/Tdlz3
      Uz4 = (U(I, J, K - 2) - 4.0*U(I, J, K - 1) + 6.0*U(I, J, K) - 4.0*U(I, J, K + 1) + U(I, J, K + 2))/Dlz4
   ELSEIF (K == 1) THEN
      Uz3 = (-U(I, J, Nz - 2) + 2.0*U(I, J, Nz - 1) - 2.0*U(I, J, 2) + U(I, J, 3))/Tdlz3
      Uz4 = (U(I, J, Nz - 2) - 4.0*U(I, J, Nz - 1) + 6.0*U(I, J, 1) - 4.0*U(I, J, 2) + U(I, J, 3))/Dlz4
   ELSEIF (K == 2) THEN
      Uz3 = (-U(I, J, Nz - 1) + 2.0*U(I, J, 1) - 2.0*U(I, J, 3) + U(I, J, 4))/(Tdlz3)
      Uz4 = (U(I, J, Nz - 1) - 4.0*U(I, J, 1) + 6.0*U(I, J, 2) - 4.0*U(I, J, 3) + U(I, J, 4))/Dlz4
   ELSEIF (K == Nz - 1) THEN
      Uz3 = (-U(I, J, Nz - 3) + 2.0*U(I, J, Nz - 2) - 2.0*U(I, J, 1) + U(I, J, 2))/Tdlz3
      Uz4 = (U(I, J, Nz - 3) - 4.0*U(I, J, Nz - 2) + 6.0*U(I, J, Nz - 1) - 4.0*U(I, J, 1) + U(I, J, 2))/Dlz4
   ELSEIF (K == Nz) THEN
      Uz3 = (-U(I, J, Nz - 2) + 2.0*U(I, J, Nz - 1) - 2.0*U(I, J, 2) + U(I, J, 3))/Tdlz3
      Uz4 = (U(I, J, Nz - 2) - 4.0*U(I, J, Nz - 1) + 6.0*U(I, J, Nz) - 4.0*U(I, J, 2) + U(I, J, 3))/Dlz4
   END IF
END SUBROUTINE pde3
!*==P3DE2.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE p3de2(Nx, Ny, U, I, J, Ux3, Ux4, Uy3, Uy4, Nxa, Nyc)
!
!     third and fourth partial derivatives in x and y
!
   USE C_PDE3COM
   USE S_P3DE1
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nx
   INTEGER, INTENT(IN) :: Ny
   REAL, DIMENSION(Nx, Ny) :: U
   INTEGER :: I
   INTEGER, INTENT(IN) :: J
   REAL :: Ux3
   REAL :: Ux4
   REAL, INTENT(OUT) :: Uy3
   REAL, INTENT(OUT) :: Uy4
   INTEGER :: Nxa
   INTEGER, INTENT(IN) :: Nyc
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: l
!
! End of declarations rewritten by SPAG
!
   l = Ny
!
!     x partial derivatives
!
   CALL p3de1(Nx, U(1, J), I, Ux3, Ux4, Nxa)
!
!     y partial derivatives
!
   IF (Nyc /= 0) THEN
!
!     not periodic in y
!
      IF (J > 2 .AND. J < Ny - 1) THEN
         Uy3 = (-U(I, J - 2) + 2.0*U(I, J - 1) - 2.0*U(I, J + 1) + U(I, J + 2))/Tdly3
         Uy4 = (U(I, J - 2) - 4.0*U(I, J - 1) + 6.0*U(I, J) - 4.0*U(I, J + 1) + U(I, J + 2))/Dly4
      ELSEIF (J == 1) THEN
         Uy3 = (-5.0*U(I, 1) + 18.0*U(I, 2) - 24.0*U(I, 3) + 14.0*U(I, 4) - 3.0*U(I, 5))/Tdly3
         Uy4 = (3.0*U(I, 1) - 14.0*U(I, 2) + 26.0*U(I, 3) - 24.0*U(I, 4) + 11.0*U(I, 5) - 2.0*U(I, 6))/Dly4
      ELSEIF (J == 2) THEN
         Uy3 = (-3.0*U(I, 1) + 10.0*U(I, 2) - 12.0*U(I, 3) + 6.0*U(I, 4) - U(I, 5))/Tdly3
         Uy4 = (2.0*U(I, 1) - 9.0*U(I, 2) + 16.0*U(I, 3) - 14.0*U(I, 4) + 6.0*U(I, 5) - U(I, 6))/Dly4
      ELSEIF (J == Ny - 1) THEN
         Uy3 = (U(I, l - 4) - 6.0*U(I, l - 3) + 12.0*U(I, l - 2) - 10.0*U(I, l - 1) + 3.0*U(I, l))/Tdly3
         Uy4 = (-U(I, l - 5) + 6.0*U(I, l - 4) - 14.0*U(I, l - 3) + 16.0*U(I, l - 2) - 9.0*U(I, l - 1) + 2.0*U(I, l))/Dly4
      ELSEIF (J == Ny) THEN
         Uy3 = (3.0*U(I, l - 4) - 14.0*U(I, l - 3) + 24.0*U(I, l - 2) - 18.0*U(I, l - 1) + 5.0*U(I, l))/Tdly3
         Uy4 = (-2.0*U(I, l - 5) + 11.0*U(I, l - 4) - 24.0*U(I, l - 3) + 26.0*U(I, l - 2) - 14.0*U(I, l - 1) + 3.0*U(I, l))/Dly4
      END IF
!
!     periodic in y
!
   ELSEIF (J > 2 .AND. J < Ny - 1) THEN
      Uy3 = (-U(I, J - 2) + 2.0*U(I, J - 1) - 2.0*U(I, J + 1) + U(I, J + 2))/Tdly3
      Uy4 = (U(I, J - 2) - 4.0*U(I, J - 1) + 6.0*U(I, J) - 4.0*U(I, J + 1) + U(I, J + 2))/Dly4
   ELSEIF (J == 1) THEN
      Uy3 = (-U(I, l - 2) + 2.0*U(I, l - 1) - 2.0*U(I, 2) + U(I, 3))/Tdly3
      Uy4 = (U(I, l - 2) - 4.0*U(I, l - 1) + 6.0*U(I, 1) - 4.0*U(I, 2) + U(I, 3))/Dly4
   ELSEIF (J == 2) THEN
      Uy3 = (-U(I, l - 1) + 2.0*U(I, 1) - 2.0*U(I, 3) + U(I, 4))/(Tdly3)
      Uy4 = (U(I, l - 1) - 4.0*U(I, 1) + 6.0*U(I, 2) - 4.0*U(I, 3) + U(I, 4))/Dly4
   ELSEIF (J == Ny - 1) THEN
      Uy3 = (-U(I, l - 3) + 2.0*U(I, l - 2) - 2.0*U(I, 1) + U(I, 2))/Tdly3
      Uy4 = (U(I, l - 3) - 4.0*U(I, l - 2) + 6.0*U(I, l - 1) - 4.0*U(I, 1) + U(I, 2))/Dly4
   ELSEIF (J == Ny) THEN
      Uy3 = (-U(I, l - 2) + 2.0*U(I, l - 1) - 2.0*U(I, 2) + U(I, 3))/Tdly3
      Uy4 = (U(I, l - 2) - 4.0*U(I, l - 1) + 6.0*U(I, l) - 4.0*U(I, 2) + U(I, 3))/Dly4
   END IF
END SUBROUTINE p3de2
!*==P3DE1.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE p3de1(Nx, U, I, Ux3, Ux4, Nxa)
!
!     third and fourth derivatives in x
!
   USE C_PDE3COM
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nx
   REAL, INTENT(IN), DIMENSION(Nx) :: U
   INTEGER, INTENT(IN) :: I
   REAL, INTENT(OUT) :: Ux3
   REAL, INTENT(OUT) :: Ux4
   INTEGER, INTENT(IN) :: Nxa
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: k
!
! End of declarations rewritten by SPAG
!
   k = Nx
   IF (Nxa /= 0) THEN
!
!     nonperiodic in x
!
      IF (I > 2 .AND. I < Nx - 1) THEN
         Ux3 = (-U(I - 2) + 2.0*U(I - 1) - 2.0*U(I + 1) + U(I + 2))/Tdlx3
         Ux4 = (U(I - 2) - 4.0*U(I - 1) + 6.0*U(I) - 4.0*U(I + 1) + U(I + 2))/Dlx4
      ELSEIF (I == 1) THEN
         Ux3 = (-5.0*U(1) + 18.0*U(2) - 24.0*U(3) + 14.0*U(4) - 3.0*U(5))/Tdlx3
         Ux4 = (3.0*U(1) - 14.0*U(2) + 26.0*U(3) - 24.0*U(4) + 11.0*U(5) - 2.0*U(6))/Dlx4
      ELSEIF (I == 2) THEN
         Ux3 = (-3.0*U(1) + 10.0*U(2) - 12.0*U(3) + 6.0*U(4) - U(5))/Tdlx3
         Ux4 = (2.0*U(1) - 9.0*U(2) + 16.0*U(3) - 14.0*U(4) + 6.0*U(5) - U(6))/Dlx4
      ELSEIF (I == Nx - 1) THEN
         Ux3 = (U(k - 4) - 6.0*U(k - 3) + 12.0*U(k - 2) - 10.0*U(k - 1) + 3.0*U(k))/Tdlx3
         Ux4 = (-U(k - 5) + 6.0*U(k - 4) - 14.0*U(k - 3) + 16.0*U(k - 2) - 9.0*U(k - 1) + 2.0*U(k))/Dlx4
      ELSEIF (I == Nx) THEN
         Ux3 = (3.0*U(k - 4) - 14.0*U(k - 3) + 24.0*U(k - 2) - 18.0*U(k - 1) + 5.0*U(k))/Tdlx3
         Ux4 = (-2.0*U(k - 5) + 11.0*U(k - 4) - 24.0*U(k - 3) + 26.0*U(k - 2) - 14.0*U(k - 1) + 3.0*U(k))/Dlx4
      END IF
!
!     periodic in x
!
   ELSEIF (I > 2 .AND. I < Nx - 1) THEN
      Ux3 = (-U(I - 2) + 2.0*U(I - 1) - 2.0*U(I + 1) + U(I + 2))/Tdlx3
      Ux4 = (U(I - 2) - 4.0*U(I - 1) + 6.0*U(I) - 4.0*U(I + 1) + U(I + 2))/Dlx4
   ELSEIF (I == 1) THEN
      Ux3 = (-U(k - 2) + 2.0*U(k - 1) - 2.0*U(2) + U(3))/Tdlx3
      Ux4 = (U(k - 2) - 4.0*U(k - 1) + 6.0*U(1) - 4.0*U(2) + U(3))/Dlx4
   ELSEIF (I == 2) THEN
      Ux3 = (-U(k - 1) + 2.0*U(1) - 2.0*U(3) + U(4))/(Tdlx3)
      Ux4 = (U(k - 1) - 4.0*U(1) + 6.0*U(2) - 4.0*U(3) + U(4))/Dlx4
   ELSEIF (I == Nx - 1) THEN
      Ux3 = (-U(k - 3) + 2.0*U(k - 2) - 2.0*U(1) + U(2))/Tdlx3
      Ux4 = (U(k - 3) - 4.0*U(k - 2) + 6.0*U(k - 1) - 4.0*U(1) + U(2))/Dlx4
   ELSEIF (I == Nx) THEN
      Ux3 = (-U(k - 2) + 2.0*U(k - 1) - 2.0*U(2) + U(3))/Tdlx3
      Ux4 = (U(k - 2) - 4.0*U(k - 1) + 6.0*U(k) - 4.0*U(2) + U(3))/Dlx4
   END IF
END SUBROUTINE p3de1
!*==FACTRI.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!
!
!     factri and factrip are:
!     subroutines called by any real mudpack solver which uses line
!     relaxation(s) within multigrid iteration.  these subroutines do
!     a vectorized factorization of m simultaneous tridiagonal systems
!     of order n arising from nonperiodic or periodic discretizations
!
SUBROUTINE factri(M, N, A, B, C)
!
!     factor the m simultaneous tridiagonal systems of order n
!
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: M
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(INOUT), DIMENSION(N, M) :: A
   REAL, INTENT(INOUT), DIMENSION(N, M) :: B
   REAL, INTENT(IN), DIMENSION(N, M) :: C
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, j
!
! End of declarations rewritten by SPAG
!
   DO i = 2, N
      DO j = 1, M
         A(i - 1, j) = A(i - 1, j)/B(i - 1, j)
         B(i, j) = B(i, j) - A(i - 1, j)*C(i - 1, j)
      END DO
   END DO
END SUBROUTINE factri
!*==FACTRP.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE factrp(M, N, A, B, C, D, E, Sum)
!
!     factor the m simultaneous "tridiagonal" systems of order n
!     from discretized periodic system (leave out periodic n point)
!     (so sweeps below only go from i=1,2,...,n-1) n > 3 is necessary
!
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: M
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(INOUT), DIMENSION(N, M) :: A
   REAL, INTENT(INOUT), DIMENSION(N, M) :: B
   REAL, INTENT(IN), DIMENSION(N, M) :: C
   REAL, INTENT(INOUT), DIMENSION(N, M) :: D
   REAL, INTENT(INOUT), DIMENSION(N, M) :: E
   REAL, INTENT(INOUT), DIMENSION(M) :: Sum
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, j
!
! End of declarations rewritten by SPAG
!
   DO j = 1, M
      D(1, j) = A(1, j)
   END DO
   DO i = 2, N - 2
      DO j = 1, M
         A(i, j) = A(i, j)/B(i - 1, j)
         B(i, j) = B(i, j) - A(i, j)*C(i - 1, j)
         D(i, j) = -A(i, j)*D(i - 1, j)
      END DO
   END DO
!
!     correct computation of last d element
!
   DO j = 1, M
      D(N - 2, j) = C(N - 2, j) + D(N - 2, j)
   END DO
   DO j = 1, M
      E(1, j) = C(N - 1, j)/B(1, j)
   END DO
   DO i = 2, N - 3
      DO j = 1, M
         E(i, j) = -E(i - 1, j)*C(i - 1, j)/B(i, j)
      END DO
   END DO
   DO j = 1, M
      E(N - 2, j) = (A(N - 1, j) - E(N - 3, j)*C(N - 3, j))/B(N - 2, j)
   END DO
!
!     compute  inner product (e,d) for each j in sum(j)
!
   DO j = 1, M
      Sum(j) = 0.
   END DO
   DO i = 1, N - 2
      DO j = 1, M
         Sum(j) = Sum(j) + E(i, j)*D(i, j)
      END DO
   END DO
!
!     set last diagonal element
!
   DO j = 1, M
      B(N - 1, j) = B(N - 1, j) - Sum(j)
   END DO
END SUBROUTINE factrp
!*==TRANSP.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE transp(N, Amat)
!
!     transpose n by n real matrix
!
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(INOUT), DIMENSION(N, N) :: Amat
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, j
   REAL :: temp
!
! End of declarations rewritten by SPAG
!
   DO i = 1, N - 1
      DO j = i + 1, N
         temp = Amat(i, j)
         Amat(i, j) = Amat(j, i)
         Amat(j, i) = temp
      END DO
   END DO
END SUBROUTINE transp
!*==SGFA.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE sgfa(A, Lda, N, Ipvt, Info)
   USE S_ISFMAX
   USE S_SSCL
   USE S_SXPY
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Lda
   REAL, INTENT(INOUT), DIMENSION(Lda, 1) :: A
   INTEGER, INTENT(IN) :: N
   INTEGER, INTENT(OUT), DIMENSION(1) :: Ipvt
   INTEGER, INTENT(OUT) :: Info
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: j, k, kp1, l, nm1
   REAL :: t
!
! End of declarations rewritten by SPAG
!
   Info = 0
   nm1 = N - 1
   IF (nm1 >= 1) THEN
      DO k = 1, nm1
         kp1 = k + 1
         l = isfmax(N - k + 1, A(k, k), 1) + k - 1
         Ipvt(k) = l
         IF (A(l, k) == 0.0E0) THEN
            Info = k
         ELSE
            IF (l /= k) THEN
               t = A(l, k)
               A(l, k) = A(k, k)
               A(k, k) = t
            END IF
            t = -1.0E0/A(k, k)
            CALL sscl(N - k, t, A(k + 1, k), 1)
            DO j = kp1, N
               t = A(l, j)
               IF (l /= k) THEN
                  A(l, j) = A(k, j)
                  A(k, j) = t
               END IF
               CALL sxpy(N - k, t, A(k + 1, k), 1, A(k + 1, j), 1)
            END DO
         END IF
      END DO
   END IF
   Ipvt(N) = N
   IF (A(N, N) == 0.0E0) Info = N
END SUBROUTINE sgfa
!*==SGSL.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE sgsl(A, Lda, N, Ipvt, B, Job)
   USE S_SDT
   USE S_SXPY
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Lda
   REAL, DIMENSION(Lda, 1) :: A
   INTEGER, INTENT(IN) :: N
   INTEGER, INTENT(IN), DIMENSION(1) :: Ipvt
   REAL, INTENT(INOUT), DIMENSION(1) :: B
   INTEGER, INTENT(IN) :: Job
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: k, kb, l, nm1
   REAL :: t
!
! End of declarations rewritten by SPAG
!
   nm1 = N - 1
   IF (Job /= 0) THEN
      DO k = 1, N
         t = sdt(k - 1, A(1, k), 1, B(1), 1)
         B(k) = (B(k) - t)/A(k, k)
      END DO
      IF (nm1 >= 1) THEN
         DO kb = 1, nm1
            k = N - kb
            B(k) = B(k) + sdt(N - k, A(k + 1, k), 1, B(k + 1), 1)
            l = Ipvt(k)
            IF (l /= k) THEN
               t = B(l)
               B(l) = B(k)
               B(k) = t
            END IF
         END DO
      END IF
   ELSE
      IF (nm1 >= 1) THEN
         DO k = 1, nm1
            l = Ipvt(k)
            t = B(l)
            IF (l /= k) THEN
               B(l) = B(k)
               B(k) = t
            END IF
            CALL sxpy(N - k, t, A(k + 1, k), 1, B(k + 1), 1)
         END DO
      END IF
      DO kb = 1, N
         k = N + 1 - kb
         B(k) = B(k)/A(k, k)
         t = -B(k)
         CALL sxpy(k - 1, t, A(1, k), 1, B(1), 1)
      END DO
   END IF
END SUBROUTINE sgsl
!*==SDT.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

FUNCTION sdt(N, Sx, Incx, Sy, Incy)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   REAL :: sdt
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(IN), DIMENSION(1) :: Sx
   INTEGER, INTENT(IN) :: Incx
   REAL, INTENT(IN), DIMENSION(1) :: Sy
   INTEGER, INTENT(IN) :: Incy
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ix, iy, m, mp1
   REAL :: stemp
!
! End of declarations rewritten by SPAG
!
   stemp = 0.0E0
   sdt = 0.0E0
   IF (N <= 0) RETURN
   IF (Incx == 1 .AND. Incy == 1) THEN
      m = mod(N, 5)
      IF (m /= 0) THEN
         DO i = 1, m
            stemp = stemp + Sx(i)*Sy(i)
         END DO
         IF (N < 5) THEN
            sdt = stemp
            RETURN
         END IF
      END IF
      mp1 = m + 1
      DO i = mp1, N, 5
         stemp = stemp + Sx(i)*Sy(i) + Sx(i + 1)*Sy(i + 1) + Sx(i + 2)*Sy(i + 2) + Sx(i + 3)*Sy(i + 3) + Sx(i + 4)*Sy(i + 4)
      END DO
      sdt = stemp
   ELSE
      ix = 1
      iy = 1
      IF (Incx < 0) ix = (-N + 1)*Incx + 1
      IF (Incy < 0) iy = (-N + 1)*Incy + 1
      DO i = 1, N
         stemp = stemp + Sx(ix)*Sy(iy)
         ix = ix + Incx
         iy = iy + Incy
      END DO
      sdt = stemp
      RETURN
   END IF
END FUNCTION sdt
!*==ISFMAX.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

FUNCTION isfmax(N, Sx, Incx)
   IMPLICIT NONE
!
! Function and Dummy argument declarations rewritten by SPAG
!
   INTEGER :: isfmax
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(IN), DIMENSION(1) :: Sx
   INTEGER, INTENT(IN) :: Incx
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ix
   REAL :: smax
!
! End of declarations rewritten by SPAG
!
   isfmax = 0
   IF (N < 1) RETURN
   isfmax = 1
   IF (N == 1) RETURN
   IF (Incx == 1) THEN
      smax = abs(Sx(1))
      DO i = 2, N
         IF (abs(Sx(i)) > smax) THEN
            isfmax = i
            smax = abs(Sx(i))
         END IF
      END DO
      RETURN
   END IF
   ix = 1
   smax = abs(Sx(1))
   ix = ix + Incx
   DO i = 2, N
      IF (abs(Sx(ix)) > smax) THEN
         isfmax = i
         smax = abs(Sx(ix))
      END IF
      ix = ix + Incx
   END DO
   RETURN
END FUNCTION isfmax
!*==SXPY.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE sxpy(N, Sa, Sx, Incx, Sy, Incy)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(IN) :: Sa
   REAL, INTENT(IN), DIMENSION(1) :: Sx
   INTEGER, INTENT(IN) :: Incx
   REAL, INTENT(INOUT), DIMENSION(1) :: Sy
   INTEGER, INTENT(IN) :: Incy
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ix, iy, m, mp1
!
! End of declarations rewritten by SPAG
!
   IF (N <= 0) RETURN
   IF (Sa == 0.0) RETURN
   IF (Incx == 1 .AND. Incy == 1) THEN
      m = mod(N, 4)
      IF (m /= 0) THEN
         DO i = 1, m
            Sy(i) = Sy(i) + Sa*Sx(i)
         END DO
         IF (N < 4) RETURN
      END IF
      mp1 = m + 1
      DO i = mp1, N, 4
         Sy(i) = Sy(i) + Sa*Sx(i)
         Sy(i + 1) = Sy(i + 1) + Sa*Sx(i + 1)
         Sy(i + 2) = Sy(i + 2) + Sa*Sx(i + 2)
         Sy(i + 3) = Sy(i + 3) + Sa*Sx(i + 3)
      END DO
   ELSE
      ix = 1
      iy = 1
      IF (Incx < 0) ix = (-N + 1)*Incx + 1
      IF (Incy < 0) iy = (-N + 1)*Incy + 1
      DO i = 1, N
         Sy(iy) = Sy(iy) + Sa*Sx(ix)
         ix = ix + Incx
         iy = iy + Incy
      END DO
      RETURN
   END IF
END SUBROUTINE sxpy
!*==SSCL.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE sscl(N, Sa, Sx, Incx)
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: N
   REAL, INTENT(IN) :: Sa
   REAL, INTENT(INOUT), DIMENSION(1) :: Sx
   INTEGER, INTENT(IN) :: Incx
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, m, mp1, nincx
!
! End of declarations rewritten by SPAG
!
   IF (N <= 0) RETURN
   IF (Incx == 1) THEN
      m = mod(N, 5)
      IF (m /= 0) THEN
         DO i = 1, m
            Sx(i) = Sa*Sx(i)
         END DO
         IF (N < 5) RETURN
      END IF
      mp1 = m + 1
      DO i = mp1, N, 5
         Sx(i) = Sa*Sx(i)
         Sx(i + 1) = Sa*Sx(i + 1)
         Sx(i + 2) = Sa*Sx(i + 2)
         Sx(i + 3) = Sa*Sx(i + 3)
         Sx(i + 4) = Sa*Sx(i + 4)
      END DO
   ELSE
      nincx = N*Incx
      DO i = 1, nincx, Incx
         Sx(i) = Sa*Sx(i)
      END DO
      RETURN
   END IF
END SUBROUTINE sscl

