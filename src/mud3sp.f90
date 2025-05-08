!*==MUD3SP.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!
!     file mud3sp.f
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
!     mud3sp attempts to produce a second order finite difference
!     approximation to the three dimensional SEPARABLE elliptic
!     partial differential equation of the form:
!
!       cxx(x)*pxx + cx(x)*px + cex(x)*p(x,y,z) +
!
!       cyy(y)*pyy + cy(y)*py + cey(y)*p(x,y,z) +
!
!       czz(z)*pzz + cz(z)*pz + cez(z)*p(x,y,z) = r(x,y,z)
!
!     SEPARABILITY means:
!
!       cxx,cx,cex depend only on x
!       cyy,cy,cey depend only on y
!       czz,cz,cez depend only on z
!
!     For example, LaPlace's equation in Cartesian coordinates is separable.
!     Nonseparable elliptic PDEs can be approximated with muh3 or mud3.
!
! ... see documentation and test files provided in this distribution
!
! ... required MUDPACK files
!
!     mudcom.f
!
SUBROUTINE mud3sp(Iparm, Fparm, Work, cfx, cfy, cfz, bndyc, Rhs, Phi, Mgopt, Ierror)
   USE C_FMUD3SP
   USE C_IMUD3SP
   USE C_MUD3SPC
   USE S_DISMD3SP
   USE S_MUD3SP1
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(INOUT), DIMENSION(22) :: Iparm
   REAL, INTENT(INOUT), DIMENSION(8) :: Fparm
   REAL, DIMENSION(*) :: Work
   EXTERNAL cfx
   EXTERNAL cfy
   EXTERNAL cfz
   EXTERNAL bndyc
   REAL, DIMENSION(*) :: Rhs
   REAL, DIMENSION(*) :: Phi
   INTEGER, INTENT(IN), DIMENSION(4) :: Mgopt
   INTEGER, INTENT(INOUT) :: Ierror
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: icx, icy, icz, k, kb, nx, ny, nz
   INTEGER, SAVE :: int
!
! End of declarations rewritten by SPAG
!
   DATA int/0/
   Ierror = 1
   Intl = Iparm(1)       ! set and check intl on ALL calls
   IF (Intl*(Intl - 1) /= 0) RETURN
   IF (int == 0) THEN
      int = 1
      IF (Intl /= 0) RETURN    ! very first call is not intl=0
   END IF
   Ierror = 0
!
!     set input parameters from iparm,fparm internally
!
   Intl = Iparm(1)
   Nxa = Iparm(2)
   Nxb = Iparm(3)
   Nyc = Iparm(4)
   Nyd = Iparm(5)
   Nze = Iparm(6)
   Nzf = Iparm(7)
!
!     set grid size params
!
   Ixp = Iparm(8)
   Jyq = Iparm(9)
   Kzr = Iparm(10)
   Iex = Iparm(11)
   Jey = Iparm(12)
   Kez = Iparm(13)
!
!     set number of subgrids for mg cycling
!
   Ngrid = max0(Iex, Jey, Kez)
   Nfx = Iparm(14)
   Nfy = Iparm(15)
   Nfz = Iparm(16)

   Iguess = Iparm(17)
   Maxcy = Iparm(18)
   Method = Iparm(19)
   Nwork = Iparm(20)
!
!     set floating point params
!
   Xa = Fparm(1)
   Xb = Fparm(2)
   Yc = Fparm(3)
   Yd = Fparm(4)
   Ze = Fparm(5)
   Zf = Fparm(6)
   Tolmax = Fparm(7)
!
!     set multigrid option parameters
!
   Kcycle = Mgopt(1)
   IF (Kcycle == 0) THEN
!
!     use default settings
!
      Kcycle = 2
      Iprer = 2
      Ipost = 1
      Intpol = 3
   ELSE
      Iprer = Mgopt(2)
      Ipost = Mgopt(3)
      Intpol = Mgopt(4)
   END IF
   IF (Intl == 0) THEN       ! intialization call
!
!     check input arguments
!
      Ierror = 2     ! check boundary condition flags
      IF (max0(Nxa, Nxb, Nyc, Nyd, Nze, Nzf) > 2) RETURN
      IF (min0(Nxa, Nxb, Nyc, Nyd, Nze, Nzf) < 0) RETURN
      IF (Nxa == 0 .AND. Nxb /= 0) RETURN
      IF (Nxa /= 0 .AND. Nxb == 0) RETURN
      IF (Nyc == 0 .AND. Nyd /= 0) RETURN
      IF (Nyc /= 0 .AND. Nyd == 0) RETURN
      IF (Nze == 0 .AND. Nzf /= 0) RETURN
      IF (Nze /= 0 .AND. Nzf == 0) RETURN
      Ierror = 3     ! check grid sizes
      IF (Ixp < 2) RETURN
      IF (Jyq < 2) RETURN
      IF (Kzr < 2) RETURN
      Ierror = 4
      Ngrid = max0(Iex, Jey, Kez)
      IF (Iex < 1) RETURN
      IF (Jey < 1) RETURN
      IF (Kez < 1) RETURN
      IF (Ngrid > 50) RETURN
      Ierror = 5
      IF (Nfx /= Ixp*2**(Iex - 1) + 1) RETURN
      IF (Nfy /= Jyq*2**(Jey - 1) + 1) RETURN
      IF (Nfz /= Kzr*2**(Kez - 1) + 1) RETURN
      Ierror = 6
      IF (Iguess*(Iguess - 1) /= 0) RETURN
      Ierror = 7
      IF (Maxcy < 1) RETURN
      Ierror = 8
      IF (Method /= 0) RETURN
      Ierror = 9
!
!     set subgrid sizes and pointers
!
      Kps = 1
      DO kb = 1, Ngrid
         k = Ngrid - kb + 1
         Nxk(k) = Ixp*2**(max0(k + Iex - Ngrid, 1) - 1) + 1
         Nyk(k) = Jyq*2**(max0(k + Jey - Ngrid, 1) - 1) + 1
         Nzk(k) = Kzr*2**(max0(k + Kez - Ngrid, 1) - 1) + 1
         nx = Nxk(k)
         ny = Nyk(k)
         nz = Nzk(k)
         Kpbgn(k) = Kps
         Krbgn(k) = Kpbgn(k) + (nx + 2)*(ny + 2)*(nz + 2)
         Kcxbgn(k) = Krbgn(k) + nx*ny*nz
         Kcybgn(k) = Kcxbgn(k) + 3*nx
         Kczbgn(k) = Kcybgn(k) + 3*ny
         Kps = Kczbgn(k) + 3*nz
      END DO
!
!     set and check minimal work space
!
      nx = Nxk(Ngrid)
      ny = Nyk(Ngrid)
      nz = Nzk(Ngrid)
      Iparm(21) = Kps + (nx + 2)*(ny + 2)*(nz + 2)
      Lwork = Iparm(21)
      IF (Lwork > Nwork) RETURN
      Ierror = 10     ! check solution region
      IF (Xb <= Xa .OR. Yd <= Yc .OR. Zf <= Ze) RETURN
      Ierror = 11
      IF (Tolmax < 0.0) RETURN
      Ierror = 12     ! multigrid parameters
      IF (Kcycle < 0) RETURN
      IF (min0(Iprer, Ipost) < 1) RETURN
      IF ((Intpol - 1)*(Intpol - 3) /= 0) RETURN
      IF (max0(Kcycle, Iprer, Ipost) > 2) Ierror = -5
      ! inefficient multigrid cycling
      IF (Ierror > 0) Ierror = 0        ! no fatal errors
!
!     discretize pde at each grid level
!
      DO kb = 1, Ngrid
         k = Ngrid - kb + 1
         nx = Nxk(k)
         ny = Nyk(k)
         nz = Nzk(k)
         icx = Kcxbgn(k)
         icy = Kcybgn(k)
         icz = Kczbgn(k)
         CALL dismd3sp(nx, ny, nz, Work(icx), Work(icy), Work(icz), bndyc, cfx, cfy, cfz, Ierror)
      END DO
      RETURN
   END IF       ! end of intl=0 initialization call block
   nx = Nfx
   ny = Nfy
   nz = Nfz
   CALL mud3sp1(nx, ny, nz, Rhs, Phi, cfx, cfy, cfz, bndyc, Work)
   Iparm(22) = Itero
   IF (Ierror <= 0) THEN
      IF (Tolmax > 0.0) THEN
!
!     set final computed maximum relative difference
!
         Fparm(8) = Relmax
!
!     flag convergence failure
!
         IF (Relmax > Tolmax .AND. Ierror == 0) Ierror = -1
      END IF
   END IF
END SUBROUTINE mud3sp
!*==MUD3SP1.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE mud3sp1(Nx, Ny, Nz, Rhsf, Phif, cfx, cfy, cfz, bndyc, Wk)
   USE C_FMUD3SP
   USE C_IMUD3SP
   USE C_MUD3SPC
   USE S_ADJMD3SP
   USE S_KCYMD3SP
   USE S_PROLON3
   USE S_SWK3
   USE S_TRSFC3
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(INOUT) :: Nx
   INTEGER, INTENT(INOUT) :: Ny
   INTEGER, INTENT(INOUT) :: Nz
   REAL, DIMENSION(Nx, Ny, Nz) :: Rhsf
   REAL, INTENT(INOUT), DIMENSION(Nx, Ny, Nz) :: Phif
   EXTERNAL cfx
   EXTERNAL cfy
   EXTERNAL cfz
   EXTERNAL bndyc
   REAL, DIMENSION(*) :: Wk
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ijk, ip, ipc, ir, irc, iter, j, jk, k, kb, kk, ncx, ncy, ncz
   REAL :: phmax
!
! End of declarations rewritten by SPAG
!
   Nx = Nxk(Ngrid)
   Ny = Nyk(Ngrid)
   Nz = Nzk(Ngrid)
   ip = Kpbgn(Ngrid)
   ir = Krbgn(Ngrid)
!
!     set phif,rhsf in wk
!
   CALL swk3(Nx, Ny, Nz, Phif, Rhsf, Wk(ip), Wk(ir))
   IF (Iguess == 0) THEN
!
!     no initial guess at finest grid level!
!
      DO kb = 2, Ngrid
         k = Ngrid - kb + 1
         Nx = Nxk(k + 1)
         Ny = Nyk(k + 1)
         Nz = Nzk(k + 1)
         ip = Kpbgn(k + 1)
         ir = Krbgn(k + 1)
         ncx = Nxk(k)
         ncy = Nyk(k)
         ncz = Nzk(k)
         ipc = Kpbgn(k)
         irc = Krbgn(k)
!
!     transfer down to all grid levels
!
         CALL trsfc3(Nx, Ny, Nz, Wk(ip), Wk(ir), ncx, ncy, ncz, Wk(ipc), Wk(irc))
      END DO
!
!     adjust right hand side at all grid levels in case
!     rhs or specified b.c. in phi or gbdy changed
!
      DO kb = 1, Ngrid
         k = Ngrid - kb + 1
         Nx = Nxk(k)
         Ny = Nyk(k)
         Nz = Nzk(k)
         ip = Kpbgn(k)
         ir = Krbgn(k)
         CALL adjmd3sp(Nx, Ny, Nz, Wk(ip), Wk(ir), bndyc, cfx, cfy, cfz)
      END DO
!
!     execute one full multigrid cycle
!
      DO k = 1, Ngrid - 1
         Kcur = k
         CALL kcymd3sp(Wk)
         Nx = Nxk(k + 1)
         Ny = Nyk(k + 1)
         Nz = Nzk(k + 1)
         ip = Kpbgn(k + 1)
         ipc = Kpbgn(k)
         ncx = Nxk(k)
         ncy = Nyk(k)
         ncz = Nzk(k)
!
!     lift or prolong approximation from k to k+1
!
         CALL prolon3(ncx, ncy, ncz, Wk(ipc), Nx, Ny, Nz, Wk(ip), Nxa, Nxb, Nyc, Nyd, Nze, Nzf, Intpol)
      END DO
   ELSE
!
!     adjust rhs at finest grid level only
!
      Nx = Nxk(Ngrid)
      Ny = Nyk(Ngrid)
      Nz = Nzk(Ngrid)
      ip = Kpbgn(Ngrid)
      ir = Krbgn(Ngrid)
      CALL adjmd3sp(Nx, Ny, Nz, Wk(ip), Wk(ir), bndyc, cfx, cfy, cfz)
   END IF
!
!     execute maxcy more multigrid k cycles from finest level
!
   Kcur = Ngrid
   DO iter = 1, Maxcy
      Itero = iter
      CALL kcymd3sp(Wk)
      IF (Tolmax > 0.0) THEN
!
!      error control
!
         Relmax = 0.0
         phmax = 0.0
         DO k = 1, Nfz
            kk = k*(Nfx + 2)*(Nfy + 2)
            DO j = 1, Nfy
               jk = kk + j*(Nfx + 2)
               DO i = 1, Nfx
                  ijk = jk + i + 1
                  phmax = amax1(phmax, abs(Wk(ijk)))
                  Relmax = amax1(Relmax, abs(Wk(ijk) - Phif(i, j, k)))
                  Phif(i, j, k) = Wk(ijk)
               END DO
            END DO
         END DO
!
!     set maximum relative difference and check for convergence
!
         IF (phmax > 0.0) Relmax = Relmax/phmax
         IF (Relmax <= Tolmax) RETURN
      END IF
   END DO
!
!     set final iterate in phif
!
   DO k = 1, Nfz
      kk = k*(Nfx + 2)*(Nfy + 2)
      DO j = 1, Nfy
         jk = kk + j*(Nfx + 2)
         DO i = 1, Nfx
            ijk = jk + i + 1
            Phif(i, j, k) = Wk(ijk)
         END DO
      END DO
   END DO
END SUBROUTINE mud3sp1
!*==KCYMD3SP.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE kcymd3sp(Wk)
!
!     perform multigrid k-cycle at kcur level
!     kcycle = 1 corresponds to v cycles
!     kcycle = 2 corresponds to w cycles
!
   USE C_IMUD3SP
   USE C_MUD3SPC
   USE S_COR3
   USE S_RELMD3SP
   USE S_RESMD3SP
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL, DIMENSION(*) :: Wk
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: icx, icy, icz, ip, ipc, ir, irc, l, ncx, ncy, ncz, nrel, nx, ny, nz
   INTEGER, DIMENSION(50) :: kount
!
! End of declarations rewritten by SPAG
!
   Klevel = Kcur
!
!     pre-relax at current finest grid level
!
   DO l = 1, Iprer
      CALL relmd3sp(Wk)
   END DO
!
!     if at coarsest level post-relax
!
   IF (Kcur /= 1) THEN
!
!     restrict residual to kcur-1
!
      nx = Nxk(Klevel)
      ny = Nyk(Klevel)
      nz = Nzk(Klevel)
      ip = Kpbgn(Klevel)
      ir = Krbgn(Klevel)
      icx = Kcxbgn(Klevel)
      icy = Kcybgn(Klevel)
      icz = Kczbgn(Klevel)
      ipc = Kpbgn(Klevel - 1)
      ncx = Nxk(Klevel - 1)
      ncy = Nyk(Klevel - 1)
      ncz = Nzk(Klevel - 1)
      irc = Krbgn(Klevel - 1)
!
!     use full weighting with residual restriction
!
      CALL resmd3sp(nx, ny, nz, Wk(ip), Wk(ir), Wk(icx), Wk(icy), Wk(icz), ncx, ncy, ncz, Wk(ipc), Wk(irc), Wk(Kps))
!
!     set counter for grid levels to zero
!
      DO l = 1, Kcur
         kount(l) = 0
      END DO
!
!    set new level and continue k-cycling
!
      Klevel = Kcur - 1
      nrel = Iprer
!
!     kcycle control point
!
!
!     post-relax when kcur revisited
!
      DO WHILE (Klevel /= Kcur)
!
!     count "hit" at current level
!
         kount(Klevel) = kount(Klevel) + 1
!
!     relax at current level
!
         DO l = 1, nrel
            CALL relmd3sp(Wk)
         END DO
         IF (kount(Klevel) == Kcycle + 1) THEN
!
!     kcycle(iprer,ipost) complete at klevel
!     inject correction to finer grid
!
            nx = Nxk(Klevel + 1)
            ny = Nyk(Klevel + 1)
            nz = Nzk(Klevel + 1)
            ip = Kpbgn(Klevel + 1)
            ncx = Nxk(Klevel)
            ncy = Nyk(Klevel)
            ncz = Nzk(Klevel)
            ipc = Kpbgn(Klevel)
            CALL cor3(nx, ny, nz, Wk(ip), ncx, ncy, ncz, Wk(ipc), Nxa, Nxb, Nyc, Nyd, Nze, Nzf, Intpol, Wk(Kps))
!
!     reset counter to zero at klevel
!
            kount(Klevel) = 0
!
!     ascend to next higher level and set to post-relax there
!
            Klevel = Klevel + 1
            nrel = Ipost
!
!     kcycle not complete so descend unless at coarsest
!
         ELSEIF (Klevel > 1) THEN
            nx = Nxk(Klevel)
            ny = Nyk(Klevel)
            nz = Nzk(Klevel)
            ip = Kpbgn(Klevel)
            ir = Krbgn(Klevel)
            icx = Kcxbgn(Klevel)
            icy = Kcybgn(Klevel)
            icz = Kczbgn(Klevel)
            ncx = Nxk(Klevel - 1)
            ncy = Nyk(Klevel - 1)
            ncz = Nzk(Klevel - 1)
            irc = Krbgn(Klevel - 1)
            ipc = Kpbgn(Klevel - 1)
            CALL resmd3sp(nx, ny, nz, Wk(ip), Wk(ir), Wk(icx), Wk(icy), Wk(icz), ncx, ncy, ncz, Wk(ipc), Wk(irc), Wk(Kps))
!
!     pre-relax at next coarser level
!
            Klevel = Klevel - 1
            nrel = Iprer
         ELSE
!
!     post-relax at coarsest level (klevel=1)
!
            DO l = 1, Ipost
               CALL relmd3sp(Wk)
            END DO
!
!     inject correction to grid level 2
!
            ipc = Kpbgn(1)
            ncx = Nxk(1)
            ncy = Nyk(1)
            ncz = Nzk(1)
            ip = Kpbgn(2)
            nx = Nxk(2)
            ny = Nyk(2)
            nz = Nzk(2)
            CALL cor3(nx, ny, nz, Wk(ip), ncx, ncy, ncz, Wk(ipc), Nxa, Nxb, Nyc, Nyd, Nze, Nzf, Intpol, Wk(Kps))
!
!     set to post-relax at level 2
!
            nrel = Ipost
            Klevel = 2
         END IF
      END DO
   END IF
!
!     post-relax at kcur level
!
   DO l = 1, Ipost
      CALL relmd3sp(Wk)
   END DO
END SUBROUTINE kcymd3sp
!*==RESMD3SP.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE resmd3sp(Nx, Ny, Nz, Phi, Rhs, Cofx, Cofy, Cofz, Ncx, Ncy, Ncz, Phic, Rhsc, Resf)
!
!     compute fully weighted residual restriction in rhsc
!
   USE C_IMUD3SP
   USE S_RES3
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
   REAL, INTENT(IN), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Phi
   REAL, INTENT(IN), DIMENSION(Nx, Ny, Nz) :: Rhs
   REAL, INTENT(IN), DIMENSION(Nx, 3) :: Cofx
   REAL, INTENT(IN), DIMENSION(Ny, 3) :: Cofy
   REAL, INTENT(IN), DIMENSION(Nz, 3) :: Cofz
   REAL, INTENT(OUT), DIMENSION(0:Ncx + 1, 0:Ncy + 1, 0:Ncz + 1) :: Phic
   REAL, DIMENSION(Ncx, Ncy, Ncz) :: Rhsc
   REAL, DIMENSION(Nx, Ny, Nz) :: Resf
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ic, ifn, ist, j, jc, jfn, jst, k, kc, kfn, kst
!
! End of declarations rewritten by SPAG
!
!
!     initialize phic to zero
!
   DO kc = 0, Ncz + 1
      DO jc = 0, Ncy + 1
         DO ic = 0, Ncx + 1
            Phic(ic, jc, kc) = 0.0
         END DO
      END DO
   END DO
!
!     intialize residual to zero and set limits
!
   DO k = 1, Nz
      DO j = 1, Ny
         DO i = 1, Nx
            Resf(i, j, k) = 0.0
         END DO
      END DO
   END DO
!
!     set loop limits
!
   ist = 1
   IF (Nxa == 1) ist = 2
   ifn = Nx
   IF (Nxb == 1) ifn = Nx - 1
   jst = 1
   IF (Nyc == 1) jst = 2
   jfn = Ny
   IF (Nyd == 1) jfn = Ny - 1
   kst = 1
   IF (Nze == 1) kst = 2
   kfn = Nz
   IF (Nzf == 1) kfn = Nz - 1
!
!     compute fine grid residual
!
!$OMP PARALLEL DO PRIVATE(i,j,k), SHARED(phi,nx,ny,nz)
!$OMP+SHARED(ist,ifn,jst,jfn,kst,kfn,cofx,cofy,cofz,rhs)
   DO k = kst, kfn
      DO j = jst, jfn
         DO i = ist, ifn
            Resf(i,j,k) = Rhs(i,j,k) - (Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)&
                & + Cofz(k, 1)*Phi(i, j, k - 1) + Cofz(k, 2)*Phi(i, j, k + 1) + (Cofx(i, 3) + Cofy(j, 3) + Cofz(k, 3))*Phi(i, j, k))
         END DO
      END DO
   END DO
!
!     restrict resf to interior coarse mesh in rhsc
!     using fully weighted residual restriction in 3-d
!
   CALL res3(Nx, Ny, Nz, Resf, Ncx, Ncy, Ncz, Rhsc, Nxa, Nxb, Nyc, Nyd, Nze, Nzf)
END SUBROUTINE resmd3sp
!*==DISMD3SP.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE dismd3sp(Nx, Ny, Nz, Cofx, Cofy, Cofz, bndyc, cfx, cfy, cfz, Ier)
!
!     discretize the 3-d elliptic pde
!
   USE C_FMUD3SP
   USE C_IMUD3SP
   USE C_MUD3SPC
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nx
   INTEGER, INTENT(IN) :: Ny
   INTEGER, INTENT(IN) :: Nz
   REAL, INTENT(INOUT), DIMENSION(Nx, 3) :: Cofx
   REAL, INTENT(INOUT), DIMENSION(Ny, 3) :: Cofy
   REAL, INTENT(INOUT), DIMENSION(Nz, 3) :: Cofz
   EXTERNAL bndyc
   EXTERNAL cfx
   EXTERNAL cfy
   EXTERNAL cfz
   INTEGER, INTENT(OUT) :: Ier
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alfa, alfmax, c1, c2, cemax, cex, cey, cez, cmin, cx, cxx, cy, cyy, cz, czz, dlx, dlx2, dlxx, dly,   &
         & dly2, dlyy, dlz, dlz2, dlzz, gbdy, x, y, z
   INTEGER :: i, ifn, ist, j, jfn, jst, k, kbdy, kfn, kst
!
! End of declarations rewritten by SPAG
!
!
!     set current grid increments
!
   dlx = (Xb - Xa)/(Nx - 1)
   dlx2 = dlx + dlx
   dlxx = dlx*dlx
   dly = (Yd - Yc)/(Ny - 1)
   dly2 = dly + dly
   dlyy = dly*dly
   dlz = (Zf - Ze)/(Nz - 1)
   dlz2 = dlz + dlz
   dlzz = dlz*dlz
   cmin = 1.0
   cemax = 0.0
!
!     set x,y,z subscript limits to bypass specified boundaries
!     when calling cfx,cfy,cfz or bndyc
!
   jst = 1
   jfn = Ny
   ist = 1
   ifn = Nx
   kst = 1
   kfn = Nz
   IF (Nxa == 1) ist = 2
   IF (Nxb == 1) ifn = Nx - 1
   IF (Nyc == 1) jst = 2
   IF (Nyd == 1) jfn = Ny - 1
   IF (Nze == 1) kst = 2
   IF (Nzf == 1) kfn = Nz - 1
   DO i = ist, ifn
      x = Xa + (i - 1)*dlx
      CALL cfx(x, cxx, cx, cex)
      cmin = amin1(cmin, cxx)
      cemax = amax1(abs(cex), cemax)
!
!     check if pde is "hyperbolic" at finest grid level
!
      IF (Klevel == Ngrid) THEN
         IF ((abs(cx)*dlx > abs(cxx + cxx))) Ier = -4
      END IF
!
!     adjust second order coefficients so that pde is not "hyperbolic"
!     this is especially possible on coarser grids if there are non-zero
!     first order terms
!
      cxx = amax1(cxx, abs(cx)*dlx*0.5)
      Cofx(i, 1) = cxx/dlxx - cx/dlx2
      Cofx(i, 2) = cxx/dlxx + cx/dlx2
      Cofx(i, 3) = cex - (Cofx(i, 1) + Cofx(i, 2))
   END DO
   DO j = jst, jfn
      y = Yc + (j - 1)*dly
      CALL cfy(y, cyy, cy, cey)
      cmin = amin1(cmin, cyy)
      cemax = amax1(abs(cey), cemax)
!
!     check if pde is "hyperbolic" at finest grid level
!
      IF (Klevel == Ngrid) THEN
         IF ((abs(cy)*dly > abs(cyy + cyy))) Ier = -4
      END IF
!
!     adjust second order coefficients so that pde is not "hyperbolic"
!     this is especially possible on coarser grids if there are non-zero
!     first order terms
!
      cyy = amax1(cyy, abs(cy)*dly*0.5)
      Cofy(j, 1) = cyy/dlyy - cy/dly2
      Cofy(j, 2) = cyy/dlyy + cy/dly2
      Cofy(j, 3) = cey - (Cofy(j, 1) + Cofy(j, 2))
   END DO
   DO k = kst, kfn
      z = Ze + (k - 1)*dlz
      CALL cfz(z, czz, cz, cez)
      cmin = amin1(cmin, czz)
      cemax = amax1(abs(cez), cemax)
!
!     check if pde is "hyperbolic" at finest grid level
!
      IF (Klevel == Ngrid) THEN
         IF ((abs(cz)*dlz > abs(czz + czz))) Ier = -4
      END IF
!
!     adjust second order coefficients so that pde is not "hyperbolic"
!     this is especially possible on coarser grids if there are non-zero
!     first order terms
!
      czz = amax1(czz, abs(cz)*dlz*0.5)
      Cofz(k, 1) = czz/dlzz - cz/dlz2
      Cofz(k, 2) = czz/dlzz + cz/dlz2
      Cofz(k, 3) = cez - (Cofz(k, 1) + Cofz(k, 2))
   END DO
!
!     set nonfatal error flag if ellipticity test fails
!
   IF (cmin <= 0.0) Ier = -2
   alfmax = 0.0
!
!     adjust equation at mixed b.c.
!
   IF (Nxa == 2) THEN
      kbdy = 1
      i = 1
      c1 = Cofx(i, 1)
      Cofx(i, 1) = 0.0
      Cofx(i, 2) = Cofx(i, 2) + c1
      y = Yc + dly
      z = Ze + dlz
!
!     compute constant coefficient alfa
!
      CALL bndyc(kbdy, y, z, alfa, gbdy)
      alfmax = amax1(alfmax, abs(alfa))
      Cofx(i, 3) = Cofx(i, 3) + dlx2*alfa*c1
   END IF
   IF (Nxb == 2) THEN
      kbdy = 2
      i = Nx
      y = Yc + dly
      z = Ze + dlz
!
!     compute constant coefficient alfa
!
      CALL bndyc(kbdy, y, z, alfa, gbdy)
      c2 = Cofx(i, 2)
      Cofx(i, 1) = Cofx(i, 1) + c2
      Cofx(i, 2) = 0.0
      Cofx(i, 3) = Cofx(i, 3) - dlx2*alfa*c2
      alfmax = amax1(abs(alfa), alfmax)
   END IF
   IF (Nyc == 2) THEN
      kbdy = 3
      j = 1
      x = Xa + dlx
      z = Ze + dlz
!
!     compute constant coefficient alfa
!
      CALL bndyc(kbdy, x, z, alfa, gbdy)
      c1 = Cofy(j, 1)
      Cofy(j, 1) = 0.0
      Cofy(j, 2) = Cofy(j, 2) + c1
      Cofy(j, 3) = Cofy(j, 3) + dly2*alfa*c1
      alfmax = amax1(abs(alfa), alfmax)
   END IF
   IF (Nyd == 2) THEN
      kbdy = 4
      j = Ny
      x = Xa + dlx
      z = Ze + dlz
!
!     compute constant coefficient alfa
!
      CALL bndyc(kbdy, x, z, alfa, gbdy)
      c2 = Cofy(j, 2)
      Cofy(j, 2) = 0.0
      Cofy(j, 1) = Cofy(j, 1) + c2
      Cofy(j, 3) = Cofy(j, 3) - dly2*c2*alfa
      alfmax = amax1(abs(alfa), alfmax)
   END IF
   IF (Nze == 2) THEN
      kbdy = 5
      k = 1
      x = Xa + dlx
      y = Yc + dly
!
!     compute constant coefficient alfa
!
      CALL bndyc(kbdy, x, y, alfa, gbdy)
      c1 = Cofz(k, 1)
      Cofz(k, 1) = 0.0
      Cofz(k, 2) = Cofz(k, 2) + c1
      Cofz(k, 3) = Cofz(k, 3) + dlz2*alfa*c1
      alfmax = amax1(abs(alfa), alfmax)
   END IF
   IF (Nzf == 2) THEN
      kbdy = 6
      k = Nz
      x = Xa + dlx
      y = Yc + dly
!
!     compute constant coefficient alfa
!
      CALL bndyc(kbdy, x, y, alfa, gbdy)
      c2 = Cofz(k, 2)
      Cofz(k, 2) = 0.0
      Cofz(k, 1) = Cofz(k, 1) + c2
      Cofz(k, 3) = Cofz(k, 3) - dlz2*alfa*c2
      alfmax = amax1(abs(alfa), alfmax)
   END IF
!
!     flag continuous singular elliptic pde if detected
!
   IF (cemax == 0.0 .AND. alfmax == 0.0) THEN
      IF (Nxa == 0 .OR. (Nxa == 2 .AND. Nxb == 2)) THEN
         IF (Nyc == 0 .OR. (Nyc == 2 .AND. Nyd == 2)) THEN
            IF (Nze == 0 .OR. (Nze == 2 .AND. Nzf == 2)) Ier = -3
         END IF
      END IF
   END IF
END SUBROUTINE dismd3sp
!*==ADJMD3SP.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE adjmd3sp(Nx, Ny, Nz, Phi, Rhs, bndyc, cfx, cfy, cfz)
!
!     adjust rhs for solution in cof(i,j,k,8) on non-initial calls
!     (i.e., values in cof have not changed)
!
   USE C_FMUD3SP
   USE C_IMUD3SP
   USE C_MUD3SPC
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER, INTENT(IN) :: Nx
   INTEGER, INTENT(IN) :: Ny
   INTEGER, INTENT(IN) :: Nz
   REAL, INTENT(IN), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Phi
   REAL, INTENT(INOUT), DIMENSION(Nx, Ny, Nz) :: Rhs
   EXTERNAL bndyc
   EXTERNAL cfx
   EXTERNAL cfy
   EXTERNAL cfz
!
! Local variable declarations rewritten by SPAG
!
   REAL :: alfa, c1, c2, cex, cey, cez, cx, cxx, cy, cyy, cz, czz, dlx, dlx2, dlxx, dly, dly2, dlyy, dlz, dlz2,&
         & dlzz, gbdy, x, y, z
   INTEGER :: i, ifn, ist, j, jfn, jst, k, kbdy, kfn, kst
!
! End of declarations rewritten by SPAG
!
!
!     set current grid increments
!
   dlx = (Xb - Xa)/(Nx - 1)
   dlx2 = dlx + dlx
   dlxx = dlx*dlx
   dly = (Yd - Yc)/(Ny - 1)
   dly2 = dly + dly
   dlyy = dly*dly
   dlz = (Zf - Ze)/(Nz - 1)
   dlz2 = dlz + dlz
   dlzz = dlz*dlz
!
!     set x,y,z subscript limits for calls to cfx,cfy,cfz,bndyc
!
   jst = 1
   jfn = Ny
   ist = 1
   ifn = Nx
   kst = 1
   kfn = Nz
   IF (Nxa == 1) ist = 2
   IF (Nxb == 1) ifn = Nx - 1
   IF (Nyc == 1) jst = 2
   IF (Nyd == 1) jfn = Ny - 1
   IF (Nze == 1) kst = 2
   IF (Nzf == 1) kfn = Nz - 1
!
!     adjust for derivative b.c.
!
   IF (Nxa == 2) THEN
      kbdy = 1
      x = Xa
      i = 1
      CALL cfx(x, cxx, cx, cex)
      cxx = amax1(cxx, abs(cx)*dlx*0.5)
      c1 = cxx/dlxx - cx/dlx2
      DO k = kst, kfn
         z = Ze + (k - 1)*dlz
         DO j = jst, jfn
            y = Yc + (j - 1)*dly
            CALL bndyc(kbdy, y, z, alfa, gbdy)
            Rhs(i, j, k) = Rhs(i, j, k) + dlx2*c1*gbdy
         END DO
      END DO
   END IF
   IF (Nxb == 2) THEN
      kbdy = 2
      x = Xb
      i = Nx
      CALL cfx(x, cxx, cx, cex)
      cxx = amax1(cxx, abs(cx)*dlx*0.5)
      c2 = cxx/dlxx + cx/dlx2
      DO k = kst, kfn
         z = Ze + (k - 1)*dlz
         DO j = jst, jfn
            y = Yc + (j - 1)*dly
            CALL bndyc(kbdy, y, z, alfa, gbdy)
            Rhs(i, j, k) = Rhs(i, j, k) - dlx2*c2*gbdy
         END DO
      END DO
   END IF
   IF (Nyc == 2) THEN
      kbdy = 3
      y = Yc
      j = 1
      CALL cfy(y, cyy, cy, cey)
      cyy = amax1(cyy, abs(cy)*dly*0.5)
      c1 = cyy/dlyy - cy/dly2
      DO k = kst, kfn
         z = Ze + (k - 1)*dlz
         DO i = ist, ifn
            x = Xa + (i - 1)*dlx
            CALL bndyc(kbdy, x, z, alfa, gbdy)
            Rhs(i, j, k) = Rhs(i, j, k) + dly2*c1*gbdy
         END DO
      END DO
   END IF
   IF (Nyd == 2) THEN
      kbdy = 4
      y = Yd
      j = Ny
      CALL cfy(y, cyy, cy, cey)
      cyy = amax1(cyy, abs(cy)*dly*0.5)
      c2 = cyy/dlyy + cy/dly2
      DO k = kst, kfn
         z = Ze + (k - 1)*dlz
         DO i = ist, ifn
            x = Xa + (i - 1)*dlx
            CALL bndyc(kbdy, x, z, alfa, gbdy)
            Rhs(i, j, k) = Rhs(i, j, k) - dly2*c2*gbdy
         END DO
      END DO
   END IF
   IF (Nze == 2) THEN
      kbdy = 5
      k = 1
      z = Ze
      CALL cfz(z, czz, cz, cez)
      czz = amax1(czz, abs(cz)*dlz*0.5)
      c1 = czz/dlzz - cz/dlz2
      DO j = jst, jfn
         y = Yc + (j - 1)*dly
         DO i = ist, ifn
            x = Xa + (i - 1)*dlx
            CALL bndyc(kbdy, x, y, alfa, gbdy)
            Rhs(i, j, k) = Rhs(i, j, k) + dlz2*c1*gbdy
         END DO
      END DO
   END IF
   IF (Nzf == 2) THEN
      kbdy = 6
      z = Zf
      k = Nz
      CALL cfz(z, czz, cz, cez)
      czz = amax1(czz, abs(cz)*dlz*0.5)
      c2 = czz/dlzz + cz/dlz2
      DO j = jst, jfn
         y = Yc + (j - 1)*dly
         DO i = ist, ifn
            x = Xa + (i - 1)*dlx
            CALL bndyc(kbdy, x, y, alfa, gbdy)
            Rhs(i, j, k) = Rhs(i, j, k) - dlz2*c2*gbdy
         END DO
      END DO
   END IF
!
!     set specified b.c.
!
   IF (Nxa == 1) THEN
      i = 1
      DO j = 1, Ny
         DO k = 1, Nz
            Rhs(i, j, k) = Phi(i, j, k)
         END DO
      END DO
   END IF
   IF (Nxb == 1) THEN
      i = Nx
      DO j = 1, Ny
         DO k = 1, Nz
            Rhs(i, j, k) = Phi(i, j, k)
         END DO
      END DO
   END IF
   IF (Nyc == 1) THEN
      j = 1
      DO k = 1, Nz
         DO i = 1, Nx
            Rhs(i, j, k) = Phi(i, j, k)
         END DO
      END DO
   END IF
   IF (Nyd == 1) THEN
      j = Ny
      DO k = 1, Nz
         DO i = 1, Nx
            Rhs(i, j, k) = Phi(i, j, k)
         END DO
      END DO
   END IF
   IF (Nze == 1) THEN
      k = 1
      DO j = 1, Ny
         DO i = 1, Nx
            Rhs(i, j, k) = Phi(i, j, k)
         END DO
      END DO
   END IF
   IF (Nzf == 1) THEN
      k = Nz
      DO j = 1, Ny
         DO i = 1, Nx
            Rhs(i, j, k) = Phi(i, j, k)
         END DO
      END DO
   END IF
END SUBROUTINE adjmd3sp
!*==RELMD3SP.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE relmd3sp(Wk)
!
!     use point or line relaxation in the x and/or y and/or z
!     or planar relaxation in the x,y or x,z or y,z planes
!
   USE C_FMUD3SP
   USE C_IMUD3SP
   USE C_MUD3SPC
   USE S_RELMD3SPP
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   REAL, DIMENSION(*) :: Wk
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: icx, icy, icz, ip, ir, nx, ny, nz
!
! End of declarations rewritten by SPAG
!
   nx = Nxk(Klevel)
   ny = Nyk(Klevel)
   nz = Nzk(Klevel)
   ip = Kpbgn(Klevel)
   ir = Krbgn(Klevel)
   icx = Kcxbgn(Klevel)
   icy = Kcybgn(Klevel)
   icz = Kczbgn(Klevel)
!
!     gauss-seidel pointwise red/black relaxation
!
   CALL relmd3spp(nx, ny, nz, Wk(ip), Wk(ir), Wk(icx), Wk(icy), Wk(icz))
END SUBROUTINE relmd3sp
!*==RELMD3SPP.f90 processed by SPAG 8.04DB 16:49  8 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code

SUBROUTINE relmd3spp(Nx, Ny, Nz, Phi, Rhs, Cofx, Cofy, Cofz)
!
!     gauss-seidel point relaxation with red/black ordering
!     in three dimensions for nonseparable pde
!     relax in order:
!     (1) red (x,y) on odd z planes
!     (2) black (x,y) on even z planes
!     (3) black (x,y) on odd z planes
!     (4) red (x,y) on even z planes
!
   USE C_IMUD3SP
   USE S_PER3VB
   IMPLICIT NONE
!
! Dummy argument declarations rewritten by SPAG
!
   INTEGER :: Nx
   INTEGER :: Ny
   INTEGER :: Nz
   REAL, INTENT(INOUT), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Phi
   REAL, INTENT(IN), DIMENSION(Nx, Ny, Nz) :: Rhs
   REAL, INTENT(IN), DIMENSION(Nx, 3) :: Cofx
   REAL, INTENT(IN), DIMENSION(Ny, 3) :: Cofy
   REAL, INTENT(IN), DIMENSION(Nz, 3) :: Cofz
!
! Local variable declarations rewritten by SPAG
!
   INTEGER :: i, ifn, ist, j, jfn, jst, k, kfn, kst, nper
!
! End of declarations rewritten by SPAG
!
!
!     set periodic b.c. indicator
!
   nper = Nxa*Nyc*Nze
!
!     set loop limits to avoid specified boundaries
!     in red/black sweeps
!
   ist = 1
   IF (Nxa == 1) ist = 3
   ifn = Nx
   IF (Nxb == 1) ifn = Nx - 1
   jst = 1
   IF (Nyc == 1) jst = 3
   jfn = Ny
   IF (Nyd == 1) jfn = Ny - 1
   kst = 1
   IF (Nze == 1) kst = 3
   kfn = Nz
   IF (Nzf == 1) kfn = Nz - 1
!
!     set periodic boundaries if necessary
!
   IF (nper == 0) CALL per3vb(Nx, Ny, Nz, Phi, Nxa, Nyc, Nze)
!
!   red (x,y) on odd z planes
!
!$OMP PARALLEL DO SHARED(ist,ifn,jst,jfn,kst,kfn,phi,rhs,cofx,cofy,cofz)
!$OMP+PRIVATE(i,j,k)
   DO k = kst, kfn, 2
      DO i = ist, ifn, 2
         DO j = jst, jfn, 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & + Cofz(k, 1)*Phi(i, j, k - 1) + Cofz(k, 2)*Phi(i, j, k + 1)))/(Cofx(i, 3) + Cofy(j, 3) + Cofz(k, 3))
         END DO
      END DO
      DO i = 2, ifn, 2
         DO j = 2, jfn, 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & + Cofz(k, 1)*Phi(i, j, k - 1) + Cofz(k, 2)*Phi(i, j, k + 1)))/(Cofx(i, 3) + Cofy(j, 3) + Cofz(k, 3))
         END DO
      END DO
   END DO
   IF (nper == 0) CALL per3vb(Nx, Ny, Nz, Phi, Nxa, Nyc, Nze)
!
!   black (x,y) or even z planes
!
!$OMP PARALLEL DO SHARED(ist,ifn,jst,jfn,kfn,phi,rhs,cofx,cofy,cofz)
!$OMP+PRIVATE(i,j,k)
   DO k = 2, kfn, 2
      DO i = ist, ifn, 2
         DO j = 2, jfn, 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & + Cofz(k, 1)*Phi(i, j, k - 1) + Cofz(k, 2)*Phi(i, j, k + 1)))/(Cofx(i, 3) + Cofy(j, 3) + Cofz(k, 3))
         END DO
      END DO
      DO i = 2, ifn, 2
         DO j = jst, jfn, 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & + Cofz(k, 1)*Phi(i, j, k - 1) + Cofz(k, 2)*Phi(i, j, k + 1)))/(Cofx(i, 3) + Cofy(j, 3) + Cofz(k, 3))
         END DO
      END DO
   END DO
   IF (nper == 0) CALL per3vb(Nx, Ny, Nz, Phi, Nxa, Nyc, Nze)
!
!   black (x,y) on odd z planes
!
!$OMP PARALLEL DO SHARED(ist,ifn,jfn,kst,kfn,phi,rhs,cofx,cofy,cofz)
!$OMP+PRIVATE(i,j,k)
   DO k = kst, kfn, 2
      DO i = ist, ifn, 2
         DO j = 2, jfn, 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & + Cofz(k, 1)*Phi(i, j, k - 1) + Cofz(k, 2)*Phi(i, j, k + 1)))/(Cofx(i, 3) + Cofy(j, 3) + Cofz(k, 3))
         END DO
      END DO
      DO i = 2, ifn, 2
         DO j = jst, jfn, 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & + Cofz(k, 1)*Phi(i, j, k - 1) + Cofz(k, 2)*Phi(i, j, k + 1)))/(Cofx(i, 3) + Cofy(j, 3) + Cofz(k, 3))
         END DO
      END DO
   END DO
   IF (nper == 0) CALL per3vb(Nx, Ny, Nz, Phi, Nxa, Nyc, Nze)
!
!   red(x,y) on even z planes
!
!$OMP PARALLEL DO SHARED(ist,ifn,jst,jfn,kfn,phi,rhs,cofx,cofy,cofz)
!$OMP+PRIVATE(i,j,k)
   DO k = 2, kfn, 2
      DO i = ist, ifn, 2
         DO j = jst, jfn, 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & + Cofz(k, 1)*Phi(i, j, k - 1) + Cofz(k, 2)*Phi(i, j, k + 1)))/(Cofx(i, 3) + Cofy(j, 3) + Cofz(k, 3))
         END DO
      END DO
      DO i = 2, ifn, 2
         DO j = 2, jfn, 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & + Cofz(k, 1)*Phi(i, j, k - 1) + Cofz(k, 2)*Phi(i, j, k + 1)))/(Cofx(i, 3) + Cofy(j, 3) + Cofz(k, 3))
         END DO
      END DO
   END DO
   IF (nper == 0) CALL per3vb(Nx, Ny, Nz, Phi, Nxa, Nyc, Nze)
END SUBROUTINE relmd3spp
