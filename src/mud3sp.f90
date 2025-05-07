!*==MUD3SP.f90 processed by SPAG 8.04DB 19:24  7 May 2025
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
subroutine mud3sp(Iparm,Fparm,Work,cfx,cfy,cfz,bndyc,Rhs,Phi,Mgopt,Ierror)
   use c_fmud3sp
   use c_imud3sp
   use c_mud3spc
   use s_dismd3sp
   use s_mud3sp1
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(inout) , dimension(22) :: Iparm
   real , intent(inout) , dimension(8) :: Fparm
   real , dimension(*) :: Work
   external cfx
   external cfy
   external cfz
   external bndyc
   real , dimension(*) :: Rhs
   real , dimension(*) :: Phi
   integer , intent(in) , dimension(4) :: Mgopt
   integer , intent(inout) :: Ierror
!
! Local variable declarations rewritten by SPAG
!
   integer :: icx , icy , icz , k , kb , nx , ny , nz
   integer , save :: int
!
! End of declarations rewritten by SPAG
!
   data int/0/
   Ierror = 1
   intl = Iparm(1)       ! set and check intl on ALL calls
   if ( intl*(intl-1)/=0 ) return
   if ( int==0 ) then
      int = 1
      if ( intl/=0 ) return  ! very first call is not intl=0
   endif
   Ierror = 0
!
!     set input parameters from iparm,fparm internally
!
   intl = Iparm(1)
   nxa = Iparm(2)
   nxb = Iparm(3)
   nyc = Iparm(4)
   nyd = Iparm(5)
   nze = Iparm(6)
   nzf = Iparm(7)
!
!     set grid size params
!
   ixp = Iparm(8)
   jyq = Iparm(9)
   kzr = Iparm(10)
   iex = Iparm(11)
   jey = Iparm(12)
   kez = Iparm(13)
!
!     set number of subgrids for mg cycling
!
   ngrid = max0(iex,jey,kez)
   nfx = Iparm(14)
   nfy = Iparm(15)
   nfz = Iparm(16)
 
   iguess = Iparm(17)
   maxcy = Iparm(18)
   method = Iparm(19)
   nwork = Iparm(20)
!
!     set floating point params
!
   xa = Fparm(1)
   xb = Fparm(2)
   yc = Fparm(3)
   yd = Fparm(4)
   ze = Fparm(5)
   zf = Fparm(6)
   tolmax = Fparm(7)
!
!     set multigrid option parameters
!
   kcycle = Mgopt(1)
   if ( kcycle==0 ) then
!
!     use default settings
!
      kcycle = 2
      iprer = 2
      ipost = 1
      intpol = 3
   else
      iprer = Mgopt(2)
      ipost = Mgopt(3)
      intpol = Mgopt(4)
   endif
   if ( intl==0 ) then       ! intialization call
!
!     check input arguments
!
      Ierror = 2   ! check boundary condition flags
      if ( max0(nxa,nxb,nyc,nyd,nze,nzf)>2 ) return
      if ( min0(nxa,nxb,nyc,nyd,nze,nzf)<0 ) return
      if ( nxa==0 .and. nxb/=0 ) return
      if ( nxa/=0 .and. nxb==0 ) return
      if ( nyc==0 .and. nyd/=0 ) return
      if ( nyc/=0 .and. nyd==0 ) return
      if ( nze==0 .and. nzf/=0 ) return
      if ( nze/=0 .and. nzf==0 ) return
      Ierror = 3   ! check grid sizes
      if ( ixp<2 ) return
      if ( jyq<2 ) return
      if ( kzr<2 ) return
      Ierror = 4
      ngrid = max0(iex,jey,kez)
      if ( iex<1 ) return
      if ( jey<1 ) return
      if ( kez<1 ) return
      if ( ngrid>50 ) return
      Ierror = 5
      if ( nfx/=ixp*2**(iex-1)+1 ) return
      if ( nfy/=jyq*2**(jey-1)+1 ) return
      if ( nfz/=kzr*2**(kez-1)+1 ) return
      Ierror = 6
      if ( iguess*(iguess-1)/=0 ) return
      Ierror = 7
      if ( maxcy<1 ) return
      Ierror = 8
      if ( method/=0 ) return
      Ierror = 9
!
!     set subgrid sizes and pointers
!
      kps = 1
      do kb = 1 , ngrid
         k = ngrid - kb + 1
         nxk(k) = ixp*2**(max0(k+iex-ngrid,1)-1) + 1
         nyk(k) = jyq*2**(max0(k+jey-ngrid,1)-1) + 1
         nzk(k) = kzr*2**(max0(k+kez-ngrid,1)-1) + 1
         nx = nxk(k)
         ny = nyk(k)
         nz = nzk(k)
         kpbgn(k) = kps
         krbgn(k) = kpbgn(k) + (nx+2)*(ny+2)*(nz+2)
         kcxbgn(k) = krbgn(k) + nx*ny*nz
         kcybgn(k) = kcxbgn(k) + 3*nx
         kczbgn(k) = kcybgn(k) + 3*ny
         kps = kczbgn(k) + 3*nz
      enddo
!
!     set and check minimal work space
!
      nx = nxk(ngrid)
      ny = nyk(ngrid)
      nz = nzk(ngrid)
      Iparm(21) = kps + (nx+2)*(ny+2)*(nz+2)
      lwork = Iparm(21)
      if ( lwork>nwork ) return
      Ierror = 10   ! check solution region
      if ( xb<=xa .or. yd<=yc .or. zf<=ze ) return
      Ierror = 11
      if ( tolmax<0.0 ) return
      Ierror = 12   ! multigrid parameters
      if ( kcycle<0 ) return
      if ( min0(iprer,ipost)<1 ) return
      if ( (intpol-1)*(intpol-3)/=0 ) return
      if ( max0(kcycle,iprer,ipost)>2 ) Ierror = -5
                      ! inefficient multigrid cycling
      if ( Ierror>0 ) Ierror = 0      ! no fatal errors
!
!     discretize pde at each grid level
!
      do kb = 1 , ngrid
         k = ngrid - kb + 1
         nx = nxk(k)
         ny = nyk(k)
         nz = nzk(k)
         icx = kcxbgn(k)
         icy = kcybgn(k)
         icz = kczbgn(k)
         call dismd3sp(nx,ny,nz,Work(icx),Work(icy),Work(icz),bndyc,cfx,cfy,cfz,Ierror)
      enddo
      return
   endif       ! end of intl=0 initialization call block
   nx = nfx
   ny = nfy
   nz = nfz
   call mud3sp1(nx,ny,nz,Rhs,Phi,cfx,cfy,cfz,bndyc,Work)
   Iparm(22) = itero
   if ( Ierror<=0 ) then
      if ( tolmax>0.0 ) then
!
!     set final computed maximum relative difference
!
         Fparm(8) = relmax
!
!     flag convergence failure
!
         if ( relmax>tolmax .and. Ierror==0 ) Ierror = -1
      endif
   endif
end subroutine mud3sp
!*==MUD3SP1.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine mud3sp1(Nx,Ny,Nz,Rhsf,Phif,cfx,cfy,cfz,bndyc,Wk)
   use c_fmud3sp
   use c_imud3sp
   use c_mud3spc
   use s_adjmd3sp
   use s_kcymd3sp
   use s_prolon3
   use s_swk3
   use s_trsfc3
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(inout) :: Nx
   integer , intent(inout) :: Ny
   integer , intent(inout) :: Nz
   real , dimension(Nx,Ny,Nz) :: Rhsf
   real , intent(inout) , dimension(Nx,Ny,Nz) :: Phif
   external cfx
   external cfy
   external cfz
   external bndyc
   real , dimension(*) :: Wk
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ijk , ip , ipc , ir , irc , iter , j , jk , k , kb , kk , ncx , ncy , ncz
   real :: phmax
!
! End of declarations rewritten by SPAG
!
   Nx = nxk(ngrid)
   Ny = nyk(ngrid)
   Nz = nzk(ngrid)
   ip = kpbgn(ngrid)
   ir = krbgn(ngrid)
!
!     set phif,rhsf in wk
!
   call swk3(Nx,Ny,Nz,Phif,Rhsf,Wk(ip),Wk(ir))
   if ( iguess==0 ) then
!
!     no initial guess at finest grid level!
!
      do kb = 2 , ngrid
         k = ngrid - kb + 1
         Nx = nxk(k+1)
         Ny = nyk(k+1)
         Nz = nzk(k+1)
         ip = kpbgn(k+1)
         ir = krbgn(k+1)
         ncx = nxk(k)
         ncy = nyk(k)
         ncz = nzk(k)
         ipc = kpbgn(k)
         irc = krbgn(k)
!
!     transfer down to all grid levels
!
         call trsfc3(Nx,Ny,Nz,Wk(ip),Wk(ir),ncx,ncy,ncz,Wk(ipc),Wk(irc))
      enddo
!
!     adjust right hand side at all grid levels in case
!     rhs or specified b.c. in phi or gbdy changed
!
      do kb = 1 , ngrid
         k = ngrid - kb + 1
         Nx = nxk(k)
         Ny = nyk(k)
         Nz = nzk(k)
         ip = kpbgn(k)
         ir = krbgn(k)
         call adjmd3sp(Nx,Ny,Nz,Wk(ip),Wk(ir),bndyc,cfx,cfy,cfz)
      enddo
!
!     execute one full multigrid cycle
!
      do k = 1 , ngrid - 1
         kcur = k
         call kcymd3sp(Wk)
         Nx = nxk(k+1)
         Ny = nyk(k+1)
         Nz = nzk(k+1)
         ip = kpbgn(k+1)
         ipc = kpbgn(k)
         ncx = nxk(k)
         ncy = nyk(k)
         ncz = nzk(k)
!
!     lift or prolong approximation from k to k+1
!
         call prolon3(ncx,ncy,ncz,Wk(ipc),Nx,Ny,Nz,Wk(ip),nxa,nxb,nyc,nyd,nze,nzf,intpol)
      enddo
   else
!
!     adjust rhs at finest grid level only
!
      Nx = nxk(ngrid)
      Ny = nyk(ngrid)
      Nz = nzk(ngrid)
      ip = kpbgn(ngrid)
      ir = krbgn(ngrid)
      call adjmd3sp(Nx,Ny,Nz,Wk(ip),Wk(ir),bndyc,cfx,cfy,cfz)
   endif
!
!     execute maxcy more multigrid k cycles from finest level
!
   kcur = ngrid
   do iter = 1 , maxcy
      itero = iter
      call kcymd3sp(Wk)
      if ( tolmax>0.0 ) then
!
!      error control
!
         relmax = 0.0
         phmax = 0.0
         do k = 1 , nfz
            kk = k*(nfx+2)*(nfy+2)
            do j = 1 , nfy
               jk = kk + j*(nfx+2)
               do i = 1 , nfx
                  ijk = jk + i + 1
                  phmax = max(phmax,abs(Wk(ijk)))
                  relmax = max(relmax,abs(Wk(ijk)-Phif(i,j,k)))
                  Phif(i,j,k) = Wk(ijk)
               enddo
            enddo
         enddo
!
!     set maximum relative difference and check for convergence
!
         if ( phmax>0.0 ) relmax = relmax/phmax
         if ( relmax<=tolmax ) return
      endif
   enddo
!
!     set final iterate in phif
!
   do k = 1 , nfz
      kk = k*(nfx+2)*(nfy+2)
      do j = 1 , nfy
         jk = kk + j*(nfx+2)
         do i = 1 , nfx
            ijk = jk + i + 1
            Phif(i,j,k) = Wk(ijk)
         enddo
      enddo
   enddo
end subroutine mud3sp1
!*==KCYMD3SP.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine kcymd3sp(Wk)
!
!     perform multigrid k-cycle at kcur level
!     kcycle = 1 corresponds to v cycles
!     kcycle = 2 corresponds to w cycles
!
   use c_imud3sp
   use c_mud3spc
   use s_cor3
   use s_relmd3sp
   use s_resmd3sp
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   real , dimension(*) :: Wk
!
! Local variable declarations rewritten by SPAG
!
   integer :: icx , icy , icz , ip , ipc , ir , irc , l , ncx , ncy , ncz , nrel , nx , ny , nz
   integer , dimension(50) :: kount
!
! End of declarations rewritten by SPAG
!
   klevel = kcur
!
!     pre-relax at current finest grid level
!
   do l = 1 , iprer
      call relmd3sp(Wk)
   enddo
!
!     if at coarsest level post-relax
!
   if ( kcur/=1 ) then
!
!     restrict residual to kcur-1
!
      nx = nxk(klevel)
      ny = nyk(klevel)
      nz = nzk(klevel)
      ip = kpbgn(klevel)
      ir = krbgn(klevel)
      icx = kcxbgn(klevel)
      icy = kcybgn(klevel)
      icz = kczbgn(klevel)
      ipc = kpbgn(klevel-1)
      ncx = nxk(klevel-1)
      ncy = nyk(klevel-1)
      ncz = nzk(klevel-1)
      irc = krbgn(klevel-1)
!
!     use full weighting with residual restriction
!
      call resmd3sp(nx,ny,nz,Wk(ip),Wk(ir),Wk(icx),Wk(icy),Wk(icz),ncx,ncy,ncz,Wk(ipc),Wk(irc),Wk(kps))
!
!     set counter for grid levels to zero
!
      do l = 1 , kcur
         kount(l) = 0
      enddo
!
!    set new level and continue k-cycling
!
      klevel = kcur - 1
      nrel = iprer
!
!     kcycle control point
!
!
!     post-relax when kcur revisited
!
      do while ( klevel/=kcur )
!
!     count "hit" at current level
!
         kount(klevel) = kount(klevel) + 1
!
!     relax at current level
!
         do l = 1 , nrel
            call relmd3sp(Wk)
         enddo
         if ( kount(klevel)==kcycle+1 ) then
!
!     kcycle(iprer,ipost) complete at klevel
!     inject correction to finer grid
!
            nx = nxk(klevel+1)
            ny = nyk(klevel+1)
            nz = nzk(klevel+1)
            ip = kpbgn(klevel+1)
            ncx = nxk(klevel)
            ncy = nyk(klevel)
            ncz = nzk(klevel)
            ipc = kpbgn(klevel)
            call cor3(nx,ny,nz,Wk(ip),ncx,ncy,ncz,Wk(ipc),nxa,nxb,nyc,nyd,nze,nzf,intpol,Wk(kps))
!
!     reset counter to zero at klevel
!
            kount(klevel) = 0
!
!     ascend to next higher level and set to post-relax there
!
            klevel = klevel + 1
            nrel = ipost
!
!     kcycle not complete so descend unless at coarsest
!
         elseif ( klevel>1 ) then
            nx = nxk(klevel)
            ny = nyk(klevel)
            nz = nzk(klevel)
            ip = kpbgn(klevel)
            ir = krbgn(klevel)
            icx = kcxbgn(klevel)
            icy = kcybgn(klevel)
            icz = kczbgn(klevel)
            ncx = nxk(klevel-1)
            ncy = nyk(klevel-1)
            ncz = nzk(klevel-1)
            irc = krbgn(klevel-1)
            ipc = kpbgn(klevel-1)
            call resmd3sp(nx,ny,nz,Wk(ip),Wk(ir),Wk(icx),Wk(icy),Wk(icz),ncx,ncy,ncz,Wk(ipc),Wk(irc),Wk(kps))
!
!     pre-relax at next coarser level
!
            klevel = klevel - 1
            nrel = iprer
         else
!
!     post-relax at coarsest level (klevel=1)
!
            do l = 1 , ipost
               call relmd3sp(Wk)
            enddo
!
!     inject correction to grid level 2
!
            ipc = kpbgn(1)
            ncx = nxk(1)
            ncy = nyk(1)
            ncz = nzk(1)
            ip = kpbgn(2)
            nx = nxk(2)
            ny = nyk(2)
            nz = nzk(2)
            call cor3(nx,ny,nz,Wk(ip),ncx,ncy,ncz,Wk(ipc),nxa,nxb,nyc,nyd,nze,nzf,intpol,Wk(kps))
!
!     set to post-relax at level 2
!
            nrel = ipost
            klevel = 2
         endif
      enddo
   endif
!
!     post-relax at kcur level
!
   do l = 1 , ipost
      call relmd3sp(Wk)
   enddo
end subroutine kcymd3sp
!*==RESMD3SP.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine resmd3sp(Nx,Ny,Nz,Phi,Rhs,Cofx,Cofy,Cofz,Ncx,Ncy,Ncz,Phic,Rhsc,Resf)
!
!     compute fully weighted residual restriction in rhsc
!
   use c_imud3sp
   use s_res3
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer :: Nx
   integer :: Ny
   integer :: Nz
   integer :: Ncx
   integer :: Ncy
   integer :: Ncz
   real , intent(in) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Phi
   real , intent(in) , dimension(Nx,Ny,Nz) :: Rhs
   real , intent(in) , dimension(Nx,3) :: Cofx
   real , intent(in) , dimension(Ny,3) :: Cofy
   real , intent(in) , dimension(Nz,3) :: Cofz
   real , intent(out) , dimension(0:Ncx+1,0:Ncy+1,0:Ncz+1) :: Phic
   real , dimension(Ncx,Ncy,Ncz) :: Rhsc
   real , dimension(Nx,Ny,Nz) :: Resf
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ic , ifn , ist , j , jc , jfn , jst , k , kc , kfn , kst
!
! End of declarations rewritten by SPAG
!
!
!     initialize phic to zero
!
   do kc = 0 , Ncz + 1
      do jc = 0 , Ncy + 1
         do ic = 0 , Ncx + 1
            Phic(ic,jc,kc) = 0.0
         enddo
      enddo
   enddo
!
!     intialize residual to zero and set limits
!
   do k = 1 , Nz
      do j = 1 , Ny
         do i = 1 , Nx
            Resf(i,j,k) = 0.0
         enddo
      enddo
   enddo
!
!     set loop limits
!
   ist = 1
   if ( nxa==1 ) ist = 2
   ifn = Nx
   if ( nxb==1 ) ifn = Nx - 1
   jst = 1
   if ( nyc==1 ) jst = 2
   jfn = Ny
   if ( nyd==1 ) jfn = Ny - 1
   kst = 1
   if ( nze==1 ) kst = 2
   kfn = Nz
   if ( nzf==1 ) kfn = Nz - 1
!
!     compute fine grid residual
!
!$OMP PARALLEL DO PRIVATE(i,j,k), SHARED(phi,nx,ny,nz)
!$OMP+SHARED(ist,ifn,jst,jfn,kst,kfn,cofx,cofy,cofz,rhs)
   do k = kst , kfn
      do j = jst , jfn
         do i = ist , ifn
            Resf(i,j,k) = Rhs(i,j,k) - (Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)&
                        & +Cofz(k,1)*Phi(i,j,k-1)+Cofz(k,2)*Phi(i,j,k+1)+(Cofx(i,3)+Cofy(j,3)+Cofz(k,3))*Phi(i,j,k))
         enddo
      enddo
   enddo
!
!     restrict resf to interior coarse mesh in rhsc
!     using fully weighted residual restriction in 3-d
!
   call res3(Nx,Ny,Nz,Resf,Ncx,Ncy,Ncz,Rhsc,nxa,nxb,nyc,nyd,nze,nzf)
end subroutine resmd3sp
!*==DISMD3SP.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine dismd3sp(Nx,Ny,Nz,Cofx,Cofy,Cofz,bndyc,cfx,cfy,cfz,Ier)
!
!     discretize the 3-d elliptic pde
!
   use c_fmud3sp
   use c_imud3sp
   use c_mud3spc
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nx
   integer , intent(in) :: Ny
   integer , intent(in) :: Nz
   real , intent(inout) , dimension(Nx,3) :: Cofx
   real , intent(inout) , dimension(Ny,3) :: Cofy
   real , intent(inout) , dimension(Nz,3) :: Cofz
   external bndyc
   external cfx
   external cfy
   external cfz
   integer , intent(out) :: Ier
!
! Local variable declarations rewritten by SPAG
!
   real :: alfa , alfmax , c1 , c2 , cemax , cex , cey , cez , cmin , cx , cxx , cy , cyy , cz , czz , dlx , dlx2 , dlxx , dly ,   &
         & dly2 , dlyy , dlz , dlz2 , dlzz , gbdy , x , y , z
   integer :: i , ifn , ist , j , jfn , jst , k , kbdy , kfn , kst
!
! End of declarations rewritten by SPAG
!
!
!     set current grid increments
!
   dlx = (xb-xa)/(Nx-1)
   dlx2 = dlx + dlx
   dlxx = dlx*dlx
   dly = (yd-yc)/(Ny-1)
   dly2 = dly + dly
   dlyy = dly*dly
   dlz = (zf-ze)/(Nz-1)
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
   if ( nxa==1 ) ist = 2
   if ( nxb==1 ) ifn = Nx - 1
   if ( nyc==1 ) jst = 2
   if ( nyd==1 ) jfn = Ny - 1
   if ( nze==1 ) kst = 2
   if ( nzf==1 ) kfn = Nz - 1
   do i = ist , ifn
      x = xa + (i-1)*dlx
      call cfx(x,cxx,cx,cex)
      cmin = min(cmin,cxx)
      cemax = max(abs(cex),cemax)
!
!     check if pde is "hyperbolic" at finest grid level
!
      if ( klevel==ngrid ) then
         if ( (abs(cx)*dlx>abs(cxx+cxx)) ) Ier = -4
      endif
!
!     adjust second order coefficients so that pde is not "hyperbolic"
!     this is especially possible on coarser grids if there are non-zero
!     first order terms
!
      cxx = max(cxx,abs(cx)*dlx*0.5)
      Cofx(i,1) = cxx/dlxx - cx/dlx2
      Cofx(i,2) = cxx/dlxx + cx/dlx2
      Cofx(i,3) = cex - (Cofx(i,1)+Cofx(i,2))
   enddo
   do j = jst , jfn
      y = yc + (j-1)*dly
      call cfy(y,cyy,cy,cey)
      cmin = min(cmin,cyy)
      cemax = max(abs(cey),cemax)
!
!     check if pde is "hyperbolic" at finest grid level
!
      if ( klevel==ngrid ) then
         if ( (abs(cy)*dly>abs(cyy+cyy)) ) Ier = -4
      endif
!
!     adjust second order coefficients so that pde is not "hyperbolic"
!     this is especially possible on coarser grids if there are non-zero
!     first order terms
!
      cyy = max(cyy,abs(cy)*dly*0.5)
      Cofy(j,1) = cyy/dlyy - cy/dly2
      Cofy(j,2) = cyy/dlyy + cy/dly2
      Cofy(j,3) = cey - (Cofy(j,1)+Cofy(j,2))
   enddo
   do k = kst , kfn
      z = ze + (k-1)*dlz
      call cfz(z,czz,cz,cez)
      cmin = min(cmin,czz)
      cemax = max(abs(cez),cemax)
!
!     check if pde is "hyperbolic" at finest grid level
!
      if ( klevel==ngrid ) then
         if ( (abs(cz)*dlz>abs(czz+czz)) ) Ier = -4
      endif
!
!     adjust second order coefficients so that pde is not "hyperbolic"
!     this is especially possible on coarser grids if there are non-zero
!     first order terms
!
      czz = max(czz,abs(cz)*dlz*0.5)
      Cofz(k,1) = czz/dlzz - cz/dlz2
      Cofz(k,2) = czz/dlzz + cz/dlz2
      Cofz(k,3) = cez - (Cofz(k,1)+Cofz(k,2))
   enddo
!
!     set nonfatal error flag if ellipticity test fails
!
   if ( cmin<=0.0 ) Ier = -2
   alfmax = 0.0
!
!     adjust equation at mixed b.c.
!
   if ( nxa==2 ) then
      kbdy = 1
      i = 1
      c1 = Cofx(i,1)
      Cofx(i,1) = 0.0
      Cofx(i,2) = Cofx(i,2) + c1
      y = yc + dly
      z = ze + dlz
!
!     compute constant coefficient alfa
!
      call bndyc(kbdy,y,z,alfa,gbdy)
      alfmax = max(alfmax,abs(alfa))
      Cofx(i,3) = Cofx(i,3) + dlx2*alfa*c1
   endif
   if ( nxb==2 ) then
      kbdy = 2
      i = Nx
      y = yc + dly
      z = ze + dlz
!
!     compute constant coefficient alfa
!
      call bndyc(kbdy,y,z,alfa,gbdy)
      c2 = Cofx(i,2)
      Cofx(i,1) = Cofx(i,1) + c2
      Cofx(i,2) = 0.0
      Cofx(i,3) = Cofx(i,3) - dlx2*alfa*c2
      alfmax = max(abs(alfa),alfmax)
   endif
   if ( nyc==2 ) then
      kbdy = 3
      j = 1
      x = xa + dlx
      z = ze + dlz
!
!     compute constant coefficient alfa
!
      call bndyc(kbdy,x,z,alfa,gbdy)
      c1 = Cofy(j,1)
      Cofy(j,1) = 0.0
      Cofy(j,2) = Cofy(j,2) + c1
      Cofy(j,3) = Cofy(j,3) + dly2*alfa*c1
      alfmax = max(abs(alfa),alfmax)
   endif
   if ( nyd==2 ) then
      kbdy = 4
      j = Ny
      x = xa + dlx
      z = ze + dlz
!
!     compute constant coefficient alfa
!
      call bndyc(kbdy,x,z,alfa,gbdy)
      c2 = Cofy(j,2)
      Cofy(j,2) = 0.0
      Cofy(j,1) = Cofy(j,1) + c2
      Cofy(j,3) = Cofy(j,3) - dly2*c2*alfa
      alfmax = max(abs(alfa),alfmax)
   endif
   if ( nze==2 ) then
      kbdy = 5
      k = 1
      x = xa + dlx
      y = yc + dly
!
!     compute constant coefficient alfa
!
      call bndyc(kbdy,x,y,alfa,gbdy)
      c1 = Cofz(k,1)
      Cofz(k,1) = 0.0
      Cofz(k,2) = Cofz(k,2) + c1
      Cofz(k,3) = Cofz(k,3) + dlz2*alfa*c1
      alfmax = max(abs(alfa),alfmax)
   endif
   if ( nzf==2 ) then
      kbdy = 6
      k = Nz
      x = xa + dlx
      y = yc + dly
!
!     compute constant coefficient alfa
!
      call bndyc(kbdy,x,y,alfa,gbdy)
      c2 = Cofz(k,2)
      Cofz(k,2) = 0.0
      Cofz(k,1) = Cofz(k,1) + c2
      Cofz(k,3) = Cofz(k,3) - dlz2*alfa*c2
      alfmax = max(abs(alfa),alfmax)
   endif
!
!     flag continuous singular elliptic pde if detected
!
   if ( cemax==0.0 .and. alfmax==0.0 ) then
      if ( nxa==0 .or. (nxa==2 .and. nxb==2) ) then
         if ( nyc==0 .or. (nyc==2 .and. nyd==2) ) then
            if ( nze==0 .or. (nze==2 .and. nzf==2) ) Ier = -3
         endif
      endif
   endif
end subroutine dismd3sp
!*==ADJMD3SP.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine adjmd3sp(Nx,Ny,Nz,Phi,Rhs,bndyc,cfx,cfy,cfz)
!
!     adjust rhs for solution in cof(i,j,k,8) on non-initial calls
!     (i.e., values in cof have not changed)
!
   use c_fmud3sp
   use c_imud3sp
   use c_mud3spc
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nx
   integer , intent(in) :: Ny
   integer , intent(in) :: Nz
   real , intent(in) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Phi
   real , intent(inout) , dimension(Nx,Ny,Nz) :: Rhs
   external bndyc
   external cfx
   external cfy
   external cfz
!
! Local variable declarations rewritten by SPAG
!
   real :: alfa , c1 , c2 , cex , cey , cez , cx , cxx , cy , cyy , cz , czz , dlx , dlx2 , dlxx , dly , dly2 , dlyy , dlz , dlz2 ,&
         & dlzz , gbdy , x , y , z
   integer :: i , ifn , ist , j , jfn , jst , k , kbdy , kfn , kst
!
! End of declarations rewritten by SPAG
!
!
!     set current grid increments
!
   dlx = (xb-xa)/(Nx-1)
   dlx2 = dlx + dlx
   dlxx = dlx*dlx
   dly = (yd-yc)/(Ny-1)
   dly2 = dly + dly
   dlyy = dly*dly
   dlz = (zf-ze)/(Nz-1)
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
   if ( nxa==1 ) ist = 2
   if ( nxb==1 ) ifn = Nx - 1
   if ( nyc==1 ) jst = 2
   if ( nyd==1 ) jfn = Ny - 1
   if ( nze==1 ) kst = 2
   if ( nzf==1 ) kfn = Nz - 1
!
!     adjust for derivative b.c.
!
   if ( nxa==2 ) then
      kbdy = 1
      x = xa
      i = 1
      call cfx(x,cxx,cx,cex)
      cxx = max(cxx,abs(cx)*dlx*0.5)
      c1 = cxx/dlxx - cx/dlx2
      do k = kst , kfn
         z = ze + (k-1)*dlz
         do j = jst , jfn
            y = yc + (j-1)*dly
            call bndyc(kbdy,y,z,alfa,gbdy)
            Rhs(i,j,k) = Rhs(i,j,k) + dlx2*c1*gbdy
         enddo
      enddo
   endif
   if ( nxb==2 ) then
      kbdy = 2
      x = xb
      i = Nx
      call cfx(x,cxx,cx,cex)
      cxx = max(cxx,abs(cx)*dlx*0.5)
      c2 = cxx/dlxx + cx/dlx2
      do k = kst , kfn
         z = ze + (k-1)*dlz
         do j = jst , jfn
            y = yc + (j-1)*dly
            call bndyc(kbdy,y,z,alfa,gbdy)
            Rhs(i,j,k) = Rhs(i,j,k) - dlx2*c2*gbdy
         enddo
      enddo
   endif
   if ( nyc==2 ) then
      kbdy = 3
      y = yc
      j = 1
      call cfy(y,cyy,cy,cey)
      cyy = max(cyy,abs(cy)*dly*0.5)
      c1 = cyy/dlyy - cy/dly2
      do k = kst , kfn
         z = ze + (k-1)*dlz
         do i = ist , ifn
            x = xa + (i-1)*dlx
            call bndyc(kbdy,x,z,alfa,gbdy)
            Rhs(i,j,k) = Rhs(i,j,k) + dly2*c1*gbdy
         enddo
      enddo
   endif
   if ( nyd==2 ) then
      kbdy = 4
      y = yd
      j = Ny
      call cfy(y,cyy,cy,cey)
      cyy = max(cyy,abs(cy)*dly*0.5)
      c2 = cyy/dlyy + cy/dly2
      do k = kst , kfn
         z = ze + (k-1)*dlz
         do i = ist , ifn
            x = xa + (i-1)*dlx
            call bndyc(kbdy,x,z,alfa,gbdy)
            Rhs(i,j,k) = Rhs(i,j,k) - dly2*c2*gbdy
         enddo
      enddo
   endif
   if ( nze==2 ) then
      kbdy = 5
      k = 1
      z = ze
      call cfz(z,czz,cz,cez)
      czz = max(czz,abs(cz)*dlz*0.5)
      c1 = czz/dlzz - cz/dlz2
      do j = jst , jfn
         y = yc + (j-1)*dly
         do i = ist , ifn
            x = xa + (i-1)*dlx
            call bndyc(kbdy,x,y,alfa,gbdy)
            Rhs(i,j,k) = Rhs(i,j,k) + dlz2*c1*gbdy
         enddo
      enddo
   endif
   if ( nzf==2 ) then
      kbdy = 6
      z = zf
      k = Nz
      call cfz(z,czz,cz,cez)
      czz = max(czz,abs(cz)*dlz*0.5)
      c2 = czz/dlzz + cz/dlz2
      do j = jst , jfn
         y = yc + (j-1)*dly
         do i = ist , ifn
            x = xa + (i-1)*dlx
            call bndyc(kbdy,x,y,alfa,gbdy)
            Rhs(i,j,k) = Rhs(i,j,k) - dlz2*c2*gbdy
         enddo
      enddo
   endif
!
!     set specified b.c.
!
   if ( nxa==1 ) then
      i = 1
      do j = 1 , Ny
         do k = 1 , Nz
            Rhs(i,j,k) = Phi(i,j,k)
         enddo
      enddo
   endif
   if ( nxb==1 ) then
      i = Nx
      do j = 1 , Ny
         do k = 1 , Nz
            Rhs(i,j,k) = Phi(i,j,k)
         enddo
      enddo
   endif
   if ( nyc==1 ) then
      j = 1
      do k = 1 , Nz
         do i = 1 , Nx
            Rhs(i,j,k) = Phi(i,j,k)
         enddo
      enddo
   endif
   if ( nyd==1 ) then
      j = Ny
      do k = 1 , Nz
         do i = 1 , Nx
            Rhs(i,j,k) = Phi(i,j,k)
         enddo
      enddo
   endif
   if ( nze==1 ) then
      k = 1
      do j = 1 , Ny
         do i = 1 , Nx
            Rhs(i,j,k) = Phi(i,j,k)
         enddo
      enddo
   endif
   if ( nzf==1 ) then
      k = Nz
      do j = 1 , Ny
         do i = 1 , Nx
            Rhs(i,j,k) = Phi(i,j,k)
         enddo
      enddo
   endif
end subroutine adjmd3sp
!*==RELMD3SP.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine relmd3sp(Wk)
!
!     use point or line relaxation in the x and/or y and/or z
!     or planar relaxation in the x,y or x,z or y,z planes
!
   use c_fmud3sp
   use c_imud3sp
   use c_mud3spc
   use s_relmd3spp
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   real , dimension(*) :: Wk
!
! Local variable declarations rewritten by SPAG
!
   integer :: icx , icy , icz , ip , ir , nx , ny , nz
!
! End of declarations rewritten by SPAG
!
   nx = nxk(klevel)
   ny = nyk(klevel)
   nz = nzk(klevel)
   ip = kpbgn(klevel)
   ir = krbgn(klevel)
   icx = kcxbgn(klevel)
   icy = kcybgn(klevel)
   icz = kczbgn(klevel)
!
!     gauss-seidel pointwise red/black relaxation
!
   call relmd3spp(nx,ny,nz,Wk(ip),Wk(ir),Wk(icx),Wk(icy),Wk(icz))
end subroutine relmd3sp
!*==RELMD3SPP.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine relmd3spp(Nx,Ny,Nz,Phi,Rhs,Cofx,Cofy,Cofz)
!
!     gauss-seidel point relaxation with red/black ordering
!     in three dimensions for nonseparable pde
!     relax in order:
!     (1) red (x,y) on odd z planes
!     (2) black (x,y) on even z planes
!     (3) black (x,y) on odd z planes
!     (4) red (x,y) on even z planes
!
   use c_imud3sp
   use s_per3vb
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer :: Nx
   integer :: Ny
   integer :: Nz
   real , intent(inout) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Phi
   real , intent(in) , dimension(Nx,Ny,Nz) :: Rhs
   real , intent(in) , dimension(Nx,3) :: Cofx
   real , intent(in) , dimension(Ny,3) :: Cofy
   real , intent(in) , dimension(Nz,3) :: Cofz
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ifn , ist , j , jfn , jst , k , kfn , kst , nper
!
! End of declarations rewritten by SPAG
!
!
!     set periodic b.c. indicator
!
   nper = nxa*nyc*nze
!
!     set loop limits to avoid specified boundaries
!     in red/black sweeps
!
   ist = 1
   if ( nxa==1 ) ist = 3
   ifn = Nx
   if ( nxb==1 ) ifn = Nx - 1
   jst = 1
   if ( nyc==1 ) jst = 3
   jfn = Ny
   if ( nyd==1 ) jfn = Ny - 1
   kst = 1
   if ( nze==1 ) kst = 3
   kfn = Nz
   if ( nzf==1 ) kfn = Nz - 1
!
!     set periodic boundaries if necessary
!
   if ( nper==0 ) call per3vb(Nx,Ny,Nz,Phi,nxa,nyc,nze)
!
!   red (x,y) on odd z planes
!
!$OMP PARALLEL DO SHARED(ist,ifn,jst,jfn,kst,kfn,phi,rhs,cofx,cofy,cofz)
!$OMP+PRIVATE(i,j,k)
   do k = kst , kfn , 2
      do i = ist , ifn , 2
         do j = jst , jfn , 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & +Cofz(k,1)*Phi(i,j,k-1)+Cofz(k,2)*Phi(i,j,k+1)))/(Cofx(i,3)+Cofy(j,3)+Cofz(k,3))
         enddo
      enddo
      do i = 2 , ifn , 2
         do j = 2 , jfn , 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & +Cofz(k,1)*Phi(i,j,k-1)+Cofz(k,2)*Phi(i,j,k+1)))/(Cofx(i,3)+Cofy(j,3)+Cofz(k,3))
         enddo
      enddo
   enddo
   if ( nper==0 ) call per3vb(Nx,Ny,Nz,Phi,nxa,nyc,nze)
!
!   black (x,y) or even z planes
!
!$OMP PARALLEL DO SHARED(ist,ifn,jst,jfn,kfn,phi,rhs,cofx,cofy,cofz)
!$OMP+PRIVATE(i,j,k)
   do k = 2 , kfn , 2
      do i = ist , ifn , 2
         do j = 2 , jfn , 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & +Cofz(k,1)*Phi(i,j,k-1)+Cofz(k,2)*Phi(i,j,k+1)))/(Cofx(i,3)+Cofy(j,3)+Cofz(k,3))
         enddo
      enddo
      do i = 2 , ifn , 2
         do j = jst , jfn , 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & +Cofz(k,1)*Phi(i,j,k-1)+Cofz(k,2)*Phi(i,j,k+1)))/(Cofx(i,3)+Cofy(j,3)+Cofz(k,3))
         enddo
      enddo
   enddo
   if ( nper==0 ) call per3vb(Nx,Ny,Nz,Phi,nxa,nyc,nze)
!
!   black (x,y) on odd z planes
!
!$OMP PARALLEL DO SHARED(ist,ifn,jfn,kst,kfn,phi,rhs,cofx,cofy,cofz)
!$OMP+PRIVATE(i,j,k)
   do k = kst , kfn , 2
      do i = ist , ifn , 2
         do j = 2 , jfn , 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & +Cofz(k,1)*Phi(i,j,k-1)+Cofz(k,2)*Phi(i,j,k+1)))/(Cofx(i,3)+Cofy(j,3)+Cofz(k,3))
         enddo
      enddo
      do i = 2 , ifn , 2
         do j = jst , jfn , 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & +Cofz(k,1)*Phi(i,j,k-1)+Cofz(k,2)*Phi(i,j,k+1)))/(Cofx(i,3)+Cofy(j,3)+Cofz(k,3))
         enddo
      enddo
   enddo
   if ( nper==0 ) call per3vb(Nx,Ny,Nz,Phi,nxa,nyc,nze)
!
!   red(x,y) on even z planes
!
!$OMP PARALLEL DO SHARED(ist,ifn,jst,jfn,kfn,phi,rhs,cofx,cofy,cofz)
!$OMP+PRIVATE(i,j,k)
   do k = 2 , kfn , 2
      do i = ist , ifn , 2
         do j = jst , jfn , 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & +Cofz(k,1)*Phi(i,j,k-1)+Cofz(k,2)*Phi(i,j,k+1)))/(Cofx(i,3)+Cofy(j,3)+Cofz(k,3))
         enddo
      enddo
      do i = 2 , ifn , 2
         do j = 2 , jfn , 2
            Phi(i,j,k) = (Rhs(i,j,k)-(Cofx(i,1)*Phi(i-1,j,k)+Cofx(i,2)*Phi(i+1,j,k)+Cofy(j,1)*Phi(i,j-1,k)+Cofy(j,2)*Phi(i,j+1,k)  &
                       & +Cofz(k,1)*Phi(i,j,k-1)+Cofz(k,2)*Phi(i,j,k+1)))/(Cofx(i,3)+Cofy(j,3)+Cofz(k,3))
         enddo
      enddo
   enddo
   if ( nper==0 ) call per3vb(Nx,Ny,Nz,Phi,nxa,nyc,nze)
end subroutine relmd3spp
