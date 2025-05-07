!*==SWK2.f90 processed by SPAG 8.04DB 19:24  7 May 2025
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
subroutine swk2(Nfx,Nfy,Phif,Rhsf,Phi,Rhs)
!
!     set phif,rhsf input in arrays which include
!     virtual boundaries for phi (for all 2-d real codes)
!
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nfx
   integer , intent(in) :: Nfy
   real , intent(in) , dimension(Nfx,Nfy) :: Phif
   real , intent(in) , dimension(Nfx,Nfy) :: Rhsf
   real , intent(out) , dimension(0:Nfx+1,0:Nfy+1) :: Phi
   real , intent(out) , dimension(Nfx,Nfy) :: Rhs
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , j
!
! End of declarations rewritten by SPAG
!
   do j = 1 , Nfy
      do i = 1 , Nfx
         Phi(i,j) = Phif(i,j)
         Rhs(i,j) = Rhsf(i,j)
      enddo
   enddo
!
!     set virtual boundaries in phi to zero
!
   do j = 0 , Nfy + 1
      Phi(0,j) = 0.0
      Phi(Nfx+1,j) = 0.0
   enddo
   do i = 0 , Nfx + 1
      Phi(i,0) = 0.0
      Phi(i,Nfy+1) = 0.0
   enddo
end subroutine swk2
!*==TRSFC2.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine trsfc2(Nx,Ny,Phi,Rhs,Ncx,Ncy,Phic,Rhsc)
!
!     transfer fine grid to coarse grid
!
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nx
   integer , intent(in) :: Ny
   integer , intent(in) :: Ncx
   integer , intent(in) :: Ncy
   real , intent(in) , dimension(0:Nx+1,0:Ny+1) :: Phi
   real , intent(in) , dimension(Nx,Ny) :: Rhs
   real , intent(out) , dimension(0:Ncx+1,0:Ncy+1) :: Phic
   real , intent(out) , dimension(Ncx,Ncy) :: Rhsc
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ic , j , jc
!
! End of declarations rewritten by SPAG
!
!
!     set virtual boundaries in phic to zero
!
   do jc = 0 , Ncy + 1
      Phic(0,jc) = 0.0
      Phic(Ncx+1,jc) = 0.0
   enddo
   do ic = 0 , Ncx + 1
      Phic(ic,0) = 0.0
      Phic(ic,Ncy+1) = 0.0
   enddo
   if ( Ncx<Nx .and. Ncy<Ny ) then
!
!     coarsening in both x and y
!
      do jc = 1 , Ncy
         j = jc + jc - 1
         do ic = 1 , Ncx
            i = ic + ic - 1
            Phic(ic,jc) = Phi(i,j)
            Rhsc(ic,jc) = Rhs(i,j)
         enddo
      enddo
   elseif ( Ncx<Nx .and. Ncy==Ny ) then
!
!     coarsening in x only
!
      do jc = 1 , Ncy
         j = jc
         do ic = 1 , Ncx
            i = ic + ic - 1
            Phic(ic,jc) = Phi(i,j)
            Rhsc(ic,jc) = Rhs(i,j)
         enddo
      enddo
   else
!
!     coarsening in y only
!
      do jc = 1 , Ncy
         j = jc + jc - 1
         do ic = 1 , Ncx
            i = ic
            Phic(ic,jc) = Phi(i,j)
            Rhsc(ic,jc) = Rhs(i,j)
         enddo
      enddo
   endif
end subroutine trsfc2
!*==RES2.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine res2(Nx,Ny,Resf,Ncx,Ncy,Rhsc,Nxa,Nxb,Nyc,Nyd)
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nx
   integer , intent(in) :: Ny
   integer , intent(in) :: Ncx
   integer , intent(in) :: Ncy
   real , intent(in) , dimension(Nx,Ny) :: Resf
   real , intent(out) , dimension(Ncx,Ncy) :: Rhsc
   integer , intent(in) :: Nxa
   integer , intent(in) :: Nxb
   integer , intent(in) :: Nyc
   integer , intent(in) :: Nyd
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ic , im1 , ip1 , ix , j , jc , jm1 , jp1 , jy
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
   if ( Ncx==Nx ) ix = 0
   jy = 1
   if ( Ncy==Ny ) jy = 0
!
!     restrict on interior
!
   if ( Ncy<Ny .and. Ncx<Nx ) then
!
!     coarsening in both directions
!
!$OMP PARALLEL DO PRIVATE(i,j,ic,jc), SHARED(resf,rhsc,ncx,ncy)
      do jc = 2 , Ncy - 1
         j = jc + jc - 1
         do ic = 2 , Ncx - 1
            i = ic + ic - 1
            Rhsc(ic,jc) = (Resf(i-1,j-1)+Resf(i+1,j-1)+Resf(i-1,j+1)+Resf(i+1,j+1)                                                 &
                        & +2.*(Resf(i-1,j)+Resf(i+1,j)+Resf(i,j-1)+Resf(i,j+1))+4.*Resf(i,j))*.0625
         enddo
      enddo
   elseif ( Ncy==Ny ) then
!
!     no coarsening in y but coarsening in x
!
!$OMP PARALLEL DO PRIVATE(i,j,ic,jc), SHARED(resf,rhsc,ncx,ncy)
      do jc = 2 , Ncy - 1
         j = jc
         do ic = 2 , Ncx - 1
            i = ic + ic - 1
            Rhsc(ic,jc) = (Resf(i-1,j-1)+Resf(i+1,j-1)+Resf(i-1,j+1)+Resf(i+1,j+1)                                                 &
                        & +2.*(Resf(i-1,j)+Resf(i+1,j)+Resf(i,j-1)+Resf(i,j+1))+4.*Resf(i,j))*.0625
         enddo
      enddo
   else
!
!     no coarsening in x but coarsening in y
!
!$OMP PARALLEL DO PRIVATE(i,j,ic,jc), SHARED(resf,rhsc,ncx,ncy)
      do jc = 2 , Ncy - 1
         j = jc + jc - 1
         do ic = 2 , Ncx - 1
            i = ic
            Rhsc(ic,jc) = (Resf(i-1,j-1)+Resf(i+1,j-1)+Resf(i-1,j+1)+Resf(i+1,j+1)                                                 &
                        & +2.*(Resf(i-1,j)+Resf(i+1,j)+Resf(i,j-1)+Resf(i,j+1))+4.*Resf(i,j))*.0625
         enddo
      enddo
   endif
!
!     set residual on boundaries
!
   do jc = 1 , Ncy , Ncy - 1
!
!     y=yc,yd boundaries
!
      j = jc + jy*(jc-1)
      jm1 = max0(j-1,2)
      jp1 = min0(j+1,Ny-1)
      if ( j==1 .and. Nyc==0 ) jm1 = Ny - 1
      if ( j==Ny .and. Nyc==0 ) jp1 = 2
!
!     y=yc,yd and x=xa,xb cornors
!
      do ic = 1 , Ncx , Ncx - 1
         i = ic + ix*(ic-1)
         im1 = max0(i-1,2)
         ip1 = min0(i+1,Nx-1)
         if ( i==1 .and. Nxa==0 ) im1 = Nx - 1
         if ( i==Nx .and. Nxa==0 ) ip1 = 2
         Rhsc(ic,jc) = (Resf(im1,jm1)+Resf(ip1,jm1)+Resf(im1,jp1)+Resf(ip1,jp1)+2.*(Resf(im1,j)+Resf(ip1,j)+Resf(i,jm1)+Resf(i,jp1)&
                     & )+4.*Resf(i,j))*.0625
      enddo
!
!     set y=yc,yd interior edges
!
      do ic = 2 , Ncx - 1
         i = ic + ix*(ic-1)
         Rhsc(ic,jc) = (Resf(i-1,jm1)+Resf(i+1,jm1)+Resf(i-1,jp1)+Resf(i+1,jp1)+2.*(Resf(i-1,j)+Resf(i+1,j)+Resf(i,jm1)+Resf(i,jp1)&
                     & )+4.*Resf(i,j))*.0625
      enddo
   enddo
!
!     set x=xa,xb interior edges
!
   do ic = 1 , Ncx , Ncx - 1
      i = ic + ix*(ic-1)
      im1 = max0(i-1,2)
      ip1 = min0(i+1,Nx-1)
      if ( i==1 .and. Nxa==0 ) im1 = Nx - 1
      if ( i==Nx .and. Nxa==0 ) ip1 = 2
      do jc = 2 , Ncy - 1
         j = jc + jy*(jc-1)
         Rhsc(ic,jc) = (Resf(im1,j-1)+Resf(ip1,j-1)+Resf(im1,j+1)+Resf(ip1,j+1)+2.*(Resf(im1,j)+Resf(ip1,j)+Resf(i,j-1)+Resf(i,j+1)&
                     & )+4.*Resf(i,j))*.0625
      enddo
   enddo
!
!     set coarse grid residual zero on specified boundaries
!
   if ( Nxa==1 ) then
      do jc = 1 , Ncy
         Rhsc(1,jc) = 0.0
      enddo
   endif
   if ( Nxb==1 ) then
      do jc = 1 , Ncy
         Rhsc(Ncx,jc) = 0.0
      enddo
   endif
   if ( Nyc==1 ) then
      do ic = 1 , Ncx
         Rhsc(ic,1) = 0.0
      enddo
   endif
   if ( Nyd==1 ) then
      do ic = 1 , Ncx
         Rhsc(ic,Ncy) = 0.0
      enddo
   endif
end subroutine res2
!*==PROLON2.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!
!     prolon2 modified from rgrd2u 11/20/97
!
subroutine prolon2(Ncx,Ncy,P,Nx,Ny,Q,Nxa,Nxb,Nyc,Nyd,Intpol)
   use s_prolon1
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer :: Ncx
   integer , intent(in) :: Ncy
   integer :: Nx
   integer , intent(in) :: Ny
   real , dimension(0:Ncx+1,0:Ncy+1) :: P
   real , intent(inout) , dimension(0:Nx+1,0:Ny+1) :: Q
   integer :: Nxa
   integer :: Nxb
   integer , intent(in) :: Nyc
   integer , intent(in) :: Nyd
   integer :: Intpol
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ifn , ist , j , jc , jfn , joddfn , joddst , jst
!
! End of declarations rewritten by SPAG
!
   ist = 1
   ifn = Nx
   jst = 1
   jfn = Ny
   joddst = 1
   joddfn = Ny
   if ( Nxa==1 ) ist = 2
   if ( Nxb==1 ) ifn = Nx - 1
   if ( Nyc==1 ) then
      jst = 2
      joddst = 3
   endif
   if ( Nyd==1 ) then
      jfn = Ny - 1
      joddfn = Ny - 2
   endif
   if ( Intpol==1 .or. Ncy<4 ) then
!
!     linearly interpolate in y
!
      if ( Ncy<Ny ) then
!
!     ncy grid is an every other point subset of ny grid
!     set odd j lines interpolating in x and then set even
!     j lines by averaging odd j lines
!
         do j = joddst , joddfn , 2
            jc = j/2 + 1
            call prolon1(Ncx,P(0,jc),Nx,Q(0,j),Nxa,Nxb,Intpol)
         enddo
         do j = 2 , jfn , 2
            do i = ist , ifn
               Q(i,j) = 0.5*(Q(i,j-1)+Q(i,j+1))
            enddo
         enddo
!
!     set periodic virtual boundaries if necessary
!
         if ( Nyc==0 ) then
            do i = ist , ifn
               Q(i,0) = Q(i,Ny-1)
               Q(i,Ny+1) = Q(i,2)
            enddo
         endif
         return
      else
!
!     ncy grid equals ny grid so interpolate in x only
!
         do j = jst , jfn
            jc = j
            call prolon1(Ncx,P(0,jc),Nx,Q(0,j),Nxa,Nxb,Intpol)
         enddo
!
!     set periodic virtual boundaries if necessary
!
         if ( Nyc==0 ) then
            do i = ist , ifn
               Q(i,0) = Q(i,Ny-1)
               Q(i,Ny+1) = Q(i,2)
            enddo
         endif
         return
      endif
!
!     cubically interpolate in y
!
   elseif ( Ncy<Ny ) then
!
!     set every other point of ny grid by interpolating in x
!
      do j = joddst , joddfn , 2
         jc = j/2 + 1
         call prolon1(Ncx,P(0,jc),Nx,Q(0,j),Nxa,Nxb,Intpol)
      enddo
!
!     set deep interior of ny grid using values just
!     generated and symmetric cubic interpolation in y
!
      do j = 4 , Ny - 3 , 2
         do i = ist , ifn
            Q(i,j) = (-Q(i,j-3)+9.*(Q(i,j-1)+Q(i,j+1))-Q(i,j+3))*.0625
         enddo
      enddo
!
!     interpolate from q at j=2 and j=ny-1
!
      if ( Nyc/=0 ) then
!
!     asymmetric formula near nonperiodic y boundaries
!
         do i = ist , ifn
            Q(i,2) = (5.*Q(i,1)+15.*Q(i,3)-5.*Q(i,5)+Q(i,7))*.0625
            Q(i,Ny-1) = (5.*Q(i,Ny)+15.*Q(i,Ny-2)-5.*Q(i,Ny-4)+Q(i,Ny-6))*.0625
         enddo
      else
!
!     periodicity in y alows symmetric formula near bndys
!
         do i = ist , ifn
            Q(i,2) = (-Q(i,Ny-2)+9.*(Q(i,1)+Q(i,3))-Q(i,5))*.0625
            Q(i,Ny-1) = (-Q(i,Ny-4)+9.*(Q(i,Ny-2)+Q(i,Ny))-Q(i,3))*.0625
            Q(i,Ny+1) = Q(i,2)
            Q(i,0) = Q(i,Ny-1)
         enddo
      endif
      return
   else
!
!     ncy grid is equals ny grid so interpolate in x only
!
      do j = jst , jfn
         jc = j
         call prolon1(Ncx,P(0,jc),Nx,Q(0,j),Nxa,Nxb,Intpol)
      enddo
!
!     set periodic virtual boundaries if necessary
!
      if ( Nyc==0 ) then
         do i = ist , ifn
            Q(i,0) = Q(i,Ny-1)
            Q(i,Ny+1) = Q(i,2)
         enddo
      endif
      return
   endif
end subroutine prolon2
!*==PROLON1.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
!
!     11/20/97  modification of rgrd1u.f for mudpack
!
subroutine prolon1(Ncx,P,Nx,Q,Nxa,Nxb,Intpol)
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Ncx
   integer , intent(in) :: Nx
   real , intent(in) , dimension(0:Ncx+1) :: P
   real , intent(inout) , dimension(0:Nx+1) :: Q
   integer , intent(in) :: Nxa
   integer , intent(in) :: Nxb
   integer , intent(in) :: Intpol
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ic , ifn , ioddfn , ioddst , ist
!
! End of declarations rewritten by SPAG
!
   ist = 1
   ioddst = 1
   ifn = Nx
   ioddfn = Nx
   if ( Nxa==1 ) then
      ist = 2
      ioddst = 3
   endif
   if ( Nxb==1 ) then
      ifn = Nx - 1
      ioddfn = Nx - 2
   endif
   if ( Intpol==1 .or. Ncx<4 ) then
!
!     linear interpolation in x
!
      if ( Ncx<Nx ) then
!
!     every other point of nx grid is ncx grid
!
         do i = ioddst , ioddfn , 2
            ic = (i+1)/2
            Q(i) = P(ic)
         enddo
         do i = 2 , ifn , 2
            Q(i) = 0.5*(Q(i-1)+Q(i+1))
         enddo
      else
!
!     nx grid equals ncx grid
!
         do i = ist , ifn
            Q(i) = P(i)
         enddo
      endif
!
!     set virtual end points if periodic
!
      if ( Nxa==0 ) then
         Q(0) = Q(Nx-1)
         Q(Nx+1) = Q(2)
      endif
      return
!
!     cubic interpolation in x
!
   elseif ( Ncx<Nx ) then
      do i = ioddst , ioddfn , 2
         ic = (i+1)/2
         Q(i) = P(ic)
      enddo
!
!      set deep interior with symmetric formula
!
      do i = 4 , Nx - 3 , 2
         Q(i) = (-Q(i-3)+9.*(Q(i-1)+Q(i+1))-Q(i+3))*.0625
      enddo
!
!     interpolate from q at i=2 and i=nx-1
!
      if ( Nxa/=0 ) then
!
!     asymmetric formula near nonperiodic bndys
!
         Q(2) = (5.*Q(1)+15.*Q(3)-5.*Q(5)+Q(7))*.0625
         Q(Nx-1) = (5.*Q(Nx)+15.*Q(Nx-2)-5.*Q(Nx-4)+Q(Nx-6))*.0625
      else
!
!     periodicity in x alows symmetric formula near bndys
!
         Q(2) = (-Q(Nx-2)+9.*(Q(1)+Q(3))-Q(5))*.0625
         Q(Nx-1) = (-Q(Nx-4)+9.*(Q(Nx-2)+Q(Nx))-Q(3))*.0625
         Q(Nx+1) = Q(2)
         Q(0) = Q(Nx-1)
      endif
      return
   else
!
!     ncx grid equals nx grid
!
      do i = ist , ifn
         Q(i) = P(i)
      enddo
      if ( Nxa==0 ) then
         Q(0) = Q(Nx-1)
         Q(Nx+1) = Q(2)
      endif
      return
   endif
end subroutine prolon1
!*==COR2.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
 
subroutine cor2(Nx,Ny,Phif,Ncx,Ncy,Phic,Nxa,Nxb,Nyc,Nyd,Intpol,Phcor)
!
!     add coarse grid correction in phic to fine grid approximation
!     in phif using linear or cubic interpolation
!
   use s_prolon2
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer :: Nx
   integer :: Ny
   integer :: Ncx
   integer :: Ncy
   real , intent(inout) , dimension(0:Nx+1,0:Ny+1) :: Phif
   real , dimension(0:Ncx+1,0:Ncy+1) :: Phic
   integer :: Nxa
   integer :: Nxb
   integer :: Nyc
   integer :: Nyd
   integer :: Intpol
   real , intent(inout) , dimension(0:Nx+1,0:Ny+1) :: Phcor
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ifn , ist , j , jfn , jst
!
! End of declarations rewritten by SPAG
!
   do j = 0 , Ny + 1
      do i = 0 , Nx + 1
         Phcor(i,j) = 0.0
      enddo
   enddo
!
!     lift correction in phic to fine grid in phcor
!
   call prolon2(Ncx,Ncy,Phic,Nx,Ny,Phcor,Nxa,Nxb,Nyc,Nyd,Intpol)
!
!     add correction in phcor to phif on nonspecified boundaries
!
   ist = 1
   ifn = Nx
   jst = 1
   jfn = Ny
   if ( Nxa==1 ) ist = 2
   if ( Nxb==1 ) ifn = Nx - 1
   if ( Nyc==1 ) jst = 2
   if ( Nyd==1 ) jfn = Ny - 1
   do j = jst , jfn
      do i = ist , ifn
         Phif(i,j) = Phif(i,j) + Phcor(i,j)
      enddo
   enddo
!
!     add periodic points if necessary
!
   if ( Nyc==0 ) then
      do i = ist , ifn
         Phif(i,0) = Phif(i,Ny-1)
         Phif(i,Ny+1) = Phif(i,2)
      enddo
   endif
   if ( Nxa==0 ) then
      do j = jst , jfn
         Phif(0,j) = Phif(Nx-1,j)
         Phif(Nx+1,j) = Phif(2,j)
      enddo
   endif
end subroutine cor2
!*==PDE2.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine pde2(Nx,Ny,U,I,J,Ux3,Ux4,Uy3,Uy4,Nxa,Nyc)
   use c_pde2com
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nx
   integer , intent(in) :: Ny
   real , intent(in) , dimension(Nx,Ny) :: U
   integer , intent(in) :: I
   integer , intent(in) :: J
   real , intent(out) :: Ux3
   real , intent(out) :: Ux4
   real , intent(out) :: Uy3
   real , intent(out) :: Uy4
   integer , intent(in) :: Nxa
   integer , intent(in) :: Nyc
!
! End of declarations rewritten by SPAG
!
!
!     use second order approximation in u to estimate (second order)
!     third and fourth partial derivatives in the x and y direction
!     non-symmetric difference formula (derived from the  routine
!     finpdf,findif) are used at and one point in from mixed boundaries.
!
   if ( Nxa/=0 ) then
!
!     nonperiodic in x
!
      if ( I>2 .and. I<Nx-1 ) then
         Ux3 = (-U(I-2,J)+2.0*U(I-1,J)-2.0*U(I+1,J)+U(I+2,J))/tdlx3
         Ux4 = (U(I-2,J)-4.0*U(I-1,J)+6.0*U(I,J)-4.0*U(I+1,J)+U(I+2,J))/dlx4
      elseif ( I==1 ) then
         Ux3 = (-5.0*U(1,J)+18.0*U(2,J)-24.0*U(3,J)+14.0*U(4,J)-3.0*U(5,J))/tdlx3
         Ux4 = (3.0*U(1,J)-14.0*U(2,J)+26.0*U(3,J)-24.0*U(4,J)+11.0*U(5,J)-2.0*U(6,J))/dlx4
      elseif ( I==2 ) then
         Ux3 = (-3.0*U(1,J)+10.0*U(2,J)-12.0*U(3,J)+6.0*U(4,J)-U(5,J))/tdlx3
         Ux4 = (2.0*U(1,J)-9.0*U(2,J)+16.0*U(3,J)-14.0*U(4,J)+6.0*U(5,J)-U(6,J))/dlx4
      elseif ( I==Nx-1 ) then
         Ux3 = (U(Nx-4,J)-6.0*U(Nx-3,J)+12.0*U(Nx-2,J)-10.0*U(Nx-1,J)+3.0*U(Nx,J))/tdlx3
         Ux4 = (-U(Nx-5,J)+6.0*U(Nx-4,J)-14.0*U(Nx-3,J)+16.0*U(Nx-2,J)-9.0*U(Nx-1,J)+2.0*U(Nx,J))/dlx4
      elseif ( I==Nx ) then
         Ux3 = (3.0*U(Nx-4,J)-14.0*U(Nx-3,J)+24.0*U(Nx-2,J)-18.0*U(Nx-1,J)+5.0*U(Nx,J))/tdlx3
         Ux4 = (-2.0*U(Nx-5,J)+11.0*U(Nx-4,J)-24.0*U(Nx-3,J)+26.0*U(Nx-2,J)-14.0*U(Nx-1,J)+3.0*U(Nx,J))/dlx4
      endif
!
!     periodic in x
!
   elseif ( I>2 .and. I<Nx-1 ) then
      Ux3 = (-U(I-2,J)+2.0*U(I-1,J)-2.0*U(I+1,J)+U(I+2,J))/tdlx3
      Ux4 = (U(I-2,J)-4.0*U(I-1,J)+6.0*U(I,J)-4.0*U(I+1,J)+U(I+2,J))/dlx4
   elseif ( I==1 ) then
      Ux3 = (-U(Nx-2,J)+2.0*U(Nx-1,J)-2.0*U(2,J)+U(3,J))/tdlx3
      Ux4 = (U(Nx-2,J)-4.0*U(Nx-1,J)+6.0*U(1,J)-4.0*U(2,J)+U(3,J))/dlx4
   elseif ( I==2 ) then
      Ux3 = (-U(Nx-1,J)+2.0*U(1,J)-2.0*U(3,J)+U(4,J))/(tdlx3)
      Ux4 = (U(Nx-1,J)-4.0*U(1,J)+6.0*U(2,J)-4.0*U(3,J)+U(4,J))/dlx4
   elseif ( I==Nx-1 ) then
      Ux3 = (-U(Nx-3,J)+2.0*U(Nx-2,J)-2.0*U(1,J)+U(2,J))/tdlx3
      Ux4 = (U(Nx-3,J)-4.0*U(Nx-2,J)+6.0*U(Nx-1,J)-4.0*U(1,J)+U(2,J))/dlx4
   elseif ( I==Nx ) then
      Ux3 = (-U(Nx-2,J)+2.0*U(Nx-1,J)-2.0*U(2,J)+U(3,J))/tdlx3
      Ux4 = (U(Nx-2,J)-4.0*U(Nx-1,J)+6.0*U(Nx,J)-4.0*U(2,J)+U(3,J))/dlx4
   endif
!
!     y partial derivatives
!
   if ( Nyc/=0 ) then
!
!     not periodic in y
!
      if ( J>2 .and. J<Ny-1 ) then
         Uy3 = (-U(I,J-2)+2.0*U(I,J-1)-2.0*U(I,J+1)+U(I,J+2))/tdly3
         Uy4 = (U(I,J-2)-4.0*U(I,J-1)+6.0*U(I,J)-4.0*U(I,J+1)+U(I,J+2))/dly4
      elseif ( J==1 ) then
         Uy3 = (-5.0*U(I,1)+18.0*U(I,2)-24.0*U(I,3)+14.0*U(I,4)-3.0*U(I,5))/tdly3
         Uy4 = (3.0*U(I,1)-14.0*U(I,2)+26.0*U(I,3)-24.0*U(I,4)+11.0*U(I,5)-2.0*U(I,6))/dly4
      elseif ( J==2 ) then
         Uy3 = (-3.0*U(I,1)+10.0*U(I,2)-12.0*U(I,3)+6.0*U(I,4)-U(I,5))/tdly3
         Uy4 = (2.0*U(I,1)-9.0*U(I,2)+16.0*U(I,3)-14.0*U(I,4)+6.0*U(I,5)-U(I,6))/dly4
      elseif ( J==Ny-1 ) then
         Uy3 = (U(I,Ny-4)-6.0*U(I,Ny-3)+12.0*U(I,Ny-2)-10.0*U(I,Ny-1)+3.0*U(I,Ny))/tdly3
         Uy4 = (-U(I,Ny-5)+6.0*U(I,Ny-4)-14.0*U(I,Ny-3)+16.0*U(I,Ny-2)-9.0*U(I,Ny-1)+2.0*U(I,Ny))/dly4
      elseif ( J==Ny ) then
         Uy3 = (3.0*U(I,Ny-4)-14.0*U(I,Ny-3)+24.0*U(I,Ny-2)-18.0*U(I,Ny-1)+5.0*U(I,Ny))/tdly3
         Uy4 = (-2.0*U(I,Ny-5)+11.0*U(I,Ny-4)-24.0*U(I,Ny-3)+26.0*U(I,Ny-2)-14.0*U(I,Ny-1)+3.0*U(I,Ny))/dly4
      endif
!
!     periodic in y
!
   elseif ( J>2 .and. J<Ny-1 ) then
      Uy3 = (-U(I,J-2)+2.0*U(I,J-1)-2.0*U(I,J+1)+U(I,J+2))/tdly3
      Uy4 = (U(I,J-2)-4.0*U(I,J-1)+6.0*U(I,J)-4.0*U(I,J+1)+U(I,J+2))/dly4
   elseif ( J==1 ) then
      Uy3 = (-U(I,Ny-2)+2.0*U(I,Ny-1)-2.0*U(I,2)+U(I,3))/tdly3
      Uy4 = (U(I,Ny-2)-4.0*U(I,Ny-1)+6.0*U(I,1)-4.0*U(I,2)+U(I,3))/dly4
   elseif ( J==2 ) then
      Uy3 = (-U(I,Ny-1)+2.0*U(I,1)-2.0*U(I,3)+U(I,4))/(tdly3)
      Uy4 = (U(I,Ny-1)-4.0*U(I,1)+6.0*U(I,2)-4.0*U(I,3)+U(I,4))/dly4
   elseif ( J==Ny-1 ) then
      Uy3 = (-U(I,Ny-3)+2.0*U(I,Ny-2)-2.0*U(I,1)+U(I,2))/tdly3
      Uy4 = (U(I,Ny-3)-4.0*U(I,Ny-2)+6.0*U(I,Ny-1)-4.0*U(I,1)+U(I,2))/dly4
   elseif ( J==Ny ) then
      Uy3 = (-U(I,Ny-2)+2.0*U(I,Ny-1)-2.0*U(I,2)+U(I,3))/tdly3
      Uy4 = (U(I,Ny-2)-4.0*U(I,Ny-1)+6.0*U(I,Ny)-4.0*U(I,2)+U(I,3))/dly4
   endif
end subroutine pde2
!*==SWK3.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine swk3(Nfx,Nfy,Nfz,Phif,Rhsf,Phi,Rhs)
!
!     set phif,rhsf input in arrays which include
!     virtual boundaries for phi (for all 2-d real codes)
!
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nfx
   integer , intent(in) :: Nfy
   integer , intent(in) :: Nfz
   real , intent(in) , dimension(Nfx,Nfy,Nfz) :: Phif
   real , intent(in) , dimension(Nfx,Nfy,Nfz) :: Rhsf
   real , intent(out) , dimension(0:Nfx+1,0:Nfy+1,0:Nfz+1) :: Phi
   real , intent(out) , dimension(Nfx,Nfy,Nfz) :: Rhs
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , j , k
!
! End of declarations rewritten by SPAG
!
   do k = 1 , Nfz
      do j = 1 , Nfy
         do i = 1 , Nfx
            Phi(i,j,k) = Phif(i,j,k)
            Rhs(i,j,k) = Rhsf(i,j,k)
         enddo
      enddo
   enddo
!
!     set virtual boundaries in phi to zero
!
   do k = 0 , Nfz + 1
      do j = 0 , Nfy + 1
         Phi(0,j,k) = 0.0
         Phi(Nfx+1,j,k) = 0.0
      enddo
   enddo
   do k = 0 , Nfz + 1
      do i = 0 , Nfx + 1
         Phi(i,0,k) = 0.0
         Phi(i,Nfy+1,k) = 0.0
      enddo
   enddo
   do j = 0 , Nfy + 1
      do i = 0 , Nfx + 1
         Phi(i,j,0) = 0.0
         Phi(i,j,Nfz+1) = 0.0
      enddo
   enddo
end subroutine swk3
!*==TRSFC3.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine trsfc3(Nx,Ny,Nz,Phi,Rhs,Ncx,Ncy,Ncz,Phic,Rhsc)
!
!     transfer fine grid to coarse grid
!
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nx
   integer , intent(in) :: Ny
   integer , intent(in) :: Nz
   integer , intent(in) :: Ncx
   integer , intent(in) :: Ncy
   integer , intent(in) :: Ncz
   real , intent(in) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Phi
   real , intent(in) , dimension(Nx,Ny,Nz) :: Rhs
   real , intent(out) , dimension(0:Ncx+1,0:Ncy+1,0:Ncz+1) :: Phic
   real , intent(out) , dimension(Ncx,Ncy,Ncz) :: Rhsc
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ic , ix , j , jc , jy , k , kc , kz
!
! End of declarations rewritten by SPAG
!
!
!     set virtual boundaries in phic to zero
!
   do kc = 0 , Ncz + 1
      do jc = 0 , Ncy + 1
         Phic(0,jc,kc) = 0.0
         Phic(Ncx+1,jc,kc) = 0.0
      enddo
   enddo
   do kc = 0 , Ncz + 1
      do ic = 0 , Ncx + 1
         Phic(ic,0,kc) = 0.0
         Phic(ic,Ncy+1,kc) = 0.0
      enddo
   enddo
   do jc = 0 , Ncy + 1
      do ic = 0 , Ncx + 1
         Phic(ic,jc,0) = 0.0
         Phic(ic,jc,Ncz+1) = 0.0
      enddo
   enddo
   if ( Ncx<Nx .and. Ncy<Ny .and. Ncz<Nz ) then
!
!     coarsening in x,y,z (usually the case?)
!
      do kc = 1 , Ncz
         k = kc + kc - 1
         do jc = 1 , Ncy
            j = jc + jc - 1
            do ic = 1 , Ncx
               i = ic + ic - 1
               Phic(ic,jc,kc) = Phi(i,j,k)
               Rhsc(ic,jc,kc) = Rhs(i,j,k)
            enddo
         enddo
      enddo
   else
!
!     no coarsening in at least one dimension
!
      ix = 1
      if ( Ncx==Nx ) ix = 0
      jy = 1
      if ( Ncy==Ny ) jy = 0
      kz = 1
      if ( Ncz==Nz ) kz = 0
 
      do kc = 1 , Ncz
         k = kc + kz*(kc-1)
         do jc = 1 , Ncy
            j = jc + jy*(jc-1)
            do ic = 1 , Ncx
               i = ic + ix*(ic-1)
               Phic(ic,jc,kc) = Phi(i,j,k)
               Rhsc(ic,jc,kc) = Rhs(i,j,k)
            enddo
         enddo
      enddo
   endif
end subroutine trsfc3
!*==RES3.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine res3(Nx,Ny,Nz,Resf,Ncx,Ncy,Ncz,Rhsc,Nxa,Nxb,Nyc,Nyd,Nze,Nzf)
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nx
   integer , intent(in) :: Ny
   integer , intent(in) :: Nz
   integer , intent(in) :: Ncx
   integer , intent(in) :: Ncy
   integer , intent(in) :: Ncz
   real , intent(in) , dimension(Nx,Ny,Nz) :: Resf
   real , intent(out) , dimension(Ncx,Ncy,Ncz) :: Rhsc
   integer , intent(in) :: Nxa
   integer , intent(in) :: Nxb
   integer , intent(in) :: Nyc
   integer , intent(in) :: Nyd
   integer , intent(in) :: Nze
   integer , intent(in) :: Nzf
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ic , im1 , ip1 , ix , j , jc , jm1 , jp1 , jy , k , kc , km1 , kp1 , kz
   real :: rk , rm , rp
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
   if ( Ncx==Nx ) ix = 0
   jy = 1
   if ( Ncy==Ny ) jy = 0
   kz = 1
   if ( Ncz==Nz ) kz = 0
!
!     restrict on interior
!
   if ( Ncz<Nz .and. Ncy<Ny .and. Ncx<Nx ) then
!
!     coarsening in x,y,z
!
!$OMP PARALLEL DO PRIVATE(i,j,k,ic,jc,kc,rm,rk,rp)
!$OMP+SHARED(resf,rhsc,ncx,ncy,ncz)
      do kc = 2 , Ncz - 1
         k = kc + kc - 1
         do jc = 2 , Ncy - 1
            j = jc + jc - 1
            do ic = 2 , Ncx - 1
               i = ic + ic - 1
!
!     weight on k-1,k,k+1 z planes in rm,rk,rp
!
               rm = (Resf(i-1,j-1,k-1)+Resf(i+1,j-1,k-1)+Resf(i-1,j+1,k-1)+Resf(i+1,j+1,k-1)                                       &
                  & +2.*(Resf(i-1,j,k-1)+Resf(i+1,j,k-1)+Resf(i,j-1,k-1)+Resf(i,j+1,k-1))+4.*Resf(i,j,k-1))*.0625
               rk = (Resf(i-1,j-1,k)+Resf(i+1,j-1,k)+Resf(i-1,j+1,k)+Resf(i+1,j+1,k)+2.*(Resf(i-1,j,k)+Resf(i+1,j,k)+Resf(i,j-1,k) &
                  & +Resf(i,j+1,k))+4.*Resf(i,j,k))*.0625
               rp = (Resf(i-1,j-1,k+1)+Resf(i+1,j-1,k+1)+Resf(i-1,j+1,k+1)+Resf(i+1,j+1,k+1)                                       &
                  & +2.*(Resf(i-1,j,k+1)+Resf(i+1,j,k+1)+Resf(i,j-1,k+1)+Resf(i,j+1,k+1))+4.*Resf(i,j,k+1))*.0625
!
!     weight in z direction for final result
!
               Rhsc(ic,jc,kc) = 0.25*(rm+2.*rk+rp)
            enddo
         enddo
      enddo
   else
!
!     allow for noncoarsening in any of x,y,z
!
!$OMP PARALLEL DO PRIVATE(i,j,k,ic,jc,kc,rm,rk,rp)
!$OMP+SHARED(ix,jy,kz,resf,rhsc,ncx,ncy,ncz)
      do kc = 2 , Ncz - 1
         k = kc + kz*(kc-1)
         do jc = 2 , Ncy - 1
            j = jc + jy*(jc-1)
            do ic = 2 , Ncx - 1
               i = ic + ix*(ic-1)
!
!     weight on k-1,k,k+1 z planes in rm,rk,rp
!
               rm = (Resf(i-1,j-1,k-1)+Resf(i+1,j-1,k-1)+Resf(i-1,j+1,k-1)+Resf(i+1,j+1,k-1)                                       &
                  & +2.*(Resf(i-1,j,k-1)+Resf(i+1,j,k-1)+Resf(i,j-1,k-1)+Resf(i,j+1,k-1))+4.*Resf(i,j,k-1))*.0625
               rk = (Resf(i-1,j-1,k)+Resf(i+1,j-1,k)+Resf(i-1,j+1,k)+Resf(i+1,j+1,k)+2.*(Resf(i-1,j,k)+Resf(i+1,j,k)+Resf(i,j-1,k) &
                  & +Resf(i,j+1,k))+4.*Resf(i,j,k))*.0625
               rp = (Resf(i-1,j-1,k+1)+Resf(i+1,j-1,k+1)+Resf(i-1,j+1,k+1)+Resf(i+1,j+1,k+1)                                       &
                  & +2.*(Resf(i-1,j,k+1)+Resf(i+1,j,k+1)+Resf(i,j-1,k+1)+Resf(i,j+1,k+1))+4.*Resf(i,j,k+1))*.0625
!
!     weight in z direction for final result
!
               Rhsc(ic,jc,kc) = 0.25*(rm+2.*rk+rp)
            enddo
         enddo
      enddo
   endif
!
!     set residual on boundaries
!
   do ic = 1 , Ncx , Ncx - 1
!
!     x=xa and x=xb
!
      i = ic + ix*(ic-1)
      im1 = max0(i-1,2)
      ip1 = min0(i+1,Nx-1)
      if ( i==1 .and. Nxa==0 ) im1 = Nx - 1
      if ( i==Nx .and. Nxb==0 ) ip1 = 2
!
!    (y,z) interior
!
!$OMP PARALLEL DO PRIVATE(j,k,jc,kc,rm,rk,rp)
!$OMP+SHARED(kz,jy,ic,im1,i,ip1,resf,rhsc,ncy,ncz)
      do kc = 2 , Ncz - 1
         k = kc + kz*(kc-1)
         do jc = 2 , Ncy - 1
            j = jc + jy*(jc-1)
            rm = (Resf(im1,j-1,k-1)+Resf(ip1,j-1,k-1)+Resf(im1,j+1,k-1)+Resf(ip1,j+1,k-1)                                          &
               & +2.*(Resf(im1,j,k-1)+Resf(ip1,j,k-1)+Resf(i,j-1,k-1)+Resf(i,j+1,k-1))+4.*Resf(i,j,k-1))*.0625
            rk = (Resf(im1,j-1,k)+Resf(ip1,j-1,k)+Resf(im1,j+1,k)+Resf(ip1,j+1,k)+2.*(Resf(im1,j,k)+Resf(ip1,j,k)+Resf(i,j-1,k)    &
               & +Resf(i,j+1,k))+4.*Resf(i,j,k))*.0625
            rp = (Resf(im1,j-1,k+1)+Resf(ip1,j-1,k+1)+Resf(im1,j+1,k+1)+Resf(ip1,j+1,k+1)                                          &
               & +2.*(Resf(im1,j,k+1)+Resf(ip1,j,k+1)+Resf(i,j-1,k+1)+Resf(i,j+1,k+1))+4.*Resf(i,j,k+1))*.0625
            Rhsc(ic,jc,kc) = 0.25*(rm+2.*rk+rp)
         enddo
      enddo
!
!     x=xa,xb and y=yc,yd interior edges
!
      do jc = 1 , Ncy , Ncy - 1
         j = jc + jy*(jc-1)
         jm1 = max0(j-1,2)
         jp1 = min0(j+1,Ny-1)
         if ( j==1 .and. Nyc==0 ) jm1 = Ny - 1
         if ( j==Ny .and. Nyc==0 ) jp1 = 2
         do kc = 2 , Ncz - 1
            k = kc + kz*(kc-1)
            rm = (Resf(im1,jm1,k-1)+Resf(ip1,jm1,k-1)+Resf(im1,jp1,k-1)+Resf(ip1,jp1,k-1)                                          &
               & +2.*(Resf(im1,j,k-1)+Resf(ip1,j,k-1)+Resf(i,jm1,k-1)+Resf(i,jp1,k-1))+4.*Resf(i,j,k-1))*.0625
            rk = (Resf(im1,jm1,k)+Resf(ip1,jm1,k)+Resf(im1,jp1,k)+Resf(ip1,jp1,k)+2.*(Resf(im1,j,k)+Resf(ip1,j,k)+Resf(i,jm1,k)    &
               & +Resf(i,jp1,k))+4.*Resf(i,j,k))*.0625
            rp = (Resf(im1,jm1,k+1)+Resf(ip1,jm1,k+1)+Resf(im1,jp1,k+1)+Resf(ip1,jp1,k+1)                                          &
               & +2.*(Resf(im1,j,k+1)+Resf(ip1,j,k+1)+Resf(i,jm1,k+1)+Resf(i,jp1,k+1))+4.*Resf(i,j,k+1))*.0625
            Rhsc(ic,jc,kc) = 0.25*(rm+2.*rk+rp)
         enddo
!     x=xa,xb; y=yc,yd; z=ze,zf cornors
         do kc = 1 , Ncz , Ncz - 1
            k = kc + kz*(kc-1)
            km1 = max0(k-1,2)
            kp1 = min0(k+1,Nz-1)
            if ( k==1 .and. Nze==0 ) km1 = Nz - 1
            if ( k==Nz .and. Nzf==0 ) kp1 = 2
            rm = (Resf(im1,jm1,km1)+Resf(ip1,jm1,km1)+Resf(im1,jp1,km1)+Resf(ip1,jp1,km1)                                          &
               & +2.*(Resf(im1,j,km1)+Resf(ip1,j,km1)+Resf(i,jm1,km1)+Resf(i,jp1,km1))+4.*Resf(i,j,km1))*.0625
            rk = (Resf(im1,jm1,k)+Resf(ip1,jm1,k)+Resf(im1,jp1,k)+Resf(ip1,jp1,k)+2.*(Resf(im1,j,k)+Resf(ip1,j,k)+Resf(i,jm1,k)    &
               & +Resf(i,jp1,k))+4.*Resf(i,j,k))*.0625
            rp = (Resf(im1,jm1,kp1)+Resf(ip1,jm1,kp1)+Resf(im1,jp1,kp1)+Resf(ip1,jp1,kp1)                                          &
               & +2.*(Resf(im1,j,kp1)+Resf(ip1,j,kp1)+Resf(i,jm1,kp1)+Resf(i,jp1,kp1))+4.*Resf(i,j,kp1))*.0625
            Rhsc(ic,jc,kc) = 0.25*(rm+2.*rk+rp)
         enddo
      enddo
!
!      x=xa,xb and z=ze,zf edges
!
      do kc = 1 , Ncz , Ncz - 1
         k = kc + kz*(kc-1)
         km1 = max0(k-1,2)
         kp1 = min0(k+1,Nz-1)
         if ( k==1 .and. Nze==0 ) km1 = Nz - 1
         if ( k==Nz .and. Nzf==0 ) kp1 = 2
         do jc = 2 , Ncy - 1
            j = jc + jy*(jc-1)
            rm = (Resf(im1,j-1,km1)+Resf(ip1,j-1,km1)+Resf(im1,j+1,km1)+Resf(ip1,j+1,km1)                                          &
               & +2.*(Resf(im1,j,km1)+Resf(ip1,j,km1)+Resf(i,j-1,km1)+Resf(i,j+1,km1))+4.*Resf(i,j,km1))*.0625
            rk = (Resf(im1,j-1,k)+Resf(ip1,j-1,k)+Resf(im1,j+1,k)+Resf(ip1,j+1,k)+2.*(Resf(im1,j,k)+Resf(ip1,j,k)+Resf(i,j-1,k)    &
               & +Resf(i,j+1,k))+4.*Resf(i,j,k))*.0625
            rp = (Resf(im1,j-1,kp1)+Resf(ip1,j-1,kp1)+Resf(im1,j+1,kp1)+Resf(ip1,j+1,kp1)                                          &
               & +2.*(Resf(im1,j,kp1)+Resf(ip1,j,kp1)+Resf(i,j-1,kp1)+Resf(i,j+1,kp1))+4.*Resf(i,j,kp1))*.0625
            Rhsc(ic,jc,kc) = 0.25*(rm+2.*rk+rp)
         enddo
      enddo
   enddo
!
!     y boundaries y=yc and y=yd
!
   do jc = 1 , Ncy , Ncy - 1
      j = jc + jy*(jc-1)
      jm1 = max0(j-1,2)
      jp1 = min0(j+1,Ny-1)
      if ( j==1 .and. Nyc==0 ) jm1 = Ny - 1
      if ( j==Ny .and. Nyd==0 ) jp1 = 2
!
!     (x,z) interior
!
!$OMP PARALLEL DO PRIVATE(i,k,ic,kc,rm,rk,rp)
!$OMP+SHARED(ix,kz,jc,jm1,j,jp1,resf,rhsc,ncx,ncz)
      do kc = 2 , Ncz - 1
         k = kc + kz*(kc-1)
         do ic = 2 , Ncx - 1
            i = ic + ix*(ic-1)
            rm = (Resf(i-1,jm1,k-1)+Resf(i+1,jm1,k-1)+Resf(i-1,jp1,k-1)+Resf(i+1,jp1,k-1)                                          &
               & +2.*(Resf(i-1,j,k-1)+Resf(i+1,j,k-1)+Resf(i,jm1,k-1)+Resf(i,jp1,k-1))+4.*Resf(i,j,k-1))*.0625
            rk = (Resf(i-1,jm1,k)+Resf(i+1,jm1,k)+Resf(i-1,jp1,k)+Resf(i+1,jp1,k)+2.*(Resf(i-1,j,k)+Resf(i+1,j,k)+Resf(i,jm1,k)    &
               & +Resf(i,jp1,k))+4.*Resf(i,j,k))*.0625
            rp = (Resf(i-1,jm1,k+1)+Resf(i+1,jm1,k+1)+Resf(i-1,jp1,k+1)+Resf(i+1,jp1,k+1)                                          &
               & +2.*(Resf(i-1,j,k+1)+Resf(i+1,j,k+1)+Resf(i,jm1,k+1)+Resf(i,jp1,k+1))+4.*Resf(i,j,k+1))*.0625
            Rhsc(ic,jc,kc) = 0.25*(rm+2.*rk+rp)
         enddo
      enddo
!
!     y=yc,yd and z=ze,zf edges
!
      do kc = 1 , Ncz , Ncz - 1
         k = kc + kz*(kc-1)
         km1 = max0(k-1,2)
         kp1 = min0(k+1,Nz-1)
         if ( k==1 .and. Nze==0 ) km1 = Nz - 1
         if ( k==Nz .and. Nzf==0 ) kp1 = 2
!
!     interior in x
!
         do ic = 2 , Ncx - 1
            i = ic + ix*(ic-1)
            rm = (Resf(i-1,jm1,km1)+Resf(i+1,jm1,km1)+Resf(i-1,jp1,km1)+Resf(i+1,jp1,km1)                                          &
               & +2.*(Resf(i-1,j,km1)+Resf(i+1,j,km1)+Resf(i,jm1,km1)+Resf(i,jp1,km1))+4.*Resf(i,j,km1))*.0625
            rk = (Resf(i-1,jm1,k)+Resf(i+1,jm1,k)+Resf(i-1,jp1,k)+Resf(i+1,jp1,k)+2.*(Resf(i-1,j,k)+Resf(i+1,j,k)+Resf(i,jm1,k)    &
               & +Resf(i,jp1,k))+4.*Resf(i,j,k))*.0625
            rp = (Resf(i-1,jm1,kp1)+Resf(i+1,jm1,kp1)+Resf(i-1,jp1,kp1)+Resf(i+1,jp1,kp1)                                          &
               & +2.*(Resf(i-1,j,kp1)+Resf(i+1,j,kp1)+Resf(i,jm1,kp1)+Resf(i,jp1,kp1))+4.*Resf(i,j,kp1))*.0625
            Rhsc(ic,jc,kc) = 0.25*(rm+2.*rk+rp)
         enddo
      enddo
   enddo
!
!     z=ze,zf boundaries
!
   do kc = 1 , Ncz , Ncz - 1
      k = kc + kz*(kc-1)
      km1 = max0(k-1,2)
      kp1 = min0(k+1,Nz-1)
      if ( k==1 .and. Nze==0 ) km1 = Nz - 1
      if ( k==Nz .and. Nzf==0 ) kp1 = 2
!
!     (x,y) interior
!
!$OMP PARALLEL DO PRIVATE(i,j,ic,jc,rm,rk,rp)
!$OMP+SHARED(ix,jy,kc,km1,k,kp1,resf,rhsc,ncx,ncz)
      do jc = 2 , Ncy - 1
         j = jc + jy*(jc-1)
         do ic = 2 , Ncx - 1
            i = ic + ix*(ic-1)
            rm = (Resf(i-1,j-1,km1)+Resf(i+1,j-1,km1)+Resf(i-1,j+1,km1)+Resf(i+1,j+1,km1)                                          &
               & +2.*(Resf(i-1,j,km1)+Resf(i+1,j,km1)+Resf(i,j-1,km1)+Resf(i,j+1,km1))+4.*Resf(i,j,km1))*.0625
            rk = (Resf(i-1,j-1,k)+Resf(i+1,j-1,k)+Resf(i-1,j+1,k)+Resf(i+1,j+1,k)+2.*(Resf(i-1,j,k)+Resf(i+1,j,k)+Resf(i,j-1,k)    &
               & +Resf(i,j+1,k))+4.*Resf(i,j,k))*.0625
            rp = (Resf(i-1,j-1,kp1)+Resf(i+1,j-1,kp1)+Resf(i-1,j+1,kp1)+Resf(i+1,j+1,kp1)                                          &
               & +2.*(Resf(i-1,j,kp1)+Resf(i+1,j,kp1)+Resf(i,j-1,kp1)+Resf(i,j+1,kp1))+4.*Resf(i,j,kp1))*.0625
            Rhsc(ic,jc,kc) = 0.25*(rm+2.*rk+rp)
         enddo
      enddo
   enddo
!
!     set coarse grid residual to zero at specified boundaries
!
   if ( Nxa==1 ) then
      ic = 1
      do kc = 1 , Ncz
         do jc = 1 , Ncy
            Rhsc(ic,jc,kc) = 0.0
         enddo
      enddo
   endif
   if ( Nxb==1 ) then
      ic = Ncx
      do kc = 1 , Ncz
         do jc = 1 , Ncy
            Rhsc(ic,jc,kc) = 0.0
         enddo
      enddo
   endif
   if ( Nyc==1 ) then
      jc = 1
      do kc = 1 , Ncz
         do ic = 1 , Ncx
            Rhsc(ic,jc,kc) = 0.0
         enddo
      enddo
   endif
   if ( Nyd==1 ) then
      jc = Ncy
      do kc = 1 , Ncz
         do ic = 1 , Ncx
            Rhsc(ic,jc,kc) = 0.0
         enddo
      enddo
   endif
   if ( Nze==1 ) then
      kc = 1
      do jc = 1 , Ncy
         do ic = 1 , Ncx
            Rhsc(ic,jc,kc) = 0.0
         enddo
      enddo
   endif
   if ( Nzf==1 ) then
      kc = Ncz
      do jc = 1 , Ncy
         do ic = 1 , Ncx
            Rhsc(ic,jc,kc) = 0.0
         enddo
      enddo
   endif
end subroutine res3
!*==PROLON3.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
!
!     prolon3 modified from prolon2 11/25/97
!
subroutine prolon3(Ncx,Ncy,Ncz,P,Nx,Ny,Nz,Q,Nxa,Nxb,Nyc,Nyd,Nze,Nzf,Intpol)
   use s_prolon2
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer :: Ncx
   integer :: Ncy
   integer , intent(in) :: Ncz
   integer :: Nx
   integer :: Ny
   integer , intent(in) :: Nz
   real , dimension(0:Ncx+1,0:Ncy+1,0:Ncz+1) :: P
   real , intent(inout) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Q
   integer :: Nxa
   integer :: Nxb
   integer :: Nyc
   integer :: Nyd
   integer , intent(in) :: Nze
   integer , intent(in) :: Nzf
   integer :: Intpol
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ifn , ist , j , jfn , jst , k , kc , kfn , koddfn , koddst , kst
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
   if ( Nxa==1 ) ist = 2
   if ( Nxb==1 ) ifn = Nx - 1
   if ( Nyc==1 ) jst = 2
   if ( Nyd==1 ) jfn = Ny - 1
   if ( Nze==1 ) then
      kst = 2
      koddst = 3
   endif
   if ( Nzf==1 ) then
      kfn = Nz - 1
      koddfn = Nz - 2
   endif
   if ( Intpol==1 .or. Ncz<4 ) then
!
!     linearly interpolate in z
!
      if ( Ncz<Nz ) then
!
!     ncz grid is an every other point subset of nz grid
!     set odd k planes interpolating in x&y and then set even
!     k planes by averaging odd k planes
!
         do k = koddst , koddfn , 2
            kc = k/2 + 1
            call prolon2(Ncx,Ncy,P(0,0,kc),Nx,Ny,Q(0,0,k),Nxa,Nxb,Nyc,Nyd,Intpol)
         enddo
         do k = 2 , kfn , 2
            do j = jst , jfn
               do i = ist , ifn
                  Q(i,j,k) = 0.5*(Q(i,j,k-1)+Q(i,j,k+1))
               enddo
            enddo
         enddo
!
!     set periodic virtual boundaries if necessary
!
         if ( Nze==0 ) then
            do j = jst , jfn
               do i = ist , ifn
                  Q(i,j,0) = Q(i,j,Nz-1)
                  Q(i,j,Nz+1) = Q(i,j,2)
               enddo
            enddo
         endif
         return
      else
!
!     ncz grid is equals nz grid so interpolate in x&y only
!
         do k = kst , kfn
            kc = k
            call prolon2(Ncx,Ncy,P(0,0,kc),Nx,Ny,Q(0,0,k),Nxa,Nxb,Nyc,Nyd,Intpol)
         enddo
!
!     set periodic virtual boundaries if necessary
!
         if ( Nze==0 ) then
            do j = jst , jfn
               do i = ist , ifn
                  Q(i,j,0) = Q(i,j,Nz-1)
                  Q(i,j,Nz+1) = Q(i,j,2)
               enddo
            enddo
         endif
         return
      endif
!
!     cubically interpolate in z
!
   elseif ( Ncz<Nz ) then
!
!     set every other point of nz grid by interpolating in x&y
!
      do k = koddst , koddfn , 2
         kc = k/2 + 1
         call prolon2(Ncx,Ncy,P(0,0,kc),Nx,Ny,Q(0,0,k),Nxa,Nxb,Nyc,Nyd,Intpol)
      enddo
!
!     set deep interior of nz grid using values just
!     generated and symmetric cubic interpolation in z
!
      do k = 4 , Nz - 3 , 2
         do j = jst , jfn
            do i = ist , ifn
               Q(i,j,k) = (-Q(i,j,k-3)+9.*(Q(i,j,k-1)+Q(i,j,k+1))-Q(i,j,k+3))*.0625
            enddo
         enddo
      enddo
!
!     interpolate from q at k=2 and k=nz-1
!
      if ( Nze/=0 ) then
!
!     asymmetric formula near nonperiodic z boundaries
!
         do j = jst , jfn
            do i = ist , ifn
               Q(i,j,2) = (5.*Q(i,j,1)+15.*Q(i,j,3)-5.*Q(i,j,5)+Q(i,j,7))*.0625
               Q(i,j,Nz-1) = (5.*Q(i,j,Nz)+15.*Q(i,j,Nz-2)-5.*Q(i,j,Nz-4)+Q(i,j,Nz-6))*.0625
            enddo
         enddo
      else
!
!     periodicity in y alows symmetric formula near bndys
!
         do j = jst , jfn
            do i = ist , ifn
               Q(i,j,2) = (-Q(i,j,Nz-2)+9.*(Q(i,j,1)+Q(i,j,3))-Q(i,j,5))*.0625
               Q(i,j,Nz-1) = (-Q(i,j,Nz-4)+9.*(Q(i,j,Nz-2)+Q(i,j,Nz))-Q(i,j,3))*.0625
               Q(i,j,Nz+1) = Q(i,j,2)
               Q(i,j,0) = Q(i,j,Nz-1)
            enddo
         enddo
      endif
      return
   else
!
!     ncz grid is equals nx grid so interpolate in x&y only
!
      do k = kst , kfn
         kc = k
         call prolon2(Ncx,Ncy,P(0,0,kc),Nx,Ny,Q(0,0,k),Nxa,Nxb,Nyc,Nyd,Intpol)
      enddo
!
!     set periodic virtual boundaries if necessary
!
      if ( Nze==0 ) then
         do j = jst , jfn
            do i = ist , ifn
               Q(i,j,0) = Q(i,j,Nz-1)
               Q(i,j,Nz+1) = Q(i,j,2)
            enddo
         enddo
      endif
      return
   endif
end subroutine prolon3
!*==COR3.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine cor3(Nx,Ny,Nz,Phif,Ncx,Ncy,Ncz,Phic,Nxa,Nxb,Nyc,Nyd,Nze,Nzf,Intpol,Phcor)
   use s_prolon3
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
   real , intent(inout) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Phif
   real , dimension(0:Ncx+1,0:Ncy+1,0:Ncz+1) :: Phic
   integer :: Nxa
   integer :: Nxb
   integer :: Nyc
   integer :: Nyd
   integer :: Nze
   integer :: Nzf
   integer :: Intpol
   real , intent(inout) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Phcor
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ifn , ist , j , jfn , jst , k , kfn , kst
!
! End of declarations rewritten by SPAG
!
!
!     add coarse grid correction in phic to fine grid approximation
!     in phif using linear or cubic interpolation
!
   do k = 0 , Nz + 1
      do j = 0 , Ny + 1
         do i = 0 , Nx + 1
            Phcor(i,j,k) = 0.0
         enddo
      enddo
   enddo
!
!     lift correction in phic to fine grid in phcor
!
   call prolon3(Ncx,Ncy,Ncz,Phic,Nx,Ny,Nz,Phcor,Nxa,Nxb,Nyc,Nyd,Nze,Nzf,Intpol)
!
!     add correction in phcor to phif on nonspecified boundaries
!
   ist = 1
   ifn = Nx
   jst = 1
   jfn = Ny
   kst = 1
   kfn = Nz
   if ( Nxa==1 ) ist = 2
   if ( Nxb==1 ) ifn = Nx - 1
   if ( Nyc==1 ) jst = 2
   if ( Nyd==1 ) jfn = Ny - 1
   if ( Nze==1 ) kst = 2
   if ( Nzf==1 ) kfn = Nz - 1
   do k = kst , kfn
      do j = jst , jfn
         do i = ist , ifn
            Phif(i,j,k) = Phif(i,j,k) + Phcor(i,j,k)
         enddo
      enddo
   enddo
!
!     add periodic points if necessary
!
   if ( Nze==0 ) then
      do j = jst , jfn
         do i = ist , ifn
            Phif(i,j,0) = Phif(i,j,Nz-1)
            Phif(i,j,Nz+1) = Phif(i,j,2)
         enddo
      enddo
   endif
   if ( Nyc==0 ) then
      do k = kst , kfn
         do i = ist , ifn
            Phif(i,0,k) = Phif(i,Ny-1,k)
            Phif(i,Ny+1,k) = Phif(i,2,k)
         enddo
      enddo
   endif
   if ( Nxa==0 ) then
      do k = kst , kfn
         do j = jst , jfn
            Phif(0,j,k) = Phif(Nx-1,j,k)
            Phif(Nx+1,j,k) = Phif(2,j,k)
         enddo
      enddo
   endif
end subroutine cor3
!*==PER3VB.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine per3vb(Nx,Ny,Nz,Phi,Nxa,Nyc,Nze)
!
!     set virtual periodic boundaries from interior values
!     in three dimensions (for all 3-d solvers)
!
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nx
   integer , intent(in) :: Ny
   integer , intent(in) :: Nz
   real , intent(inout) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Phi
   integer , intent(in) :: Nxa
   integer , intent(in) :: Nyc
   integer , intent(in) :: Nze
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , j , k
!
! End of declarations rewritten by SPAG
!
   if ( Nxa==0 ) then
      do k = 1 , Nz
         do j = 1 , Ny
            Phi(0,j,k) = Phi(Nx-1,j,k)
            Phi(Nx,j,k) = Phi(1,j,k)
            Phi(Nx+1,j,k) = Phi(2,j,k)
         enddo
      enddo
   endif
   if ( Nyc==0 ) then
      do k = 1 , Nz
         do i = 1 , Nx
            Phi(i,0,k) = Phi(i,Ny-1,k)
            Phi(i,Ny,k) = Phi(i,1,k)
            Phi(i,Ny+1,k) = Phi(i,2,k)
         enddo
      enddo
   endif
   if ( Nze==0 ) then
      do j = 1 , Ny
         do i = 1 , Nx
            Phi(i,j,0) = Phi(i,j,Nz-1)
            Phi(i,j,Nz) = Phi(i,j,1)
            Phi(i,j,Nz+1) = Phi(i,j,2)
         enddo
      enddo
   endif
end subroutine per3vb
!*==PDE2CR.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine pde2cr(Nx,Ny,U,I,J,Ux3y,Uxy3,Ux2y2)
!
!     compute mixed partial derivative approximations
!
   use c_com2dcr
   use c_fmud2cr
   use c_imud2cr
   use c_pde2com
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nx
   integer , intent(in) :: Ny
   real , intent(in) , dimension(Nx,Ny) :: U
   integer , intent(in) :: I
   integer , intent(in) :: J
   real , intent(out) :: Ux3y
   real , intent(out) :: Uxy3
   real , intent(out) :: Ux2y2
!
! Local variable declarations rewritten by SPAG
!
   integer :: m1 , m2 , m3 , m4 , n1 , n2 , n3 , n4
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
 
   if ( I==1 ) then
 
      if ( (J>2 .and. J<Ny-1) ) then
!     x=xa, yinterior
         Ux3y = (5*U(1,J-1)-18*U(2,J-1)+24*U(3,J-1)-14*U(4,J-1)+3*U(5,J-1)-5*U(1,J+1)+18*U(2,J+1)-24*U(3,J+1)+14*U(4,J+1)          &
              & -3*U(5,J+1))/dxxxy4
         Uxy3 = (3*U(1,J-2)-4*U(2,J-2)+U(3,J-2)-6*U(1,J-1)+8*U(2,J-1)-2*U(3,J-1)+6*U(1,J+1)-8*U(2,J+1)+2*U(3,J+1)-3*U(1,J+2)       &
              & +4*U(2,J+2)-U(3,J+2))/dxyyy4
      elseif ( J==1 ) then
!     (xa,yc)
         Ux3y = (15*U(1,1)-54*U(2,1)+72*U(3,1)-42*U(4,1)+9*U(5,1)-20*U(1,2)+72*U(2,2)-96*U(3,2)+56*U(4,2)-12*U(5,2)+5*U(1,3)       &
              & -18*U(2,3)+24*U(3,3)-14*U(4,3)+3*U(5,3))/dxxxy4
         Uxy3 = (15*U(1,1)-20*U(2,1)+5*U(3,1)-54*U(1,2)+72*U(2,2)-18*U(3,2)+72*U(1,3)-96*U(2,3)+24*U(3,3)-42*U(1,4)+56*U(2,4)      &
              & -14*U(3,4)+9*U(1,5)-12*U(2,5)+3*U(3,5))/dxyyy4
         Ux2y2 = (4*U(1,1)-10*U(2,1)+8*U(3,1)-2*U(4,1)-10*U(1,2)+25*U(2,2)-20*U(3,2)+5*U(4,2)+8*U(1,3)-20*U(2,3)+16*U(3,3)-4*U(4,3)&
               & -2*U(1,4)+5*U(2,4)-4*U(3,4)+U(4,4))/dxxyy
      elseif ( J==2 ) then
!     (xa,yc+dly)
         Ux3y = (5*U(1,1)-18*U(2,1)+24*U(3,1)-14*U(4,1)+3*U(5,1)-5*U(1,3)+18*U(2,3)-24*U(3,3)+14*U(4,3)-3*U(5,3))/dxxxy4
         Uxy3 = (9*U(1,1)-12*U(2,1)+3*U(3,1)-30*U(1,2)+40*U(2,2)-10*U(3,2)+36*U(1,3)-48*U(2,3)+12*U(3,3)-18*U(1,4)+24*U(2,4)       &
              & -6*U(3,4)+3*U(1,5)-4*U(2,5)+U(3,5))/dxyyy4
      elseif ( J==Ny-1 ) then
!     x=xa,y=yd-dly
         Ux3y = (5*U(1,J-1)-18*U(2,J-1)+24*U(3,J-1)-14*U(4,J-1)+3*U(5,J-1)-5*U(1,J+1)+18*U(2,J+1)-24*U(3,J+1)+14*U(4,J+1)          &
              & -3*U(5,J+1))
         Uxy3 = (5*U(1,n2)-18*U(2,n2)+24*U(3,n2)-14*U(4,n2)+3*U(5,n2)-5*U(1,Ny)+18*U(2,Ny)-24*U(3,Ny)+14*U(4,Ny)-3*U(5,Ny))/dxyyy4
      elseif ( J==Ny ) then
!     x=xa, y=yd
         Ux3y = (-5*U(1,n2)+18*U(2,n2)-24*U(3,n2)+14*U(4,n2)-3*U(5,n2)+20*U(1,n1)-72*U(2,n1)+96*U(3,n1)-56*U(4,n1)+12*U(5,n1)      &
              & -15*U(1,Ny)+54*U(2,Ny)-72*U(3,Ny)+42*U(4,Ny)-9*U(5,Ny))/dxxxy4
         Uxy3 = (-9*U(1,n4)+12*U(2,n4)-3*U(3,n4)+42*U(1,n3)-56*U(2,n3)+14*U(3,n3)-72*U(1,n2)+96*U(2,n2)-24*U(3,n2)+54*U(1,n1)      &
              & -72*U(2,n1)+18*U(3,n1)-15*U(1,Ny)+20*U(2,Ny)-5*U(3,Ny))/dxyyy4
         Ux2y2 = (-2*U(1,n3)+5*U(2,n3)-4*U(3,n3)+U(4,n3)+8*U(1,n2)-20*U(2,n2)+16*U(3,n2)-4*U(4,n2)-10*U(1,n1)+25*U(2,n1)-20*U(3,n1)&
               & +5*U(4,n1)+4*U(1,Ny)-10*U(2,Ny)+8*U(3,Ny)-2*U(4,Ny))/dxxyy
      endif
 
   elseif ( I==2 ) then
 
      if ( (J>2 .and. J<Ny-1) ) then
!     x=xa+dlx, y interior
         Ux3y = (3*U(1,J-1)-10*U(2,J-1)+12*U(3,J-1)-6*U(4,J-1)+U(5,J-1)-3*U(1,J+1)+10*U(2,J+1)-12*U(3,J+1)+6*U(4,J+1)-U(5,J+1))    &
              & /dxxxy4
         Uxy3 = (U(1,J-2)-U(3,J-2)-2*U(1,J-1)+2*U(3,J-1)+2*U(1,J+1)-2*U(3,J+1)-U(1,J+2)+U(3,J+2))/dxyyy4
      elseif ( J==1 ) then
!     x=xa+dlx, y=yc
         Ux3y = (9*U(1,1)-30*U(2,1)+36*U(3,1)-18*U(4,1)+3*U(5,1)-12*U(1,2)+40*U(2,2)-48*U(3,2)+24*U(4,2)-4*U(5,2)+3*U(1,3)         &
              & -10*U(2,3)+12*U(3,3)-6*U(4,3)+U(5,3))/dxxxy4
         Uxy3 = (5*U(1,1)-5*U(3,1)-18*U(1,2)+18*U(3,2)+24*U(1,3)-24*U(3,3)-14*U(1,4)+14*U(3,4)+3*U(1,5)-3*U(3,5))/dxyyy4
      elseif ( J==2 ) then
!     at x=xa+dlx,y=yc+dly
         Ux3y = (3*U(1,1)-10*U(2,1)+12*U(3,1)-6*U(4,1)+U(5,1)-3*U(1,3)+10*U(2,3)-12*U(3,3)+6*U(4,3)-U(5,3))/dxxxy4
         Uxy3 = (3*U(1,1)-3*U(3,1)-10*U(1,2)+10*U(3,2)+12*U(1,3)-12*U(3,3)-6*U(1,4)+6*U(3,4)+U(1,5)-U(3,5))/dxyyy4
      elseif ( J==Ny-1 ) then
!     x=xa+dlx,y=yd-dly
         Ux3y = (3*U(1,n2)-10*U(2,n2)+12*U(3,n2)-6*U(4,n2)+U(5,n2)-3*U(1,Ny)+10*U(2,Ny)-12*U(3,Ny)+6*U(4,Ny)-U(5,Ny))/dxxxy4
         Uxy3 = (-U(1,n4)+U(3,n4)+6*U(1,n3)-6*U(3,n3)-12*U(1,n2)+12*U(3,n2)+10*U(1,n1)-10*U(3,n1)-3*U(1,Ny)+3*U(3,Ny))/dxyyy4
      elseif ( J==Ny ) then
!     at x=xa+dlx,y=yd
         Ux3y = (-3*U(1,n2)+10*U(2,n2)-12*U(3,n2)+6*U(4,n2)-U(5,n2)+12*U(1,n1)-40*U(2,n1)+48*U(3,n1)-24*U(4,n1)+4*U(5,n1)-9*U(1,Ny)&
              & +30*U(2,Ny)-36*U(3,Ny)+18*U(4,Ny)-3*U(5,Ny))/dxxxy4
         Uxy3 = (-3*U(1,n4)+3*U(3,n4)+14*U(1,n3)-14*U(3,n3)-24*U(1,n2)+24*U(3,n2)+18*U(1,n1)-18*U(3,n1)-5*U(1,Ny)+5*U(3,Ny))/dxyyy4
      endif
 
   elseif ( I>2 .and. I<Nx-1 ) then
 
      if ( J==1 ) then
!     y=yc,x interior
         Ux3y = (3.0*U(I-2,1)-6.0*U(I-1,1)+6.0*U(I+1,1)-3.0*U(I+2,1)-4.0*U(I-2,2)+8.0*U(I-1,2)-8.0*U(I+1,2)+4.0*U(I+2,2)+U(I-2,3)  &
              & -2.0*U(I-1,3)+2.0*U(I+1,3)-U(I+2,3))/dxxxy4
         Uxy3 = (5.0*U(I-1,1)-5.0*U(I+1,1)-18.0*U(I-1,2)+18.0*U(I+1,2)+24.0*U(I-1,3)-24.0*U(I+1,3)-14.0*U(I-1,4)+14.0*U(I+1,4)     &
              & +3.0*U(I-1,5)-3.0*U(I+1,5))/dxyyy4
      elseif ( J==2 ) then
!     y=yc+dly,x interior
         Ux3y = (U(I-2,1)-2.0*U(I-1,1)+2.0*U(I+1,1)-U(I+2,1)-U(I-2,3)+2.0*U(I-1,3)-2.0*U(I+1,3)+U(I+2,3))/dxxxy4
         Uxy3 = (U(I-1,1)-U(I+1,1)-2.0*U(I-1,2)+2.0*U(I+1,2)+2.0*U(I-1,4)-2.0*U(I+1,4)-U(I-1,5)+U(I+1,5))/dxyyy4
      elseif ( J==Ny-1 ) then
!     y=yd-dly, x interior
         Ux3y = (U(I-2,n2)-2.0*U(I-1,n2)+2.0*U(I+1,n2)-U(I+2,n2)-U(I-2,Ny)+2.0*U(I-1,Ny)-2.0*U(I+1,Ny)+U(I+2,Ny))/dxxxy4
         Uxy3 = (-U(I-1,n4)+U(I+1,n4)+6.0*U(I-1,n3)-6.0*U(I+1,n3)-12.0*U(I-1,n2)+12.0*U(I+1,n2)+10.0*U(I-1,n1)-10.0*U(I+1,n1)      &
              & -3.0*U(I-1,Ny)+3.0*U(I+1,Ny))/dxyyy4
      elseif ( J==Ny ) then
!     at y=yd, x interior
         Ux3y = (-U(I-2,n2)+2.0*U(I-1,n2)-2.0*U(I+1,n2)+U(I+2,n2)+4.0*U(I-2,n1)-8.0*U(I-1,n1)+8.0*U(I+1,n1)-4.0*U(I+2,n1)          &
              & -3.0*U(I-2,Ny)+6.0*U(I-1,Ny)-6.0*U(I+1,Ny)+3.0*U(I+2,Ny))/dxxxy4
         Uxy3 = (-3.0*U(I-1,n4)+3.0*U(I+1,n4)+14.0*U(I-1,n3)-14.0*U(I+1,n3)-24.0*U(I-1,n2)+24.0*U(I+1,n2)+18.0*U(I-1,n1)           &
              & -18.0*U(I+1,n1)-5.0*U(I-1,Ny)+5.0*U(I+1,Ny))/dxyyy4
      endif
 
   elseif ( I==Nx-1 ) then
 
      if ( (J>2 .and. J<Ny-1) ) then
!     x=xb-dlx,y interior
         Ux3y = (-U(m4,J-1)+6.*U(m3,J-1)-12.*U(m2,J-1)+10.*U(m1,J-1)-3.*U(Nx,J-1)+U(m4,J+1)-6.*U(m3,J+1)+12.*U(m2,J+1)             &
              & -10.*U(m1,J+1)+3.*U(Nx,J+1))/dxxxy4
         Uxy3 = (U(m2,J-2)-U(Nx,J-2)-2.*U(m2,J-1)+2.*U(Nx,J-1)+2.*U(m2,J+1)-2.*U(Nx,J+1)-U(m2,J+2)+U(Nx,J+2))/dxyyy4
      elseif ( J==1 ) then
!     at x=xb-dlx, y=yc
         Ux3y = (-3.0*U(m4,1)+18.0*U(m3,1)-36.0*U(m2,1)+30.0*U(m1,1)-9.0*U(Nx,1)+4.0*U(m4,2)-24.0*U(m3,2)+48.0*U(m2,2)-40.0*U(m1,2)&
              & +12.0*U(Nx,2)-U(m4,3)+6.0*U(m3,3)-12.0*U(m2,3)+10.0*U(m1,3)-3.0*U(Nx,3))/dxxxy4
         Uxy3 = (5.0*U(m2,1)-5.0*U(Nx,1)-18.0*U(m2,2)+18.0*U(Nx,2)+24.0*U(m2,3)-24.0*U(Nx,3)-14.0*U(m2,4)+14.0*U(Nx,4)+3.0*U(m2,5) &
              & -3.0*U(Nx,5))/dxyyy4
      elseif ( J==2 ) then
!     x=xb-dlx,y=yc+dly
         Ux3y = (-U(m4,1)+6.0*U(m3,1)-12.0*U(m2,1)+10.*U(m1,1)-3.*U(Nx,1)+U(m4,3)-6.0*U(m3,3)+12.0*U(m2,3)-10.*U(m1,3)+3.*U(Nx,3)) &
              & /dxxxy4
         Uxy3 = (3.0*U(m2,1)-3.*U(Nx,1)-10.*U(m2,2)+10.*U(Nx,2)+12.*U(m2,3)-12.*U(Nx,3)-6.*U(m2,4)+6.*U(Nx,4)+U(m2,5)-U(Nx,5))     &
              & /dxyyy4
      elseif ( J==Ny-1 ) then
!     at x=xb-dlx,y=yd-dly
         Ux3y = (-U(m4,n2)+6.*U(m3,n2)-12.*U(m2,n2)+10.*U(m1,n2)-3.*U(Nx,n2)+U(m4,Ny)-6.*U(m3,Ny)+12.*U(m2,Ny)-10.*U(m1,Ny)        &
              & +3.*U(Nx,Ny))/dxxxy4
         Uxy3 = (-U(m2,n4)+U(Nx,n4)+6*U(m2,n3)-6.*U(Nx,n3)-12.*U(m2,n2)+12.*U(Nx,n2)+10.*U(m2,n1)-10.*U(Nx,n1)-3.*U(m2,Ny)         &
              & +3.*U(Nx,Ny))/dxyyy4
      elseif ( J==Ny ) then
!     at x=xb.dlx,y=yd
         Ux3y = (U(m4,n2)-6.*U(m3,n2)+12.*U(m2,n2)-10.*U(m1,n2)+3.*U(Nx,n2)-4.*U(m4,n1)+24.*U(m3,n1)-48.*U(m2,n1)+40.*U(m1,n1)     &
              & -12.*U(Nx,n1)+3.*U(m4,Ny)-18.*U(m3,Ny)+36.*U(m2,Ny)-30.*U(m1,Ny)+9.*U(Nx,Ny))/dxxxy4
         Uxy3 = (-3.*U(m2,n4)+3.*U(Nx,n4)+14.*U(m2,n3)-14.*U(Nx,n3)-24.*U(m2,n2)+24.*U(Nx,n2)+18.*U(m2,n1)-18.*U(Nx,n1)-5.*U(m2,Ny)&
              & +5.*U(Nx,Ny))/dxyyy4
      endif
 
   elseif ( I==Nx ) then
 
      if ( (J>2 .and. J<Ny-1) ) then
!     x=xb,y interior
         Ux3y = (-3.*U(m4,J-1)+14.*U(m3,J-1)-24.*U(m2,J-1)+18.*U(m1,J-1)-5.*U(Nx,J-1)+3.*U(m4,J+1)-14.*U(m3,J+1)+24.*U(m2,J+1)     &
              & -18.*U(m1,J+1)+5.*U(Nx,J+1))/dxxxy4
         Uxy3 = (-U(m2,J-2)+4.*U(m1,J-2)-3.*U(Nx,J-2)+2.*U(m2,J-1)-8.*U(m1,J-1)+6.*U(Nx,J-1)-2.*U(m2,J+1)+8.*U(m1,J+1)-6.*U(Nx,J+1)&
              & +U(m2,J+2)-4.*U(m1,J+2)+3.*U(Nx,J+2))/dxyyy4
      elseif ( J==1 ) then
!     x=xb,y=yc
         Ux3y = (-9.*U(m4,1)+42.*U(m3,1)-72.*U(m2,1)+54.*U(m1,1)-15.*U(Nx,1)+12.*U(m4,2)-56.*U(m3,2)+96.*U(m2,2)-72.*U(m1,2)       &
              & +20.*U(Nx,2)-3.*U(m4,3)+14.*U(m3,3)-24.*U(m2,3)+18.*U(m1,3)-5.*U(Nx,3))/dxxxy4
         Uxy3 = (-5.*U(m2,1)+20.*U(m1,1)-15.*U(Nx,1)+18.*U(m2,2)-72.*U(m1,2)+54.*U(Nx,2)-24.*U(m2,3)+96.*U(m1,3)-72.*U(Nx,3)       &
              & +14.*U(m2,4)-56.*U(m1,4)+42.*U(Nx,4)-3.*U(m2,5)+12.*U(m1,5)-9.*U(Nx,5))/dxyyy4
         Ux2y2 = (-2.*U(m3,1)+8.*U(m2,1)-10.*U(m1,1)+4.*U(Nx,1)+5.*U(m3,2)-20.*U(m2,2)+25.*U(m1,2)-10.*U(Nx,2)-4.*U(m3,3)          &
               & +16.*U(m2,3)-20.*U(m1,3)+8.*U(Nx,3)+U(m3,4)-4.*U(m2,4)+5.*U(m1,4)-2.*U(Nx,4))/dxxyy
      elseif ( J==2 ) then
!     x=xb,y=yc+dly
         Ux3y = (-3.*U(m4,1)+14.*U(m3,1)-24.*U(m2,1)+18.*U(m1,1)-5.*U(Nx,1)+3.*U(m4,3)-14.*U(m3,3)+24.*U(m2,3)-18.*U(m1,3)         &
              & +5.*U(Nx,3))/dxxxy4
         Uxy3 = (-3.*U(m2,1)+12.*U(m1,1)-9.*U(Nx,1)+10.*U(m2,2)-40.*U(m1,2)+30.*U(Nx,2)-12.*U(m2,3)+48.*U(m1,3)-36.*U(Nx,3)        &
              & +6.*U(m2,4)-24.*U(m1,4)+18.*U(Nx,4)-U(m2,5)+4.*U(m1,5)-3.*U(Nx,5))/dxyyy4
      elseif ( J==Ny-1 ) then
!     x=xb,y=yd-dly
         Ux3y = (-3.*U(m4,n2)+14.*U(m3,n2)-24.*U(m2,n2)+18.*U(m1,n2)-5.*U(Nx,n2)+3.*U(m4,Ny)-14.*U(m3,Ny)+24.*U(m2,Ny)-18.*U(m1,Ny)&
              & +5.*U(Nx,Ny))/dxxxy4
         Uxy3 = (U(m2,n4)-4.*U(m1,n4)+3.*U(Nx,n4)-6.*U(m2,n3)+24.*U(m1,n3)-18.*U(Nx,n3)+12.*U(m2,n2)-48.*U(m1,n2)+36.*U(Nx,n2)     &
              & -10.*U(m2,n1)+40.*U(m1,n1)-30.*U(Nx,n1)+3.*U(m2,Ny)-12.*U(m1,Ny)+9.*U(Nx,Ny))/dxyyy4
      elseif ( J==Ny ) then
!     x=xb,y=yd
         Ux3y = (3.*U(m4,n2)-14.*U(m3,n2)+24.*U(m2,n2)-18.*U(m1,n2)+5.*U(Nx,n2)-12.*U(m4,n1)+56.*U(m3,n1)-96.*U(m2,n1)+72.*U(m1,n1)&
              & -20.*U(Nx,n1)+9.*U(m4,Ny)-42.*U(m3,Ny)+72.*U(m2,Ny)-54.*U(m1,Ny)+15.*U(Nx,Ny))/dxxxy4
         Uxy3 = (3.*U(m2,n4)-12.*U(m1,n4)+9.*U(Nx,n4)-14.*U(m2,n3)+56.*U(m1,n3)-42.*U(Nx,n3)+24.*U(m2,n2)-96.*U(m1,n2)+72.*U(Nx,n2)&
              & -18.*U(m2,n1)+72.*U(m1,n1)-54.*U(Nx,n1)+5.*U(m2,Ny)-20.*U(m1,Ny)+15.*U(Nx,Ny))/dxyyy4
         Ux2y2 = (U(m3,n3)-4.*U(m2,n3)+5.*U(m1,n3)-2.*U(Nx,n3)-4.*U(m3,n2)+16.*U(m2,n2)-20.*U(m1,n2)+8.*U(Nx,n2)+5.0*U(m3,n1)      &
               & -20.*U(m2,n1)+25.*U(m1,n1)-10.*U(Nx,n1)-2.*U(m3,Ny)+8.*U(m2,Ny)-10.*U(m1,Ny)+4.*U(Nx,Ny))/dxxyy
      endif
 
   endif
 
end subroutine pde2cr
!*==PDE3.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
subroutine pde3(Nx,Ny,Nz,U,I,J,K,Ux3,Ux4,Uy3,Uy4,Uz3,Uz4,Nxa,Nyc,Nze)
!
!     estimate third and fourth partial derivatives in x,y,z
!
   use c_pde3com
   use s_p3de2
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer :: Nx
   integer :: Ny
   integer , intent(in) :: Nz
   real , dimension(Nx,Ny,Nz) :: U
   integer :: I
   integer :: J
   integer , intent(in) :: K
   real :: Ux3
   real :: Ux4
   real :: Uy3
   real :: Uy4
   real , intent(out) :: Uz3
   real , intent(out) :: Uz4
   integer :: Nxa
   integer :: Nyc
   integer , intent(in) :: Nze
!
! End of declarations rewritten by SPAG
!
!
!     x,y partial derivatives
!
   call p3de2(Nx,Ny,U(1,1,K),I,J,Ux3,Ux4,Uy3,Uy4,Nxa,Nyc)
!
!     z partial derivatives
!
   if ( Nze/=0 ) then
!
!     nonperiodic in z
!
      if ( K>2 .and. K<Nz-1 ) then
         Uz3 = (-U(I,J,K-2)+2.0*U(I,J,K-1)-2.0*U(I,J,K+1)+U(I,J,K+2))/tdlz3
         Uz4 = (U(I,J,K-2)-4.0*U(I,J,K-1)+6.0*U(I,J,K)-4.0*U(I,J,K+1)+U(I,J,K+2))/dlz4
      elseif ( K==1 ) then
         Uz3 = (-5.0*U(I,J,1)+18.0*U(I,J,2)-24.0*U(I,J,3)+14.0*U(I,J,4)-3.0*U(I,J,5))/tdlz3
         Uz4 = (3.0*U(I,J,1)-14.0*U(I,J,2)+26.0*U(I,J,3)-24.0*U(I,J,4)+11.0*U(I,J,5)-2.0*U(I,J,6))/dlz4
      elseif ( K==2 ) then
         Uz3 = (-3.0*U(I,J,1)+10.0*U(I,J,2)-12.0*U(I,J,3)+6.0*U(I,J,4)-U(I,J,5))/tdlz3
         Uz4 = (2.0*U(I,J,1)-9.0*U(I,J,2)+16.0*U(I,J,3)-14.0*U(I,J,4)+6.0*U(I,J,5)-U(I,J,6))/dlz4
      elseif ( K==Nz-1 ) then
         Uz3 = (U(I,J,Nz-4)-6.0*U(I,J,Nz-3)+12.0*U(I,J,Nz-2)-10.0*U(I,J,Nz-1)+3.0*U(I,J,Nz))/tdlz3
         Uz4 = (-U(I,J,Nz-5)+6.0*U(I,J,Nz-4)-14.0*U(I,J,Nz-3)+16.0*U(I,J,Nz-2)-9.0*U(I,J,Nz-1)+2.0*U(I,J,Nz))/dlz4
      elseif ( K==Nz ) then
         Uz3 = (3.0*U(I,J,Nz-4)-14.0*U(I,J,Nz-3)+24.0*U(I,J,Nz-2)-18.0*U(I,J,Nz-1)+5.0*U(I,J,Nz))/tdlz3
         Uz4 = (-2.0*U(I,J,Nz-5)+11.0*U(I,J,Nz-4)-24.0*U(I,J,Nz-3)+26.0*U(I,J,Nz-2)-14.0*U(I,J,Nz-1)+3.0*U(I,J,Nz))/dlz4
      endif
!
!     periodic in z so use symmetric formula even "near" z boundaies
!
   elseif ( K>2 .and. K<Nz-1 ) then
      Uz3 = (-U(I,J,K-2)+2.0*U(I,J,K-1)-2.0*U(I,J,K+1)+U(I,J,K+2))/tdlz3
      Uz4 = (U(I,J,K-2)-4.0*U(I,J,K-1)+6.0*U(I,J,K)-4.0*U(I,J,K+1)+U(I,J,K+2))/dlz4
   elseif ( K==1 ) then
      Uz3 = (-U(I,J,Nz-2)+2.0*U(I,J,Nz-1)-2.0*U(I,J,2)+U(I,J,3))/tdlz3
      Uz4 = (U(I,J,Nz-2)-4.0*U(I,J,Nz-1)+6.0*U(I,J,1)-4.0*U(I,J,2)+U(I,J,3))/dlz4
   elseif ( K==2 ) then
      Uz3 = (-U(I,J,Nz-1)+2.0*U(I,J,1)-2.0*U(I,J,3)+U(I,J,4))/(tdlz3)
      Uz4 = (U(I,J,Nz-1)-4.0*U(I,J,1)+6.0*U(I,J,2)-4.0*U(I,J,3)+U(I,J,4))/dlz4
   elseif ( K==Nz-1 ) then
      Uz3 = (-U(I,J,Nz-3)+2.0*U(I,J,Nz-2)-2.0*U(I,J,1)+U(I,J,2))/tdlz3
      Uz4 = (U(I,J,Nz-3)-4.0*U(I,J,Nz-2)+6.0*U(I,J,Nz-1)-4.0*U(I,J,1)+U(I,J,2))/dlz4
   elseif ( K==Nz ) then
      Uz3 = (-U(I,J,Nz-2)+2.0*U(I,J,Nz-1)-2.0*U(I,J,2)+U(I,J,3))/tdlz3
      Uz4 = (U(I,J,Nz-2)-4.0*U(I,J,Nz-1)+6.0*U(I,J,Nz)-4.0*U(I,J,2)+U(I,J,3))/dlz4
   endif
end subroutine pde3
!*==P3DE2.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine p3de2(Nx,Ny,U,I,J,Ux3,Ux4,Uy3,Uy4,Nxa,Nyc)
!
!     third and fourth partial derivatives in x and y
!
   use c_pde3com
   use s_p3de1
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer :: Nx
   integer , intent(in) :: Ny
   real , dimension(Nx,Ny) :: U
   integer :: I
   integer , intent(in) :: J
   real :: Ux3
   real :: Ux4
   real , intent(out) :: Uy3
   real , intent(out) :: Uy4
   integer :: Nxa
   integer , intent(in) :: Nyc
!
! Local variable declarations rewritten by SPAG
!
   integer :: l
!
! End of declarations rewritten by SPAG
!
   l = Ny
!
!     x partial derivatives
!
   call p3de1(Nx,U(1,J),I,Ux3,Ux4,Nxa)
!
!     y partial derivatives
!
   if ( Nyc/=0 ) then
!
!     not periodic in y
!
      if ( J>2 .and. J<Ny-1 ) then
         Uy3 = (-U(I,J-2)+2.0*U(I,J-1)-2.0*U(I,J+1)+U(I,J+2))/tdly3
         Uy4 = (U(I,J-2)-4.0*U(I,J-1)+6.0*U(I,J)-4.0*U(I,J+1)+U(I,J+2))/dly4
      elseif ( J==1 ) then
         Uy3 = (-5.0*U(I,1)+18.0*U(I,2)-24.0*U(I,3)+14.0*U(I,4)-3.0*U(I,5))/tdly3
         Uy4 = (3.0*U(I,1)-14.0*U(I,2)+26.0*U(I,3)-24.0*U(I,4)+11.0*U(I,5)-2.0*U(I,6))/dly4
      elseif ( J==2 ) then
         Uy3 = (-3.0*U(I,1)+10.0*U(I,2)-12.0*U(I,3)+6.0*U(I,4)-U(I,5))/tdly3
         Uy4 = (2.0*U(I,1)-9.0*U(I,2)+16.0*U(I,3)-14.0*U(I,4)+6.0*U(I,5)-U(I,6))/dly4
      elseif ( J==Ny-1 ) then
         Uy3 = (U(I,l-4)-6.0*U(I,l-3)+12.0*U(I,l-2)-10.0*U(I,l-1)+3.0*U(I,l))/tdly3
         Uy4 = (-U(I,l-5)+6.0*U(I,l-4)-14.0*U(I,l-3)+16.0*U(I,l-2)-9.0*U(I,l-1)+2.0*U(I,l))/dly4
      elseif ( J==Ny ) then
         Uy3 = (3.0*U(I,l-4)-14.0*U(I,l-3)+24.0*U(I,l-2)-18.0*U(I,l-1)+5.0*U(I,l))/tdly3
         Uy4 = (-2.0*U(I,l-5)+11.0*U(I,l-4)-24.0*U(I,l-3)+26.0*U(I,l-2)-14.0*U(I,l-1)+3.0*U(I,l))/dly4
      endif
!
!     periodic in y
!
   elseif ( J>2 .and. J<Ny-1 ) then
      Uy3 = (-U(I,J-2)+2.0*U(I,J-1)-2.0*U(I,J+1)+U(I,J+2))/tdly3
      Uy4 = (U(I,J-2)-4.0*U(I,J-1)+6.0*U(I,J)-4.0*U(I,J+1)+U(I,J+2))/dly4
   elseif ( J==1 ) then
      Uy3 = (-U(I,l-2)+2.0*U(I,l-1)-2.0*U(I,2)+U(I,3))/tdly3
      Uy4 = (U(I,l-2)-4.0*U(I,l-1)+6.0*U(I,1)-4.0*U(I,2)+U(I,3))/dly4
   elseif ( J==2 ) then
      Uy3 = (-U(I,l-1)+2.0*U(I,1)-2.0*U(I,3)+U(I,4))/(tdly3)
      Uy4 = (U(I,l-1)-4.0*U(I,1)+6.0*U(I,2)-4.0*U(I,3)+U(I,4))/dly4
   elseif ( J==Ny-1 ) then
      Uy3 = (-U(I,l-3)+2.0*U(I,l-2)-2.0*U(I,1)+U(I,2))/tdly3
      Uy4 = (U(I,l-3)-4.0*U(I,l-2)+6.0*U(I,l-1)-4.0*U(I,1)+U(I,2))/dly4
   elseif ( J==Ny ) then
      Uy3 = (-U(I,l-2)+2.0*U(I,l-1)-2.0*U(I,2)+U(I,3))/tdly3
      Uy4 = (U(I,l-2)-4.0*U(I,l-1)+6.0*U(I,l)-4.0*U(I,2)+U(I,3))/dly4
   endif
end subroutine p3de2
!*==P3DE1.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine p3de1(Nx,U,I,Ux3,Ux4,Nxa)
!
!     third and fourth derivatives in x
!
   use c_pde3com
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Nx
   real , intent(in) , dimension(Nx) :: U
   integer , intent(in) :: I
   real , intent(out) :: Ux3
   real , intent(out) :: Ux4
   integer , intent(in) :: Nxa
!
! Local variable declarations rewritten by SPAG
!
   integer :: k
!
! End of declarations rewritten by SPAG
!
   k = Nx
   if ( Nxa/=0 ) then
!
!     nonperiodic in x
!
      if ( I>2 .and. I<Nx-1 ) then
         Ux3 = (-U(I-2)+2.0*U(I-1)-2.0*U(I+1)+U(I+2))/tdlx3
         Ux4 = (U(I-2)-4.0*U(I-1)+6.0*U(I)-4.0*U(I+1)+U(I+2))/dlx4
      elseif ( I==1 ) then
         Ux3 = (-5.0*U(1)+18.0*U(2)-24.0*U(3)+14.0*U(4)-3.0*U(5))/tdlx3
         Ux4 = (3.0*U(1)-14.0*U(2)+26.0*U(3)-24.0*U(4)+11.0*U(5)-2.0*U(6))/dlx4
      elseif ( I==2 ) then
         Ux3 = (-3.0*U(1)+10.0*U(2)-12.0*U(3)+6.0*U(4)-U(5))/tdlx3
         Ux4 = (2.0*U(1)-9.0*U(2)+16.0*U(3)-14.0*U(4)+6.0*U(5)-U(6))/dlx4
      elseif ( I==Nx-1 ) then
         Ux3 = (U(k-4)-6.0*U(k-3)+12.0*U(k-2)-10.0*U(k-1)+3.0*U(k))/tdlx3
         Ux4 = (-U(k-5)+6.0*U(k-4)-14.0*U(k-3)+16.0*U(k-2)-9.0*U(k-1)+2.0*U(k))/dlx4
      elseif ( I==Nx ) then
         Ux3 = (3.0*U(k-4)-14.0*U(k-3)+24.0*U(k-2)-18.0*U(k-1)+5.0*U(k))/tdlx3
         Ux4 = (-2.0*U(k-5)+11.0*U(k-4)-24.0*U(k-3)+26.0*U(k-2)-14.0*U(k-1)+3.0*U(k))/dlx4
      endif
!
!     periodic in x
!
   elseif ( I>2 .and. I<Nx-1 ) then
      Ux3 = (-U(I-2)+2.0*U(I-1)-2.0*U(I+1)+U(I+2))/tdlx3
      Ux4 = (U(I-2)-4.0*U(I-1)+6.0*U(I)-4.0*U(I+1)+U(I+2))/dlx4
   elseif ( I==1 ) then
      Ux3 = (-U(k-2)+2.0*U(k-1)-2.0*U(2)+U(3))/tdlx3
      Ux4 = (U(k-2)-4.0*U(k-1)+6.0*U(1)-4.0*U(2)+U(3))/dlx4
   elseif ( I==2 ) then
      Ux3 = (-U(k-1)+2.0*U(1)-2.0*U(3)+U(4))/(tdlx3)
      Ux4 = (U(k-1)-4.0*U(1)+6.0*U(2)-4.0*U(3)+U(4))/dlx4
   elseif ( I==Nx-1 ) then
      Ux3 = (-U(k-3)+2.0*U(k-2)-2.0*U(1)+U(2))/tdlx3
      Ux4 = (U(k-3)-4.0*U(k-2)+6.0*U(k-1)-4.0*U(1)+U(2))/dlx4
   elseif ( I==Nx ) then
      Ux3 = (-U(k-2)+2.0*U(k-1)-2.0*U(2)+U(3))/tdlx3
      Ux4 = (U(k-2)-4.0*U(k-1)+6.0*U(k)-4.0*U(2)+U(3))/dlx4
   endif
end subroutine p3de1
!*==FACTRI.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
!
!
!     factri and factrip are:
!     subroutines called by any real mudpack solver which uses line
!     relaxation(s) within multigrid iteration.  these subroutines do
!     a vectorized factorization of m simultaneous tridiagonal systems
!     of order n arising from nonperiodic or periodic discretizations
!
subroutine factri(M,N,A,B,C)
!
!     factor the m simultaneous tridiagonal systems of order n
!
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: M
   integer , intent(in) :: N
   real , intent(inout) , dimension(N,M) :: A
   real , intent(inout) , dimension(N,M) :: B
   real , intent(in) , dimension(N,M) :: C
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , j
!
! End of declarations rewritten by SPAG
!
   do i = 2 , N
      do j = 1 , M
         A(i-1,j) = A(i-1,j)/B(i-1,j)
         B(i,j) = B(i,j) - A(i-1,j)*C(i-1,j)
      enddo
   enddo
end subroutine factri
!*==FACTRP.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine factrp(M,N,A,B,C,D,E,Sum)
!
!     factor the m simultaneous "tridiagonal" systems of order n
!     from discretized periodic system (leave out periodic n point)
!     (so sweeps below only go from i=1,2,...,n-1) n > 3 is necessary
!
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: M
   integer , intent(in) :: N
   real , intent(inout) , dimension(N,M) :: A
   real , intent(inout) , dimension(N,M) :: B
   real , intent(in) , dimension(N,M) :: C
   real , intent(inout) , dimension(N,M) :: D
   real , intent(inout) , dimension(N,M) :: E
   real , intent(inout) , dimension(M) :: Sum
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , j
!
! End of declarations rewritten by SPAG
!
   do j = 1 , M
      D(1,j) = A(1,j)
   enddo
   do i = 2 , N - 2
      do j = 1 , M
         A(i,j) = A(i,j)/B(i-1,j)
         B(i,j) = B(i,j) - A(i,j)*C(i-1,j)
         D(i,j) = -A(i,j)*D(i-1,j)
      enddo
   enddo
!
!     correct computation of last d element
!
   do j = 1 , M
      D(N-2,j) = C(N-2,j) + D(N-2,j)
   enddo
   do j = 1 , M
      E(1,j) = C(N-1,j)/B(1,j)
   enddo
   do i = 2 , N - 3
      do j = 1 , M
         E(i,j) = -E(i-1,j)*C(i-1,j)/B(i,j)
      enddo
   enddo
   do j = 1 , M
      E(N-2,j) = (A(N-1,j)-E(N-3,j)*C(N-3,j))/B(N-2,j)
   enddo
!
!     compute  inner product (e,d) for each j in sum(j)
!
   do j = 1 , M
      Sum(j) = 0.
   enddo
   do i = 1 , N - 2
      do j = 1 , M
         Sum(j) = Sum(j) + E(i,j)*D(i,j)
      enddo
   enddo
!
!     set last diagonal element
!
   do j = 1 , M
      B(N-1,j) = B(N-1,j) - Sum(j)
   enddo
end subroutine factrp
!*==TRANSP.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine transp(N,Amat)
!
!     transpose n by n real matrix
!
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: N
   real , intent(inout) , dimension(N,N) :: Amat
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , j
   real :: temp
!
! End of declarations rewritten by SPAG
!
   do i = 1 , N - 1
      do j = i + 1 , N
         temp = Amat(i,j)
         Amat(i,j) = Amat(j,i)
         Amat(j,i) = temp
      enddo
   enddo
end subroutine transp
!*==SGFA.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine sgfa(A,Lda,N,Ipvt,Info)
   use s_isfmax
   use s_sscl
   use s_sxpy
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Lda
   real , intent(inout) , dimension(Lda,1) :: A
   integer , intent(in) :: N
   integer , intent(out) , dimension(1) :: Ipvt
   integer , intent(out) :: Info
!
! Local variable declarations rewritten by SPAG
!
   integer :: j , k , kp1 , l , nm1
   real :: t
!
! End of declarations rewritten by SPAG
!
   Info = 0
   nm1 = N - 1
   if ( nm1>=1 ) then
      do k = 1 , nm1
         kp1 = k + 1
         l = isfmax(N-k+1,A(k,k),1) + k - 1
         Ipvt(k) = l
         if ( A(l,k)==0.0E0 ) then
            Info = k
         else
            if ( l/=k ) then
               t = A(l,k)
               A(l,k) = A(k,k)
               A(k,k) = t
            endif
            t = -1.0E0/A(k,k)
            call sscl(N-k,t,A(k+1,k),1)
            do j = kp1 , N
               t = A(l,j)
               if ( l/=k ) then
                  A(l,j) = A(k,j)
                  A(k,j) = t
               endif
               call sxpy(N-k,t,A(k+1,k),1,A(k+1,j),1)
            enddo
         endif
      enddo
   endif
   Ipvt(N) = N
   if ( A(N,N)==0.0E0 ) Info = N
end subroutine sgfa
!*==SGSL.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine sgsl(A,Lda,N,Ipvt,B,Job)
   use s_sdt
   use s_sxpy
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: Lda
   real , dimension(Lda,1) :: A
   integer , intent(in) :: N
   integer , intent(in) , dimension(1) :: Ipvt
   real , intent(inout) , dimension(1) :: B
   integer , intent(in) :: Job
!
! Local variable declarations rewritten by SPAG
!
   integer :: k , kb , l , nm1
   real :: t
!
! End of declarations rewritten by SPAG
!
   nm1 = N - 1
   if ( Job/=0 ) then
      do k = 1 , N
         t = sdt(k-1,A(1,k),1,B(1),1)
         B(k) = (B(k)-t)/A(k,k)
      enddo
      if ( nm1>=1 ) then
         do kb = 1 , nm1
            k = N - kb
            B(k) = B(k) + sdt(N-k,A(k+1,k),1,B(k+1),1)
            l = Ipvt(k)
            if ( l/=k ) then
               t = B(l)
               B(l) = B(k)
               B(k) = t
            endif
         enddo
      endif
   else
      if ( nm1>=1 ) then
         do k = 1 , nm1
            l = Ipvt(k)
            t = B(l)
            if ( l/=k ) then
               B(l) = B(k)
               B(k) = t
            endif
            call sxpy(N-k,t,A(k+1,k),1,B(k+1),1)
         enddo
      endif
      do kb = 1 , N
         k = N + 1 - kb
         B(k) = B(k)/A(k,k)
         t = -B(k)
         call sxpy(k-1,t,A(1,k),1,B(1),1)
      enddo
   endif
end subroutine sgsl
!*==SDT.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
function sdt(N,Sx,Incx,Sy,Incy) result(sdt2)
   implicit none
!
! Function and Dummy argument declarations rewritten by SPAG
!
   real :: sdt2
   integer , intent(in) :: N
   real , intent(in) , dimension(1) :: Sx
   integer , intent(in) :: Incx
   real , intent(in) , dimension(1) :: Sy
   integer , intent(in) :: Incy
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ix , iy , m , mp1
   real :: stemp
!
! End of declarations rewritten by SPAG
!
   stemp = 0.0E0
   sdt2 = 0.0E0
   if ( N<=0 ) return
   if ( Incx==1 .and. Incy==1 ) then
      m = mod(N,5)
      if ( m/=0 ) then
         do i = 1 , m
            stemp = stemp + Sx(i)*Sy(i)
         enddo
         if ( N<5 ) then
            sdt2 = stemp
            return
         endif
      endif
      mp1 = m + 1
      do i = mp1 , N , 5
         stemp = stemp + Sx(i)*Sy(i) + Sx(i+1)*Sy(i+1) + Sx(i+2)*Sy(i+2) + Sx(i+3)*Sy(i+3) + Sx(i+4)*Sy(i+4)
      enddo
      sdt2 = stemp
   else
      ix = 1
      iy = 1
      if ( Incx<0 ) ix = (-N+1)*Incx + 1
      if ( Incy<0 ) iy = (-N+1)*Incy + 1
      do i = 1 , N
         stemp = stemp + Sx(ix)*Sy(iy)
         ix = ix + Incx
         iy = iy + Incy
      enddo
      sdt2 = stemp
      return
   endif
end function sdt
!*==ISFMAX.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
function isfmax(N,Sx,Incx) result(isfmax2)
   implicit none
!
! Function and Dummy argument declarations rewritten by SPAG
!
   integer :: isfmax2
   integer , intent(in) :: N
   real , intent(in) , dimension(1) :: Sx
   integer , intent(in) :: Incx
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ix
   real :: smax
!
! End of declarations rewritten by SPAG
!
   isfmax2 = 0
   if ( N<1 ) return
   isfmax2 = 1
   if ( N==1 ) return
   if ( Incx==1 ) then
      smax = abs(Sx(1))
      do i = 2 , N
         if ( abs(Sx(i))>smax ) then
            isfmax2 = i
            smax = abs(Sx(i))
         endif
      enddo
      return
   endif
   ix = 1
   smax = abs(Sx(1))
   ix = ix + Incx
   do i = 2 , N
      if ( abs(Sx(ix))>smax ) then
         isfmax2 = i
         smax = abs(Sx(ix))
      endif
      ix = ix + Incx
   enddo
   return
end function isfmax
!*==SXPY.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine sxpy(N,Sa,Sx,Incx,Sy,Incy)
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: N
   real , intent(in) :: Sa
   real , intent(in) , dimension(1) :: Sx
   integer , intent(in) :: Incx
   real , intent(inout) , dimension(1) :: Sy
   integer , intent(in) :: Incy
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , ix , iy , m , mp1
!
! End of declarations rewritten by SPAG
!
   if ( N<=0 ) return
   if ( Sa==0.0 ) return
   if ( Incx==1 .and. Incy==1 ) then
      m = mod(N,4)
      if ( m/=0 ) then
         do i = 1 , m
            Sy(i) = Sy(i) + Sa*Sx(i)
         enddo
         if ( N<4 ) return
      endif
      mp1 = m + 1
      do i = mp1 , N , 4
         Sy(i) = Sy(i) + Sa*Sx(i)
         Sy(i+1) = Sy(i+1) + Sa*Sx(i+1)
         Sy(i+2) = Sy(i+2) + Sa*Sx(i+2)
         Sy(i+3) = Sy(i+3) + Sa*Sx(i+3)
      enddo
   else
      ix = 1
      iy = 1
      if ( Incx<0 ) ix = (-N+1)*Incx + 1
      if ( Incy<0 ) iy = (-N+1)*Incy + 1
      do i = 1 , N
         Sy(iy) = Sy(iy) + Sa*Sx(ix)
         ix = ix + Incx
         iy = iy + Incy
      enddo
      return
   endif
end subroutine sxpy
!*==SSCL.f90 processed by SPAG 8.04DB 19:24  7 May 2025
!!SPAG Open source Personal, Educational or Academic User National Technical University  NON-COMMERCIAL USE - Not for use on proprietary or closed source code
 
subroutine sscl(N,Sa,Sx,Incx)
   implicit none
!
! Dummy argument declarations rewritten by SPAG
!
   integer , intent(in) :: N
   real , intent(in) :: Sa
   real , intent(inout) , dimension(1) :: Sx
   integer , intent(in) :: Incx
!
! Local variable declarations rewritten by SPAG
!
   integer :: i , m , mp1 , nincx
!
! End of declarations rewritten by SPAG
!
   if ( N<=0 ) return
   if ( Incx==1 ) then
      m = mod(N,5)
      if ( m/=0 ) then
         do i = 1 , m
            Sx(i) = Sa*Sx(i)
         enddo
         if ( N<5 ) return
      endif
      mp1 = m + 1
      do i = mp1 , N , 5
         Sx(i) = Sa*Sx(i)
         Sx(i+1) = Sa*Sx(i+1)
         Sx(i+2) = Sa*Sx(i+2)
         Sx(i+3) = Sa*Sx(i+3)
         Sx(i+4) = Sa*Sx(i+4)
      enddo
   else
      nincx = N*Incx
      do i = 1 , nincx , Incx
         Sx(i) = Sa*Sx(i)
      enddo
      return
   endif
end subroutine sscl
