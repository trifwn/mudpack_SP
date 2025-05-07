!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_MUD3SP
   INTERFACE
      subroutine mud3sp(Iparm,Fparm,Work,cfx,cfy,cfz,bndyc,Rhs,Phi,Mgopt,Ierror)
         use c_fmud3sp
         use c_imud3sp
         use c_mud3spc
         implicit none
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
      END SUBROUTINE MUD3SP
   END INTERFACE
END MODULE S_MUD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_MUD3SP1
   INTERFACE
      subroutine mud3sp1(Nx,Ny,Nz,Rhsf,Phif,cfx,cfy,cfz,bndyc,Wk)
         use c_fmud3sp
         use c_imud3sp
         use c_mud3spc
         implicit none
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
      END SUBROUTINE MUD3SP1
   END INTERFACE
END MODULE S_MUD3SP1
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_KCYMD3SP
   INTERFACE
      subroutine kcymd3sp(Wk)
         use c_imud3sp
         use c_mud3spc
         implicit none
         real , dimension(*) :: Wk
      END SUBROUTINE KCYMD3SP
   END INTERFACE
END MODULE S_KCYMD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_RESMD3SP
   INTERFACE
      subroutine resmd3sp(Nx,Ny,Nz,Phi,Rhs,Cofx,Cofy,Cofz,Ncx,Ncy,Ncz,Phic,Rhsc,Resf)
         use c_imud3sp
         implicit none
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
      END SUBROUTINE RESMD3SP
   END INTERFACE
END MODULE S_RESMD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_DISMD3SP
   INTERFACE
      subroutine dismd3sp(Nx,Ny,Nz,Cofx,Cofy,Cofz,bndyc,cfx,cfy,cfz,Ier)
         use c_fmud3sp
         use c_imud3sp
         use c_mud3spc
         implicit none
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
      END SUBROUTINE DISMD3SP
   END INTERFACE
END MODULE S_DISMD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_ADJMD3SP
   INTERFACE
      subroutine adjmd3sp(Nx,Ny,Nz,Phi,Rhs,bndyc,cfx,cfy,cfz)
         use c_fmud3sp
         use c_imud3sp
         use c_mud3spc
         implicit none
         integer , intent(in) :: Nx
         integer , intent(in) :: Ny
         integer , intent(in) :: Nz
         real , intent(in) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Phi
         real , intent(inout) , dimension(Nx,Ny,Nz) :: Rhs
         external bndyc
         external cfx
         external cfy
         external cfz
      END SUBROUTINE ADJMD3SP
   END INTERFACE
END MODULE S_ADJMD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_RELMD3SP
   INTERFACE
      subroutine relmd3sp(Wk)
         use c_fmud3sp
         use c_imud3sp
         use c_mud3spc
         implicit none
         real , dimension(*) :: Wk
      END SUBROUTINE RELMD3SP
   END INTERFACE
END MODULE S_RELMD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_RELMD3SPP
   INTERFACE
      subroutine relmd3spp(Nx,Ny,Nz,Phi,Rhs,Cofx,Cofy,Cofz)
         use c_imud3sp
         implicit none
         integer :: Nx
         integer :: Ny
         integer :: Nz
         real , intent(inout) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Phi
         real , intent(in) , dimension(Nx,Ny,Nz) :: Rhs
         real , intent(in) , dimension(Nx,3) :: Cofx
         real , intent(in) , dimension(Ny,3) :: Cofy
         real , intent(in) , dimension(Nz,3) :: Cofz
      END SUBROUTINE RELMD3SPP
   END INTERFACE
END MODULE S_RELMD3SPP
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_SWK2
   INTERFACE
      subroutine swk2(Nfx,Nfy,Phif,Rhsf,Phi,Rhs)
         implicit none
         integer , intent(in) :: Nfx
         integer , intent(in) :: Nfy
         real , intent(in) , dimension(Nfx,Nfy) :: Phif
         real , intent(in) , dimension(Nfx,Nfy) :: Rhsf
         real , intent(out) , dimension(0:Nfx+1,0:Nfy+1) :: Phi
         real , intent(out) , dimension(Nfx,Nfy) :: Rhs
      END SUBROUTINE SWK2
   END INTERFACE
END MODULE S_SWK2
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_TRSFC2
   INTERFACE
      subroutine trsfc2(Nx,Ny,Phi,Rhs,Ncx,Ncy,Phic,Rhsc)
         implicit none
         integer , intent(in) :: Nx
         integer , intent(in) :: Ny
         integer , intent(in) :: Ncx
         integer , intent(in) :: Ncy
         real , intent(in) , dimension(0:Nx+1,0:Ny+1) :: Phi
         real , intent(in) , dimension(Nx,Ny) :: Rhs
         real , intent(out) , dimension(0:Ncx+1,0:Ncy+1) :: Phic
         real , intent(out) , dimension(Ncx,Ncy) :: Rhsc
      END SUBROUTINE TRSFC2
   END INTERFACE
END MODULE S_TRSFC2
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_RES2
   INTERFACE
      subroutine res2(Nx,Ny,Resf,Ncx,Ncy,Rhsc,Nxa,Nxb,Nyc,Nyd)
         implicit none
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
      END SUBROUTINE RES2
   END INTERFACE
END MODULE S_RES2
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_PROLON2
   INTERFACE
      subroutine prolon2(Ncx,Ncy,P,Nx,Ny,Q,Nxa,Nxb,Nyc,Nyd,Intpol)
         implicit none
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
      END SUBROUTINE PROLON2
   END INTERFACE
END MODULE S_PROLON2
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_PROLON1
   INTERFACE
      subroutine prolon1(Ncx,P,Nx,Q,Nxa,Nxb,Intpol)
         implicit none
         integer , intent(in) :: Ncx
         integer , intent(in) :: Nx
         real , intent(in) , dimension(0:Ncx+1) :: P
         real , intent(inout) , dimension(0:Nx+1) :: Q
         integer , intent(in) :: Nxa
         integer , intent(in) :: Nxb
         integer , intent(in) :: Intpol
      END SUBROUTINE PROLON1
   END INTERFACE
END MODULE S_PROLON1
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_COR2
   INTERFACE
      subroutine cor2(Nx,Ny,Phif,Ncx,Ncy,Phic,Nxa,Nxb,Nyc,Nyd,Intpol,Phcor)
         implicit none
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
      END SUBROUTINE COR2
   END INTERFACE
END MODULE S_COR2
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_PDE2
   INTERFACE
      subroutine pde2(Nx,Ny,U,I,J,Ux3,Ux4,Uy3,Uy4,Nxa,Nyc)
         use c_pde2com
         implicit none
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
      END SUBROUTINE PDE2
   END INTERFACE
END MODULE S_PDE2
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_SWK3
   INTERFACE
      subroutine swk3(Nfx,Nfy,Nfz,Phif,Rhsf,Phi,Rhs)
         implicit none
         integer , intent(in) :: Nfx
         integer , intent(in) :: Nfy
         integer , intent(in) :: Nfz
         real , intent(in) , dimension(Nfx,Nfy,Nfz) :: Phif
         real , intent(in) , dimension(Nfx,Nfy,Nfz) :: Rhsf
         real , intent(out) , dimension(0:Nfx+1,0:Nfy+1,0:Nfz+1) :: Phi
         real , intent(out) , dimension(Nfx,Nfy,Nfz) :: Rhs
      END SUBROUTINE SWK3
   END INTERFACE
END MODULE S_SWK3
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_TRSFC3
   INTERFACE
      subroutine trsfc3(Nx,Ny,Nz,Phi,Rhs,Ncx,Ncy,Ncz,Phic,Rhsc)
         implicit none
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
      END SUBROUTINE TRSFC3
   END INTERFACE
END MODULE S_TRSFC3
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_RES3
   INTERFACE
      subroutine res3(Nx,Ny,Nz,Resf,Ncx,Ncy,Ncz,Rhsc,Nxa,Nxb,Nyc,Nyd,Nze,Nzf)
         implicit none
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
      END SUBROUTINE RES3
   END INTERFACE
END MODULE S_RES3
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_PROLON3
   INTERFACE
      subroutine prolon3(Ncx,Ncy,Ncz,P,Nx,Ny,Nz,Q,Nxa,Nxb,Nyc,Nyd,Nze,Nzf,Intpol)
         implicit none
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
      END SUBROUTINE PROLON3
   END INTERFACE
END MODULE S_PROLON3
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_COR3
   INTERFACE
      subroutine cor3(Nx,Ny,Nz,Phif,Ncx,Ncy,Ncz,Phic,Nxa,Nxb,Nyc,Nyd,Nze,Nzf,Intpol,Phcor)
         implicit none
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
      END SUBROUTINE COR3
   END INTERFACE
END MODULE S_COR3
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_PER3VB
   INTERFACE
      subroutine per3vb(Nx,Ny,Nz,Phi,Nxa,Nyc,Nze)
         implicit none
         integer , intent(in) :: Nx
         integer , intent(in) :: Ny
         integer , intent(in) :: Nz
         real , intent(inout) , dimension(0:Nx+1,0:Ny+1,0:Nz+1) :: Phi
         integer , intent(in) :: Nxa
         integer , intent(in) :: Nyc
         integer , intent(in) :: Nze
      END SUBROUTINE PER3VB
   END INTERFACE
END MODULE S_PER3VB
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_PDE2CR
   INTERFACE
      subroutine pde2cr(Nx,Ny,U,I,J,Ux3y,Uxy3,Ux2y2)
         use c_com2dcr
         use c_fmud2cr
         use c_imud2cr
         use c_pde2com
         implicit none
         integer , intent(in) :: Nx
         integer , intent(in) :: Ny
         real , intent(in) , dimension(Nx,Ny) :: U
         integer , intent(in) :: I
         integer , intent(in) :: J
         real , intent(out) :: Ux3y
         real , intent(out) :: Uxy3
         real , intent(out) :: Ux2y2
      END SUBROUTINE PDE2CR
   END INTERFACE
END MODULE S_PDE2CR
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_PDE3
   INTERFACE
      subroutine pde3(Nx,Ny,Nz,U,I,J,K,Ux3,Ux4,Uy3,Uy4,Uz3,Uz4,Nxa,Nyc,Nze)
         use c_pde3com
         implicit none
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
      END SUBROUTINE PDE3
   END INTERFACE
END MODULE S_PDE3
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_P3DE2
   INTERFACE
      subroutine p3de2(Nx,Ny,U,I,J,Ux3,Ux4,Uy3,Uy4,Nxa,Nyc)
         use c_pde3com
         implicit none
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
      END SUBROUTINE P3DE2
   END INTERFACE
END MODULE S_P3DE2
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_P3DE1
   INTERFACE
      subroutine p3de1(Nx,U,I,Ux3,Ux4,Nxa)
         use c_pde3com
         implicit none
         integer , intent(in) :: Nx
         real , intent(in) , dimension(Nx) :: U
         integer , intent(in) :: I
         real , intent(out) :: Ux3
         real , intent(out) :: Ux4
         integer , intent(in) :: Nxa
      END SUBROUTINE P3DE1
   END INTERFACE
END MODULE S_P3DE1
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_FACTRI
   INTERFACE
      subroutine factri(M,N,A,B,C)
         implicit none
         integer , intent(in) :: M
         integer , intent(in) :: N
         real , intent(inout) , dimension(N,M) :: A
         real , intent(inout) , dimension(N,M) :: B
         real , intent(in) , dimension(N,M) :: C
      END SUBROUTINE FACTRI
   END INTERFACE
END MODULE S_FACTRI
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_FACTRP
   INTERFACE
      subroutine factrp(M,N,A,B,C,D,E,Sum)
         implicit none
         integer , intent(in) :: M
         integer , intent(in) :: N
         real , intent(inout) , dimension(N,M) :: A
         real , intent(inout) , dimension(N,M) :: B
         real , intent(in) , dimension(N,M) :: C
         real , intent(inout) , dimension(N,M) :: D
         real , intent(inout) , dimension(N,M) :: E
         real , intent(inout) , dimension(M) :: Sum
      END SUBROUTINE FACTRP
   END INTERFACE
END MODULE S_FACTRP
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_TRANSP
   INTERFACE
      subroutine transp(N,Amat)
         implicit none
         integer , intent(in) :: N
         real , intent(inout) , dimension(N,N) :: Amat
      END SUBROUTINE TRANSP
   END INTERFACE
END MODULE S_TRANSP
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_SGFA
   INTERFACE
      subroutine sgfa(A,Lda,N,Ipvt,Info)
         implicit none
         integer , intent(in) :: Lda
         real , intent(inout) , dimension(Lda,1) :: A
         integer , intent(in) :: N
         integer , intent(out) , dimension(1) :: Ipvt
         integer , intent(out) :: Info
      END SUBROUTINE SGFA
   END INTERFACE
END MODULE S_SGFA
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_SGSL
   INTERFACE
      subroutine sgsl(A,Lda,N,Ipvt,B,Job)
         implicit none
         integer , intent(in) :: Lda
         real , dimension(Lda,1) :: A
         integer , intent(in) :: N
         integer , intent(in) , dimension(1) :: Ipvt
         real , intent(inout) , dimension(1) :: B
         integer , intent(in) :: Job
      END SUBROUTINE SGSL
   END INTERFACE
END MODULE S_SGSL
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_SDT
   INTERFACE
      function sdt(N,Sx,Incx,Sy,Incy) result(sdt2)
         implicit none
         real :: sdt2
         integer , intent(in) :: N
         real , intent(in) , dimension(1) :: Sx
         integer , intent(in) :: Incx
         real , intent(in) , dimension(1) :: Sy
         integer , intent(in) :: Incy
      END FUNCTION SDT
   END INTERFACE
END MODULE S_SDT
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_ISFMAX
   INTERFACE
      function isfmax(N,Sx,Incx) result(isfmax2)
         implicit none
         integer :: isfmax2
         integer , intent(in) :: N
         real , intent(in) , dimension(1) :: Sx
         integer , intent(in) :: Incx
      END FUNCTION ISFMAX
   END INTERFACE
END MODULE S_ISFMAX
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_SXPY
   INTERFACE
      subroutine sxpy(N,Sa,Sx,Incx,Sy,Incy)
         implicit none
         integer , intent(in) :: N
         real , intent(in) :: Sa
         real , intent(in) , dimension(1) :: Sx
         integer , intent(in) :: Incx
         real , intent(inout) , dimension(1) :: Sy
         integer , intent(in) :: Incy
      END SUBROUTINE SXPY
   END INTERFACE
END MODULE S_SXPY
!*==intfaces.f90  created by SPAG 8.04DB at 19:24 on  7 May 2025
MODULE S_SSCL
   INTERFACE
      subroutine sscl(N,Sa,Sx,Incx)
         implicit none
         integer , intent(in) :: N
         real , intent(in) :: Sa
         real , intent(inout) , dimension(1) :: Sx
         integer , intent(in) :: Incx
      END SUBROUTINE SSCL
   END INTERFACE
END MODULE S_SSCL
