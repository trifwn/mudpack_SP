!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_MUD3SP
   INTERFACE
      SUBROUTINE mud3sp(Iparm, Fparm, Work, cfx, cfy, cfz, bndyc, Rhs, Phi, Mgopt, Ierror)
         USE C_FMUD3SP
         USE C_IMUD3SP
         USE C_MUD3SPC
         IMPLICIT NONE
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
      END SUBROUTINE MUD3SP
   END INTERFACE
END MODULE S_MUD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_MUD3SP1
   INTERFACE
      SUBROUTINE mud3sp1(Nx, Ny, Nz, Rhsf, Phif, cfx, cfy, cfz, bndyc, Wk)
         USE C_FMUD3SP
         USE C_IMUD3SP
         USE C_MUD3SPC
         IMPLICIT NONE
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
      END SUBROUTINE MUD3SP1
   END INTERFACE
END MODULE S_MUD3SP1
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_KCYMD3SP
   INTERFACE
      SUBROUTINE kcymd3sp(Wk)
         USE C_IMUD3SP
         USE C_MUD3SPC
         IMPLICIT NONE
         REAL, DIMENSION(*) :: Wk
      END SUBROUTINE KCYMD3SP
   END INTERFACE
END MODULE S_KCYMD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_RESMD3SP
   INTERFACE
      SUBROUTINE resmd3sp(Nx, Ny, Nz, Phi, Rhs, Cofx, Cofy, Cofz, Ncx, Ncy, Ncz, Phic, Rhsc, Resf)
         USE C_IMUD3SP
         IMPLICIT NONE
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
      END SUBROUTINE RESMD3SP
   END INTERFACE
END MODULE S_RESMD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_DISMD3SP
   INTERFACE
      SUBROUTINE dismd3sp(Nx, Ny, Nz, Cofx, Cofy, Cofz, bndyc, cfx, cfy, cfz, Ier)
         USE C_FMUD3SP
         USE C_IMUD3SP
         USE C_MUD3SPC
         IMPLICIT NONE
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
      END SUBROUTINE DISMD3SP
   END INTERFACE
END MODULE S_DISMD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_ADJMD3SP
   INTERFACE
      SUBROUTINE adjmd3sp(Nx, Ny, Nz, Phi, Rhs, bndyc, cfx, cfy, cfz)
         USE C_FMUD3SP
         USE C_IMUD3SP
         USE C_MUD3SPC
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: Nx
         INTEGER, INTENT(IN) :: Ny
         INTEGER, INTENT(IN) :: Nz
         REAL, INTENT(IN), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Phi
         REAL, INTENT(INOUT), DIMENSION(Nx, Ny, Nz) :: Rhs
         EXTERNAL bndyc
         EXTERNAL cfx
         EXTERNAL cfy
         EXTERNAL cfz
      END SUBROUTINE ADJMD3SP
   END INTERFACE
END MODULE S_ADJMD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_RELMD3SP
   INTERFACE
      SUBROUTINE relmd3sp(Wk)
         USE C_FMUD3SP
         USE C_IMUD3SP
         USE C_MUD3SPC
         IMPLICIT NONE
         REAL, DIMENSION(*) :: Wk
      END SUBROUTINE RELMD3SP
   END INTERFACE
END MODULE S_RELMD3SP
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_RELMD3SPP
   INTERFACE
      SUBROUTINE relmd3spp(Nx, Ny, Nz, Phi, Rhs, Cofx, Cofy, Cofz)
         USE C_IMUD3SP
         IMPLICIT NONE
         INTEGER :: Nx
         INTEGER :: Ny
         INTEGER :: Nz
         REAL, INTENT(INOUT), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Phi
         REAL, INTENT(IN), DIMENSION(Nx, Ny, Nz) :: Rhs
         REAL, INTENT(IN), DIMENSION(Nx, 3) :: Cofx
         REAL, INTENT(IN), DIMENSION(Ny, 3) :: Cofy
         REAL, INTENT(IN), DIMENSION(Nz, 3) :: Cofz
      END SUBROUTINE RELMD3SPP
   END INTERFACE
END MODULE S_RELMD3SPP
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_SWK2
   INTERFACE
      SUBROUTINE swk2(Nfx, Nfy, Phif, Rhsf, Phi, Rhs)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: Nfx
         INTEGER, INTENT(IN) :: Nfy
         REAL, INTENT(IN), DIMENSION(Nfx, Nfy) :: Phif
         REAL, INTENT(IN), DIMENSION(Nfx, Nfy) :: Rhsf
         REAL, INTENT(OUT), DIMENSION(0:Nfx + 1, 0:Nfy + 1) :: Phi
         REAL, INTENT(OUT), DIMENSION(Nfx, Nfy) :: Rhs
      END SUBROUTINE SWK2
   END INTERFACE
END MODULE S_SWK2
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_TRSFC2
   INTERFACE
      SUBROUTINE trsfc2(Nx, Ny, Phi, Rhs, Ncx, Ncy, Phic, Rhsc)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: Nx
         INTEGER, INTENT(IN) :: Ny
         INTEGER, INTENT(IN) :: Ncx
         INTEGER, INTENT(IN) :: Ncy
         REAL, INTENT(IN), DIMENSION(0:Nx + 1, 0:Ny + 1) :: Phi
         REAL, INTENT(IN), DIMENSION(Nx, Ny) :: Rhs
         REAL, INTENT(OUT), DIMENSION(0:Ncx + 1, 0:Ncy + 1) :: Phic
         REAL, INTENT(OUT), DIMENSION(Ncx, Ncy) :: Rhsc
      END SUBROUTINE TRSFC2
   END INTERFACE
END MODULE S_TRSFC2
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_RES2
   INTERFACE
      SUBROUTINE res2(Nx, Ny, Resf, Ncx, Ncy, Rhsc, Nxa, Nxb, Nyc, Nyd)
         IMPLICIT NONE
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
      END SUBROUTINE RES2
   END INTERFACE
END MODULE S_RES2
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_PROLON2
   INTERFACE
      SUBROUTINE prolon2(Ncx, Ncy, P, Nx, Ny, Q, Nxa, Nxb, Nyc, Nyd, Intpol)
         IMPLICIT NONE
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
      END SUBROUTINE PROLON2
   END INTERFACE
END MODULE S_PROLON2
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_PROLON1
   INTERFACE
      SUBROUTINE prolon1(Ncx, P, Nx, Q, Nxa, Nxb, Intpol)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: Ncx
         INTEGER, INTENT(IN) :: Nx
         REAL, INTENT(IN), DIMENSION(0:Ncx + 1) :: P
         REAL, INTENT(INOUT), DIMENSION(0:Nx + 1) :: Q
         INTEGER, INTENT(IN) :: Nxa
         INTEGER, INTENT(IN) :: Nxb
         INTEGER, INTENT(IN) :: Intpol
      END SUBROUTINE PROLON1
   END INTERFACE
END MODULE S_PROLON1
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_COR2
   INTERFACE
      SUBROUTINE cor2(Nx, Ny, Phif, Ncx, Ncy, Phic, Nxa, Nxb, Nyc, Nyd, Intpol, Phcor)
         IMPLICIT NONE
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
      END SUBROUTINE COR2
   END INTERFACE
END MODULE S_COR2
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_PDE2
   INTERFACE
      SUBROUTINE pde2(Nx, Ny, U, I, J, Ux3, Ux4, Uy3, Uy4, Nxa, Nyc)
         USE C_PDE2COM
         IMPLICIT NONE
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
      END SUBROUTINE PDE2
   END INTERFACE
END MODULE S_PDE2
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_SWK3
   INTERFACE
      SUBROUTINE swk3(Nfx, Nfy, Nfz, Phif, Rhsf, Phi, Rhs)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: Nfx
         INTEGER, INTENT(IN) :: Nfy
         INTEGER, INTENT(IN) :: Nfz
         REAL, INTENT(IN), DIMENSION(Nfx, Nfy, Nfz) :: Phif
         REAL, INTENT(IN), DIMENSION(Nfx, Nfy, Nfz) :: Rhsf
         REAL, INTENT(OUT), DIMENSION(0:Nfx + 1, 0:Nfy + 1, 0:Nfz + 1) :: Phi
         REAL, INTENT(OUT), DIMENSION(Nfx, Nfy, Nfz) :: Rhs
      END SUBROUTINE SWK3
   END INTERFACE
END MODULE S_SWK3
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_TRSFC3
   INTERFACE
      SUBROUTINE trsfc3(Nx, Ny, Nz, Phi, Rhs, Ncx, Ncy, Ncz, Phic, Rhsc)
         IMPLICIT NONE
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
      END SUBROUTINE TRSFC3
   END INTERFACE
END MODULE S_TRSFC3
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_RES3
   INTERFACE
      SUBROUTINE res3(Nx, Ny, Nz, Resf, Ncx, Ncy, Ncz, Rhsc, Nxa, Nxb, Nyc, Nyd, Nze, Nzf)
         IMPLICIT NONE
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
      END SUBROUTINE RES3
   END INTERFACE
END MODULE S_RES3
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_PROLON3
   INTERFACE
      SUBROUTINE prolon3(Ncx, Ncy, Ncz, P, Nx, Ny, Nz, Q, Nxa, Nxb, Nyc, Nyd, Nze, Nzf, Intpol)
         IMPLICIT NONE
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
      END SUBROUTINE PROLON3
   END INTERFACE
END MODULE S_PROLON3
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_COR3
   INTERFACE
      SUBROUTINE cor3(Nx, Ny, Nz, Phif, Ncx, Ncy, Ncz, Phic, Nxa, Nxb, Nyc, Nyd, Nze, Nzf, Intpol, Phcor)
         IMPLICIT NONE
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
      END SUBROUTINE COR3
   END INTERFACE
END MODULE S_COR3
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_PER3VB
   INTERFACE
      SUBROUTINE per3vb(Nx, Ny, Nz, Phi, Nxa, Nyc, Nze)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: Nx
         INTEGER, INTENT(IN) :: Ny
         INTEGER, INTENT(IN) :: Nz
         REAL, INTENT(INOUT), DIMENSION(0:Nx + 1, 0:Ny + 1, 0:Nz + 1) :: Phi
         INTEGER, INTENT(IN) :: Nxa
         INTEGER, INTENT(IN) :: Nyc
         INTEGER, INTENT(IN) :: Nze
      END SUBROUTINE PER3VB
   END INTERFACE
END MODULE S_PER3VB
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_PDE2CR
   INTERFACE
      SUBROUTINE pde2cr(Nx, Ny, U, I, J, Ux3y, Uxy3, Ux2y2)
         USE C_COM2DCR
         USE C_FMUD2CR
         USE C_IMUD2CR
         USE C_PDE2COM
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: Nx
         INTEGER, INTENT(IN) :: Ny
         REAL, INTENT(IN), DIMENSION(Nx, Ny) :: U
         INTEGER, INTENT(IN) :: I
         INTEGER, INTENT(IN) :: J
         REAL, INTENT(OUT) :: Ux3y
         REAL, INTENT(OUT) :: Uxy3
         REAL, INTENT(OUT) :: Ux2y2
      END SUBROUTINE PDE2CR
   END INTERFACE
END MODULE S_PDE2CR
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_PDE3
   INTERFACE
      SUBROUTINE pde3(Nx, Ny, Nz, U, I, J, K, Ux3, Ux4, Uy3, Uy4, Uz3, Uz4, Nxa, Nyc, Nze)
         USE C_PDE3COM
         IMPLICIT NONE
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
      END SUBROUTINE PDE3
   END INTERFACE
END MODULE S_PDE3
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_P3DE2
   INTERFACE
      SUBROUTINE p3de2(Nx, Ny, U, I, J, Ux3, Ux4, Uy3, Uy4, Nxa, Nyc)
         USE C_PDE3COM
         IMPLICIT NONE
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
      END SUBROUTINE P3DE2
   END INTERFACE
END MODULE S_P3DE2
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_P3DE1
   INTERFACE
      SUBROUTINE p3de1(Nx, U, I, Ux3, Ux4, Nxa)
         USE C_PDE3COM
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: Nx
         REAL, INTENT(IN), DIMENSION(Nx) :: U
         INTEGER, INTENT(IN) :: I
         REAL, INTENT(OUT) :: Ux3
         REAL, INTENT(OUT) :: Ux4
         INTEGER, INTENT(IN) :: Nxa
      END SUBROUTINE P3DE1
   END INTERFACE
END MODULE S_P3DE1
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_FACTRI
   INTERFACE
      SUBROUTINE factri(M, N, A, B, C)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: M
         INTEGER, INTENT(IN) :: N
         REAL, INTENT(INOUT), DIMENSION(N, M) :: A
         REAL, INTENT(INOUT), DIMENSION(N, M) :: B
         REAL, INTENT(IN), DIMENSION(N, M) :: C
      END SUBROUTINE FACTRI
   END INTERFACE
END MODULE S_FACTRI
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_FACTRP
   INTERFACE
      SUBROUTINE factrp(M, N, A, B, C, D, E, Sum)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: M
         INTEGER, INTENT(IN) :: N
         REAL, INTENT(INOUT), DIMENSION(N, M) :: A
         REAL, INTENT(INOUT), DIMENSION(N, M) :: B
         REAL, INTENT(IN), DIMENSION(N, M) :: C
         REAL, INTENT(INOUT), DIMENSION(N, M) :: D
         REAL, INTENT(INOUT), DIMENSION(N, M) :: E
         REAL, INTENT(INOUT), DIMENSION(M) :: Sum
      END SUBROUTINE FACTRP
   END INTERFACE
END MODULE S_FACTRP
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_TRANSP
   INTERFACE
      SUBROUTINE transp(N, Amat)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         REAL, INTENT(INOUT), DIMENSION(N, N) :: Amat
      END SUBROUTINE TRANSP
   END INTERFACE
END MODULE S_TRANSP
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_SGFA
   INTERFACE
      SUBROUTINE sgfa(A, Lda, N, Ipvt, Info)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: Lda
         REAL, INTENT(INOUT), DIMENSION(Lda, 1) :: A
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(OUT), DIMENSION(1) :: Ipvt
         INTEGER, INTENT(OUT) :: Info
      END SUBROUTINE SGFA
   END INTERFACE
END MODULE S_SGFA
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_SGSL
   INTERFACE
      SUBROUTINE sgsl(A, Lda, N, Ipvt, B, Job)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: Lda
         REAL, DIMENSION(Lda, 1) :: A
         INTEGER, INTENT(IN) :: N
         INTEGER, INTENT(IN), DIMENSION(1) :: Ipvt
         REAL, INTENT(INOUT), DIMENSION(1) :: B
         INTEGER, INTENT(IN) :: Job
      END SUBROUTINE SGSL
   END INTERFACE
END MODULE S_SGSL
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_SDT
   INTERFACE
      FUNCTION sdt(N, Sx, Incx, Sy, Incy)
         IMPLICIT NONE
         REAL :: sdt
         INTEGER, INTENT(IN) :: N
         REAL, INTENT(IN), DIMENSION(1) :: Sx
         INTEGER, INTENT(IN) :: Incx
         REAL, INTENT(IN), DIMENSION(1) :: Sy
         INTEGER, INTENT(IN) :: Incy
      END FUNCTION SDT
   END INTERFACE
END MODULE S_SDT
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_ISFMAX
   INTERFACE
      FUNCTION isfmax(N, Sx, Incx)
         IMPLICIT NONE
         INTEGER :: isfmax
         INTEGER, INTENT(IN) :: N
         REAL, INTENT(IN), DIMENSION(1) :: Sx
         INTEGER, INTENT(IN) :: Incx
      END FUNCTION ISFMAX
   END INTERFACE
END MODULE S_ISFMAX
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_SXPY
   INTERFACE
      SUBROUTINE sxpy(N, Sa, Sx, Incx, Sy, Incy)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         REAL, INTENT(IN) :: Sa
         REAL, INTENT(IN), DIMENSION(1) :: Sx
         INTEGER, INTENT(IN) :: Incx
         REAL, INTENT(INOUT), DIMENSION(1) :: Sy
         INTEGER, INTENT(IN) :: Incy
      END SUBROUTINE SXPY
   END INTERFACE
END MODULE S_SXPY
!*==intfaces.f90  created by SPAG 8.04DB at 16:49 on  8 May 2025
MODULE S_SSCL
   INTERFACE
      SUBROUTINE sscl(N, Sa, Sx, Incx)
         IMPLICIT NONE
         INTEGER, INTENT(IN) :: N
         REAL, INTENT(IN) :: Sa
         REAL, INTENT(INOUT), DIMENSION(1) :: Sx
         INTEGER, INTENT(IN) :: Incx
      END SUBROUTINE SSCL
   END INTERFACE
END MODULE S_SSCL
