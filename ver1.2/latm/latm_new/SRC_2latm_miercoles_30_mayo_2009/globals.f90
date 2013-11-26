MODULE globals
  USE inparams
  IMPLICIT NONE
  
  INTEGER,  ALLOCATABLE :: ind_Trans(:,:)        ! array to index transitions
  REAL(DP), ALLOCATABLE :: transition_Energy(:,:)! transition energies
  LOGICAL,  ALLOCATABLE :: includeTetrahedron(:) ! choose which tetrahedron to use
  REAL(DP), ALLOCATABLE :: tetrahedronVolume(:)  ! relative volume of each tetrahed
  LOGICAL,  ALLOCATABLE :: includeTransition(:)  ! choose which transitions to use
  INTEGER,  ALLOCATABLE :: tetCorner(:,:)        ! tetrahedron corner indices
  REAL(DP), ALLOCATABLE :: f(:,:)                ! integrand
  
CONTAINS
  SUBROUTINE Allocate_Global_Arrays
    IMPLICIT NONE
    
!    SELECT CASE (crystal_class)
!    CASE('znbl')
!    CASE('wrtz')
!    CASE DEFAULT
!       STOP 'crystal_class wrong'
!    END SELECT
    
    WRITE(6,*) 'Spin factor is', spin_factor
    
    ALLOCATE(transition_Energy(kMax,nMax*nMax))
!    ALLOCATE(includeTetrahedron(nTetra))
!    ALLOCATE(tetrahedronVolume(nTetra))
!    ALLOCATE(tetCorner(nTetra,4))
    ALLOCATE(f(kMax,nMax*nMax))
    ALLOCATE(ind_Trans(nMax,nMax))               ! array to index transitions
    ALLOCATE(includeTransition(nMax*nMax))
    ALLOCATE(energy_Out(energy_Steps))
    
  END SUBROUTINE Allocate_Global_Arrays
END MODULE globals
