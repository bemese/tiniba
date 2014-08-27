MODULE structFile
  USE Global, ONLY : debug
  USE Global, ONLY : PI
  USE CommandLineArguments, ONLY : printSymmetries
!!!
!!! This module provides structReader which parses the case.struct file.
!!!
!!! The module also provides the subroutine bravai subroutines that
!!! provides the lattice vectors.
!!!
  IMPLICIT NONE
  
  DOUBLE PRECISION :: a1, a2, a3, alpha, beta, gamma
  CHARACTER( LEN=4 ) :: latticeLabel
  DOUBLE PRECISION :: rbas(3,3), rbasTranspose(3,3), rbasTransposeInverse(3,3)
!!! rbas : translation vectors
  DOUBLE PRECISION :: gbas(3,3)
!!! gbas : reciprocal lattice vectors
  
  INTEGER :: ibrava
  
!!! The symmetry Matrices
!!! SymMatsConv - The symmetry matrices in the conventional basis
!!! SymMatsCt - The symmetry matrices in the Cartesian basis
!!! SymMatsPl - The symmetry matrices in the direct lattice basis
!!! SymMatsRl - The symmetry matrices in the reciprocal lattice basis
  INTEGER, ALLOCATABLE :: symMatsConv(:,:,:)
  DOUBLE PRECISION, ALLOCATABLE :: symMatsCt(:,:,:)
  INTEGER, ALLOCATABLE :: symMatsPL(:,:,:)
  INTEGER, ALLOCATABLE :: symMatsRl(:,:,:)
  INTEGER :: iord
  
!!! nat - Number of AToms
  INTEGER :: nat
  
!!! ortho describes whether the crystal structure is orthogonal
!!! or not.
  LOGICAL :: ortho
  
!!! The WIEN case
  CHARACTER( LEN=80 ) :: case
  
CONTAINS
  
  SUBROUTINE structReader
!!!!!    USE 3by3matrices, ONLY : invert3by3
!!!    Modified from init.f found in SRC_irrep
    IMPLICIT NONE
    
    CHARACTER( LEN=120 ) :: filename
!!!    CHARACTER( LEN=80 ) :: case
    CHARACTER( LEN=80 ) :: title
    CHARACTER( LEN=10 ) :: ana
    INTEGER :: iat
!!!    INTEGER :: ntype
    INTEGER :: ipgr
    INTEGER, ALLOCATABLE :: iz(:,:,:)
    INTEGER :: itype, index
    DOUBLE PRECISION :: pos(3)
    DOUBLE PRECISION, ALLOCATABLE :: tau (:,:)
    INTEGER, ALLOCATABLE :: natom(:)
    INTEGER, ALLOCATABLE :: nlo(:), nlov(:), nlon(:)
    INTEGER :: jrj
    DOUBLE PRECISION :: ro, rmt
    INTEGER :: noiord
    
    INTEGER :: i, ina, i1, i2
    
    IF ( debug ) WRITE(*,'(A)') "Program Flow: Entered structREADER"
    
!!!    filename = "Si100c.struct"
    
    WRITE(*,*) "Enter case name (the working directory): "
    READ(*,*) case
    WRITE(*,*) "Case is: ", TRIM(case)
    
    filename = TRIM(case)//'.struct'
    IF (debug) WRITE(*,*) "(File Control) Opening file: ", filename
    
    OPEN( UNIT=20, FILE=filename, STATUS="OLD")
    
    READ( 20, 530 ) title
530 FORMAT( A80 )
    
    IF ( debug ) WRITE(*,'(2A)') "(structReader) Title: ", TRIM(title)
    
    READ( 20, 541 ) latticeLabel, nat, ipgr
541 FORMAT( A4,23X,I3,I3 )
    
    IF ( debug ) WRITE(*,'(2A)') "(structReader) latticeLabel: ", latticeLabel
    IF ( debug ) WRITE(*,'(A20,I4)') "(structReader) nat: ", nat
    
    ALLOCATE( natom(nat), nlo(nat), nlov(nat), nlon(nat) )
    
    READ( 20, 530 )
    
    READ( 20, 580 ) a1, a2, a3, alpha, beta, gamma
580 FORMAT( 6F10.7, 10X, F9.6 )
    
    IF ( debug ) THEN
       WRITE(*,'(A,F16.8)') "(structReader) a1: ", a1
       WRITE(*,'(A,F16.8)') "(structReader) a2: ", a2
       WRITE(*,'(A,F16.8)') "(structReader) a3: ", a3
       WRITE(*,'(A,F16.8)') "(structReader) alpha: ", alpha
       WRITE(*,'(A,F16.8)') "(structReader) beta: ", beta
       WRITE(*,'(A,F16.8)') "(structReader) gamma: ", gamma
    END IF
    
    ! Change angles to radians
    IF (alpha.EQ.0.d0) THEN
       alpha = PI/2
    ELSE
       alpha = alpha * PI/180
    END IF
    IF (debug) WRITE(*,'(A,F16.8)') "(structReader) alpha changed to ", alpha
    
    IF (beta.EQ.0.d0) THEN
       beta = PI/2
    ELSE
       beta = beta * PI/180
    END IF
    IF (debug) WRITE(*,'(A,F16.8)') "(structReader) beta changed to ", beta
    
    IF (gamma.EQ.0.d0) THEN
       gamma = PI/2
    ELSE
       gamma = gamma * PI/180
    END IF
    IF (debug) WRITE(*,'(A,F16.8)') "(structReader) gamma changed to ", gamma
    
    index = 0
    DO iat = 1, nat
!!!       index = index + 1
       
       READ( 20, 543 ) ( pos(i), i=1,3 )
543    FORMAT ( 12X, F10.7, 3X, F10.7, 3X, F10.7 )
       
       IF (debug) WRITE(*,'(A,3F16.8)') "(structReader) pos(1:3): ", pos(1:3)
       
       READ( 20, 544 ) natom(iat)
544    FORMAT ( 15X, I2 )
       
       IF (debug) WRITE(*,'(A,I5)') "(structReader) natom(iat): ", natom(iat) 
       
       DO ina = 2, natom(iat)
!!!         index = index + 1
          
          READ( 20, 543 ) ( pos(i), i=1,3 )
          
          IF (debug) WRITE(*,'(A,3F16.8)') "(structReader) pos(1:3): ", pos(1:3)
          
       END DO
       
       READ( 20, 545 ) ana, jrj, ro, rmt
545    FORMAT ( A10, 5X, I5, 5X, F10.9, 5X, F10.8  )
       
       READ(20,*)
       READ(20,*)
       READ(20,*)
    END DO
    
    READ(20,*) iord
    
    IF (debug) WRITE(*,'(A,I5)') "(structReader) iord: ", iord
    
    WRITE(*,'(A44,I3)') "The number of symmetry matrices expected is ", iord
    
    ALLOCATE( iz(3,3,iord) )
    ALLOCATE( tau(3,iord) )
    
    DO i = 1, iord
       
       IF (debug) WRITE(*,'(A,I4)') "(structReader) i: ", i
       
       READ( 20, 512) ( ( iz( i1, i2, i), i2=1,3), tau(i1, i), i1=1,3 )
512    FORMAT ( 2(3I2, F10.5,/), (3I2, F10.5) ) 
       
       IF (debug) THEN
          WRITE(*,'(A,3I5)') "(structReader) iz(1,1:3,i): ", iz(1,1:3,i)
          WRITE(*,'(A,3I5)') "(structReader) iz(2,1:3,i): ", iz(2,1:3,i)
          WRITE(*,'(A,3I5)') "(structReader) iz(3,1:3,i): ", iz(3,1:3,i)
          WRITE(*,'(A,3F16.8)') "(structReader) tau(1:3,i): ", tau(1:3,i)
       END IF
       
       READ( 20, 514) noiord
514    FORMAT ( I8 )
       
       IF (debug) WRITE(*,'(A,I4)') "(structReader) noiord: ", noiord
       
    END DO
    
    IF ( noiord .NE. iord ) THEN
       WRITE(*,'(A)') "The variables noiord and iord shoudl be equal.  They are not!"
       WRITE(*,'(A)') "      STOPPING"
       STOP "The variables noiord and iord are not equal"
    END IF
    
    CALL Bravai
    ! Bravai creates rbas.  The primitive lattice vectors are in
    ! the rows of rbas.
    
    IF (debug) THEN
       WRITE(*,'(A,3F13.8)') "(structReader) rbas(1,1:3): ", rbas(1,1:3)
       WRITE(*,'(A,3F13.8)') "(structReader) rbas(2,1:3): ", rbas(2,1:3)
       WRITE(*,'(A,3F13.8)') "(structReader) rbas(3,1:3): ", rbas(3,1:3)
    END IF
    
    rbasTranspose = TRANSPOSE(rbas)
    
    CALL invert3by3 (rbas, gbas)
    rbasTransposeInverse = TRANSPOSE(gbas)
    gbas = 2.d0*PI*gbas
    
    IF (debug) THEN
       WRITE(*,'(A,3F13.8)') "(structReader) gbas(1:1,3): ", gbas(1,1:3)
       WRITE(*,'(A,3F13.8)') "(structReader) gbas(2:1,3): ", gbas(2,1:3)
       WRITE(*,'(A,3F13.8)') "(structReader) gbas(3:1,3): ", gbas(3,1:3)
    END IF
    
    CALL SymmetryMatrices(iz)
    
!!    WRITE(*,*) SymMatsRL
    
  END SUBROUTINE structReader
  
  SUBROUTINE Bravai
    IMPLICIT NONE
    
    IF (debug) WRITE(*,*) "Program Flow: Entered Bravai";
    
    SELECT CASE ( latticeLabel )
    CASE ( 'P   ' )
       ortho = .FALSE.
       IF ( ( ABS(alpha - 1.570796d0) .GT. 0.0001 ) .OR. &
            ( ABS(beta - 1.570796d0)  .GT. 0.0001 ) .OR. &
            ( ABS(gamma - 1.570796d0) .GT. 0.0001 ) ) THEN
          CALL SetupLatticePAngles ()
       ELSE
          ortho = .TRUE.
          CALL SetupLatticeP ()
       END IF
    CASE ( 'F   ' )
       ortho = .TRUE.
       CALL SetupLatticeF ()
    CASE ( 'B   ' )
       ortho = .TRUE.
       CALL SetupLatticeB ()
    CASE ( 'H   ' )
       ortho = .FALSE.
       CALL SetupLatticeH ()
    CASE ( 'CXY ' )
       IF ( ABS(gamma - 1.570796d0) .GT. 0.0001 ) THEN
          ortho = .FALSE.
          CALL SetupLatticeCAngles ()
       ELSE
          ortho = .TRUE.
          CALL SetupLatticeCXY ()
       END IF
    CASE ( 'CXZ ' )
       IF ( ABS(gamma - 1.570796d0) .GT. 0.0001 ) THEN
          ortho = .FALSE.
          CALL SetupLatticeCAngles ()
       ELSE
          ortho = .TRUE.
          CALL SetupLatticeCXZ ()
       END IF
    CASE ( 'CYZ ' )
       IF ( ABS(gamma - 1.570796d0) .GT. 0.0001 ) THEN
          ortho = .FALSE.
          CALL SetupLatticeCAngles ()
       ELSE
          ortho = .TRUE.
          CALL SetupLatticeCYZ ()
       END IF
    CASE ( 'R   ' )
       ortho = .FALSE.
       CALL SetupLatticeR ()
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  These cases may be implememnted in the future
!!!    CASE ( 'S   ' )
!!!       CALL SetupLatticeS ()
!!!    CASE ( 'MXZ ' )
!!!    CASE ( 'M   ' )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    CASE DEFAULT
       WRITE(*,*) "PROBLEM: Reached CASE DEFAULT in Bravai subroutine."
       WRITE(*,*) "    Case was ", latticeLabel
       WRITE(*,*) "    Will STOP"
       STOP "PROBLEM: Reached CASE DEFAULT in Bravai subroutine. "
    END SELECT
    
    IF (debug) WRITE(*,'(A,3F12.8)') "(Bravai) rbas(1,1:3): ", rbas(1,1:3)
    IF (debug) WRITE(*,'(A,3F12.8)') "(Bravai) rbas(2,1:3): ", rbas(2,1:3)
    IF (debug) WRITE(*,'(A,3F12.8)') "(Bravai) rbas(3,1:3): ", rbas(3,1:3)
    
  END SUBROUTINE BRAVAI
  
  SUBROUTINE SetupLatticePAngles()
    IMPLICIT NONE
    DOUBLE PRECISION :: cosg1, gamma0
    INTEGER :: iarb(3)
    
    IF (debug) WRITE(*,*) "Program Flow: Entered SetupLatticePAngles";
    
    cosg1 = COS(gamma) - COS(alpha)*COS(beta)
    cosg1 = cosg1 / SIN(alpha) / SIN(beta)
    gamma0 = ACOS(cosg1)
    
    rbas(1,1) = a1*SIN(gamma0)*SIN(beta)
    rbas(1,2) = a1*COS(gamma0)*SIN(beta)
    rbas(1,3) = a1*COS(beta)
    
    rbas(2,1) = 0.d0
    rbas(2,2) = a2*SIN(alpha)
    rbas(2,3) = a2*COS(alpha)
    
    rbas(3,1) = 0.d0
    rbas(3,2) = 0.d0
    rbas(3,3) = a3
    
    ibrava = 1
    
    iarb(1) = 0
    iarb(2) = 0
    iarb(3) = 0
  END SUBROUTINE SetupLatticePAngles
  
  SUBROUTINE SetupLatticeP()
    IMPLICIT NONE
    INTEGER :: iarb(3)
    
    IF (debug) WRITE(*,*) "Program Flow: Entered SetupLatticeP";
    
    rbas(1,1) = a1
    rbas(1,2) = 0.d0
    rbas(1,3) = 0.d0
    
    rbas(2,1) = 0.d0
    rbas(2,2) = a2
    rbas(2,3) = 0.d0
    
    rbas(3,1) = 0.d0
    rbas(3,2) = 0.d0
    rbas(3,3) = a3
    
    ibrava = 4
    
    iarb(1) = 0
    iarb(2) = 0
    iarb(3) = 0
  END SUBROUTINE SetupLatticeP
  
  SUBROUTINE SetupLatticeF()
    IMPLICIT NONE
    INTEGER :: iarb(3)
    DOUBLE PRECISION :: halfa1, halfa2, halfa3
    
    IF (debug) WRITE(*,*) "Program Flow: Entered SetupLatticeF";
    
    halfa1 = 0.5d0 * a1
    halfa2 = 0.5d0 * a2
    halfa3 = 0.5d0 * a3
    
    rbas(1,1) = 0.d0
    rbas(1,2) = halfa2
    rbas(1,3) = halfa3
    
    rbas(2,1) = halfa1
    rbas(2,2) = 0.d0
    rbas(2,3) = halfa3
    
    rbas(3,1) = halfa1
    rbas(3,2) = halfa2
    rbas(3,3) = 0.d0
    
    ibrava = 7
    
    iarb(1) = 0    
    iarb(2) = 0    
    iarb(3) = 0    
  END SUBROUTINE SetupLatticeF
  
  SUBROUTINE SetupLatticeB()
    IMPLICIT NONE
    INTEGER :: iarb(3)
    DOUBLE PRECISION :: halfa1, halfa2, halfa3
    
    IF (debug) WRITE(*,*) "Program Flow: Entered SetupLatticeB";
    
    halfa1 = 0.5d0 * a1
    halfa2 = 0.5d0 * a2
    halfa3 = 0.5d0 * a3    
    
    rbas(1,1) = -halfa1
    rbas(1,2) = halfa2
    rbas(1,3) = halfa3
    
    rbas(2,1) = halfa1
    rbas(2,2) = -halfa2
    rbas(2,3) = halfa3
    
    rbas(3,1) = halfa1
    rbas(3,2) = halfa2
    rbas(3,3) = -halfa3
    
    ibrava = 6
    
    iarb(1) = 1
    iarb(2) = 1
    iarb(3) = 1
  END SUBROUTINE SetupLatticeB
  
  SUBROUTINE SetupLatticeH()
    IMPLICIT NONE
    INTEGER :: iarb(3)
    
    IF (debug) WRITE(*,*) "Program Flow: Entered SetupLatticeH";
    
    rbas(1,1) = a1 * SQRT(0.75d0)
    rbas(1,2) = -0.5d0 * a1
    rbas(1,3) = 0.d0
    
    rbas(2,1) = 0.d0
    rbas(2,2) = a1
    rbas(2,3) = 0.d0
    
    rbas(3,1) = 0.d0
    rbas(3,2) = 0.d0
    rbas(3,3) = a3
    
    ibrava = 11
    
    iarb(1) = 1
    iarb(2) = 0
    iarb(3) = 0
  END SUBROUTINE SetupLatticeH
  
  SUBROUTINE SetupLatticeCAngles ()
    IMPLICIT NONE
    INTEGER :: iarb(3)
    DOUBLE PRECISION :: halfa1SINgamma, halfa1COSgamma
    
    IF (debug) WRITE(*,*) "Program Flow: Entered SetupLatticeCAngles";
    
    halfa1SINgamma = 0.5d0 * a1 * SIN (gamma)
    halfa1COSgamma = 0.5d0 * a1 * COS (gamma)
    
    rbas(1,1) = halfa1SINgamma
    rbas(1,2) = halfa1COSgamma
    rbas(1,3) =-a3
    
    rbas(2,1) = 0.d0
    rbas(2,2) = a2
    rbas(2,3) = 0.d0
    
    rbas(3,1) = halfa1SINgamma
    rbas(3,2) = halfa1COSgamma
    rbas(3,3) = a3
    
    ibrava = 3
    
    iarb(1) = 0
    iarb(2) = 1
    iarb(3) = 0
  END SUBROUTINE SetupLatticeCAngles
  
  SUBROUTINE SetupLatticeCXZ ()
    IMPLICIT NONE
    
    DOUBLE PRECISION :: halfa1, halfa3
    INTEGER :: iarb(3)
    
    halfa1 = 0.5d0 * a1
    halfa3 = 0.5d0 * a3
    
    rbas(1,1) = halfa1
    rbas(1,2) = 0.d0
    rbas(1,3) =-halfa3
    
    rbas(2,1) = 0.d0
    rbas(2,2) = a2
    rbas(2,3) = 0.d0
    
    rbas(3,1) = halfa1
    rbas(3,2) = 0.d0
    rbas(3,3) = halfa3
    
    ibrava = 5
    
    iarb(1) = 0
    iarb(2) = 1
    iarb(3) = 0
  END SUBROUTINE SetupLatticeCXZ
  
  SUBROUTINE SetupLatticeCYZ ()
    IMPLICIT NONE
    
    DOUBLE PRECISION :: halfa2, halfa3
    INTEGER :: iarb(3)
    
    halfa2 = 0.5d0 * a2
    halfa3 = 0.5d0 * a3
    
    rbas(1,1) = a1
    rbas(1,2) = 0.d0
    rbas(1,3) = 0.d0
    
    rbas(2,1) = 0.d0
    rbas(2,2) = halfa2
    rbas(2,3) =-halfa3
    
    rbas(3,1) = 0.d0
    rbas(3,2) = halfa2
    rbas(3,3) = halfa3
    
    ibrava = 5
    
    iarb(1) = 0
    iarb(2) = 0
    iarb(3) = 1
  END SUBROUTINE SetupLatticeCYZ
  
  SUBROUTINE SetupLatticeCXY ()
    IMPLICIT NONE
    
    DOUBLE PRECISION :: halfa1, halfa2
    INTEGER :: iarb(3)
    
    halfa1 = 0.5d0 * a1
    halfa2 = 0.5d0 * a2
    
    rbas(1,1) = halfa1
    rbas(1,2) =-halfa2
    rbas(1,3) = 0.d0
    
    rbas(2,1) = halfa1
    rbas(2,2) = halfa2
    rbas(2,3) = 0.d0
    
    rbas(3,1) = 0.d0
    rbas(3,2) = 0.d0
    rbas(3,3) = a3
    
    ibrava = 5
    
    iarb(1) = 1
    iarb(2) = 0
    iarb(3) = 0
  END SUBROUTINE SetupLatticeCXY
  
  SUBROUTINE SetupLatticeR ()
    IMPLICIT NONE
    
    INTEGER :: iarb(3)
    
    rbas(1,1) = 0.5d0 * a1 / SQRT(3.d0)
    rbas(1,2) =-0.5d0 * a1 
    rbas(1,3) = a3 / 3.d0
    
    rbas(2,1) = 0.5d0 * a1 / SQRT(3.d0)
    rbas(2,2) = 0.5d0 * a1
    rbas(2,3) = a3 / 3.d0
    
    rbas(3,1) =-a1 / SQRT(3.d0)
    rbas(3,2) = 0.d0
    rbas(3,3) = a3 / 3.d0
    
    ibrava = 10
    
    iarb(1) = 1
    iarb(2) = 1
    iarb(3) = 1
  END SUBROUTINE SetupLatticeR
  
  SUBROUTINE SymmetryMatrices(iz)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: iz(:,:,:)
    INTEGER :: iSym
    DOUBLE PRECISION :: tempMat(3,3), tempMat2(3,3)
    
    IF (debug) WRITE(*,*) "Program Flow: Entered SymmetryMatrices"
    
    ALLOCATE( symMatsConv(3,3,iord) )
    ALLOCATE( symMatsCt(3,3,iord) )
    ALLOCATE( symMatsPL(3,3,iord) )
    ALLOCATE( symMatsRL(3,3,iord) )
    
    symMatsCt(1:3,1:3,1:iord) = 0.d0
    symMatsConv(1:3,1:3,1:iord) = 0
    symMatsPL(1:3,1:3,1:iord) = 0
    symMatsRL(1:3,1:3,1:iord) = 0
    
!!! There are three cases.
!!! 1) Lattice is ortho.
!!! 2) Lattice is not ortho and CXZ.
!!! 3) Lattice is not ortho and not CXZ.
    
!!!  The matrices in iz are in the conventional basis
    symMatsConv = iz
    
    DO iSym=1,iord
       
       IF (debug) WRITE(*,*) "(SymmetryMatrices) iSym: ", iSym
       
       IF (ortho) THEN
          ! Symmetry matrices in Cartesian basis
          tempMat = REAL(symMatsConv(1:3,1:3,iSym))
          symMatsCt(1:3,1:3,iSym) = tempMat(1:3,1:3)
          
          ! Symmetry matrices in Primitive lattice basis
          tempMat2 = MATMUL(tempMat,rbasTranspose)
          tempMat = MATMUL(rbasTransposeInverse,tempMat2)
          symMatsPl(1:3,1:3,iSym) = NINT(tempMat(1:3,1:3))
          
       ELSE IF (.NOT.ortho) THEN
          IF (latticeLabel.eq.'CXZ') THEN
          ELSE
             ! Symmetry matrices in Primtive lattice basis
             symMatsPl(1:3,1:3,iSym) = symMatsConv(1:3,1:3,iSym)
             
             ! Symmetry matrices in Cartesian basis
             
             tempMat = REAL(symMatsPl(1:3,1:3,iSym))
             tempMat2 = MATMUL(tempMat,rbasTransposeInverse)
             symMatsCt(1:3,1:3,iSym) = MATMUL(rbasTranspose,tempMat2)
             
          END IF
       ELSE
          WRITE(6,*) "(SymmetryMatrices): Problem with variable ortho"
          WRITE(6,*) "STOPPING"
          STOP "Problem with variable ortho"
       END IF
       
       ! inverse Symmetry matrices in Reciprocial latice basis
       tempMat(1:3,1:3) = symMatsPl(1:3,1:3,iSym)
       tempMat2 = TRANSPOSE(tempMat)
       symMatsRl(1:3,1:3,iSym) = tempMat2(1:3,1:3)
       
    END DO
    
!!! Write out the matrices
    IF (printSymmetries) THEN
       OPEN(UNIT=1,FILE="IBZsymmetries")
       WRITE(1,*) "The symmetry matrices in various coordinate systems"
       WRITE(1,*) "===========  Conventional Basis ================="
       DO iSym=1,iord
          WRITE(1,*) "Symmetry ", iSym
          WRITE(1,*) symMatsConv(1,1:3,iSym)
          WRITE(1,*) symMatsConv(2,1:3,iSym)
          WRITE(1,*) symMatsConv(3,1:3,iSym)
       END DO
       WRITE(1,*) "===========  Cartesian Basis ================="
       DO iSym=1,iord
          WRITE(1,*) "Symmetry ", iSym
          WRITE(1,'(3F15.8)') symMatsCt(1,1:3,iSym)
          WRITE(1,'(3F15.8)') symMatsCt(2,1:3,iSym)
          WRITE(1,'(3F15.8)') symMatsCt(3,1:3,iSym)
       END DO
       WRITE(1,*) "===========  Direct Primitive Basis ================="
       DO iSym=1,iord
          WRITE(1,*) "Symmetry ", iSym
          WRITE(1,*) symMatsPl(1,1:3,iSym)
          WRITE(1,*) symMatsPl(2,1:3,iSym)
          WRITE(1,*) symMatsPl(3,1:3,iSym)
       END DO
       WRITE(1,*) "===========  Reciprocal Primitive Basis ================="
       WRITE(1,*) "NOTE: These are the symmetry operations satisfied by"
       WRITE(1,*) "       the energy eigenvalues in the BZ "
       DO iSym=1,iord
          WRITE(1,*) "Symmetry ", iSym
          WRITE(1,*) symMatsRl(1,1:3,iSym)
          WRITE(1,*) symMatsRl(2,1:3,iSym)
          WRITE(1,*) symMatsRl(3,1:3,iSym)
       END DO
       CLOSE(1)
    END IF
    
    IF (printSymmetries) THEN
       OPEN(UNIT=1, FILE="Symmetries.Cartesian")
       WRITE(1,*) iord
       DO iSym=1,iord
          WRITE(1,'(3F15.8)') symMatsCt(1,1:3,iSym)
          WRITE(1,'(3F15.8)') symMatsCt(2,1:3,iSym)
          WRITE(1,'(3F15.8)') symMatsCt(3,1:3,iSym)
       END DO
       CLOSE(1)
    END IF
    
  END SUBROUTINE SymmetryMatrices
  
  SUBROUTINE convertWienDataToInternalData()
    USE Global, ONLY : d1, d2, d3
    USE Symmetries, ONLY : G, nSym
    USE Global, ONLY : debug
    IMPLICIT NONE
    INTEGER :: iSym
    
    IF (debug) WRITE(*,*) "Program Flow: Entered convertWienDataToInternalData"
    
    nSym = iord
    
    d1(1:3) = rBas(1,1:3)
    d2(1:3) = rBas(2,1:3)
    d3(1:3) = rBas(3,1:3)
    
    ALLOCATE(G(nSym))
    
    DO iSym=1, iord
       G(iSym)%el(:,:) = symMatsRL(:,:,iSym)
    END DO
    
    DEALLOCATE (symMatsRL)
    DEALLOCATE (symMatsPL)
    DEALLOCATE (symMatsCt)
    DEALLOCATE (symMatsConv)
    
  END SUBROUTINE convertWienDataToInternalData
END MODULE structFile
