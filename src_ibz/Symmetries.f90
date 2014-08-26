MODULE Symmetries
  USE Global, ONLY : debug
  USE Global, ONLY : matrix
  
  IMPLICIT NONE
  
  ! nSym is the number of symmetry matrices
  INTEGER :: nSym
  
  ! G contains the symmetry matrices
  TYPE(matrix), ALLOCATABLE :: G(:)
  
  ! containsInversion is true if the system has inversionSymmetry
  ! or if the user demands inversionSymmetry is present
  LOGICAL :: containsInversion
  
CONTAINS
  SUBROUTINE checkForInversion
    INTEGER :: iSym, jSym
    
    ! inverseMap is an array that, for each matrix, points to its inverse.
    INTEGER, ALLOCATABLE :: inverseMap(:)
    
    ! thisSymmetryMatrix is matrix we are currently analysing
    TYPE(matrix) :: thisSymmetryMatrix
    
    LOGICAL :: inverseExists
    LOGICAL, ALLOCATABLE :: inverseTest(:), refArray(:)
    
    IF ( debug ) WRITE(*,*) "Program Flow: Entered checkForInversion"
    
    ! Loop over symmetry matrices.  Check that they are unique
    DO iSym  = 1, nSym-1
       DO jSym = iSym+1, nSym
          IF (ALL (G(iSym)%el(:,:) .EQ. G(jSym)%el(:,:))) THEN
             WRITE(*,*) ""
             WRITE(*,*) "Error: Found two equal symmetry matrices ", iSym, jSym
             WRITE(*,*) "Stopping"
             STOP "Symmetry matrices are not unique"
          END IF
       END DO
    END DO
    
    WRITE(*,*) "Check: Symmetry Matrices are unique."
    
    ! Initialize containsInversion
    containsInversion = .FALSE.
    
    ! Initialize inverseMap(1:nSym) to 
    ALLOCATE( inverseMap(nSym) )
    DO iSym = 1, nSym
       inverseMap(iSym) = iSym
    END DO
    
    ALLOCATE(inverseTest(nSym))
    ALLOCATE(refArray(nSym))
    inverseTest(1:nSym) = .FALSE.
    refArray(1:nSym) = .TRUE.
    
    ! Loop over symmetry matrices.  Check that each one has
    ! only one inversion pair, or that no inversion exists.
    DO iSym = 1, nSym-1
       inverseExists = .FALSE.
       DO jSym = 2, nSym
          
          ! compare thisSymmetryMatrix to G(jSym)
          IF (ALL (G(jSym)%el(1:3,1:3) .EQ. -G(iSym)%el(1:3,1:3))) THEN
             
             IF ( inverseExists ) THEN
                WRITE(*,*) "Found multiple inversion pairs!"
                STOP "Error with Symmetry matrices."
             ELSE
                inverseExists = .TRUE.
                inverseTest(iSym) = .TRUE.
                inverseTest(jSym) = .TRUE.
             END IF
          END IF
          
       END DO
       
    END DO
    
    IF (ALL(inverseTest(1:nSym) .EQ. refArray(1:nSym))) THEN
       containsInversion = .TRUE.
    ELSE IF (ALL(inverseTest(1:nSym) .NE. refArray(1:nSym))) THEN
       containsInversion = .FALSE.
    ELSE
       ! Error !
       WRITE(*,*) "Some, but not all symmetries have an inversion!"
       STOP
    END IF
    
  END SUBROUTINE checkForInversion
  
  SUBROUTINE addInversion
    
    TYPE (matrix), ALLOCATABLE :: tempSymmetries(:)
    INTEGER :: nSymOld, iSym, i
    
    IF (containsInversion) THEN
       WRITE(*,*) "Cannot add inversion.  Structure is centrosymmetric."
       WRITE(*,*) "Error!"
       STOP "Error.  Tried to add inversion to centrosymmetric structure."
    END IF
    
    ! copy the symmetry matrices to a temporary array
    ALLOCATE ( tempSymmetries(nSym) )
    tempSymmetries(1:nSym) = G(1:nSym)
    
    ! double the number of Symmetries
    nSymOld = nSym
    nSym = 2*nSym
    DEALLOCATE(G)
    ALLOCATE(G(nSym))
    
    i=0
    DO iSym=1, nSymOld
       i=i+1
       G(i) = tempSymmetries(iSym)
       i=i+1
       G(i)%el(1:3,1:3) = -tempSymmetries(iSym)%el(1:3,1:3)
    END DO
    
  END SUBROUTINE addInversion
  
END MODULE Symmetries
