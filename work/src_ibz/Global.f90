MODULE Global
  
  IMPLICIT NONE
  
  LOGICAL :: debug
  
  ! NIRpts, NIRtet and NSym are the number of irreducible points,
  ! irreducible tetrahedra, and symmetry matrices.
  ! cell_type specifies which body diagonal
  ! of the primitive cell is shortest and hence how to divide the 
  ! submesh cells into tetrahedra.  Array TET() temporarily holds
  ! the six tetrahedra that are created from a submesh cell.
  INTEGER :: NIRpts, NIRtet, cell_type
  
  ! Array TET() temporarily holds the six tetrahedra that
  ! are created from a submesh cell.
  INTEGER :: TET(6,4)
  
  ! Direct lattice and reciprocal lattice primitive vectors
  DOUBLE PRECISION, DIMENSION(3) :: d1, d2, d3, b1, b2, b3 
  
  ! Lattice parameters and Brillouin zone volume
  DOUBLE PRECISION :: a, b, c, BZ_VOL
  
  !IRpts holds the irreducible points
  INTEGER, ALLOCATABLE :: IRpts(:,:)
  
  TYPE matrix
     INTEGER :: el(3,3) !Matrix elements
  END TYPE matrix
  
  INTEGER, ALLOCATABLE :: IRtet(:,:) !Holds irreducible tetrahedra
  INTEGER, ALLOCATABLE :: tetrahedronMultiplicity(:) ! Holds tetrahedra multiplicities
  
  TYPE CUBE
     INTEGER :: corner(8)
  END TYPE CUBE
  
!!!  TYPE(MATRIX), ALLOCATABLE :: G(:) !Holds symmetry matrices
  
  DOUBLE PRECISION, PARAMETER :: PI=3.14159265358979323844d0
  
  ! passNumber indicates which pass we are running for the adaptive mode
  INTEGER :: passNumber
  
!!!  INTEGER, ALLOCATABLE :: multiplicity(:)
!!!  DOUBLE PRECISION, ALLOCATABLE :: weights(:)
!!!  TYPE TETRAHEDRA
!!!     INTEGER :: crn(4) !Tetrahedra corners
!!!     INTEGER :: mult  !Multiplicity/Weight
!!!  END TYPE TETRAHEDRA
!!!  TYPE(TETRAHEDRA), ALLOCATABLE :: IRtet(:) !Holds irreducible tetrahedra  
  
CONTAINS
  
  SUBROUTINE getRLV ()
    ! Get reciprocal lattice primitive vectors from direct
    ! lattice primitive vectors.
    ! Also find Brillouin zone volume
    IMPLICIT NONE
    
    !Local variables
    DOUBLE PRECISION :: tmp(3)
    
    IF ( debug ) WRITE(*,*) "Program Flow: Entered getRLV"
    
    !  PI = ACOS(-1.d0)
    
    !Use the standard formular for reciprocal lattice primitive vectors
    CALL XPROD (d2, d3, tmp)
    b1 = 2*PI*(1/(DOT_PRODUCT(d1, tmp)))*tmp
    
    CALL XPROD (d3, d1, tmp)
    b2 = 2*PI*(1/(DOT_PRODUCT(d2, tmp)))*tmp
    
    CALL XPROD (d1, d2, tmp)
    b3 = 2*PI*(1/(DOT_PRODUCT(d3, tmp)))*tmp
    
    CALL XPROD (b2, b3, tmp)

    BZ_VOL = DABS(DOT_PRODUCT(b1, tmp))
    
  END SUBROUTINE getRLV
  
  SUBROUTINE XPROD (v1, v2, x)
    !Sets x to the cross product of v1 and v2
    IMPLICIT NONE
    
    !Calling arguments
    DOUBLE PRECISION,INTENT(IN) :: v1(3),v2(3)
    DOUBLE PRECISION, INTENT(OUT) :: x(3)
    
    x(1) = v1(2)*v2(3) - v1(3)*v2(2)
    x(2) = v1(3)*v2(1) - v1(1)*v2(3)
    x(3) = v1(1)*v2(2) - v1(2)*v2(1)
    
  END SUBROUTINE XPROD
  
  SUBROUTINE CharToInt(inChar,outInt)
    ! Turns the character 0, 1, 2, etc. to its respective digit integer 0, 1, 2, etc.
    IMPLICIT NONE
    CHARACTER(LEN=1), INTENT(IN) :: inChar
    INTEGER, INTENT(OUT) :: outInt
    
    SELECT CASE (inChar)
    CASE ("0")
       outInt = 0
    CASE ("1")
       outInt = 1
    CASE ("2")
       outInt = 2
    CASE ("3")
       outInt = 3
    CASE ("4")
       outInt = 4
    CASE ("5")
       outInt = 5
    CASE ("6")
       outInt = 6
    CASE ("7")
       outInt = 7
    CASE ("8")
       outInt = 8
    CASE ("9")
       outInt = 9
    CASE DEFAULT
       WRITE(*,*) ""
       WRITE(*,*) "    ERROR: Passed illegal value to CharToInt subroutine"
       WRITE(*,*) "    STOPPING"
       WRITE(*,*) ""
       STOP "Illegal value passed to CharToInt subroutine"
    END SELECT
  END SUBROUTINE CharToInt
  
  SUBROUTINE IntToChar(inInt, outChar)
    IMPLICIT NONE
    CHARACTER(LEN=1), INTENT(OUT) :: outChar
    INTEGER, INTENT(IN) :: inInt
    
    SELECT CASE (inInt)
    CASE (0)
       outChar = "0"
    CASE (1)
       outChar = "1"
    CASE (2)
       outChar = "2"
    CASE (3)
       outChar = "3"
    CASE (4)
       outChar = "4"
    CASE (5)
       outChar = "5"
    CASE (6)
       outChar = "6"
    CASE (7)
       outChar = "7"
    CASE (8)
       outChar = "8"
    CASE (9)
       outChar = "9"
    CASE DEFAULT
       WRITE(*,*) ""
       WRITE(*,*) "    ERROR: Passed illegal value to IntToChar subroutine"
       WRITE(*,*) "    STOPPING"
       WRITE(*,*) ""
       STOP "Illegal value passed to IntToChar subroutine"
    END SELECT
  END SUBROUTINE IntToChar
  
END MODULE Global
