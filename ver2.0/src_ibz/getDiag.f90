!Find the shortest diagonal of the primitive cell
SUBROUTINE getDIAG ()
  USE GLOBAL, ONLY : b1, b2, b3, cell_type
  USE GLOBAL, ONLY : debug
  IMPLICIT NONE
  
  !Local variables
  INTEGER :: i
  !diagonal() is the body diagonal vector, length is the norm of this vector
  DOUBLE PRECISION :: diagonal(3), length(4)
  
  IF ( debug ) WRITE(*,*) "Program Flow: Entered getDIAG"
  
  !(0,1,0) -> (1,0,1) diagonal
  diagonal = b1 + b3 - b2
  length(1) = SQRT( diagonal(1)**2 + diagonal(2)**2 + diagonal(3)**2 )
  
  !(0,0,0) -> (1,1,1) diagonal
  diagonal = b1 + b2 + b3
  length(2) = SQRT( diagonal(1)**2 + diagonal(2)**2 + diagonal(3)**2 )
  
  !(1,1,0) -> (0,0,1) diagonal
  diagonal = b3 - b1 - b2
  length(3) = SQRT( diagonal(1)**2 + diagonal(2)**2 + diagonal(3)**2 )
  
  !(1,0,0) -> (0,1,1) diagonal
  diagonal = b2 + b3 - b1
  length(4) = SQRT( diagonal(1)**2 + diagonal(2)**2 + diagonal(3)**2 )
  
  !Find the shortest diagonal
  cell_type = 1
  DO i = 2, 4
     IF (length(i).LT.length(cell_type)) THEN
        cell_type = i
     END IF
  END DO
  
END SUBROUTINE getDIAG
