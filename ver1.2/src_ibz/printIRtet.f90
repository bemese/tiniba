SUBROUTINE printIRtet ()
  USE GLOBAL, ONLY : debug
  USE GLOBAL, ONLY : NIRtet
  USE GLOBAl, ONLY : IRtet, tetrahedronMultiplicity
  USE GLOBAL, ONLY : BZ_vol
  USE GRID, ONLY : N1, N2, N3
  IMPLICIT NONE
  
  !Local variables
  INTEGER :: i
  
  IF ( debug ) WRITE(*,*) "Program Flow: Entered printIRtet"
  
  PRINT *, "Total number of tetrahedra:", (N1-1)*(N2-1)*(N3-1)*6
  PRINT *, "Number of irreducible tetrahedra:", NIRtet
  !Amount of reduction is the ration of total tetra to irreducible tetra
  PRINT *, "Amount of Reduction:", REAL((N1-1)*(N2-1)*(N3-1)*6)/REAL(NIRtet)
  write(*,*)"BZ_VOL:", BZ_VOL
  
  OPEN (UNIT=10, FILE="tetrahedra", ACTION="WRITE")
  
  WRITE(UNIT=10,FMT=*) NIRtet
  
  !Print irreducible tetrahedra
  DO i = 1, NIRtet
     
     WRITE(UNIT=10, FMT='(4I6,I8,E20.10)')           &
          IRtet(1:4,i), tetrahedronMultiplicity(i),  &
          BZ_VOL/DBLE((N1-1)*(N2-1)*(N3-1)*6)
  END DO
  
  CLOSE(10)
  
END SUBROUTINE printIRtet
