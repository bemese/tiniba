MODULE DefaultInput
  USE Global, ONLY : debug
  IMPLICIT NONE
  
CONTAINS
  
  SUBROUTINE getGridInput
    USE Grid, ONLY : N1, N2, N3
    IMPLICIT NONE
    INTEGER :: istat
    
    IF (debug) WRITE (*,'(A)') "Program Flow: Entered getGridInput"
    
    IF (debug) WRITE(*,'(A)') "File control: Opening grid"
    OPEN (UNIT=12,FILE="grid",STATUS="OLD",ACTION="READ",IOSTAT=istat)
    IF (istat.NE.0) THEN
       WRITE(*,*) "Error: Cannot open file grid"
       STOP "Error opening file grid"
    END IF
    
    READ(UNIT=12, FMT=*) N1, N2, N3
    
    IF (debug) WRITE(*,'(A27,3I5)') "(getGridInput) N1, N2, N3: ", N1, N2, N3
    
    CLOSE (12)
    
    RETURN
    
999 STOP ' error reading file: grid '
  END SUBROUTINE getGridInput
  
  
  SUBROUTINE getPrimitiveVectors
    USE GLOBAL, ONLY : d1, d2, d3, a, b, c
    IMPLICIT NONE
    INTEGER :: istat
    
    IF (debug) WRITE (*,'(A)') "Program Flow: Entered getPrimitiveVectors"
    
    IF ( debug ) WRITE(*,'(A)') "File control: Opening pvectors"
    OPEN (UNIT=11,FILE="pvectors",STATUS="OLD",ACTION="READ",IOSTAT=istat)
    IF ( istat.NE.0 ) THEN
       WRITE(*,*) "Error: Cannot open file pvectors"
       STOP "Error opening file pvectors"
    END IF
    
    READ(UNIT=11, FMT=*, ERR=998) d1(1:3)
    READ(UNIT=11, FMT=*, ERR=998) d2(1:3)
    READ(UNIT=11, FMT=*, ERR=998) d3(1:3)
    READ(UNIT=11, FMT=*, ERR=998) a, b, c
    
    d1 = a * d1
    d2 = b * d2
    d3 = c * d3
    
    IF ( debug ) WRITE(*,'(A,3F)') "(getPrimitiveVectors) d1(1:3): ", d1(1:3)
    IF ( debug ) WRITE(*,'(A,3F)') "(getPrimitiveVectors) d2(1:3): ", d2(1:3)
    IF ( debug ) WRITE(*,'(A,3F)') "(getPrimitiveVectors) d3(1:3): ", d3(1:3)
    
    CLOSE (11)
    
    RETURN
    
998 STOP ' error reading file: pvectors '    
  END SUBROUTINE getPrimitiveVectors
  
  
  SUBROUTINE getSymmetryMatrices
    USE Symmetries, ONLY : nSym, G
    IMPLICIT NONE
    INTEGER :: S(3,3), i 
    INTEGER :: istat 
    
    IF ( debug ) WRITE(*,'(A)') "File control: Opening symmetries"
    OPEN (UNIT=10,FILE="symmetries",STATUS="OLD",ACTION="READ",IOSTAT=istat)
    IF ( istat.NE.0 ) THEN
       WRITE(*,*) "Error: Cannot open file symmetries"
       STOP "Error opening file symmetries"
    END IF
    
    nSym = 0
    
    ! This looping structure is outdated.  Remove GOTOs.
100 CONTINUE
    READ(UNIT=10, FMT=*, ERR=997, END=200) S(1:3,1),S(1:3,2),S(1:3,3)
    nSym = nSym + 1 
    GOTO 100 ! Loop back
    
200 CONTINUE ! File ended so come here
    
    ALLOCATE( G(nSym) )
    
    CLOSE (10)
    
    ! Open symmetries file again
    OPEN (UNIT=10,FILE="symmetries",STATUS="OLD",ACTION="READ")
    
    !Now we extract the symmetries, and store them in the array G
    DO i = 1, NSym
       READ(UNIT=10, FMT=*, ERR=997, END=200) & 
            G(i)%el(1:3,1), G(i)%el(1:3,2), G(i)%el(1:3,3)
    END DO
    
    CLOSE (10)    
    
    RETURN
    
997 STOP ' error reading file: symmetries '
  END SUBROUTINE getSymmetryMatrices
  
  SUBROUTINE getInput ()
    IMPLICIT NONE
    
    IF (debug) WRITE (*,'(A)') "Program Flow: Entered getINPUT"
    
    ! Get the symmetry matrices
    CALL getSymmetryMatrices
    
    ! Get the primitive vectors
    CALL getPrimitiveVectors
    
    ! Get the grid dimensions
    CALL getGridInput
    
  END SUBROUTINE getINPUT
  
END MODULE DefaultInput
