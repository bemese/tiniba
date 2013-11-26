SUBROUTINE checkDimensions ()
  ! Check symmetries and grid dimensions to ensure that satisfy
  ! the conditions outlined in the documentation
  
  ! Checks the following cases
  ! N1 .ne. N2 . ne. N3
  ! N1 .eq. N2 . ne. N3
  ! N1 .eq. N3 . ne. N2
  ! N2 .eq. N3 . ne. N1
  
  USE Global, ONLY : debug
  USE Symmetries, ONLY : G, nSym
  USE Grid, ONLY : N1, N2, N3
  
  IMPLICIT NONE
  
  ! Local variables.
  REAL :: M(3,3)
  ! Nprob counts the number of problem matrices.
  INTEGER :: i, j, k, Nprob
  
  ! bad(NSym) Holds the problem matrices.
  INTEGER, DIMENSION(nSym) :: bad
  
  ! logical PROBLEM will be false unless a conflict between
  ! matrices and grid dimensions is found.
  LOGICAL :: exitFlag, PROBLEM
  
  IF ( debug ) WRITE(*,*) "Program Flow: Entered checkDIMENSIONS"
  
  PROBLEM = .TRUE.
  Nprob = 0
  
  IF ((N1 .EQ. N2) .AND. (N1.EQ.N3) .AND. (N2.EQ.N3)) THEN
     PROBLEM = .FALSE.
     IF ( debug ) WRITE(*,*) "Program Flow: check on N1 N2 N3 not needed"
  END IF
  
  ! Case N1, N2, N3 all different  
  IF ( N1 /= N2 .AND. N1 /= N3 .AND. N2 /= N3 ) THEN
     ! In this case the symmetry matrix must be diagonal
     
     ! Loop over the symmetry matrices
     DO i = 1, NSym
        
        exitFlag = .FALSE.
        
        ! loop over the symmetry matrix elements
        DO j = 1, 3
           DO k = 1, 3
              
              ! If the matrix is not diagonal add it to the list
              ! of problem matrices
              IF (j /= k .AND. G(i)%el(j,k) /= 0) THEN
                 ! then off-diagonal compnent is nonzero
                 Nprob = Nprob + 1
                 bad(Nprob) = i
                 exitFlag = .TRUE.
                 EXIT
              END IF
              
           END DO
           
           IF (exitFlag) EXIT
           
        END DO
     END DO
     
     ! If problem matrices have been found print them out
     IF ( Nprob > 0 ) THEN
        WRITE(*,*) "N1, N2, and N3 cannot all be different because"//&
             "the following matrices are not diagonal:"
        DO i = 1, Nprob
           WRITE(*,*) bad(i)
        END DO
        PROBLEM = .TRUE.
     ELSE
        PROBLEM = .FALSE.
     END IF
  END IF
  
  Nprob = 0 
  
  ! Case N1 = N2 /= N3
  IF (N1 == N2 .AND. N1 /= N3) THEN
     
     ! Loop over the symmetry matrices
     DO i = 1, NSym
        M = G(i)%el
        
        ! If the matrix is not block diagonal add it to
        ! list of problem matrices
        IF ( M(1,3) /= 0 .OR. M(2,3) /= 0 .OR. &
             M(3,1) /= 0 .OR. M(3,2) /= 0 ) THEN
           Nprob = Nprob + 1
           bad(Nprob) = i
        END IF
        
     END DO
     
     !If problem matrices have been found print them out  
     IF ( Nprob > 0 ) THEN
        WRITE(*,*) "Cannot have N1 = N2 /= N3 because of symmetries:"
        DO i = 1, Nprob
           WRITE(*,*) bad(i)
        END DO
        PROBLEM = .TRUE.
     ELSE
        PROBLEM = .FALSE.
     END IF
  END IF
  
  Nprob = 0 
  
  !Case N1 = N3 /= N2
  IF ( N1 == N3 .AND. N1 /= N2 ) THEN
     
     !Loop over symmetry matrices
     DO i = 1, NSym
        M = G(i)%el
        
        !If the matrix is not block diagonal add it to list of problem matrices
        IF ( M(1,2) /= 0 .OR. M(2,1) /= 0 .OR. &
             M(2,3) /= 0 .OR. M(3,2) /= 0 ) THEN
           Nprob = Nprob + 1
           bad(Nprob) = i
        END IF
        
     END DO
     
     !If problem matrices have been found print them out      
     IF ( Nprob > 0 ) THEN
        WRITE(*,*) "Cannot have N1 = N3 /= N2 because of symmetries:"
        DO i = 1, Nprob
           WRITE(*,*) bad(i)
        END DO
        PROBLEM = .TRUE.
     ELSE
        PROBLEM = .FALSE.
     END IF
     
  END IF
  
  Nprob = 0 
  
  !Case N2 = N3 /= N1
  IF ( N2 == N3 .AND. N1 /= N3 ) THEN
     
     !Loop over symmetry matrices
     DO i = 1, NSym
        M = G(i)%el
        
        !If the matrix is not block diagonal add it to list of problem matrices
        IF ( M(1,2) /= 0 .OR. M(1,3) /= 0 .OR. &
             M(2,1) /= 0 .OR. M(3,1) /= 0 ) THEN
           Nprob = Nprob + 1
           bad(Nprob) = i
        END IF
        
     END DO
     
     !If problem matrices have been found print them out    
     IF ( Nprob > 0 ) THEN
        WRITE(*,*) "Cannot have N2 = N3 /= N1 because of symmetries:"
        DO i = 1, Nprob
           WRITE(*,*) bad(i)
        END DO
        PROBLEM = .TRUE.
     ELSE
        PROBLEM = .FALSE.
     END IF
     
  END IF
  
  !If conflicts have been found: stop the program
  IF ( PROBLEM) THEN
     WRITE(*,*) ""
     WRITE(*,*) " A problem has been found with the choice of (N1, N2, N3)"
     WRITE(*,*) " If the above error message is not understandable, please"
     WRITE(*,*) " contact developers."
     WRITE(*,*) ""
  END IF
  IF ( PROBLEM ) STOP 'Problem in checkDimensions'
  
END SUBROUTINE checkDimensions

