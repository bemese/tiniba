MODULE InitializeDataMod
  USE DefaultInput, ONLY : getInput, getGridInput
  
  USE CommandLineArguments, ONLY : wien2k, abinit, defaultFormat, mesh
  
  USE structFile, ONLY : structReader, convertWienDataToInternalData
  
  USE abinitReader, ONLY : abinitWFKReader, convertAbinitDataToInternalData
  
  USE global, ONLY : debug
  
  IMPLICIT NONE
  
CONTAINS
  
  SUBROUTINE initializeData
    
    IF ( debug ) WRITE(*,*) "Program Flow: Entered initializeData"
    
    ! determine whether we need the wien2k struct file
    IF ( wien2k ) THEN
       
       ! Get input from WIEN code struct file
       CALL structReader ()
       CALL convertWienDataToInternalData ()
       IF ( mesh ) THEN
          CALL getGridInput
       END IF
    ELSEIF ( abinit ) THEN
       
       ! Get input from ABINIT unformatted file
!       CALL abinitWFKReader ()
       CALL convertAbinitDataToInternalData ()
       IF ( mesh ) THEN
          CALL getGridInput
       END IF
    ELSEIF ( defaultFormat ) THEN
       ! Get user input in 'default format' from files
       CALL getInput ()
       
    ELSE
       WRITE(*,*) ""
       WRITE(*,*) "     Error: No format for input specified."
       WRITE(*,*) "     THIS IS AN INTERNAL ERROR, AND SHOULD NOT HAVE"
       WRITE(*,*) "     HAPPENED.  PLEASE CONTACT AUTHORS."
       WRITE(*,*) ""
       STOP       "     INTERNAL ERROR.  PLEASE CONTACT AUTHORS."
    END IF
    
  END SUBROUTINE initializeData
  
  SUBROUTINE calculateGridDivisions
    USE Global, ONLY : b1, b2, b3
    USE Grid, ONLY : N1, N2, N3
    INTEGER :: nDivTotal
    DOUBLE PRECISION :: L1, L2, L3
    
    WRITE(*,*) "How many k-points do you want throughout the cell?"
    READ(*,*) nDivTotal
    
    L1 = SQRT (b1(1)**2 + b1(2)**2 + b1(3)**2)
    L2 = SQRT (b2(1)**2 + b2(2)**2 + b2(3)**2)
    L3 = SQRT (b3(1)**2 + b3(2)**2 + b3(3)**2)
    
    WRITE(*,*) L1, L2, L3
    
    WRITE(*,*) (L1**2/(L2*L3)*REAL(nDivTotal))**(1.d0/3.d0)
    WRITE(*,*) (L2**2/(L1*L3)*REAL(nDivTotal))**(1.d0/3.d0)
    WRITE(*,*) (L3**2/(L1*L3)*REAL(nDivTotal))**(1.d0/3.d0)
    
    N1 = 1 + INT ((L1**2/(L2*L3)*REAL(nDivTotal))**(1.d0/3.d0))
    N2 = 1 + INT ((L2**2/(L1*L3)*REAL(nDivTotal))**(1.d0/3.d0))
    N3 = 1 + INT ((L3**2/(L1*L2)*REAL(nDivTotal))**(1.d0/3.d0))
    
    WRITE(*,'(A,3I)') " N1 N2 N3 are found to be ", N1, N2, N3
    
  END SUBROUTINE calculateGridDivisions
  
END MODULE InitializeDataMod
