MODULE CommandLineArgumentsMod
  
  USE DebugMod, ONLY : debug
  USE UtilitiesMod, ONLY : charToInt, convertToLowerCase
  
  IMPLICIT NONE
  
  LOGICAL :: help
  LOGICAL :: ene   ! extract energy eignevalues
  LOGICAL :: mme   ! calculate momentum matrix elements
  LOGICAL :: sme   ! calculate spinor matrix elements
  LOGICAL :: nor   ! check normalization
  LOGICAL :: den   ! calculate densities
  CHARACTER(LEN=50) :: filename
  
CONTAINS
  
  SUBROUTINE parseCommandLineArguments
    IMPLICIT NONE
    
    INTEGER :: numberOfArguments, i, j
    CHARACTER(LEN=12) :: charArray(0:9)
    CHARACTER(LEN=12), ALLOCATABLE :: commandLineArgument(:)
    !CHARACTER(LEN=12), knownCommandLineArguments(10)
    !CHARACTER(LEN=1) :: charArrayFixed(0:9)=(/'0','1','2','3','4','5','6','7','8','9'/)
    INTEGER, EXTERNAL :: IARGC
    
    debug = .FALSE.
    help = .FALSE.
    
    numberOfArguments = IARGC()
    
    ! If no command line arguments, then quit with help message
    IF (0==numberOfArguments) THEN
       CALL printHelp
       STOP
    END IF
    
    ! If only one argument is given, check it for a filename
    
    IF (1==numberOfArguments) THEN
       CALL GETARG(numberOfArguments, filename)
       filename = TRIM(filename)
       WRITE(*,*) "Filename to process is ", filename
    ELSE
       ALLOCATE(commandLineArgument(numberOfArguments))
       ! Get all command line arguments except the final one
       DO i=1, numberOfArguments - 1
          CALL GETARG(i, commandLineArgument(i))
          CALL convertToLowerCase( commandLineArgument(i) )
       END DO
       ! Should something here to scan the commandLineArgments and
       ! reject them, with an error message, if the commandLineArgument
       ! is not valid.
       
       ! Scan for help
       DO i=1, numberOfArguments - 1
          IF (commandLineArgument(i) .EQ. '-help') THEN
             CALL printHelp
             STOP
          END IF
       END DO
       
       ! Scan for normalization
       DO i=1, numberOfArguments - 1
          IF (commandLineArgument(i) .EQ. '-norm') THEN
             nor = .TRUE.
          END IF
       END DO
       
       ! Scan for ene
       DO i=1, numberOfArguments - 1
          IF (commandLineArgument(i) .EQ. '-ene') THEN
             ene = .TRUE.
          END IF
       END DO
       
       ! Scan for mme
       DO i=1, numberOfArguments - 1
          IF (commandLineArgument(i) .EQ. '-mme') THEN
             mme = .TRUE.
          END IF
       END DO
       
       ! Scan for sme
       DO i=1, numberOfArguments - 1
          IF (commandLineArgument(i) .EQ. '-sme') THEN
             sme = .TRUE.
          END IF
       END DO
       
       ! Scan for debug
       DO j=1, numberOfArguments - 1
          IF (commandLineArgument(j) .EQ. '-debug') THEN
             WRITE(*,*) "Debugging turned on."
             debug = .TRUE.
          END IF
       END DO
       
       ! Scan for all
       DO j=1, numberOfArguments
          IF (commandLineArgument(j) .EQ. '-all') THEN
             debug = .TRUE.
             nor = .TRUE.
             mme = .TRUE.
             sme = .TRUE.
          END IF
       END DO
       
       ! Scan for density
       DO j=1, numberOfArguments - 1
          IF (commandLineArgument(j) .EQ. '-den') THEN
             den = .TRUE.
          END IF
       END DO
       
       CALL GETARG(numberOfArguments, filename)
       filename = TRIM(filename)
       WRITE(*,*) "Filename to process is ", filename       
       
       DEALLOCATE(commandLineArgument)
    END IF
    
  END SUBROUTINE parseCommandLineArguments
  
  SUBROUTINE printHelp
    IMPLICIT NONE
    WRITE(*,*) " "
    WRITE(*,*) "readwfk: reads abinit wfk file"
    WRITE(*,*) " "
    WRITE(*,*) "USAGE: readwfk [options] filename"
    WRITE(*,*) "Where filename is the name of the ABINIT WFK file and "
    WRITE(*,*) "options can be any of: "
    WRITE(*,*) " -debug           Use this to turn on debugging information."
    WRITE(*,*) " -ene             Use this to print out the energy eigenvalues."
    WRITE(*,*) " -mme             Use this to print out momentum matrix elements."
    WRITE(*,*) " -sme             Use this to print out the spinor matrix elements."
    WRITE(*,*) " -nor             Use this to print out the normalization (as a check)."
    WRITE(*,*) " "
    WRITE(*,*) " -all             USe this to turn on all the above options."
    WRITE(*,*) " "
    WRITE(*,*) " "
  END SUBROUTINE printHelp
  
END MODULE CommandLineArgumentsMod
