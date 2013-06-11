MODULE CommandLineArguments
  
  USE Global, ONLY : debug
  USE Global, ONLY : charToInt
  USE Global, ONLY : passNumber
  
  IMPLICIT NONE
  
  ! firstPass and nextPass are logicals that control the program flow
  ! and determine whether the cubes will be written, or refined.
  ! If firstPass is false and nextPass is fasle then the default procedure is applied.
  ! If firstPass is true and nextPass is false then the grid cube corners are listed.
  ! If firstPass is false and nextPass is true then the grid is refined according
  ! to the refining procedure.
  ! If firstPass is true and nextPass is true then there is an error and the
  ! program terminates
  LOGICAL :: firstPass
  LOGICAL :: nextPass
  
  ! wien2k is a logical that controls whether wien style kpoints should be
  ! output or not.
  logical :: wien2k
  
  ! abinit is a logical that controls whether abinit style kpoints should
  ! be output or not.  It also determines whether the input file should be
  ! an abinit input file.
  logical :: abinit
  
  ! defaultFormal is a logical that controls whether data should be provided
  ! in the default format.  It is only made true if neither wien2k or abinit
  ! formats are specified.
  logical :: defaultFormat
  
  ! cartesian is a logical that controls whether the kpoints in Cartesian
  ! cooridnates are output or not
  logical :: cartesian
  
  ! reduced is a logical that controls whether the kpoints in reduced
  ! coordinates are output.  Reduced coordinates means in the basis of the
  ! reciprocal lattive vectors
  logical :: reduced
  
  ! reducedInteger is a logical that controls whether the kpoints in
  ! reduced coordinates, but in terms of integer ratios are print out.
  ! This is not necessarily the same as the wien2k format.
  ! The kpoints are written to the kpoints.integer
  logical :: reducedInteger
  
  ! printSymmetries is a logical that controls whether the symmetries are
  ! printed.  If true, then the point group symmetries are printed twice
  ! One file, IBZsymmetries, contains a large list of the symmetries and
  ! the other file, Symmetries.Cartesian, contains just the symmetries
  ! in Cartesian coordinates.  The Symmetries.Cartesian is used by
  ! my (Fred's) tetrahedron integration code.
  logical :: printSymmetries
  
  ! printTetrahedra is a logical that determines whether to print out
  ! the tetrahedra.  The tetrahedra are required for the linear analytic
  ! tetrahedron method.
  logical :: printTetrahedra
  
  ! printMap is a logical that determines whether the kpoint map is
  ! printed out.  The kpoint map contains the information on which
  ! kpoint on the full grid was transformed to the irreducible grid
  ! and a symmerty matrix that took it there.  Of course, multiple
  ! symmetry matrices can relate two kpoints.
  logical :: printMap
  
  logical :: adaptive
  
  logical :: manualDebug
  
  ! mesh is a logical that is used to specify whether a file should
  ! be specified to with the divisions in it, or whether that should
  ! be found automatically.  If mesh is true then the program looks
  ! for a file called "grid"
  logical :: mesh
  
!!!  logical :: debug
  
CONTAINS
  
  SUBROUTINE parseCommandLineArguments
    IMPLICIT NONE
    
    INTEGER :: numberOfArguments, i, j
    CHARACTER(LEN=12) :: charArray(0:9)
    CHARACTER(LEN=12), ALLOCATABLE :: commandLineArgument(:)
    CHARACTER(LEN=1) :: charArrayFixed(0:9)=(/'0','1','2','3','4','5','6','7','8','9'/)
    INTEGER, EXTERNAL :: IARGC
    
    debug = .TRUE.
    
    numberOfArguments = IARGC()
    
    IF (debug) WRITE(*,*) "Number of Command line arguments: ", numberOfArguments
    
    ALLOCATE(commandLineArgument(numberOfArguments))
    
    firstPass = .FALSE.
    nextPass  = .FALSE.
    cartesian = .FALSE.
    reduced   = .FALSE.
    reducedInteger = .FALSE.
    wien2k = .FALSE.
    abinit = .FALSE.
    printSymmetries = .FALSE.
    printTetrahedra = .FALSE.
    printMap = .FALSE.
    adaptive = .FALSE.
    passNumber  = -1
    manualDebug = .FALSE.
    
!!! First get all command line arguments, and scan for the -help option
    DO i=1, numberOfArguments
       CALL GETARG(i, commandLineArgument(i))
       ! 
       ! Should add something here to convert the commandLineArgument to lowercase
       !
       IF (debug) WRITE(*,*) 'CommandLineArgument is: ', TRIM(commandLineArgument(i))
       IF (commandLineArgument(i) .EQ. '-help') THEN
          CALL printHelp
          STOP
       END IF
    END DO
    
!!! Scan for debug
    DO i=1, numberOfArguments
       IF (commandLineArgument(i) .EQ. '-debug') THEN
          WRITE(*,*) "Debugging turned on"
          manualDebug = .TRUE.
          debug = .TRUE.
       END IF
    END DO
    
!!! Scan for adaptive flag
    DO i=1, numberOfArguments
       IF (commandLineArgument(i) .EQ. '-pass') THEN
          adaptive = .TRUE.
          charArray(0:9) = commandLineArgument(i+1)
          IF (ANY( charArray(0:9).EQ.charArrayFixed(0:9))) THEN
             CALL CharToInt(commandLineArgument(i), passNumber)
          ELSE
             WRITE(*,*) ""
             WRITE(*,*) "   ERROR"
             WRITE(*,*) "   Bad command line format."
             WRITE(*,*) "   Read integer ", commandLineArgument(i)
             WRITE(*,*) "   Stopping"
             STOP
          END IF
          EXIT
       END IF
    END DO
    
!!! Scan for rest of flags    
    DO i=1, numberOfArguments
       IF (commandLineArgument(i) .EQ. '-firstpass') THEN
          firstPass = .TRUE.
       ELSE IF (commandLineArgument(i) .EQ. '-nextpass') THEN
          nextPass = .TRUE.
       ELSE IF (commandLineArgument(i) .EQ. '-cartesian') THEN
          cartesian = .TRUE.
       ELSE IF (commandLineArgument(i) .EQ. '-reduced') THEN
          reduced = .TRUE.
       ELSE IF (commandLineArgument(i) .EQ. '-reducedint') THEN
          reducedInteger = .TRUE.
       ELSE IF (commandLineArgument(i) .EQ. '-wien2k') THEN
          wien2k = .TRUE.
       ELSE IF (commandLineArgument(i) .EQ. '-abinit') THEN
          abinit = .TRUE.
       ELSE IF (commandLineArgument(i) .EQ. '-symmetries') THEN
          printSymmetries = .TRUE.
       ELSE IF (commandLineArgument(i) .EQ. '-tetrahedra') THEN
          printTetrahedra = .TRUE.
       ELSE IF (commandLineArgument(i) .EQ. '-mesh') THEN
          mesh = .TRUE.
!!!       ELSE
!!!          WRITE(*,*) "Command line argument not understood"
!!!          WRITE(*,*) "Command line argument read: ", commandLineArgument(i)
!!!          WRITE(*,*) " "
!!!          CALL printHelp
!!!          STOP 'Error with command line arguments.  Run ibz -help for options.'
       END IF
    END DO
    
    IF (wien2k .AND. abinit) THEN
       WRITE(*,*) ""
       WRITE(*,*) "     ERROR"
       WRITE(*,*) "     -wien2k and -abinit were specified.  Choose only one option."
       WRITE(*,*) "     Stopping."
       WRITE(*,*) ""
       STOP
    END IF
    
    IF ((.NOT.wien2k).AND.(.NOT.abinit)) THEN
       defaultFormat = .TRUE.
    END IF
    
    IF (adaptive .AND. (passNumber .EQ. -1)) THEN
       WRITE(*,*) ""
       WRITE(*,*) "     ERROR"
       WRITE(*,*) "     -pass was specified without an iteration number."
       WRITE(*,*) "     Stopping"
       STOP
    END IF
    
!!!    IF (numberOfArguments .EQ. 0) THEN
!!!       firstPass = .true.
!!!    END IF
!!!!!! IF ( (.NOT.firstPass) .AND. (.NOT.nextPass)) firstPass = .true.
    
    IF ( passNumber .EQ. 0 ) THEN
       firstPass = .TRUE.
       reducedInteger = .TRUE.
    ELSE IF (passNumber .GT. 0) THEN
       firstPass = .FALSE.
       nextPass = .TRUE.
    END IF
    
    IF ( (.NOT.(firstPass)).AND.(.NOT.(nextpass))) THEN
       firstpass = .TRUE.
    END IF
    
!!!    IF ( firstPass .OR. nextPass ) THEN
!!!       reducedInteger = .TRUE.
!!!    END IF
    
    IF ( firstPass .AND. nextPass ) THEN
       WRITE(*,*) ""
       WRITE(*,*) "      ERORR"
       WRITE(*,*) "      Cannot specify -firstpass and -nextpass options simultaneously."
       STOP "Cannot specify -firstpass and -nextpass options simultaneously."
    END IF
    
    DEALLOCATE(commandLineArgument)
    
  END SUBROUTINE parseCommandLineArguments
  
  SUBROUTINE printHelp
    IMPLICIT NONE
    WRITE(*,*) " "
    WRITE(*,*) "ibz: tetrahedral grid maker"
    WRITE(*,*) "USAGE: ibz [options]"
    WRITE(*,*) "Where options can be any of: "
    WRITE(*,*) ""
    WRITE(*,*) "-firstpass       Use for adaptive grid generation.  The first pass"
    WRITE(*,*) "                 option prints a cubes file necessary for the nextpass"
    WRITE(*,*) "                 option."
    WRITE(*,*) ""
    WRITE(*,*) "-nextpass        (Obsolete) Use for further refinement of the grid. This"
    WRITE(*,*) "                 option requires the firstpass option to have been run"
    WRITE(*,*) "                 first and it needs a file called cubesToDivide which"
    WRITE(*,*) "                 lists which cubes should be divided."
    WRITE(*,*) ""
    WRITE(*,*) "-pass passNum    Use this to sepcify which pass you are on. passNum 0"
    WRITE(*,*) "                 corresponds to firstPass, passNum 1 is the first level"
    WRITE(*,*) "                 of refinement and so on."
    WRITE(*,*) ""
    WRITE(*,*) "-cartesian       Use to print out a list of kpoints in Cartesian"
    WRITE(*,*) "                 coordinates."
    WRITE(*,*) ""
    WRITE(*,*) "-reduced         Use to print out the list of kpoints in terms of the"
    WRITE(*,*) "                 reciprocal lattice, or so called reduced coordinates."
    WRITE(*,*) ""
    WRITE(*,*) "-reducedint      Use to print out the list of irreducible kpoints in"
    WRITE(*,*) "                 reduced cordinates but with the coordinates expressed"
    WRITE(*,*) "                 as integer ratios."
    WRITE(*,*) ""
    WRITE(*,*) "-wien2k          Use to print out the list of kpoints to be used with"
    WRITE(*,*) "                 the WIEN code.  The resulting file kpoints.klist can be"
    WRITE(*,*) "                 used with lapw1."
    WRITE(*,*) ""
    WRITE(*,*) "-abinit          Use to print out the list of kpoint to be used with"
    WRITE(*,*) "                 the ABINIT code.  Also expects an ABINIT WFK file to"
    WRITE(*,*) "                 be present so that it can process it for relevant info."
    WRITE(*,*) ""
    WRITE(*,*) "-symmetries      Use to print out the symmetry matrices as read from the"
    WRITE(*,*) "                 Wien2k code case.struct file.  Two files are made.  One"
    WRITE(*,*) "                 contains the symmetry matrices in a variety of bases,"
    WRITE(*,*) "                 the other has the Symmetry matrices in only the"
    WRITE(*,*) "                 Cartesian basis."
    WRITE(*,*) ""
    WRITE(*,*) "-tetrahedra      Use this option to print out the tetrahedra to file"
    WRITE(*,*) "                 called tetrahedra."
    WRITE(*,*) ""
    WRITE(*,*) "-map             Use this option to print the map of original grid"
    WRITE(*,*) "                 points to irreducible grid points."
    WRITE(*,*) ""
    WRITE(*,*) "-mesh            Use this option if you want to specify the mesh "
    WRITE(*,*) "                 divisions in a file called grid.  Otherwise, the"
    WRITE(*,*) "                 the program will ask you how many total kpoints you"
    WRITE(*,*) "                 want and will calculate the divisions automatically."
    WRITE(*,*) ""
    WRITE(*,*) "-debug           Use this to turn on debugging information."
    WRITE(*,*) " "
  END SUBROUTINE printHelp
  
END MODULE CommandLineArguments
