MODULE NextPassMod
  
  USE Global, ONLY : debug
  
  USE Global, ONLY : IRpts
  USE Global, ONLY : passNumber
  USE Global, ONLY : IntToChar
  
  IMPLICIT NONE
  
  PRIVATE
  INTEGER :: NptsNew, NcubeNew, NIRPtsNew
  INTEGER :: nCubes, nPts
  INTEGER, ALLOCATABLE :: cubeCorners(:,:)
  LOGICAL, ALLOCATABLE :: divideThisCube(:)
  INTEGER :: numberOfCubesToDivide
  INTEGER, ALLOCATABLE :: IRPtsPointer(:)
  INTEGER :: numberOfPreviousPasses
  INTEGER :: N1old, N2old, N3old
  
!!! temporary variables. To become permamant when everything is working.
  INTEGER, ALLOCATABLE :: cube0Corners(:,:)
  INTEGER, ALLOCATABLE :: cube1Corners(:,:)  
  INTEGER, ALLOCATABLE :: cube2Corners(:,:)
  INTEGER, ALLOCATABLE :: cube3Corners(:,:)
  INTEGER, ALLOCATABLE :: cube4Corners(:,:)
  INTEGER, ALLOCATABLE :: cube5Corners(:,:)
  INTEGER, ALLOCATABLE :: cube6Corners(:,:)
  INTEGER, ALLOCATABLE :: cube7Corners(:,:)
  INTEGER, ALLOCATABLE :: cube8Corners(:,:)
  INTEGER, ALLOCATABLE :: cube9Corners(:,:)
  
  PUBLIC setupNextPass, readCubes, readWhichCubesToDivide
  PUBLIC readKpointsMap, readKpointsInteger
  PUBLIC divideCubes, reduceNewPoints, printNextPassResults
  
CONTAINS
  
  SUBROUTINE setupNextPass
    USE Grid, ONLY : N1, N2, N3
    IMPLICIT NONE
    IF (debug) WRITE(*,*) "Program Flow: Entered setupNextPass"
    NptsNew = 0
    NcubeNew = 0
    NIRPtsNew = 0
    N1old = N1
    N2old = N2
    N3old = N3
  END SUBROUTINE setupNextPass
  
  SUBROUTINE readCubes
    ! This subroutine reads the list of cube corners from the previous pass
    IMPLICIT NONE
    INTEGER :: j
    CHARACTER(LEN=5) :: charTemp
    CHARACTER(LEN=6) :: cubesFilename
    CHARACTER(LEN=26) :: charTemp26
    CHARACTER(LEN=1) :: charTemp1
    LOGICAL :: fileExists
    
    IF (debug) WRITE(*,*) "Program Flow: Entered readCubes"
    
    CALL IntToChar(passNumber-1,charTemp1)
    cubesFilename = "cubes"//TRIM(charTemp1)
    INQUIRE(FILE=cubesFilename, EXIST=fileExists)
    IF (.NOT.fileExists) THEN
       WRITE(*,*) ""
       WRITE(*,*) "      ERROR: File "//TRIM(cubesFilename)//" does not exist."
       WRITE(*,*) "      STOPPING"
       WRITE(*,*) ""
       STOP "Cubes file from previous pass does not exist."
    END IF
    OPEN(UNIT=10, FILE=cubesFilename, ACTION="READ")
    WRITE(*,*) "We are on iteration ", passNumber
    READ(10,*) nCubes
    SELECT CASE (passNumber)
    CASE(1)
       ALLOCATE ( cubeCorners(nCubes,8) )
    CASE DEFAULT
       WRITE(*,*) ""
       WRITE(*,*) "      ERROR"
       WRITE(*,*) "      Variable passNumber is illegal in subroutine readCubes"
       WRITE(*,*) "      STOPPING"
       WRITE(*,*) ""
       STOP
    END SELECT
    
    DO j = 1, nCubes
       READ(UNIT=10, FMT=*) cubeCorners(j,1:8)
    END DO
    
    CLOSE(10)
    
!!!    DO j=1,numberOfPreviousPasses
!!!       READ(10,*) charTemp, passNumber
!!!       IF (passNumber .NE. j) THEN
!!!          WRITE(*,*) ""
!!!          WRITE(*,*) "   Inconsistency in cubes file."
!!!          WRITE(*,*) "   Expected passNumber: ", j
!!!          WRITE(*,*) "   but read passnumber: ", passNumber
!!!          WRITE(*,*) "   Stopping"
!!!          STOP
!!!       END IF
!!!       READ(10,*) nCubes
!!!!!  ADD MORE HERE...
!!!    END DO
    
  END SUBROUTINE readCubes
  
  SUBROUTINE readWhichCubesToDivide
    USE Global, ONLY : debug
    IMPLICIT NONE
    INTEGER :: iTemp, j
    LOGICAL :: fileExists
    IF (debug) WRITE(*,*) "Program Flow: Entered readWhichCubesToDivide"
    
    INQUIRE(FILE="cubesToDivide", EXIST=fileExists)
    IF (.NOT.fileExists) THEN
       WRITE(*,*) ""
       WRITE(*,*) "      ERROR: File cubesToDivide does not exist."
       WRITE(*,*) "      STOPPING"
       WRITE(*,*) ""
       STOP "cubesToDivide file does not exist."
    END IF
    OPEN(UNIT=10, FILE="cubesToDivide", ACTION="READ")
    READ(10,*) iTemp
    IF (iTemp.NE.nCubes) THEN
       WRITE(*,*) "Inconsistency with the cubes file and cubesToDivide file"
       WRITE(*,*) "Stopping"
       STOP "Inconsistency with the cubes file and cubesToDivide file"
    END IF
    ALLOCATE ( divideThisCube(iTemp) )
    
    iTemp = 0
    numberOfCubesToDivide = 0
    
    DO iTemp = 1, nCubes
       READ(10,*) j, divideThisCube(iTemp)
       IF (j .NE. iTemp) THEN
          WRITE(*,*) "File cubesToDivide does not list cubes in order."
          WRITE(*,*) "Stopping"
          STOP "File cubesToDivide does not list cubes in order."
       END IF
       IF (divideThisCube(iTemp)) THEN
          numberOfCubesToDivide = numberOfCubesToDivide + 1
       END IF
    END DO
    
    CLOSE(10)
    
    IF (debug) WRITE(*,*) "(readWhichCubesToDivide) numberOfCubesToDivide:", &
         numberOfCubesToDivide
    
  END SUBROUTINE readWhichCubesToDivide
  
  SUBROUTINE readKpointsMap
    USE Grid, ONLY : N1, N2, N3
    USE Grid, ONLY : gridCoordinates, gridPointer
    USE Grid, ONLY : reducibleWeights, weights
    USE Grid, ONLY : multiplicity, irreducible
    IMPLICIT NONE
    INTEGER :: sizeEstimate
    INTEGER :: i
    LOGICAL :: fileExists
    IF (debug) WRITE(*,*) "Program Flow: Entered readKpointsMap"
    
    ! Double the grid
    N1 = N1*2 - 1
    N2 = N2*2 - 1
    N3 = N3*2 - 1
    
    ! Estimate size of grid we will need.
    sizeEstimate = N1*N2*N3  !! This is the most we could need
    
    ! If 64*Ncubes is less than N1*N2*N3 then we use that instead
    
    IF (64*Ncubes .LT. N1*N2*N3) THEN
       sizeEstimate = 64*Ncubes
    END IF
    
    WRITE(*,*) "sizeEstimate",sizeEstimate
    
    ALLOCATE ( gridCoordinates(sizeEstimate,3) )
    ALLOCATE ( gridPointer(sizeEstimate,2) )
    ALLOCATE ( IRpts(sizeEstimate,1:3) )
    ALLOCATE ( IRptsPointer(sizeEstimate) )
    ALLOCATE ( reducibleWeights(sizeEstimate) )
    ALLOCATE ( weights(sizeEstimate) )
    ALLOCATE ( multiplicity(sizeEstimate) )
    ALLOCATE ( irreducible(sizeEstimate) )
    
    gridCoordinates(1:sizeEstimate,1:3) = 0
    gridPointer(1:sizeEstimate,1:2) = 0
    irreducible(1:sizeEstimate) = .TRUE.
    
    INQUIRE(FILE="kpoints.map", EXIST=fileExists)
    IF (.NOT.fileExists) THEN
       WRITE(*,*) ""
       WRITE(*,*) "      ERROR: File kpoints.map does not exist."
       WRITE(*,*) "      STOPPING"
       WRITE(*,*) ""
       STOP "kpoints.map file does not exist."
    END IF
    OPEN(UNIT=10,FILE="kpoints.map",ACTION="READ")
    READ(10,*) nPts
    ! In the first iteration of nextPass, after running firstPass, nPts = N1*N2*N3.
    ! In the second iteration of nextPass, nPts = oldN1*oldN2*oldN3 + numberOfNewPoints
    
    DO i=1,nPts
       READ(UNIT=10,FMT='(3I8,2I10)') gridCoordinates(i,1:3), gridPointer(i,1:2)
       ! We double the integer values since the coordinates are for
       ! the old grid parameters N1, N2, and N3.
       gridCoordinates(i,1:3) = 2*gridCoordinates(i,1:3)
    END DO
    CLOSE(10)
    
    IRpts(1:sizeEstimate,1:3) = 0
    IRptsPointer(1:sizeEstimate) = 0
    reducibleWeights(1:sizeEstimate) = 0.d0
    weights(1:sizeEstimate) = 0.d0
    multiplicity(1:sizeEstimate) = 0
    
  END SUBROUTINE readKpointsMap
  
  SUBROUTINE readKpointsInteger
    USE Global, ONLY : NIRpts
    USE Grid, ONLY : N1, N2, N3
    IMPLICIT NONE
    CHARACTER (LEN=10) :: charTemp
    INTEGER :: intTemp(3), j
    LOGICAL :: fileExists
    IF (debug) WRITE(*,*) "Program Flow: Entered readKpointsInteger"
    
    INQUIRE(FILE="kpoints.integer", EXIST=fileExists)
    IF (.NOT.fileExists) THEN
       WRITE(*,*) ""
       WRITE(*,*) "      ERROR: File kpoints.integer does not exist."
       WRITE(*,*) "      STOPPING"
       WRITE(*,*) ""
       STOP "kpoints.integer file does not exist."
    END IF
    OPEN(UNIT=10, FILE="kpoints.integer")
    
    READ(10,*) NIRpts
    READ(UNIT=10,FMT="(A10,3I6)") charTemp, intTemp(1:3)
    IF (charTemp .NE. "Divide by:") THEN
       WRITE(*,FMT="(A10,A21)") charTemp, " should be Divide by:"
       WRITE(*,*) "Stopping"
       STOP 'Second line of kpoints.integer is not as expected.'
    END IF
    intTemp(1:3) = intTemp(1:3) + 1
    IF ( ALL( intTemp .NE. (/N1old, N2old, N3old/) ) ) THEN
       WRITE(*,FMT="(3I6,A11,3I6)") intTemp(1:3), " should be ", (/N1old, N2old, N3old/)
       WRITE(*,*) "Stopping"
       STOP 'Second line of kpoints.integer does not conform.'
    END IF
    
    DO j=1,NIRpts
       READ(UNIT=10,FMT='(TR10,3I6)') IRpts(j,1:3)
       IRpts(j,1:3) = 2*IRpts(j,1:3)
    END DO
    CLOSE(10)
    
  END SUBROUTINE readKpointsInteger
  
  SUBROUTINE divideCubes
    USE Grid, ONLY : N1, N2, N3
    USE Grid, ONLY : gridCoordinates, gridPointer
    USE Grid, ONLY : reducibleWeights
    IMPLICIT NONE
    INTEGER :: iCube, M, iCounter
    INTEGER :: i, j, k, s
    INTEGER :: corn(27), cubeNew(8,8), tempPoint(1:3), tempPointer
    LOGICAL :: fileExists, foundNewPoint
    CHARACTER(LEN=1) :: passNumberChar
    CHARACTER(LEN=6) :: cubesFilename
    IF (debug) WRITE(*,*) "Program Flow: Entered divideCubes"
    
    ! We overwrite the cubesToDivide file with the new cubes from this pass.
    CALL intToChar(passNumber,passNumberChar)
    cubesFilename = "cubes"//TRIM(passNumberChar)
    INQUIRE(FILE=cubesFilename, EXIST=fileExists)
    IF (fileExists) THEN
       WRITE(*,*) ""
       WRITE(*,*) "      ERROR: File "//TRIM(cubesFilename)//" already exists."
       WRITE(*,*) "      STOPPING"
       WRITE(*,*) ""
    END IF
    
    OPEN(UNIT=10, FILE=cubesFilename, ACTION="WRITE")
    
    WRITE(10,*) 8*numberOfCubesToDivide
    
    DO iCube = 1, nCubes
       IF (divideThisCube(iCube)) THEN
          
          NcubeNew = NcubeNew + 8
          
          ! The array crn(1:27) holds the 27 grid points which constitute
          ! the corners of the 8 new cubes.  Here we get the 8 old grid
          ! points which define the old cube to be divided.
          corn(1)  = cubeCorners(iCube, 1)
          corn(3)  = cubeCorners(iCube, 2)
          corn(7)  = cubeCorners(iCube, 3)
          corn(9)  = cubeCorners(iCube, 4)
          corn(19) = cubeCorners(iCube, 5)
          corn(21) = cubeCorners(iCube, 6)
          corn(25) = cubeCorners(iCube, 7)
          corn(27) = cubeCorners(iCube, 8)
          
          iCounter = 0
          
!!!       WRITE(*,'(8I5)') cubeCorners(iCube,1:8)
          
          ! We define the 19 new grid points and check to ensure that
          ! they are not duplicates
          
          DO i=0,2
             DO j=0,2
                DO k=0,2
                   
                   iCounter = iCounter + 1
                   
                   ! check to avoid points that are corners of the large old cube
                   IF ( ANY((/i,j,k/).EQ.(/1,1,1/)) ) THEN
                      
                      foundNewPoint = .TRUE.
                      
                      ! setup the potentially new point in tempPoint and tempPointer
                      tempPoint(1:3) = gridCoordinates(cubeCorners(iCube,1),1:3) &
                           + (/i,j,k/)
                      tempPointer = 1 + tempPoint(3) + N3*tempPoint(2) &
                           + N2*N3*tempPoint(1)
                      ! A new point cannot be in the original set of kpoints from the
                      ! previous pass, but a new point could be a duplicate from another
                      ! cube that was divided on this pass, so we test that the current
                      ! point is not a duplicate.
                      !
                      ! This is an order NptsNew^2 procedure.  If the code runs too
                      ! slow, this should be changed.
                      DO s = Npts+1, Npts+NptsNew
                         IF ( gridPointer(s,1) .EQ. tempPointer ) THEN
                            foundNewPoint = .FALSE.
                            corn(iCounter) = s
                            EXIT
                         END IF
                      END DO
                      
                      IF (foundNewPoint) THEN
                         NptsNew = NptsNew+1
                         M = Npts + NptsNew
                         gridCoordinates(M,1:3) = tempPoint(1:3)
!!!                      WRITE(*,*) gridCoordinates(M,1:3)
                         gridPointer(M,1) = tempPointer
                         corn(iCounter) = M
                      END IF
                      
                   END IF
                END DO
             END DO
          END DO
          
          cubeNew(1,1:8) = &
               (/corn(1),corn(2),corn(4),corn(5),corn(10),corn(11),corn(13),corn(14)/)
          cubeNew(2,1:8) = &
               (/corn(2),corn(3),corn(5),corn(6),corn(11),corn(12),corn(14),corn(15)/)
          cubeNew(3,1:8) = &
               (/corn(4),corn(5),corn(7),corn(8),corn(13),corn(14),corn(16),corn(17)/)
          cubeNew(4,1:8) = &
               (/corn(5),corn(6),corn(8),corn(9),corn(14),corn(15),corn(17),corn(18)/)
          cubeNew(5,1:8) = &
               (/corn(10),corn(11),corn(13),corn(14),corn(19),corn(20),corn(22),corn(23)/)
          cubeNew(6,1:8) = &
               (/corn(11),corn(12),corn(14),corn(15),corn(20),corn(21),corn(23),corn(24)/)
          cubeNew(7,1:8) = &
               (/corn(13),corn(14),corn(16),corn(17),corn(22),corn(23),corn(25),corn(26)/)
          cubeNew(8,1:8) = &
               (/corn(14),corn(15),corn(17),corn(18),corn(23),corn(24),corn(26),corn(27)/)
          
          ! Loop over the new cubes and tally up the weights of the k-points.
          DO i=1,8
             DO j=1,8
                reducibleWeights( cubeNew(i,j) ) = reducibleWeights( cubeNew(i,j) ) &
                     + (0.125d0)**(passNumber)
             END DO
          END DO
          
          ! Write the corners of the new cubes to the file cubes
          DO j=1,8
             WRITE(UNIT=10, FMT='(8I8)') cubeNew(j,1:8)
          END DO
          
       END IF ! IF(divideThisCube(iCube))
    END DO ! iCube
    
    WRITE(*,*) "reducible weights:"
    WRITE(*,*) reducibleWeights(:)
    WRITE(*,*) "sum of reducible weights", SUM(reducibleWeights(:))
    
    WRITE(*,*) "Npts: ", Npts
    WRITE(*,*) "NptsNew", NptsNew
    
    DEALLOCATE ( divideThisCube )
    DEALLOCATE ( cubeCorners )
    
!!!!!!  SHOULD ALLOCATE THINGS HERE TO NptsNew NOT sizeEstimate
    
    WRITE(*,*) "Number of points ", M
    
  END SUBROUTINE divideCubes
  
  SUBROUTINE reduceNewPoints
    ! A symmetry reduction of the new grid points is now done
!!!    USE Global, ONLY : NIRpts, IRpts, G, nSym
    USE Symmetries, ONLY : G, nSym
    USE Global, ONLY : NIRpts
    USE Grid, ONLY : gridCoordinates, gridPointer, N1, N2, N3, shift
    USE Grid, ONLY : reducibleWeights, weights, multiplicity
    IMPLICIT NONE
    INTEGER :: i, j, k, N, trans(3)
    INTEGER :: newPoint
    LOGICAL :: foundEquivalentPoint
    IF ( debug ) WRITE(*,*) "Program Flow: Entered reduceNewPoints"
    
    ! We only loop over the new points. It is not possible for any
    ! old grid point to be equivalent to a new one.
    
    DO i = nPts+1, npts+nPtsNew
       foundEquivalentPoint = .false.
       
       ! Loop over symmetry matrices
       DO j=1,nSym
          ! Transform grid point i with symmetry matrix j
          
          trans = MATMUL( G(j)%el, gridCoordinates(i,1:3))
          
          ! Shift the transformed point back into the primitive cell
          CALL SHIFT( trans )
          
          ! Determine the index of this new point
          N = trans(3) + N3*trans(2) + N2*N3*trans(1) + 1
          
          ! Compare with all the new points from nPts to i-1
          DO k = nPts+1, i-1
             IF (N.EQ.gridPointer(k,1)) THEN
                ! found that popint with index N is reducible to
                ! point with index gridPointer(k,1)
             END IF
          END DO
          
          ! Compare with the current set of irreducible points
          DO k = NIRpts + 1, NIRpts + NIRptsNew
             IF (N.EQ.IRptsPointer(k)) THEN
                gridPointer(i,1) = k
                gridPointer(i,2) = j
                
                multiplicity(k) = multiplicity(k) + 1
                
                foundEquivalentPoint = .TRUE.
                weights(k) = weights(k) + reducibleWeights(i)
                WRITE(*,*) "FOUND EQUIVALENT POINT", weights(k), reducibleWeights(i)
                EXIT
             END IF
          END DO
          
          IF (foundEquivalentPoint) EXIT
          
       END DO
       
       ! If the point was not reduced then add it to the list of
       ! irreducible points
       IF (.NOT.foundEquivalentPoint) THEN
          NIRPtsNew = NIRPtsNew + 1
          newPoint = NIRpts + NIRptsNew
          IRpts(newPoint,1:3) = gridCoordinates(i,1:3)
!!!          WRITE(*,*) "newPoint ", newPoint, IRpts(newPoint,1:3)
          N = 1 + IRpts(newPoint,3) + N3*IRpts(newPoint,2) + N2*N3*IRpts(newPoint,1)
          IRptsPointer(newPoint) = N
          gridPointer(i,1) = newPoint
          weights(newPoint) = reducibleWeights(i)
          multiplicity(newPoint) = 1
          WRITE(*,*) "FOUND EQUIVALENT POINT", weights(k), reducibleWeights(i)
       END IF
       
    END DO
    
    ! SHOULD REALLOCATE THE ARRAYS HERE.
    
    WRITE(*,*) "multiplicities:"
    WRITE(*,*) multiplicity(:)
    WRITE(*,*) "sum of multiplicity", SUM(multiplicity(:))
    
    DEALLOCATE ( IRptsPointer )
    DEALLOCATE ( reducibleWeights )
  END SUBROUTINE reduceNewPoints
  
  SUBROUTINE printNextPassResults
!!!    USE Global, ONLY : NIRpts, IRpts
    USE Global, ONLY : NIRpts
    USE Global, ONLY : PI, b1, b2, b3
    USE StructFile, ONLY : a1, a2, a3, ortho
    USE Grid, ONLY : N1, N2, N3
    USE Grid, ONLY : gridCoordinates, gridPointer
    USE Grid, ONLY : weights
    IMPLICIT NONE
    
    INTEGER :: i, itmp(3), divisor
    DOUBLE PRECISION :: tmp(3)
    IF (debug) WRITE(*,*) "Pogram Flow: Entered printNextPassResults"
    
!!!    WRITE(*,*) SIZE(IRpts(:,1)), SIZE(IRpts(:,2)), SIZE(IRpts(:,3))
!!!    WRITE(*,*) IRpts(36,1)
    
    OPEN (UNIT=10, FILE="kpoints.map", ACTION="WRITE")
    WRITE(10,*) Npts + NptsNew
    ! Print k-point map
    DO i = 1, Npts + NptsNew
       WRITE(UNIT=10,FMT='(3I8,2I10)')  gridCoordinates(i,1:3), gridPointer(i,1:2)
    END DO
    CLOSE(10)
    
    ! Print the grid divisions
    OPEN (UNIT=11, FILE="gridNew", ACTION="WRITE")
    WRITE(UNIT=11,FMT=*) N1, N2, N3
    CLOSE(11)
    
    ! Print kpoints in integer form
    OPEN(UNIT=10, FILE="kpoints.integer", ACTION="WRITE")
    WRITE(UNIT=10, FMT='(I10)') NIRpts + NIRptsNew
    WRITE(UNIT=10, FMT='(A10,3I6)') "Divide by:", N1-1, N2-1, N3-1
    DO i = 1, NIRpts + NIRpts+NIRptsNew
       WRITE(UNIT=10,FMT='(TR10,3I6)') IRpts(i,1:3)
    END DO
    CLOSE(10)
    
    OPEN(UNIT=12, FILE="kpoints.wien2k", ACTION="WRITE")
    DO i = NIRpts + 1, NIRpts + NIRptsNew
       
       tmp =  ( IRpts(i,1) / REAL(N1-1) ) * b1 &
            + ( IRpts(i,2) / REAL(N2-1) ) * b2 &
            + ( IRpts(i,3) / REAL(N3-1) ) * b3
       
       ! Print integer representation of k-points in WIEN coordinates
       
       IF (ortho) THEN
          tmp(1) = tmp(1)*a1/(2.d0*PI)*REAL(N1-1)*REAL(N2-1)*REAL(N3-1)
          tmp(2) = tmp(2)*a2/(2.d0*PI)*REAL(N1-1)*REAL(N2-1)*REAL(N3-1)
          tmp(3) = tmp(3)*a3/(2.d0*PI)*REAL(N1-1)*REAL(N2-1)*REAL(N3-1)
          itmp(:) = NINT(tmp(:))
       ELSE
          itmp(1) = IRpts(i,1)*(N2-1)*(N3-1)
          itmp(2) = IRpts(i,2)*(N1-1)*(N3-1)
          itmp(3) = IRpts(i,3)*(N1-1)*(N2-1)
       END IF
       divisor = (N1-1)*(N2-1)*(N3-1)
       CALL reducePoint(itmp,divisor)
       WRITE(UNIT=12,FMT='(I10,4I5,F9.5)') i, itmp(1:3), divisor, weights(i)
    END DO
    
    WRITE(UNIT=10,FMT='(A3)') "END"
    
    CLOSE(10)
    
  END SUBROUTINE printNextPassResults
  
END MODULE NextPassMod
