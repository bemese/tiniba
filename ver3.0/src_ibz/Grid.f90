MODULE Grid
!!! This module contains the variables and subroutines
!!! needed for working with the k-space grid
  
  USE CommandLineArguments, ONLY : firstPass
  USE Global, ONLY : debug
  USE MyAllocateMod, ONLY : myAllocate
!!!!  USE Global, ONLY : IRpts
  
  IMPLICIT NONE
  
  ! N1, N2, and N3 are the grid dimensions.
  INTEGER :: N1, N2, N3
  
  ! Npts is the total number of pts (in firstPass it is N1*N2*N3)
  INTEGER :: Npts
  
  ! Ndiv(1:3) is the number of divisions along each reciprocal
  ! lattice vector.  In firstPass it is (/N1-1, N2-1, N3-1/)
  INTEGER :: Ndiv(1:3)
  
  ! gridPointer holds the map between grid points and irreducible
  ! points as well as which symmetry matrix that relates the two.
  ! The first argument to gridPointer is the kpoint index on the
  ! original mesh, and the value of gridPointer is the kpoint index
  ! that point is associated with.
  INTEGER, ALLOCATABLE :: gridPointer(:,:)
  
  ! gridCoordinates holds the grid points for the entire mesh over
  ! the BZ.
  INTEGER, ALLOCATABLE :: gridCoordinates(:,:)
  
  ! weights holds the weight of each kpoint.  This is first
  ! allocated to the size of the entire reducable grid array
  ! Later, it is reduced to just the irreducible kpoints, and
  ! the weights are combined to give an effective weight.
  DOUBLE PRECISION, ALLOCATABLE :: weights(:)
  DOUBLE PRECISION, ALLOCATABLE :: reducibleWeights(:)
  
  ! multiplicity holds the number of reducible points
  ! that are mapped to a given irreducible point, including
  ! the irreducible point itself.
  INTEGER, ALLOCATABLE :: multiplicity(:)
  
  ! logical irreducible is a flag that tells you whether a kpoint
  ! is reducible or not.  If it is true, then it is irreducible.
  LOGICAL, ALLOCATABLE :: irreducible(:)
  
  ! irrptPointer(i) is an integer array that for the i-th irreducible
  ! k-point gives a corresponding k-point index in the full grid.
  INTEGER, ALLOCATABLE :: irrptPointer(:)
  
CONTAINS
  
  SUBROUTINE initializeGrid
    IMPLICIT NONE
    INTEGER :: i, j, k, M, p
    INTEGER :: corn(1:8)
    INTEGER :: iostatus
    CHARACTER(LEN=6) :: cubesfilename
    
    IF (debug) WRITE(*,*) "Program Flow: Entered initializeGrid"
    
    Npts = N1*N2*N3
    Ndiv(1) = N1-1
    Ndiv(2) = N2-1
    Ndiv(3) = N3-1
    
    ! Allocate necesseray arrays.  Set array dimensions for
    ! point reduction
    CALL myAllocate ( reducibleWeights, "reducibleWeights", Npts )
    
    CALL myAllocate ( irreducible, "irreducible", Npts )
    
    ! We initialize all the points as irreducible points.  In
    ! subroutine transformGrid we find out which points are
    ! reducible.
    irreducible(:) = .TRUE.
    
    CALL myAllocate ( gridCoordinates, "gridCoordinates", Npts, 3 )
    
    ! Divide the k-space primitive cell into an N1 * N2 * N3 grid
    DO i = 0, Ndiv(1)
       DO j = 0, Ndiv(2)
          DO k = 0, Ndiv(3)
             M = k + (j * N3) + (i * N3 * N2) + 1  ! k-point index
             gridCoordinates(M,1:3) = (/i, j, k/)
          END DO
       END DO
    END DO
    
    ! Initialize pointer array: each point is associated with itself.
    ! Later, each point will be associated with an equivalent
    ! irreducible point.
    CALL myAllocate ( gridPointer, "gridPointer", Npts, 2 )
    
    DO i = 1, Npts
       gridPointer(i,1) = i
    END DO
    
    ! Write out cubes file.  Only needed if this is a firstPass in a
    ! FS calculation
    
    cubesfilename = "cubes0"
    IF (firstPass) THEN
       IF ( debug ) WRITE(*,*) "File Control: Opening "//TRIM(cubesfilename)//" file to write."
       OPEN (UNIT=14, FILE="cubes0", ACTION="WRITE", IOSTAT=iostatus)
       IF (iostatus .NE. 0) THEN
          WRITE(*,*) "Could not open file: "//TRIM(cubesfilename)//".  Stopping"
          STOP 'Could not open file.  Stopping'
       END IF
       WRITE(14,*) Ndiv(1)*Ndiv(2)*Ndiv(3)
    END IF
    
    DO i = 0, Ndiv(1)-1
       DO j = 0, Ndiv(2)-1
          DO k = 0, Ndiv(3)-1
             
             M = k + (j * N3) + (i * N3 * N2) + 1
             
             ! Define the corners of the cell
             corn(1) = M
             corn(2) = M + 1
             corn(3) = M + N3
             corn(4) = M + N3 + 1
             corn(5) = M + N2*N3
             corn(6) = M + N2*N3 + 1
             corn(7) = M + N2*N3 + N3
             corn(8) = M + N2*N3 + N3 + 1
             
             IF ( firstPass ) THEN
                ! Write cubes
                WRITE(UNIT=14, FMT='(8I8)') corn(1:8)
             END IF
             
             ! Calculate weights
             DO p = 1, 8
                reducibleWeights( corn(p) ) = reducibleWeights( corn(p) ) + 0.125d0
             END DO
             
          END DO
       END DO
    END DO
    
    IF ( firstPass ) THEN
       IF ( debug ) WRITE(*,*) "File Control: Closing cubes file."
       CLOSE(14)
    END IF
    
!!!    WRITE(*,*) reducibleWeights(:)
    
  END SUBROUTINE initializeGrid
  
  SUBROUTINE transformGrid ()
    ! Use symmetry to map submesh onto itself, keeping track
    ! of the map in the array gridPointer
    USE Symmetries, ONLY : nSym, G
    IMPLICIT NONE
    
    ! Local variables, TRANS holds a transformed k point
    INTEGER :: i, j, N, trans(3)
    INTEGER, ALLOCATABLE :: mappedPoint(:)
    
    IF ( debug ) WRITE(*,*) "Program Flow: Entered transformGrid"
    
    ALLOCATE ( mappedPoint(NSym) )
    
    ! Loop over k-points
    
    DO i = 2, Npts
       ! Loop over symmetry matrices
       DO j = 1, NSym
          ! Transform grid point i with symmetry matrix j.
          trans = MATMUL( G(j)%el, gridCoordinates(i,1:3) )
          ! Shift the transformed vector back into the primitive cell.
          CALL shift(trans)
          ! Determine the index of this new point.
          N = trans(3) + ( trans(2) * N3 ) + ( trans(1) * N3 * N2 ) + 1
          ! Collect the star of kpoint i into mappedPoint.
          mappedPoint(j) = N
          ! One could exit the loop as soon as a related point
          ! is found, but this is a little cleaner.
       END DO
       ! Find the lowest index of all symmetry related points.
       N = MINVAL(mappedPoint)
       ! If the transformed point has an index lower than the original
       ! point we keep track of the map between these points and the
       ! first symmetry matrix that connects the two points.
       IF (N.LT.i) THEN
          ! The point i is said to be reducible to point N.
!!!          WRITE(*,*) i, N
          irreducible(i) = .FALSE.
          gridPointer(i,1) = gridPointer(N,1)
          ! Store a symmetry matrix index in the second array variable.
          gridPointer(i,2) = MINLOC( mappedPoint, DIM=1 )
       END IF
    END DO
    DEALLOCATE ( mappedPoint )
    
    IF ( debug ) WRITE(*,*) "Program Flow: Exiting transformGrid"
    
  END SUBROUTINE transformGrid
  
  SUBROUTINE reduceGrid ()
    ! Find an irreducible set of k points
    USE GLOBAL, ONLY : NIRpts, IRpts
    IMPLICIT NONE
    
    ! Local variables
    INTEGER :: i, itmp
    INTEGER :: irrpntsCounter = 0
    
    IF ( debug ) WRITE(*,*) "Program Flow: Entered getIRpts"
    
    NIRpts = 0
    
    ! Find the number of irreducible points.
    DO i = 1, Npts
       IF (irreducible(i)) THEN
          NIRpts = NIRpts + 1
       END IF
    END DO
    WRITE(*,*) "Number of irreducible points: ", NIRpts
    
    CALL myAllocate ( IRpts, "IRpts", NIRpts, 3 )
    CALL myAllocate ( irrptPointer, "irrptPointer", NIRpts )
    
    ! Loop over the k-points
    DO i = 1, Npts
       
       ! If the point was only mapped to itself then it is
       ! taken as an irreducible point
       IF (irreducible(i)) THEN
          ! Increase the number of irreducible points by 1
          irrpntsCounter = irrpntsCounter + 1
          gridPointer(i,1) = irrpntsCounter
          irrptPointer(irrpntsCounter) = i
       ELSE
          ! Renumber the map so that gridPointer now connects
          ! the original k point with the irreducible k point
          itmp = gridPointer(i,1)
          gridPointer(i,1) = gridPointer(itmp,1)
       END IF
    END DO
    
    DO i = 1, Npts
       ! For each irreducible point, we can chose from any of
       ! its "star of k-points".  To be consistent with Bloechl's
       ! original prescription, and the above, we use the following
       ! choice.
       IF (irreducible(i)) THEN
!!!       WRITE(*,*) i, gridPointer(i,1)
          IRpts(gridPointer(i,1), 1:3) = gridCoordinates(i,1:3)
       END IF
    END DO
    
    ! Get multiplicity and weights
    CALL myAllocate ( multiplicity, "multiplicity", NIRpts )
    CALL myAllocate ( weights, "weights", NIRpts )
    
    !  Loop over the k-points
    DO i = 1, Npts
       ! Increase the multiplicity of the irreducible point by 1.
       multiplicity( gridPointer(i,1) ) = &
            multiplicity( gridPointer(i,1) ) + 1
       
       ! Add the weights of the reducible point to the irreducible point.
       ! Reducible points can have different weights.
       weights(gridPointer(i,1)) = weights(gridPointer(i,1)) &
            + reducibleWeights(i)
    END DO
    DEALLOCATE ( reducibleWeights )
    DEALLOCATE ( irreducible )
    
  END SUBROUTINE reduceGrid
  
  SUBROUTINE shift (vec)
    ! Shifts a vector back into the primitive cell
    IMPLICIT NONE
    
    ! Calling arguments
    INTEGER, INTENT(IN OUT) :: vec(3) !Vector to shift
    
    ! Local variables
    INTEGER :: N(1:3) !Grid dimensions
    INTEGER :: i
    
    N(1:3) = (/ N1, N2, N3 /)
    ! Loop over the components of the vector checking if they are outside of the
    ! primitive cell and shifting them by a lattice vector if they are
    DO i = 1, 3
       DO WHILE ( vec(i) < 0 .OR. vec(i) >= ( N(i) - 1) )
          
          IF ( vec(i) < 0 ) THEN
             vec(i) = vec(i) + ( N(i) - 1 )
          END IF
          
          IF ( vec(i) >= (N(i) - 1) ) THEN
             vec(i) = vec(i) - ( N(i) - 1 )
          END IF
          
       END DO
    END DO
  END SUBROUTINE shift
  
END MODULE Grid
