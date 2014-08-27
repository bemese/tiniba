SUBROUTINE getIRtet ()
  ! Divide each primitive cell of the submesh into 6 equal
  ! volume tetrahedra, using the shortest main diagonal as a
  ! common edge of all 6, write the corners in terms of the
  ! irreducible points.  Put the corners in ascending order,
  ! then compare tetrahedra to find an irreducible set.
  ! USE GLOBAL, ONLY : TET
  USE GLOBAL, ONLY : IRtet, tetrahedronMultiplicity
  USE Global, ONLY : NIRtet
  USE GLOBAl, ONLY : debug
  USE GRID, ONLY : N1, N2, N3, gridPointer
  
  IMPLICIT NONE
  
  ! Local variables
  INTEGER :: i, j, k, p, q, M, CORN(8)
  LOGICAL :: REDUCED
  
  ! Array TET() temporarily holds the six tetrahedra that
  ! are created from a submesh cell.
  INTEGER, ALLOCATABLE :: TET(:,:)
  INTEGER, ALLOCAtABLE :: TET1(:)
  INTEGER :: istat
  INTEGER :: numberReducibleTetrahedra
  INTEGER, ALLOCATABLE :: redTet(:,:)  ! reducible Tetrahedra
  INTEGER, ALLOCATABLE :: redTetMult(:)
  
  debug = .false.
  
  IF ( debug ) WRITE(*,*) "Program Flow: Entered getIRtet"
  
  !Set array dimensions for tetrahedra reduction
!!!  ALLOCATE (IRtet((N1-1)*(N2-1)*(N3-1)*6))
  M = (N1-1)*(N2-1)*(N3-1)*6
  
  ALLOCATE (redTet(M,4))
  redTet(1:M,1:4) = 0
  numberReducibleTetrahedra = 0
  
  ALLOCATE(TET(4,6))
  M = 0
  ! Loop over submesh cells
  DO i = 0, (N1-2)
     DO j = 0, (N2-2)
        DO k = 0, (N3-2)
           
           ! M is the index of the bottom left corner of the submesh cell
           M = k + (j * N3) + (i * N3 * N2) + 1
           
           ! Define the corners of the cell
           CORN(1) = M
           CORN(2) = M + 1
           CORN(3) = M + N3
           CORN(4) = M + N3 + 1
           CORN(5) = M + N2*N3
           CORN(6) = M + N2*N3 + 1
           CORN(7) = M + N2*N3 + N3
           CORN(8) = M + N2*N3 + N3 + 1
           
           ! Define the tetrahedra
           CALL getTET (CORN, TET)
           
           DO p=1,6
              numberReducibleTetrahedra = numberReducibleTetrahedra + 1
              redTet(numberReducibleTetrahedra,1:4) = TET(1:4,p)
           END DO
           
        END DO
     END DO
  END DO
  DEALLOCATE( tet )
  
!!! New algorithm to reduce tetrahedra.
!!! First sort, then compare.
  
  CALL sortTetrahedra(redTet, numberReducibleTetrahedra)
  
!!! Check that the elements are properly sorted
  CALL checks ( redTet, numberReducibleTetrahedra )
  
  M = (N1-1)*(N2-1)*(N3-1)*6
  
  ALLOCATE ( TET1(4) )
  ALLOCATE (redTetMult(M), STAT=istat)
  IF (istat .NE. 0) THEN
     WRITE(*,*) " "
     WRITE(*,*) "Error in getIRtet: Could not allocate redTetMult"
     WRITE(*,*) "Attempted to allocate size: ", M
     WRITE(*,*) "Stopping"
     STOP "Error allocating array redTetMult"
  END IF
  redTetMult(1:M) = 0
  
  i = 1
  tet1(1:4) = redTet( i, 1:4)
  redTetMult(i) = 1
  nIRtet = 1
  
  DO q=2, numberReducibleTetrahedra
     IF ( ALL(redTet(q,1:4)==tet1(1:4)) ) THEN
        ! then tetrahedra q is reducible
        redTetMult(i) = redTetMult(i) + 1
        redTetMult(q) = 0
     ELSE
        i=q
        nIRTet = nIRTet + 1
        redTetMult(i) = 1
        tet1(1:4) = redTet(q,1:4)
     END IF
  END DO
  
!!!  WRITE(*,*) redTetMult
  
  ! NIRtet is now the number of irreducible tetrahedra
  WRITE(*,*) "Number of irreducible tetrahedra is ", NIRtet
  
!!! Now collect only the irreducible ones into IRtet
  ALLOCATE (IRtet(4,NIRtet))
!!! Note that the dimension of IRtet are transposed from redTet!!
  IRtet(1:4,1:NIRtet) = 0
  ALLOCATE (tetrahedronMultiplicity(NIRtet))
  tetrahedronMultiplicity(1:NIRtet) = 0
  
  M = 0
  DO q = 1, numberReducibleTetrahedra
     IF (redTetMult(q) .NE. 0) THEN
        ! q represents an irreducible tetrahedron
        M = M + 1
!!!        WRITE(*,*) M
        IRtet(1:4,M) = redTet(q,1:4)
        tetrahedronMultiplicity(M) = redTetMult(q)
     END IF
  END DO
  
  IF (M.NE.NIRTET) THEN
     WRITE(*,*) " "
     WRITE(*,*) "The generated tetrahedra have an error. "
     WRITE(*,*) " "
     STOP 'Consistency check on nmumber of tetrahedra generated failed.'
  END IF
  
!!!!  Replace everything inside this do loop with a new
!!!!  algorithm to search for the tetrahedra.
!!!!           ! Loop over the newly created tetrahedra
!!!!           DO p = 1, 6
!!!!              
!!!!              REDUCED = .FALSE.
!!!!              
!!!!              ! Loop over the irreducible tetrahedra
!!!!              DO q = 1, NIRtet 
!!!!                 
!!!!                 ! If the two tetrahedra are equivalent, increase
!!!!                 ! the multiplicity of the irreducible tetrahedra by 1
!!!!                 IF ( (TET(4,p).EQ.IRtet(q)%crn(4)).AND.  &
!!!!                      (TET(3,p).EQ.IRtet(q)%crn(3)).AND.  &
!!!!                      (TET(2,p).EQ.IRtet(q)%crn(2)).AND.  &
!!!!                      (TET(1,p).EQ.IRtet(q)%crn(1)) ) THEN
!!!!                      
!!!!                    IRtet(q)%mult = IRtet(q)%mult + 1
!!!!                    
!!!!                 IF ( (TET(1,p).EQ.IRtet(1,q)).AND.  &
!!!!                      (TET(2,p).EQ.IRtet(2,q)).AND.  &
!!!!                      (TET(3,p).EQ.IRtet(3,q)).AND.  &
!!!!                      (TET(4,p).EQ.IRtet(4,q)) ) THEN
!!!!            
!!!!               tetrahedronMultiplicity(q) = tetrahedronMultiplicity(q) + 1
!!!!                
!!!!                REDUCED = .TRUE.
!!!!                    
!!!!                    EXIT
!!!!                    
!!!!                 END IF
!!!!              END DO
!!!!              
!!!!              ! If the tetrahedra was not reduced then add it to the
!!!!              ! list of irreducible tetrahedra
!!!!              IF (.NOT.REDUCED) THEN
!!!!                 NIRtet = NIRtet + 1
!!!!                 IRtet(NIRtet)%crn = TET(1:4,p)
!!!!                 IRtet(NIRtet)%mult = 1
!!!!                 IRtet(1:4,NIRtet) = TET(1:4,p)
!!!!                 tetrahedronMultiplicity(NIRtet) = 1
!!!!              END IF
!!!!           END DO ! p
END SUBROUTINE getIRtet

SUBROUTINE sortTetrahedra(mArr,M)
  USE Sorting, ONLY : indx, initializeIndex, destroyIndex
  USE Sorting, ONLY : quickSortIndex, partitionIndex
  !
  ! The tetrahedra are a collection of M quartets of integers.
  ! Each tetrahedra is supposed to be ordered from left to right
  ! allready.
  ! For example, if M=5, we could have something like
  !  1  3  4  4
  !  1  3  3  4
  !  3  4  5  6 
  !  3  3  4  5
  !  1  2  3  4
  ! as input.  Note that within each row the numbers are in
  ! ascending order.
  !
  ! The subroutine orders these tetrahedra in a simple way, so
  ! that they can be reduced later.
  ! 
  ! The subroutine first orders the quartets by the first integer.
  ! So, with the above exmaple, the array becomes
  !  1  3  4  4
  !  1  3  3  4
  !  1  2  3  4
  !  3  4  5  6 
  !  3  3  4  5
  ! 
  ! The subroutine then only rearranges the subgroups that have the
  ! same first integer.  There are two such subgroups in our example.
  ! The ordering is on the second integer.  For our example, this gives
  !  1  2  3  4
  !  1  3  4  4
  !  1  3  3  4
  !  3  3  4  5
  !  3  4  5  6 
  !
  ! This ordering is done for the third and fourth columns, making
  ! sure that all the integers to left are the same.  For the
  ! example, this finally gives
  !  1  2  3  4
  !  1  3  3  4
  !  1  3  4  4
  !  3  3  4  5
  !  3  4  5  6 
  !
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: M
  INTEGER, INTENT(INOUT) :: mArr(M,4)
!!!  M  --> numberReducibleTetrahedra
!!!  mArr(M,4) -->  redTet( numberReducibleTetrahedra, 4 )
  INTEGER, ALLOCATABLE :: iArr(:)
  INTEGER, ALLOCATABLE :: subArray(:,:)
  REAL :: timing
  INTEGER :: iCount, lBound, uBound
  INTEGER :: i, j, iColumn
  INTEGER, ALLOCATABLE :: iTmp(:)
  LOGICAL :: sortFlag
  
  sortFlag = .false.
  
  CALL cpu_time(timing)
  WRITE(*,*) "Start:", timing
  
!  DO i=1,M
!     WRITE(*,*) mArr(i,1:4)
!  END DO
  
  CALL initializeIndex(M)
  
  ALLOCATE(iArr(M))
  iArr(1:M) = mArr(1:M,1)
  CALL quickSortIndex( iArr, indx, 1, M)
  
  CALL cpu_time(timing)
  WRITE(*,*) M, timing
  
  ! Now that iArr is arranged, and we have an indx, rearrange
  ! the entries of mArr
  mArr(:,1) = iArr(:)
  DO i=2,4
     iArr(:) = mArr(indx(:), i)
     mArr(:,i) = iArr(:)
  END DO
  
!!!  CALL cpu_time(timing)
!!!  WRITE(*,*) M, timing
  
  DEALLOCATE( iArr )
  CALL destroyIndex
  
  ! Now sort each subset of the array (the subset that has the
  ! same first column) by the second column, and so on.
  
  DO iColumn = 1, 3
     
     ALLOCATE(iTmp(1:iColumn))
     iTmp = mArr(1,1:iColumn)
     iCount = 1
     lBound = 1
     DO i=2, M
        
        ! IF (debugDeep) WRITE(*,*) i, iTmp, mArr(i,iColumn)
        
        ! Check that all elements to the left of the column
        ! we want to sort on are the same
        
        IF (ALL(mArr(i,1:iColumn)==iTmp(1:iColumn))) THEN
           
           iCount = iCount + 1
           sortFlag = .false.
           
        ELSE
            
           sortFlag = .true.
           
        END IF
        
        IF (i.EQ.M) THEN
           ! Make sure we sort if we reach the last element
           
           sortFlag = .true.
           
        END IF
        
        IF (sortFlag) THEN
           ! The index mArr(i,iColumn) is different than mArr(i-1,iColumn).
           ! The array we want to sort goes from lBound to i-1.
           ! We sort the list on the second entry.
           IF ( iCount .EQ. 1) THEN
              ! there is only one element.  No need to sort.
!!!         iTmp = mArr(i,iColumn)
!!!         lBound = i
!!!         iCount = 1
           
           ELSE IF (iCount .GT. 1) THEN
              ! Allocate a temporary array, subArray, of
              ! size iCount*(4-iColumn)
              ALLOCATE( subArray(1:iCount,1:4-iColumn) )
              
              ! Allocate and initialize the indexing array
              CALL initializeIndex(iCount)
              
              ! Transfer the required elements into subArray
              uBound = lBound + iCount - 1
              subArray(1:iCount, 1:4-iColumn) = mArr(lBound:uBound,iColumn+1:4)
              
              ! Sort the array on the first entry
              ALLOCATE(iArr(iCount))
              iArr(1:iCount) = subArray(1:iCount,1)
              CALL quickSortIndex(iArr, indx, 1, SIZE(iArr) )
              
              ! Now that we have the indx rearrange the entries
              subArray(1:iCount,1) = iArr(1:iCount)
              DO j=2, 4-iColumn
!!!              DO j=1, 4-iColumn
                 iArr(:) = subArray(indx(:), j)
                 subArray(:,j) = iArr(:)
              END DO
              
              DEALLOCATE( iArr )
              CALL destroyIndex
              ! Sub array is now sorted. Merge subArray back
              ! into the full array
              mArr(lBound:uBound, iColumn+1:4) = subArray(1:iCount,1:4-iColumn)
              
              DEALLOCATE( subArray )
              
           ELSE
              ! there is something wrong
              STOP 'Unexpected error when sorting tetrahedra.'
           END IF
           
           ! Reset counter
           iCount = 1
           
           ! Reset iTmp
           iTmp = mArr(i,1:iColumn)
           
           ! Reset lower bound lBound
           lBound = i
           
        END IF
     END DO
     
     DEALLOCATE( iTmp )
     
  END DO
  
END SUBROUTINE sortTetrahedra

!Divide a submesh cell into 6 equal volume tetrahedra, using
!the shortest main diagonal as a common edge of all 6.  Write
!tetrahedra corners in terms of irreducible k-points and put in
!ascending order with respect to index.
SUBROUTINE getTET (CORN, TET)
  USE GLOBAL, ONLY : cell_type, debug
  USE Grid, ONLY : gridPointer
  IMPLICIT NONE
  
  !Calling arguments
  INTEGER, INTENT(IN) :: CORN(8)
  INTEGER, INTENT(OUT) :: TET(4,6)
  
  ! IF (debug) WRITE(*,*) "(Program Flow): Entered getTET"
  
  !Case: (0,1,0) -> (1,0,1) diagonal shortest
  IF (cell_type == 1) THEN
     
     TET(1,1) = gridPointer( CORN(2) , 1)
     TET(2,1) = gridPointer( CORN(3) , 1)
     TET(3,1) = gridPointer( CORN(4) , 1)
     TET(4,1) = gridPointer( CORN(6) , 1)
     CALL ORDER (1, TET)
     
     TET(1,2) = gridPointer( CORN(1) , 1)
     TET(2,2) = gridPointer( CORN(3) , 1)
     TET(3,2) = gridPointer( CORN(5) , 1)
     TET(4,2) = gridPointer( CORN(6) , 1)
     CALL ORDER (2, TET)
     
     TET(1,3) = gridPointer( CORN(1) , 1)
     TET(2,3) = gridPointer( CORN(2) , 1)
     TET(3,3) = gridPointer( CORN(3) , 1)
     TET(4,3) = gridPointer( CORN(6) , 1)
     CALL ORDER (3, TET)
     
     TET(1,4) = gridPointer( CORN(3) , 1)
     TET(2,4) = gridPointer( CORN(4) , 1)
     TET(3,4) = gridPointer( CORN(6) , 1)
     TET(4,4) = gridPointer( CORN(8) , 1)
     CALL ORDER (4, TET)
     
     TET(1,5) = gridPointer( CORN(3) , 1)
     TET(2,5) = gridPointer( CORN(5) , 1)
     TET(3,5) = gridPointer( CORN(6) , 1)
     TET(4,5) = gridPointer( CORN(7) , 1)
     CALL ORDER (5, TET)
     
     TET(1,6) = gridPointer( CORN(3) , 1)
     TET(2,6) = gridPointer( CORN(6) , 1)
     TET(3,6) = gridPointer( CORN(7) , 1)
     TET(4,6) = gridPointer( CORN(8) , 1)
     CALL ORDER (6, TET)
     
     !Case: (0,0,0) -> (1,1,1) diagonal shortest
  ELSE IF (cell_type == 2) THEN
     
     TET(1,1) = gridPointer( CORN(1) , 1)
     TET(2,1) = gridPointer( CORN(2) , 1)
     TET(3,1) = gridPointer( CORN(6) , 1)
     TET(4,1) = gridPointer( CORN(8) , 1)
     CALL ORDER (1, TET)
     
     TET(1,2) = gridPointer( CORN(1) , 1)
     TET(2,2) = gridPointer( CORN(5) , 1)
     TET(3,2) = gridPointer( CORN(7) , 1)
     TET(4,2) = gridPointer( CORN(8) , 1)
     CALL ORDER (2, TET)
     
     TET(1,3) = gridPointer( CORN(1) , 1)
     TET(2,3) = gridPointer( CORN(5) , 1)
     TET(3,3) = gridPointer( CORN(6) , 1)
     TET(4,3) = gridPointer( CORN(8) , 1)
     CALL ORDER (3, TET)
     
     TET(1,4) = gridPointer( CORN(1) , 1)
     TET(2,4) = gridPointer( CORN(2) , 1)
     TET(3,4) = gridPointer( CORN(4) , 1)
     TET(4,4) = gridPointer( CORN(8) , 1)
     CALL ORDER (4, TET)
     
     TET(1,5) = gridPointer( CORN(1) , 1)
     TET(2,5) = gridPointer( CORN(3) , 1)
     TET(3,5) = gridPointer( CORN(7) , 1)
     TET(4,5) = gridPointer( CORN(8) , 1)
     CALL ORDER (5, TET)
     
     TET(1,6) = gridPointer( CORN(1) , 1)
     TET(2,6) = gridPointer( CORN(3) , 1)
     TET(3,6) = gridPointer( CORN(4) , 1)
     TET(4,6) = gridPointer( CORN(8) , 1)
     CALL ORDER (6, TET)
     
     !Case: (1,1,0) -> (0,0,1) diagonal shortest
  ELSE IF (cell_type == 3) THEN
     
     TET(1,1) = gridPointer( CORN(2) , 1)
     TET(2,1) = gridPointer( CORN(6) , 1)
     TET(3,1) = gridPointer( CORN(7) , 1)
     TET(4,1) = gridPointer( CORN(8) , 1)
     CALL ORDER (1, TET)
     
     TET(1,2) = gridPointer( CORN(1) , 1)
     TET(2,2) = gridPointer( CORN(2) , 1)
     TET(3,2) = gridPointer( CORN(5) , 1)
     TET(4,2) = gridPointer( CORN(7) , 1)
     CALL ORDER (2, TET)
     
     TET(1,3) = gridPointer( CORN(2) , 1)
     TET(2,3) = gridPointer( CORN(5) , 1)
     TET(3,3) = gridPointer( CORN(6) , 1)
     TET(4,3) = gridPointer( CORN(7) , 1)
     CALL ORDER (3, TET)
     
     TET(1,4) = gridPointer( CORN(2) , 1)
     TET(2,4) = gridPointer( CORN(4) , 1)
     TET(3,4) = gridPointer( CORN(7) , 1)
     TET(4,4) = gridPointer( CORN(8) , 1)
     CALL ORDER (4, TET)
     
     TET(1,5) = gridPointer( CORN(1) , 1)
     TET(2,5) = gridPointer( CORN(2) , 1)
     TET(3,5) = gridPointer( CORN(3) , 1)
     TET(4,5) = gridPointer( CORN(7) , 1)
     CALL ORDER (5, TET)
     
     TET(1,6) = gridPointer( CORN(2) , 1)
     TET(2,6) = gridPointer( CORN(3) , 1)
     TET(3,6) = gridPointer( CORN(4) , 1)
     TET(4,6) = gridPointer( CORN(7) , 1)
     CALL ORDER (6, TET)
     
     !Case: (1,0,0) -> (0,1,1) diagonal shortest
  ELSE IF (cell_type == 4) THEN
     
     TET(1,1) = gridPointer( CORN(2) , 1)
     TET(2,1) = gridPointer( CORN(4) , 1)
     TET(3,1) = gridPointer( CORN(5) , 1)
     TET(4,1) = gridPointer( CORN(6) , 1)
     CALL ORDER (1, TET)
     
     TET(1,2) = gridPointer( CORN(1) , 1)
     TET(2,2) = gridPointer( CORN(3) , 1)
     TET(3,2) = gridPointer( CORN(4) , 1)
     TET(4,2) = gridPointer( CORN(5) , 1)
     CALL ORDER (2, TET)
     
     TET(1,3) = gridPointer( CORN(1) , 1)
     TET(2,3) = gridPointer( CORN(2) , 1)
     TET(3,3) = gridPointer( CORN(4) , 1)
     TET(4,3) = gridPointer( CORN(5) , 1)
     CALL ORDER (3, TET)
     
     TET(1,4) = gridPointer( CORN(4) , 1)
     TET(2,4) = gridPointer( CORN(5) , 1)
     TET(3,4) = gridPointer( CORN(6) , 1)
     TET(4,4) = gridPointer( CORN(8) , 1)
     CALL ORDER (4, TET)
     
     TET(1,5) = gridPointer( CORN(3) , 1)
     TET(2,5) = gridPointer( CORN(4) , 1)
     TET(3,5) = gridPointer( CORN(5) , 1)
     TET(4,5) = gridPointer( CORN(7) , 1)
     CALL ORDER (5, TET)
     
     TET(1,6) = gridPointer( CORN(4) , 1)
     TET(2,6) = gridPointer( CORN(5) , 1)
     TET(3,6) = gridPointer( CORN(7) , 1)
     TET(4,6) = gridPointer( CORN(8) , 1)
     CALL ORDER (6, TET)
     
  END IF
  
END SUBROUTINE getTET


SUBROUTINE ORDER (tetIndex, TET)
!!! Puts 4 integers in ascending order using insertion sort
!!!  USE GLOBAL, ONLY : TET
  IMPLICIT NONE
  
  ! Calling arguments
  INTEGER, INTENT(IN) :: tetIndex
  INTEGER, INTENT(INOUT) :: TET(4,6)
  
  ! Local varibles
  INTEGER :: i, j, tmp
  
  DO i=1,3
     DO j=1,(4-i)
        IF (TET(j,tetIndex) > TET(j+1, tetIndex)) THEN
           tmp = TET(j,tetIndex)
           TET(j,tetIndex) = TET(j+1,tetIndex)
           TET(j+1,tetIndex) = tmp
        END IF
     END DO
  END DO
  
END SUBROUTINE ORDER


SUBROUTINE checks ( redTet, numberReducibleTetrahedra )
  USE Global, ONLY : debug
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: numberReducibleTetrahedra
  INTEGER, INTENT(INOUT) :: redTet(numberReducibleTetrahedra,4)
  INTEGER :: j
  LOGICAL :: oneLEtwo, twoLEthree, threeLEfour
  
  DO j=1, numberReducibleTetrahedra
     
     IF ( ALL(redTet(j,1:4)==(/0,0,0,0/)) ) THEN
        WRITE(*,*) "Tetrahedra ", j, " set to zero. Incorrect."
        STOP "Tetrahedra set to zero for unknown reason."
     END IF
     
     oneLEtwo    = (redTet(j,1).LE.redTet(j,2))
     twoLEthree  = (redTet(j,2).LE.redTet(j,3))
     threeLEfour = (redTet(j,3).LE.redTet(j,4))
     IF ( (oneLEtwo.AND.twoLEthree).AND.threeLEfour ) THEN
        !! OK
     ELSE
        WRITE(*,*) " "
        WRITE(*,*) "     PROBLEM"
        WRITE(*,*) redTet(j,1:4)
        STOP "Tetrahedron not properly ordered"    
     END IF
  END DO
  IF (debug) WRITE(*,*) "First check passed"
  
!!! Count irreducible tetrahedra
  
  DO j=1,numberReducibleTetrahedra - 1
     IF ( redTet(j,1).GT.redTet(j+1,1) ) THEN
        WRITE(*,*) "Problem with tetrahedron order"
        STOP "error"
     ELSE IF ( redTet(j,1).EQ.redTet(j+1,1) ) THEN
!!! Check other entries
        IF ( redTet(j,2).GT.redTet(j+1,2) ) THEN
           WRITE(*,*) "Problem with tetrahedron order"
           STOP "error"
        ELSE IF ( redTet(j,2).EQ.redTet(j+1,2) ) THEN   
!!! Check other entries
           IF ( redTet(j,3).GT.redTet(j+1,3) ) THEN
              WRITE(*,*) "Problem with tetrahedron order"
              STOP "error"
           ELSE IF ( redTet(j,3).EQ.redTet(j+1,3) ) THEN
!!! Check other entries
              IF ( redTet(j,4).GT.redTet(j+1,4) ) THEN
                 WRITE(*,*) "Problem with tetrahedron order"
                 STOP "error"
              END IF
           END IF
        END IF
     END IF
  END DO
  IF (debug) WRITE(*,*) "Second check passed"
  
END SUBROUTINE checks
