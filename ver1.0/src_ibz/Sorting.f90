MODULE Sorting
  ! From Cormen et al.
  IMPLICIT NONE
  INTEGER, ALLOCATABLE :: indx(:) 
  
CONTAINS
  
  SUBROUTINE initializeIndex (M)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: M
    INTEGER :: i
    ALLOCATE(indx(M))
    DO i=1,M
       indx(i) = i
    END DO
  END SUBROUTINE initializeIndex
  
  SUBROUTINE destroyIndex
    IMPLICIT NONE
    DEALLOCATE(indx)
  END SUBROUTINE destroyIndex
  
  RECURSIVE SUBROUTINE quickSort(A, p, r)
    INTEGER, INTENT(INOUT), DIMENSION(:) :: A
    INTEGER, INTENT(IN) :: p, r
    INTEGER :: q
    
    IF ( p .LT. r) THEN
       CALL Partition(A, p, r, q)
       CALL quickSort(A, p, q-1)
       CALL quickSort(A, q+1, r)
    END IF
  END SUBROUTINE quickSort
  
  SUBROUTINE Partition(A, p, r, q)
    INTEGER, INTENT(INOUT), DIMENSION(:) :: A
    INTEGER, INTENT(IN) :: p, r
    INTEGER, INTENT(OUT) :: q
    INTEGER :: i, j
    INTEGER :: swapTemp
    INTEGER :: x
    
    x = A(r)  ! pivot element
    i = p-1
    DO j = p, r-1
       IF ( A(j) .LE. x) THEN
          i = i+1
          swapTemp = A(i)
          A(i) = A(j)
          A(j) = swapTemp
       END IF
    END DO
    swapTemp = A(i+1)
    A(i+1) = A(r)
    A(r) = swapTemp
    q = i+1
    RETURN
  END SUBROUTINE Partition
  
  RECURSIVE SUBROUTINE quickSortIndex(A, indx, p, r)
    ! Sorts the array A, and makes an index
    INTEGER, INTENT(INOUT), DIMENSION(:) :: A, indx
    INTEGER :: q, p, r
    
    IF ( p .LT. r) THEN
       CALL PartitionIndex(A, indx, p, r, q)
       CALL quickSortIndex(A, indx, p, q-1)
       CALL quickSortIndex(A, indx, q+1, r)
    END IF
  END SUBROUTINE quickSortIndex
  
  SUBROUTINE PartitionIndex(A, indx, p, r, q)
    INTEGER, INTENT(INOUT), DIMENSION(:) :: A, indx
    INTEGER, INTENT(IN) :: p, r
    INTEGER, INTENT(OUT) :: q
    INTEGER :: i, j, k
    INTEGER :: swapTemp
    INTEGER :: x
    
    x = A(r)  ! pivot element
    i = p-1
    DO j = p, r-1
       IF ( A(j) .LE. x) THEN
          i = i+1
          swapTemp = A(i)
          A(i) = A(j)
          A(j) = swapTemp
          swapTemp = indx(i)
          indx(i) = indx(j)
          indx(j) = swapTemp
       END IF
    END DO
    swapTemp = A(i+1)
    A(i+1) = A(r)
    A(r) = swapTemp
    swapTemp = indx(i+1)
    indx(i+1) = indx(r)
    indx(r) = swapTemp
    
    q = i+1
    RETURN
  END SUBROUTINE PartitionIndex
  
  SUBROUTINE insertionSort(a)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT), DIMENSION(:) :: a
    INTEGER :: i,j,n
    INTEGER :: key
    N=SIZE(a)
    DO j=2, N
       key = a(j)
       i=j-1
       DO WHILE (a(i)>key)
          a(i+1) = a(i)
          i=i-1
          IF (i==0) EXIT
       END DO
       a(i+1) = key
    END DO
    RETURN
  END SUBROUTINE insertionSort
  
  SUBROUTINE insertionSortIndex(a, indx)
    IMPLICIT NONE
    INTEGER, INTENT(INOUT), DIMENSION(:) :: a, indx
    INTEGER :: i,j,n
    INTEGER :: key, keyIndex
    N=SIZE(a)
    DO j = 2, N
       key = a(j)
       keyIndex = indx(j)
       i=j-1
       DO WHILE ((a(i)>key))
          a(i+1) = a(i)
          indx(i+1) = indx(i)
          i=i-1
          IF (i==0) EXIT
       END DO
       a(i+1) = key
       indx(i+1) = keyIndex
    END DO
    RETURN
  END SUBROUTINE insertionSortIndex
  
END MODULE Sorting
