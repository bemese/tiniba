MODULE myAllocateMod
  USE Global, ONLY : debug
  
  INTERFACE myAllocate
     MODULE PROCEDURE myAllocateInt1, myAllocateDP1, myAllocateL1, myAllocateInt2
  END INTERFACE
CONTAINS
  
  SUBROUTINE myAllocateInt1 (array, stringArrayName, arraySize)
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: array(:)
    CHARACTER(LEN=*), INTENT(IN) :: stringArrayName
    INTEGER, INTENT(IN) :: arraySize
    INTEGER :: istatus
    
    ALLOCATE ( array(arraySize), STAT=istatus )
    IF (istatus .EQ. 0) THEN
       IF (debug) WRITE(*,*) "(myAllocateIn1) Array "//TRIM(stringArrayName)//" allocated"
    ELSE
       WRITE(*,*) "Could not allocate "//TRIM(stringArrayName)//".  Stopping."
       STOP "Could not allocate array."
    END IF
    
    array(:) = 0
  END SUBROUTINE myAllocateInt1
  
  SUBROUTINE myAllocateDP1 (array, stringArrayName, arraySize)
    IMPLICIT NONE
    DOUBLE PRECISION, ALLOCATABLE :: array(:)
    CHARACTER(LEN=*), INTENT(IN) :: stringArrayName
    INTEGER, INTENT(IN) :: arraySize
    INTEGER :: istatus
    
    ALLOCATE ( array(arraySize), STAT=istatus )
    IF (istatus .EQ. 0) THEN
       IF (debug) WRITE(*,*) "(myAllocateDP1): "//TRIM(stringArrayName)//" allocated"
    ELSE
       WRITE(*,*) "Could not allocate "//TRIM(stringArrayName)//".  Stopping."
       STOP "Could not allocate array."
    END IF
    
    array(:) = 0.d0
  END SUBROUTINE myAllocateDP1
  
  SUBROUTINE myAllocateL1 (array, stringArrayName, arraySize)
    IMPLICIT NONE
    LOGICAL, ALLOCATABLE :: array(:)
    CHARACTER(LEN=*), INTENT(IN) :: stringArrayName
    INTEGER, INTENT(IN) :: arraySize
    INTEGER :: istatus
    
    ALLOCATE ( array(arraySize), STAT=istatus )
    IF (istatus .EQ. 0) THEN
       IF (debug) WRITE(*,*) "(myAllocateL1): "//TRIM(stringArrayName)//" allocated"
    ELSE
       WRITE(*,*) "Could not allocate "//TRIM(stringArrayName)//".  Stopping."
       STOP "Could not allocate array."
    END IF
    
    array(:) = .FALSE.
  END SUBROUTINE myAllocateL1
  
  SUBROUTINE myAllocateInt2 (array, stringArrayName, arraySize1, arraySize2)
    IMPLICIT NONE
    INTEGER, ALLOCATABLE :: array(:,:)
    CHARACTER(LEN=*), INTENT(IN) :: stringArrayName
    INTEGER, INTENT(IN) :: arraySize1, arraySize2
    INTEGER :: istatus
    
    ALLOCATE ( array(arraySize1, arraySize2), STAT=istatus )
    IF (istatus .EQ. 0) THEN
       IF (debug) WRITE(*,*) "(myAllocateInt2): "//TRIM(stringArrayName)//" allocated"
    ELSE
       WRITE(*,*) "Could not allocate "//TRIM(stringArrayName)//".  Stopping."
       STOP "Could not allocate array."
    END IF
    
    array(:,:) = 0
  END SUBROUTINE myAllocateInt2
  
END MODULE myAllocateMod
