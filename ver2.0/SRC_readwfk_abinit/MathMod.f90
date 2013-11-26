MODULE MathMod
  
  IMPLICIT NONE
  
  DOUBLE PRECISION, PARAMETER :: PI=3.14159265358979323846d0
  
  TYPE matrix3by3
     INTEGER :: el(3,3) !Matrix elements
  END TYPE matrix3by3

  ! Contains:
  !   invert3by3
  !   greatestCommonDivisor
  
CONTAINS
  
  SUBROUTINE invert3by3(inMat,outMat)
    IMPLICIT NONE
    
    DOUBLE PRECISION, INTENT(IN) :: inMat(3,3)
    DOUBLE PRECISION, INTENT(OUT) :: outMat(3,3)
    DOUBLE PRECISION :: determinant
    DOUBLE PRECISION :: det(3,3)
    
    det(1,1) = (inMat(2,2)*inMat(3,3)-inMat(2,3)*inMat(3,2))
    det(1,2) = (inMat(2,1)*inMat(3,3)-inMat(2,3)*inMat(3,1))
    det(1,3) = (inMat(2,1)*inMat(3,2)-inMat(2,2)*inMat(3,1))
    
    det(2,1) = (inMat(1,2)*inMat(3,3)-inMat(1,3)*inMat(3,2))
    det(2,2) = (inMat(1,1)*inMat(3,3)-inMat(1,3)*inMat(3,1))
    det(2,3) = (inMat(1,1)*inMat(3,2)-inMat(1,2)*inMat(3,1))
    
    det(3,1) = (inMat(1,2)*inMat(2,3)-inMat(1,3)*inMat(2,2))
    det(3,2) = (inMat(1,1)*inMat(2,3)-inMat(1,3)*inMat(2,1))
    det(3,3) = (inMat(1,1)*inMat(2,2)-inMat(1,2)*inMat(2,1))
    
    determinant = inMat(1,1)*det(1,1)-inMat(1,2)*det(1,2)+inMat(1,3)*det(1,3)
    
    outMat(1,1) =  det(1,1)/determinant
    outMat(1,2) = -det(2,1)/determinant
    outMat(1,3) =  det(3,1)/determinant
    outMat(2,1) = -det(1,2)/determinant
    outMat(2,2) =  det(2,2)/determinant
    outMat(2,3) = -det(3,2)/determinant
    outMat(3,1) =  det(1,3)/determinant
    outMat(3,2) = -det(2,3)/determinant
    outMat(3,3) =  det(3,3)/determinant
    
  END SUBROUTINE invert3by3
  
  SUBROUTINE greatestCommonDivisor(a,b,res)
    INTEGER, INTENT(IN) :: a, b
    INTEGER, INTENT(OUT) :: res
    INTEGER :: i, temp1, temp2, tempA, tempB
    
    IF((b.EQ.0).OR.(a.EQ.0)) THEN
       STOP 'Error with greatest common divisor. zero input.' 
    END IF
    
    i=0
    tempA = a
    tempB = b
    DO
       IF (tempB .EQ. 0) THEN
          res = tempA
          EXIT
       END IF
       temp1 = tempB
       temp2 = MOD(tempA,tempB)
       tempA = temp1
       tempB = temp2
       i=i+1
       IF (i.EQ.10**6) THEN
          STOP 'Error in greatest common divisor.' 
       END IF
    END DO
    
  END SUBROUTINE greatestCommonDivisor
  
END MODULE MathMod
