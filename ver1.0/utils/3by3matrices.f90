
SUBROUTINE mult3by3(outMat, A, B)
  IMPLICIT NONE
  DOUBLE PRECISION, INTENT(IN) :: A(3,3), B(3,3)
  DOUBLE PRECISION, INTENT(OUT) :: outMat(3,3)
  DOUBLE PRECISION :: tmpMat(3,3)
  INTEGER :: i, j
  
  DO i=1,3
     DO j=1,3
        tmpMat(i,j) = A(i,1)*B(1,j) + A(i,2)*B(2,j) + A(i,3)*B(3,j)
     END DO
  END DO
  
  ! the reason for using tmpMat is that we can then safely
  ! call the subroutine with outMat equal to A or B.
  outMat = tmpMat
  
END SUBROUTINE MULT3BY3

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
  
!!!  WRITE(*,*) "determinant ", determinant
  
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
