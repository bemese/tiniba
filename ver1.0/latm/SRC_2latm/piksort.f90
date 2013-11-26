SUBROUTINE piksort(n,arr1,arr2)
USE globals, ONLY: DP
IMPLICIT NONE
INTEGER, INTENT(IN) :: n
REAL(DP), INTENT(INOUT) :: arr1(n), arr2(n)
INTEGER :: i, j
REAL(DP) :: a1, a2
DO j=2,n
   a1=arr1(j)
   a2=arr2(j)
   DO i=j-1,1,-1
      IF(arr1(i).LE.a1) exit
      arr1(i+1)=arr1(i)
      arr2(i+1)=arr2(i)
   END DO
   arr1(i+1)=a1
   arr2(i+1)=a2
END DO
END SUBROUTINE piksort

