SUBROUTINE printIRpts ()
  USE Global, ONLY : NIRpts, IRpts, b1, b2, b3
  USE Global, ONLY : debug
  USE Global, ONLY : PI
  USE Grid, ONLY : N1, N2, N3
  USE Grid, ONLY : gridCoordinates, gridPointer
  USE Grid, ONLY : multiplicity, weights
  USE StructFile, ONLY : ortho, a1, a2, a3
  USE CommandLineArguments, ONLY : wien2k, reduced, cartesian, reducedInteger
  IMPLICIT NONE
  
  !Local variables
  DOUBLE PRECISION :: tmp(3)
  INTEGER :: i
  INTEGER :: itmp(3), divisor
  
  IF ( debug ) WRITE(*,*) "Program Flow: Entered printIRpts"
  
  !Print the amount of reduction to the screen
  WRITE(*,*) "Total number of points: ", N1*N2*N3
  WRITE(*,*) "Number of irreducible points: ", NIRpts
  !Amount of reduction is the ratio of total points to irreducible points
  WRITE(*,*) "Amount of Reduction: ", (N1*N2*N3)/REAL(NIRpts)
  
  IF (reduced) THEN
     CALL printOutRL
  END IF
  
  IF (cartesian) THEN
     CALL printOutCartesian
  END IF
  
  IF (reducedInteger) THEN
     CALL printOutRLInt
  END IF
  
  IF (wien2k) THEN
     CALL printOutWien2k
  END IF
  
  OPEN (UNIT=14, FILE="kpoints.map", ACTION="WRITE")
  WRITE(14,*) N1*N2*N3
  !Print k-point map
  DO i = 1, N1*N2*N3
     WRITE(UNIT=14,FMT='(3I8,2I10)')  gridCoordinates(i,1:3), gridPointer(i,1:2)
  END DO
  CLOSE(14)
  
  DEALLOCATE( weights )
  DEALLOCATE( gridCoordinates )
  
END SUBROUTINE printIRpts

SUBROUTINE printOutRL
  USE Global, ONLY : NIRpts, IRpts
  USE Global, ONLY : debug
  USE Grid, ONLY : N1, N2, N3
  
  IMPLICIT NONE
  
  INTEGER :: i
  DOUBLE PRECISION :: tmp(3) 
  
  OPEN (UNIT=10, FILE="kpoints.reciprocal", ACTION="WRITE")
  
  DO i = 1, NIRpts
     
     !Print irreducible points in primitive reciprocal lattice basis
     tmp(1) = IRpts(i,1)/REAL(N1-1)
     tmp(2) = IRpts(i,2)/REAL(N2-1) 
     tmp(3) = IRpts(i,3)/REAL(N3-1)
     
     WRITE(UNIT=10, FMT='(3F15.8)') tmp
     
  END DO
  CLOSE(10)
END SUBROUTINE printOutRL

SUBROUTINE printOutCartesian
  USE Global, ONLY : NIRpts, IRpts, b1, b2, b3
  USE Global, ONLY : debug
  USE Grid, ONLY : N1, N2, N3
  IMPLICIT NONE
  
  INTEGER :: i
  DOUBLE PRECISION :: tmp(3)
  
  OPEN (UNIT=10, FILE="kpoints.cartesian", ACTION="WRITE")
  
  DO i = 1, NIRpts
     !Print irreducible points in cartesian coordinates
     tmp = ( IRpts(i,1) / REAL(N1-1) ) * b1 &
          + ( IRpts(i,2) / REAL(N2-1) ) * b2 &
          + ( IRpts(i,3) / REAL(N3-1) ) * b3
     
     WRITE(UNIT=10, FMT='(3F15.8)') tmp
  END DO
  
  CLOSE(10)
END SUBROUTINE printOutCartesian

SUBROUTINE printOutRLInt
  USE Global, ONLY : NIRpts, IRpts
  USE Global, ONLY : debug
  USE Grid, ONLY :  N1, N2, N3
  IMPLICIT NONE
  
  INTEGER :: i
  DOUBLE PRECISION :: tmp(3) 
  
  OPEN (UNIT=10, FILE="kpoints.integer", ACTION="WRITE")
  WRITE(UNIT=10, FMT='(I10)') NIRpts
  WRITE(UNIT=10, FMT='(A10,3I6)') "Divide by:", N1-1, N2-1, N3-1  
  
  DO i = 1, NIRpts
     !Print integer representation of k-points
     WRITE(UNIT=10,FMT='(TR10,3I6)') IRpts(i,1:3)
  END DO
  CLOSE(10)
  
END SUBROUTINE printOutRLInt


SUBROUTINE printOutWien2k
  USE Global, ONLY : NIRpts, IRpts, b1, b2, b3
  USE Global, ONLY : debug
  USE Global, ONLY : PI
  USE Grid, ONLY : N1, N2, N3
  USE Grid, ONLY : weights
  USE StructFile, ONLY : ortho, a1, a2, a3
  IMPLICIT NONE
  
  INTEGER :: i
  DOUBLE PRECISION :: tmp(3)
  INTEGER :: itmp(3), divisor
  INTEGER :: gcd1, gcd2, lcm1, lcm2 , fact
  
  OPEN (UNIT=10, FILE="kpoints.wien2k", ACTION="WRITE")
  
  ! find lowest common multiple of N1-1, N2-1, and N3-1.
  CALL greatestCommonDivisor(N1-1,N2-1,gcd1)
  lcm1 = (N1-1)*(N2-1)/gcd1
  CALL greatestCommonDivisor(lcm1,N3-1,gcd2)
  lcm2 = (N3-1)*lcm1/gcd2
  
  ! lcm2 = lcm(N1-1,N2-1,N3-1)
  
  fact = (N1-1)*(N2-1)*(N3-1)/lcm2
  ! fact is the factor you need to divide (N1-1)*(N2-1)*(N3-1) by
  ! to get lcm2.
  
  ! Note that fact=gcd1*gcd2
  
  DO i = 1, NIRpts
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
     divisor = divisor/fact
     itmp(1:3) = itmp(1:3)/fact
!     CALL reducePoint(itmp,divisor)
     WRITE(UNIT=10,FMT='(I10,4I5,F5.1)') i, itmp(1:3), divisor, weights(i)
  END DO
  
  WRITE(UNIT=10,FMT='(A3)') "END"
  
  CLOSE(10)
  
END SUBROUTINE printOutWien2k


SUBROUTINE reducePoint (kpoint, divisor)
  IMPLICIT NONE
  
  !Calling arguments
  INTEGER, INTENT (INOUT) :: kpoint(3), divisor
  
  !Local variables, primeARRAY will hold the prime numbers that
  !will be divided out as common factors.
  INTEGER :: i, p, t1, t2, t3, t4
  INTEGER, PARAMETER :: primeArray(26) = (/ 2, 3, 5, 7, 11, &
       13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, &
       73, 79, 83, 89, 97, 101/)
  
  DO i = 1, 20
     p = primeArray(i)
     DO
        IF ((MOD(kpoint(1),p) == 0).AND.(MOD(kpoint(2),p) == 0).AND.&
             (MOD(kpoint(3),p) == 0).AND.(MOD(divisor,p) == 0)) THEN
           kpoint(1:3)=kpoint(1:3)/p
           divisor = divisor/p
        ELSE
           EXIT
        END IF
     END DO
  END DO
  
END SUBROUTINE reducePoint

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
