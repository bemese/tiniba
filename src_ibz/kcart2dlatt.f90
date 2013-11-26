PROGRAM cart2direct
!!!
!!! given any k-vector:
!!! if given in Cartesian coordinates it is translated into Direct Lattice coordinate
!!! if given in Direct Lattice it is translated into Cartesian coordinates
!!!
  IMPLICIT NONE
  integer :: istat,cual
  DOUBLE PRECISION, DIMENSION(3) :: d1, d2, d3, b1, b2, b3, tmp
  DOUBLE PRECISION :: mB(3,3),mK(3,3),mBi(3,3)
  DOUBLE PRECISION :: a,b,c,PI
  DOUBLE PRECISION :: kx,ky,kz
  PI=acos(-1.d0)
  mK=0
!!! reads the data and 
!!! 1 for Cartesian->Direct
!!! or
!!! 2 for Direct->Cartesian
!!! put it into a diagonal matrix
  read(*,*)mK(1,1),mK(2,2),mK(3,3),cual
!!! open the file with the primitive vectors in real Cartesian Space
  OPEN (UNIT=11,FILE="symmetries/pvectors",STATUS="OLD",ACTION="READ",IOSTAT=istat)
  IF ( istat.NE.0 ) THEN
     WRITE(*,*) "Error: Cannot open file pvectors" 
     STOP "Error opening file pvectors"
  END IF
!!! reads the pvectors
  READ(11,*) d1(1:3)
  READ(11,*) d2(1:3)
  READ(11,*) d3(1:3)
  READ(11,*) a,b,c
  d1 = a * d1
  d2 = b * d2
  d3 = c * d3
!!! Use the standard formular for reciprocal lattice primitive vectors
  CALL XPROD (d2, d3, tmp)
  b1 = 2*PI*(1/(DOT_PRODUCT(d1, tmp)))*tmp
  CALL XPROD (d3, d1, tmp)
  b2 = 2*PI*(1/(DOT_PRODUCT(d2, tmp)))*tmp
  CALL XPROD (d1, d2, tmp)
  b3 = 2*PI*(1/(DOT_PRODUCT(d3, tmp)))*tmp
!!! form the rotation matrix from direct Direct Lattice to Cartesian
     mB(1:3,1) = b1(1:3)  
     mB(1:3,2) = b2(1:3) 
     mB(1:3,3) = b3(1:3)   
  CALL invert3by3 (mB, mBi)  
!!!from Cartesian to Direct
  if (cual.eq.1) then
     tmp=   mBi(1:3,1)*mK(1,1) + mBi(1:3,2)*mK(2,2) + mBi(1:3,3)*mK(3,3)
     write(*,*)'Cartesian to Direct Lattice'
  end if
!!!from Direct to Cartesian
  if (cual.eq.2) then
     tmp=   mB(1:3,1)*mK(1,1) + mB(1:3,2)*mK(2,2) + mB(1:3,3)*mK(3,3)
     write(*,*)'Direct Lattice to Cartesian'
  end if
  WRITE(*, FMT='(3F15.8)') tmp
!!!  
END PROGRAM cart2direct
!!!
SUBROUTINE XPROD (v1, v2, x)
  !Sets x to the cross product of v1 and v2
  IMPLICIT NONE
  
  !Calling arguments
  DOUBLE PRECISION,INTENT(IN) :: v1(3),v2(3)
  DOUBLE PRECISION, INTENT(OUT) :: x(3)
  
  x(1) = v1(2)*v2(3) - v1(3)*v2(2)
  x(2) = v1(3)*v2(1) - v1(1)*v2(3)
  x(3) = v1(1)*v2(2) - v1(2)*v2(1)
  
END SUBROUTINE XPROD
