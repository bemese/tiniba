MODULE geometry
  IMPLICIT NONE
  
CONTAINS
  
  SUBROUTINE rrtorc(rred,rprimd,rcart)
    !##  Conversion  r reduced coordinates -> cartesian coordinates
    
    IMPLICIT NONE
    DOUBLE PRECISION, DIMENSION(3) :: rred,rcart
    DOUBLE PRECISION, DIMENSION(3,3) :: rprimd
    DOUBLE PRECISION :: ra1(3),ra2(3),ra3(3)
    
    ra1(:)=rprimd(:,1)
    ra2(:)=rprimd(:,2)
    ra3(:)=rprimd(:,3)
    rcart(1)= rred(1)*ra1(1) + rred(2)*ra2(1) + rred(3)*ra3(1)
    rcart(2)= rred(1)*ra1(2) + rred(2)*ra2(2) + rred(3)*ra3(2)
    rcart(3)= rred(1)*ra1(3) + rred(2)*ra2(3) + rred(3)*ra3(3)
    !   write(66,"(3F16.8)")rcart
    
  END SUBROUTINE rrtorc
  
!!!##############################################
  SUBROUTINE krtokc(kred,b1,b2,b3,kcart)
    IMPLICIT NONE
    !  Conversion  k reduced coordinates -> cartesian coordinates
    
    DOUBLE PRECISION, DIMENSION(3) :: kred,kcart
    DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
      
    kcart(1)= kred(1)*b1(1) + kred(2)*b2(1) + kred(3)*b3(1)
    kcart(2)= kred(1)*b1(2) + kred(2)*b2(2) + kred(3)*b3(2)
    kcart(3)= kred(1)*b1(3) + kred(2)*b2(3) + kred(3)*b3(3)
    !   write(66,"(3F16.8)")kcart
    
  END SUBROUTINE krtokc
  
!!!##################################################
  
  SUBROUTINE r2k(a,b)
    IMPLICIT NONE
    ! Conversion from real space to reciprocal space
    
    DOUBLE PRECISION :: pi,tsp,twopi
    DOUBLE PRECISION :: adotbXc
    DOUBLE PRECISION, DIMENSION(3,3) :: a,b
    DOUBLE PRECISION :: a1(3),a2(3),a3(3),v(3)
    
    pi = dacos(-1.d0)
    twopi= 2.d0*pi
    
    a1(:)=a(:,1) !
    a2(:)=a(:,2) !
    a3(:)=a(:,3) !
    
    tsp=adotbXc(a1,a2,a3)
    
    ! b1,b2,b3
    call saxb(a2,a3,v)
    b(1,:) = twopi*(v(:)/tsp)
    
    v = 0
    call saxb(a3,a1,v)
    b(2,:) = twopi*(v(:)/tsp)
    
    v = 0
    call saxb(a1,a2,v)
    b(3,:) = twopi*(v(:)/tsp)
    
  END SUBROUTINE r2k
  
!!!##################################################
  
  SUBROUTINE saxb(k1,k2,v)
    IMPLICIT NONE
    ! Vectorial product  = (k2 X k3)
    !
    DOUBLE PRECISION, DIMENSION(3) :: k1,k2,k3,v
    
    k3(1) =   ( k1(2)*k2(3) - k1(3)*k2(2) )
    k3(2) = - ( k1(1)*k2(3) - k1(3)*k2(1) )
    k3(3) =   ( k1(1)*k2(2) - k1(2)*k2(1) )
    
    v(:)=k3(:)
    
  END SUBROUTINE saxb
  
  FUNCTION adotbXc(k1,k2,k3)
    IMPLICIT NONE
    ! Triple scalar product  = k1 * (k2 X k3)
    
    DOUBLE PRECISION :: adotbXc
    DOUBLE PRECISION :: vx,vy,vz
    DOUBLE PRECISION, DIMENSION(3) :: k1,k2,k3
    
    vx =   k1(1)*( k2(2)*k3(3) - k2(3)*k3(2) )
    vy = - k1(2)*( k2(1)*k3(3) - k2(3)*k3(1) )
    vz =   k1(3)*( k2(1)*k3(2) - k2(2)*k3(1) )
    
    adotbXc = vx + vy + vz
    
  END FUNCTION adotbXc
  
  FUNCTION volume(r1,r2,r3)
    IMPLICIT NONE
    ! Volume of a tetrahedra volume = |r1 * (r2 X r3)|
    
    DOUBLE PRECISION :: volume
    DOUBLE PRECISION :: vx,vy,vz
    DOUBLE PRECISION, DIMENSION(3) :: r1,r2,r3
    
    vx = r1(1)*( r2(2)*r3(3) - r2(3)*r3(2) )
    vy = - r1(2)*( r2(1)*r3(3) - r2(3)*r3(1) )
    vz = r1(3)*( r2(1)*r3(2) - r2(2)*r3(1) )
    volume = dabs(vx + vy + vz)
    
  END FUNCTION volume
  
  FUNCTION areaofrectangle(r1,r2)
    IMPLICIT NONE
    ! Area = |r1|*|r2| of a rectangle
    
    DOUBLE PRECISION :: areaofrectangle,l1,l2
    DOUBLE PRECISION, DIMENSION(3) :: r1,r2
    
    l1 = dsqrt(r1(1)**2+r1(2)**2+r1(3)**2)
    l2 = dsqrt(r2(1)**2+r2(2)**2+r2(3)**2)
    areaofrectangle = l1*l2
    
  END FUNCTION areaofrectangle
  
END MODULE geometry
