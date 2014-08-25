!!!#######################
PROGRAM layer
!!!#######################
  real, allocatable  :: z(:),zp(:),zl(:)
  read(*,*)Nplanes,vacuum
  allocate(z(Nplanes+1),zp(Nplanes),zl(0:Nplanes))
  do i = 1,Nplanes
     read(1,*)z(i) 
  end do
!  do i = 2,Nplanes
!     diff(i-1) = abs(z(i)-z(i-1))
!  end do
  z(Nplanes+1)=z(Nplanes)
!  write(*,*)"aqui dog"
!  stop
  k=0
   do i = 2,Nplanes,2
      k = k + 1
      diffp = abs(z(i)-z(i-1))
      diffl = abs(z(i)-z(i+1))
      zp(k) = z(i) + diffp/2.   
      zl(k) = z(i) - diffl/2.
!      write(15,*)'rep ',zp(k)
!      write(16,*)'rep ',zl(k)
   end do
   kmax = k
!!! top and bottom surfaces with equal vacuum
  zl(0)=zp(1)+vacuum
  zl(kmax)=zl(kmax)-vacuum
  do i = 1,kmax
     db = abs(zp(i)-zl(i))
     df = abs(zp(i)-zl(i-1))
     write(99,*)zp(i),df,db
     write(2,*)'rep v,',zp(i)+df,',u'
     write(3,*)'rep v,',zp(i)-db,',u'
     write(7,*)'rep ',zp(i)+df
     write(8,*)'rep ',zp(i)-db
  end do
!!! for plotting using load 'fort.2/3
!!!#######################
END PROGRAM layer
!!!#######################


