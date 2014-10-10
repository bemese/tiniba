PROGRAM add2files
  !     
  !	adds the columns of two given files
  !
  IMPLICIT NONE
  INTEGER :: i,j,row,col
  DOUBLE PRECISION , ALLOCATABLE :: f1(:),f2(:)
  ! reads number of rows and columns
  read(*,*)row,col
  !allocates size
  allocate(f1(col),f2(col))
  ! loops over rows
  do i=1,row
     read(1,*,end=69)(f1(j),j=1,col)     
     read(2,*,end=69)(f2(j),j=1,col)
!     write(3,*)f1(1),-f2(2),f1(3),-f2(4),f1(5),-f2(6)
     write(3,*)(f2(j),j=1,col)
!     write(3,*)(f1(j)+f2(j),j=1,col)
!     write(3,*)f1(1)+f2(1),f1(2)-f2(2)&
!          ,f1(3)+f2(3),f1(4)-f2(4)&
!          ,f1(5)+f2(5),f1(6)-f2(6)
  end do
69 deallocate(f1,f2)
end PROGRAM add2files
