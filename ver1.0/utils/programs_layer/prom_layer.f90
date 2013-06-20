PROGRAM maxima
!!
!! finds the layers of any slab
!!
  implicit none
  integer, parameter :: DP = KIND(1.0D0)      ! double precision
  integer :: i,j,nfs,nba,nap,nbs
  real(DP)  x,y
  real(DP), allocatable :: zfs(:),zb(:),zbs(:)
!!  
!  write(*,*)'# front surface atoms'
!  write(*,*)'# bulk atomic planes and # of atoms per plane'
!  write(*,*)'# back surface atoms'
  read(*,*)nfs,nba,nap,nbs
  allocate(zfs(nfs),zb(nap),zbs(nbs))
!!! front surface
  do j=1,nfs
   read(1,*)x,y,zfs(j)
  end do
!  write(2,*)sum(zfs(1:nfs))/nfs
  write(2,*)maxval(zfs)
  write(2,*)minval(zfs)
!!! bulk planes
  do j=1,nba
!!! each plane
     do i=1,nap
        read(1,*)x,y,zb(i)
     end do
!     write(2,*)sum(zb(1:nap))/nap
     write(2,*)maxval(zb)
     write(2,*)minval(zb)
  end do
!!! back surface
  do j=1,nbs
   read(1,*)x,y,zbs(j)
  end do
!  write(2,*)sum(zbs(1:nbs))/nbs
  write(2,*)maxval(zbs)
  write(2,*)minval(zbs)
!!!
end program maxima

