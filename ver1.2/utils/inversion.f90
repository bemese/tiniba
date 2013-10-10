program inversion
implicit none
integer   :: i,nsym
integer   :: x1,y1,z1
integer   :: x2,y2,z2
integer   :: x3,y3,z3
character(len=3) :: res
res='ncs'
read(9,*)nsym
do i=1,nsym
read(9,*)x1,y1,z1,x2,y2,z2,x3,y3,z3
if((x1.eq.-1).and.(y2.eq.-1).and.(z3.eq.-1))then
res='scs'
end if
end do
write(*,*)res
end program inversion

