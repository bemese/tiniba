!PROGRAM THAT MAKES OPERATIONS ON TWO NUMBERS. x y 
!if option = 1 -> x - y
!if option = 2 -> (x - y) / 2
!if option = 3 -> abs(x - y) / 2
!it accepts any input format
!examples:
! rest 1 2
! rest 1.0 2.0E+03
!the output is printed on the screen

program operations

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!! declaration of variables      !!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  IMPLICIT NONE
  integer:: ios,i
  double precision:: x,y
  integer:: x_i, y_i, opt
  real:: c
  character (80):: x_c,y_c, opt_c
  logical::back
  intrinsic scan

!  back = true
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!getting command line argunments!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call getarg(1,x_c) !first number  
  call getarg(2,y_c) !second number
  call getarg(3,opt_c) !option

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! Adaptations so that it can accept any input format !!!!!!!!!!!!!!!!!!!!!
!!!!! i.e. integers, exponents !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if( scan(x_c,'.') .ne. 0 ) then
     read(x_c, '( G22.14 )') x     
  else
     read(x_c, '( i14 )') x_i
     x = x_i
  endif

  if( scan(y_c,'.') .ne. 0 ) then
     read(y_c, '( G22.14 )') y     
  else
     read(y_c, '( i14 )') y_i
     y = y_i
  endif

  read(opt_c, '( i2 )') opt

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!! Operations !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  if ( opt.eq.1) then
     c = x - y
  elseif ( opt.eq.2) then
     c = (x - y) / 2
  elseif ( opt.eq.3) then
     c = abs(x - y) / 2
  endif

  write(*,*)c
!  write(*,*) x_c,'  ',y_c 
!  write(*,*) 'x ', x, 'y ', y,'x - y ',c
  
end program operations
