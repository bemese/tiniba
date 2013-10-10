program layer

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!! declaration of variables      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  IMPLICIT NONE
  integer:: ios
  INTEGER :: fsa, bap, nap, bsa
  double precision:: vacuum
  character (80):: vacuum_c, fsa_c, bap_c, nap_c, bsa_c, case
  character(80)::file
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!getting command line argunments!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  call getarg(1,vacuum_c)                                               ! response file (input file)
  call getarg(2,fsa_c)                                                    ! (output file)            
  call getarg(3,bap_c)                                                     ! fill or erase
  call getarg(4,nap_c)                                                    ! temporal file to read variables. 
  call getarg(5,bsa_c)                                                     ! response file size = factor * ( response.sm.file size ). 

  read( vacuum_c, '( E22.14 )' ) vacuum                                               !converting character to double
  read( fsa_c, '( i2 )' ) fsa                                               !converting character to integer
  read( bap_c, '( i2 )' ) bap                                               !converting character to integer
  read( nap_c, '( i2 )' ) nap                                               !converting character to integer
  read( bsa_c, '( i2 )' ) bsa                                               !converting character to integer

  
  write( *,* ) 'vacuum ',vacuum
  write( *,* ) '  fsa ', fsa, '  bap ', bap, '  nap ', nap, '  bsa ', bsa
  call system ( "echo $PWD | awk -F / '{print$NF}'   > fort.1")

  read(1,*)case
  write(*,*)'case  ',trim(case) 
  file= trim(case) // ".xyz"
  write(*,*)'file ',file

  open( 2, file=file, form='formatted',status='old', iostat=ios)
  IF (ios.NE.0) THEN
     WRITE(*,*) ios
     WRITE(*,*) 'read error with ',trim(file)
     STOP 'STOPPING'
  END IF
  close(2)
end program layer
