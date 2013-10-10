program scissors
implicit none
integer   :: IArgC,ArgC,isthis
character :: ArgV(6)*80
character(len=80) :: filein
character(len=80) :: fileout
logical :: filexist
integer :: NofR !!Number of Rows
integer :: NofL !!Number of Lines 
integer :: NBANDAV !!NBAND VALENCIA
integer :: iteracion,i,j,k
double precision :: lambda
INTEGER, PARAMETER :: DP = KIND(1.d0) 
REAL(DP) :: intmpf(1000) 
REAL(DP),ALLOCATABLE :: inputdata(:)
REAL(DP),ALLOCATABLE :: utildata(:,:)
REAL ::  shiftscissors
INTEGER ::  ioerror, nMax
!!!================================
 ArgC=IArgC() !number of arguemnts
 
     if (ArgC==5) then
          do isthis=1,ArgC
             call GetArg(isthis,ArgV(isthis))
          enddo
         filein=ArgV(1)
           read(ArgV(2),"( 1I6 )" )NofR ! 
           read(ArgV(3),"( 1I6 )" )NofL! 
           read(ArgV(4),"( 1I6 )" )NBANDAV!  
           read(ArgV(5),*)shiftscissors ! 

           write(*,*)shiftscissors 
       if ( shiftscissors.eq.0.0)then 
       Stop "       Notihig to do:  Scissors =0"
       end if   
            
          
       else
        write(*,*) " Usage: ./scissors [file] [correction_band_gap]"
        write(*,*) " -------------------------------"
        write(*,*) " where:"
        write(*,*) " [file]=file eigenenergies"
        write(*,*) " [correction_band_gap]= it could be any value" 
        write(*,*) " [0 to 5]"   
        write(*,*) " Examples: ./scissors zeta6.dat 0.5"
        write(*,*) " Copyright(C) 2006  "
        write(*,*) " This is free software;.There is NO warranty"
        write(*,*) " Version 0.1, last modification&
                     &04 December 2006 9:40 hrs "  
        write(*,*) " Optics of Surfaces  Group at C.I.O  jl" 
        write(*,*) " http://andromeda.cio.mx/surfaceoptics/ "
        stop
       endif

       !write(*,*) "the input FILE is: ", filein
       fileout=trim(filein)//'_WS'
       fileout=trim(fileout)
       filein=trim(filein)
       
        
 
      !Checking the existence of data file
     inquire (file=filein,exist=filexist)
     if (.NOT. filexist) then
     write(*,*)'Error, missing file: ',filein
     stop
     end if

!!!================================
!      write(*,*)"The input FILE is: ",trim(filein)
!      write(*,*)'Number of Lines=',NofL 
!      write(*,*)'Number of Rows=',NofR
!      write(*,*)'band gap correction is(ev) ',shiftscissors 
!      write(*,*)'the last valence band ',NBANDAV
!      write(*,*)"The output FILE is: fort.35 "
        
  
        if ((NofR.eq.0).or.(NofL.eq.0)) then
           write(*,*)'Number of Lines or Rows couldnt be',NofL 
           stop
        end if
       ALLOCATE(utildata(NofL,NofR))
       ALLOCATE(inputdata(1:NofR)) 
    i=0
 OPEN(10,FILE=filein) 
  DO
     READ(10,FMT=*,IOSTAT=ioerror) inputdata(1:NofR) 
    !WRITE(35,'(300E15.7)')inputdata(1:NofR) !! be carefull with 300
     IF (ioerror==0) THEN 
      i=i+1
      utildata(i,2:NofR)=inputdata(2:NofR)
    ELSE IF (ioerror == -1) THEN
          IF (i == 0) THEN
             STOP 'Error:  Input file is possibly empty'
          END IF
          nMax = i
          EXIT
       ELSE
          STOP 'ERROR in line 94 scissors.f90'
       END IF
    ENDDO 
 close(10)
 
 do j=1,NofR
   do i=1,(NofL)  
     if (j.gt.(NBANDAV+1)) THEN
          ! BEGIN FIRST CONDUCTION BAND
      utildata(i,j)=utildata(i,j)+shiftscissors
     end if
   end do 
end do 
  
  do i=1,(NofL)
  WRITE(35,'(600E15.7)')i*1.d0,utildata(i,2:NofR)
  end do


 !OPEN(14,FILE=fileout, status="unknown")
   !do i=1,NofL
   ! WRITE(40,'(800E15.7)')i*1.d0,utildata(i,2:NofR)
   !  WRITE(40,'(800f15.12)')i*1.d0,utildata(i,2:NofR)
   !end do
 ! close(14)
!WRITE(35,'(300E15.7)')i*1.d0,utildata(i,2:NofR)
!WRITE(35,'(300E15.7)')inputdata(1:2) !! be carefull with 300

 deallocate(inputdata,utildata)
end program scissors 
 
