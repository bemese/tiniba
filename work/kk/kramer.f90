MODULE KK
  IMPLICIT NONE
  INTEGER, PARAMETER :: DP = KIND(1.d0)
  REAL(DP), PARAMETER :: pi = 3.14159265358979
  REAL(DP), ALLOCATABLE :: arr(:), x(:)
CONTAINS
!========================================================  
  SUBROUTINE READFILE(FNAME,nMax)
!========================================================
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: fname
    INTEGER, INTENT(INOUT) :: nMax
    REAL(DP), ALLOCATABLE :: array_tmp(:)
    REAL(DP) :: tot, energy, h, intmpx, intmpf
    INTEGER :: i, ioerror
    nMax = 1
    ALLOCATE( arr(nMax), x(nMax) )
    OPEN(UNIT = 1,FILE = FNAME)
    PRINT *, 'FILE IS OPEN'
    i=0
    DO
       READ(1,FMT=*,IOSTAT=ioerror) intmpx, intmpf
       !PRINT *,'ioerror ', ioerror
       IF (ioerror==0) THEN
          !PRINT *, i
          i=i+1
          !CALL PUSH(intmpx,x)
          ALLOCATE(array_tmp(i))
          array_tmp=x
          DEALLOCATE(x)
          ALLOCATE(x(i))
          x=array_tmp
          x(i)=intmpx
          !PRINT *, i, x(i)
          DEALLOCATE(array_tmp)
          !CALL PUSH(intmpf,arr)
          ALLOCATE(array_tmp(i))
          array_tmp = arr
          DEALLOCATE(arr)
          ALLOCATE(arr(i))
          arr=array_tmp
          arr(i)=intmpf
          DEALLOCATE(array_tmp)
       ELSE IF (ioerror == -1) THEN
          IF (i == 0) THEN
             STOP 'Error:  Input file is possibly empty'
          END IF
          nMax = i
          EXIT
       ELSE
          STOP 'ERROR'
       END IF
    ENDDO
  END SUBROUTINE READFILE

!========================================================
  SUBROUTINE WRITEFILE(OUTFILE,x,res,arr,nMax)
!========================================================
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN) :: OUTFILE
    INTEGER, INTENT(IN) :: nMax
    REAL(DP), INTENT(IN) :: x(nmax), res(nMax), arr(nMax)
    INTEGER :: i
    OPEN(UNIT=2,FILE=OUTFILE)
    DO i = 1, nMax
       WRITE(2,*) x(i), res(i), arr(i)
    ENDDO
    CLOSE(2)    
  END SUBROUTINE WRITEFILE
  
!========================================================
  SUBROUTINE INTEGRATE(nMax,x,arr,res)
!========================================================
    IMPLICIT NONE
    INTEGER, INTENT (IN) :: nMax
    REAL(DP), INTENT(IN) :: arr(nMax), x(nMax)
    REAL(DP), INTENT(INOUT) :: res(nMax)
    REAL(DP) :: integrand1(nMax), integrand2(nMax)
    REAL(DP) :: energy
    INTEGER :: i
    
    integrand1(2:nMax) = arr(2:nMax)/x(2:nMax)
    res(1) = SUM(integrand1(2:nMax))
    print *, res(1)
    ! now the rest
    DO i = 2, nMax-1
       ! print *, i
       ! CALL integrand(nMax, arr, x, i, g)
       energy = x(i)
       integrand1(1:nMax) = arr(1:nMax)/(x(1:nMax)+energy)
       integrand2(1:i-1) = arr(1:i-1)/(x(1:i-1)-energy)
       integrand2(i+1:nMax) = arr(i+1:nMax)/(x(i+1:nMax)-energy)
       res(i) = SUM(integrand1)
       res(i) = res(i) + SUM(integrand2(1:i-1))
       res(i) = res(i) + SUM(integrand2(i+1:nMax))
       res(i) = 0.5d0*res(i)
       ! print *, x(i), res(i)
    END DO
    ! Now for i=nMax
    energy = x(nMax)
    integrand1(1:nMax) = arr(1:nMax)/(energy + x(1:nMax))
    integrand2(1:nMax-1) = arr(1:nMax-1)/(energy - x(1:i-1))
    res(nMax) = SUM(integrand1(1:nMax)) + SUM(integrand2(1:nMax-1))
    res(nMax) = 0.5d0*res(nMax)
  END SUBROUTINE INTEGRATE

!========================================================
  SUBROUTINE INTEGRATEANOMALOUS(nMax,x,arr,res)
!========================================================
    IMPLICIT NONE
    INTEGER, INTENT (IN) :: nMax
    REAL(DP), INTENT(IN) :: arr(nMax), x(nMax)
    REAL(DP), INTENT(INOUT) :: res(nMax)
    REAL(DP) :: integrand1(nMax), integrand2(nMax)
    REAL(DP) :: energy
    INTEGER :: i
    
    res(1) = 0.d0
    ! now the rest
    DO i = 2, nMax-1
       ! print *, i
       ! CALL integrand(nMax, arr, x, i, g)
       energy = x(i)
       integrand1(1:nMax) = arr(1:nMax)/(x(1:nMax)+energy)
       integrand2(1:i-1) = arr(1:i-1)/(x(1:i-1)-energy)
       integrand2(i+1:nMax) = arr(i+1:nMax)/(x(i+1:nMax)-energy)
       res(i) = -SUM(integrand1)
       res(i) = res(i) + SUM(integrand2(1:i-1))
       res(i) = res(i) + SUM(integrand2(i+1:nMax))
       res(i) = 0.5d0*res(i)
       ! print *, x(i), res(i)
    END DO
    ! Now for i=nMax
    energy = x(nMax)
    integrand1(1:nMax) = arr(1:nMax)/(energy + x(1:nMax))
    integrand2(1:nMax-1) = arr(1:nMax-1)/(energy - x(1:i-1))
    res(nMax) = SUM(integrand1(1:nMax)) + SUM(integrand2(1:nMax-1))    
    res(nMax) = 0.5d0*res(nMax)
  END SUBROUTINE INTEGRATEANOMALOUS
END MODULE KK

!=========================================================
PROGRAM KRAMERSKRONIG
!=========================================================
  !A simple implementation of kramers-kronig
  USE KK
  IMPLICIT NONE
  INTEGER :: nMax
  REAL(DP), ALLOCATABLE :: res(:)
  REAL(DP) :: tot, h
  CHARACTER(1) :: flag
  CHARACTER(60) :: fname, ofname
  
  WRITE (6,*) 'KRAMERS-KRONIG v0.1  -- NO GARAUNTEES!'
  
  CALL GETARG(1,flag)
  IF (flag == '1') THEN
     PRINT *,'Usual Kramers Kronig Calculation'
  ELSE IF(flag == '2')THEN
     PRINT *,'Anomalous Kramers Kronig Calculation'
  ELSE
     PRINT *, 'Wrong option for inital flag'
     PRINT *, 'Usage:'
     PRINT *, '  KramersKronig [method flag] [input file] [output file]'
     PRINT *, 'method flag = 1 --> Normal Kramers-Kronig'
     PRINT *, 'method flag = 2 --> Annomalous Kramers-Kronig'
     STOP
  END IF
  CALL GETARG(2,fname)
  PRINT *,'OPENING ',fname
  CALL GETARG(3,ofname)
  PRINT *,'OUTPUT WILL BE WRITTEN TO ', ofname
  
  CALL READFILE(fname,nMax)
  
  ! x and arr should be read by now. nMax should be the array length
  
  ALLOCATE(res(nMax))  ! the result goes in res
  
  ! Output some info from File
  WRITE(6,*) 'From the file, I learned that:'
  WRITE(6,*) 'o xmin is ', x(1)
  WRITE(6,*) 'o xmax is ', x(nMax)
  tot = SUM(x)
  h = tot * 2.d0/(nMax)/(nMax-1)
  WRITE(6,FMT='(A28,E18.10)') 'o spacing is approximately ',h
  
  IF (flag == '1') CALL INTEGRATE(nMax,x,arr,res)
  IF (flag == '2') CALL INTEGRATEANOMALOUS(nMax,x,arr,res)
  
  res = res*h*2.d0/pi
  
  CALL WRITEFILE(ofname,x,res,arr,nMax)
  
END PROGRAM KRAMERSKRONIG

!**************************************************
