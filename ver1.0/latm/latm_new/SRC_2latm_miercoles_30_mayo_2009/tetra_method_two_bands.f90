!##########################
SUBROUTINE Transition_Index
!##########################
  USE globals, ONLY : ind_Trans, nMax
  IMPLICIT NONE
  INTEGER iv, ic, iTrans
  
  iTrans=0
  DO iv = 1,nMax
     DO ic = 1,nMax
        iTrans = iTrans+1
        ind_Trans(iv,ic)=iTrans
     END DO
  END DO
  WRITE(6,*) 'Transition_Index DONE'
!!!****************************
END SUBROUTINE Transition_Index
!!!****************************

!!!############################
SUBROUTINE Load_Tetrahedra_Data
!!!############################
!!! Reads tetrahedra indices and tetrahedra volumes

  USE globals, ONLY : tetCorner, tetrahedronVolume, tet_list_filename
  USE globals, ONLY : includeTetrahedron
  USE globals, ONLY : nTetra
  USE globals, ONLY : DP, PI
  IMPLICIT NONE
  INTEGER :: file_nTetra, iTetra
  REAL(DP) :: tVolume
  REAL(DP), ALLOCATABLE :: tetrahedraWeight(:)
  
  WRITE(6,'(A10,A80)') 'opening: ', tet_list_filename
  OPEN (1,FILE = tet_list_filename,FORM = 'formatted',STATUS = 'old')
  READ (1,*) file_nTetra
!!!  IF (file_nTetra.NE.nTetra) THEN
!!!     WRITE(6,*) ' '
!!!     WRITE(6,*) 'Inconsistency of first line of', &
!!!          tet_list_filename, 'with internal value for nTetra'
!!!     WRITE(6,*) 'First line of', tet_list_filename, ' is ', file_nTetra
!!!     WRITE(6,*) 'Internal value of nTetra is ', nTetra
!!!     WRITE(6,*) 'STOPPING'
!!!     WRITE(6,*) ' '
!!!     STOP
!!!  END IF
  
  nTetra = file_nTetra
  
  ALLOCATE( tetrahedraWeight(nTetra) )
  ALLOCATE(includeTetrahedron(nTetra))
  ALLOCATE(tetrahedronVolume(nTetra))
  ALLOCATE(tetCorner(nTetra,4))

  tetCorner(:,:) = 0 ! initialize tetCorner array
  tetrahedronVolume(1:nTetra) = 0.d0 ! initialize tetVolume array
    
  DO iTetra = 1, nTetra
     ! New format, that has the real actual volume of the tetrahedron, in atomic
     ! units, not the relative volume
     READ(1,*)  tetCorner(iTetra,1:4), tetrahedraWeight(iTetra), &
          tetrahedronVolume(iTetra)
!!!     READ(1,*)  tetCorner(iTetra,1:4), tetrahedronVolume(iTetra)
!!!     WRITE(6,1001) tetCorner(iTetra,1:4), tetrahedronVolume(iTetra)
     tetrahedronVolume(iTetra) = tetrahedraWeight(iTetra) * &
          tetrahedronVolume(iTetra)/(8.d0*PI**3)
  END DO
1001 FORMAT(4I6,E18.8)
  CLOSE(1)
  
  !tVolume = SUM(tetrahedronVolume(1:nTetra))
!!!   For old format, check that relative volumes add up to 1.
  !IF (ABS(tVolume-1.d0).GT.1d-6) THEN
  !   STOP 'Tetrahedra volumes do not add to one'
  !ELSE
  !   WRITE(6,*) 'Tetrahedra volumes add to ', tVolume
  !END IF
  
  DEALLOCATE( tetrahedraWeight )
  
  WRITE(6,*) 'Load_Tetrahedra_Data DONE'
!!!********************************
END SUBROUTINE Load_Tetrahedra_Data
!!!********************************

!!!########################
SUBROUTINE Which_Tetrahedra
!!!########################
!!! specify which tetrahedra to include in the calculation
  USE globals
  IMPLICIT NONE
  INTEGER :: iTetra
  
!!! Initialize all as false
  includeTetrahedron(1:nTetra) = .false.
  
!!! Initialize all as true
  includeTetrahedron(1:nTetra) = .true.
  
  !! dihydride surface 81 kpts include gamma point only
  !! includeTetrahedron(1:3) = .true.
  !! includeTetrahedron(91:96) = .true.

  WRITE(6,*) 'Which_Tetrahedra DONE'
!!!****************************
END SUBROUTINE Which_Tetrahedra
!!!****************************

!!!###########################
SUBROUTINE Which_Transitions
!!!###########################
!!! determine which transition to include in the calculation
  USE globals, ONLY: ind_Trans, nMax, nVal, nMax_tetra, nVal_tetra
  USE globals, ONLY: includeTransition
  IMPLICIT NONE
  INTEGER :: iTrans, iv, ic, low_band, high_band
  INTEGER :: thisIV, thisIC
  !!!!!!!!!!
  !!!!!!!!!!
  !!! 27 Abril 2009  cabellos 
  integer :: includeValenceBands(1000)
  integer :: includeConductionBands(1000)
  includeValenceBands(:)=0
  includeConductionBands(:)=0   !!! right now cabellos
   !includeValenceBands(145)=1
   !includeValenceBands(146)=1
   !includeValenceBands(147)=1
   !includeValenceBands(148)=1
   includeValenceBands(8)=1
!   includeValenceBands(150)=1
  !! conductions 
  includeConductionBands(9)=1    
!  includeConductionBands(152)=1
!  includeConductionBands(153)=1    
!  includeConductionBands(154)=1
!  includeConductionBands(155)=1    
!  includeConductionBands(156)=1

  DO iv = 1, nMax
     DO ic = 1, nMax
        iTrans = ind_Trans(iv,ic)
        includeTransition(iTrans)=.false.
     END DO
  END DO
  low_band = nVal + 1 - nVal_tetra
  high_band = nVal + nMax_tetra
  DO iv = low_band, nVal
     DO ic = nVal+1, high_band
!!!!!!!!!!! IF ((iv == nVal-3).OR.(iv == nVal-2)) THEN
!!!!!!!!!!! IF (iv == thisIV) THEN
         IF (includeValenceBands(iv).eq.1) THEN
!!!!!!!!!!!!!  IF ((ic == nVal+1).OR.(ic == nVal+2)) THEN
        !!!!!! IF (ic == thisIC) THEN
            IF (includeConductionBands(ic).eq.1) THEN
              includeTransition(ind_Trans(iv,ic))=.true.
            END IF
        END IF
     END DO
  END DO
  
  WRITE(6,*) 'Which_Transitions DONE'
!!!*****************************
END SUBROUTINE Which_Transitions
!!!*****************************

!!!##################################
SUBROUTINE Load_Transition_Energies
!!!##################################
  USE constants, ONLY: DP
  USE globals, ONLY: ind_Trans,energys_data_filename, kMax, nMax
  USE globals, ONLY: Transition_Energy
  IMPLICIT NONE
  REAL(DP) :: rdummy, edifference
  REAL(DP), ALLOCATABLE :: energy(:,:)
  REAL(DP), ALLOCATABLE :: energy1(:)
  INTEGER :: file_ik, iband, iv, ic, ik, iTrans, istat, ios
  
  WRITE(6,*) 'sroutine Load_Transition_Energies started'
  
  ALLOCATE(energy(kMax,nMax),STAT=istat)
  
  IF (istat /= 0) THEN
     WRITE(6,*) ' '
     WRITE(6,*) ' Could not allocate array energy(:,:)'
     WRITE(6,*) ' Error code is', istat
     WRITE(6,*) ' Stopping '
     WRITE(6,*) ' '
     STOP
  END IF
  
  ALLOCATE(energy1(nMax),STAT=istat)
  
  IF (istat /= 0) THEN
     WRITE(6,*) ' '
     WRITE(6,*) ' Could not allocate array energy(:,:)'
     WRITE(6,*) ' Error code is', istat
     WRITE(6,*) ' Stopping '
     WRITE(6,*) ' '
     STOP
  END IF
  
  WRITE(6,'(A10,A80)') 'opening: ', energys_data_filename
  OPEN (1, FILE = energys_data_filename, &
       FORM = 'FORMATTED', STATUS = 'OLD', IOSTAT=ios)
  IF (ios /=0 ) THEN
     WRITE(6,*) ' '
     WRITE(6,*) ' Could not open file ', energys_data_filename
     WRITE(6,*) ' Error code is ', istat
     WRITE(6,*) ' Stopping '
     WRITE(6,*) ' '
     STOP
  END IF
  DO ik = 1, kMax
     WRITE(6,*) ik
     READ (UNIT=1,FMT=*,IOSTAT=ios) file_ik, energy(ik,1:nMax)
!     READ (UNIT=1,FMT=*,IOSTAT=ios) file_ik, energy1(1:nMax)
!     WRITE(6,*) ios
!     WRITE(6,*) energy1(1:nMax)
! old code
!     IF (file_ik.NE.ik) PAUSE & 
!             'ERROR IN Load_transitions: file_ik not equal to ik'
! end old code
! new code
     IF (file_ik.NE.ik) THEN
        WRITE(6,*) 'ERROR IN Load_transitions file_ik .ne. ik'
        STOP
     END IF
! end new code
     DO iband = 1, nMax-1
        IF (energy(ik,iband+1).LT.energy(ik,iband)) THEN
!        IF (energy1(iband+1).LT.energy1(iband)) THEN
           WRITE(*,*) ik, iband
           PAUSE 'ENERGY OUT OF ORDER'
        END IF
     END DO
     ! WRITE (*,*) file_ik, energy(ik,1:nMax)
  END DO
  CLOSE(1)
  
!!! STOP 'STOPPING'
  
!!!  WRITE(6,*) 'Transitions energies between HOVB and LUCB'
!!!  WRITE(6,*) energy(1:kMax,nVal+1)-energy(1:kMax,nVal)
  
!!! cycle over energy differences to make transitions
  
  DO ik = 1, kmax
     DO iv = 1, nMax
        DO ic = 1, nMax
           iTrans = ind_Trans(iv,ic)
           edifference = energy(ik,ic) - energy(ik,iv)
!           WRITE(6,*) ik, iv, ic
           Transition_Energy(ik,iTrans) = edifference
        END DO
     END DO
  END DO
  
  WRITE(6,*) 'Load_Transition_Energies DONE'
!!!************************************
END SUBROUTINE Load_Transition_Energies
!!!************************************

!!!######################
SUBROUTINE Load_Integrand
!!!######################
  USE globals, ONLY: ind_Trans, kMax, nVal, nMax, units_factor
  USE globals, ONLY: integrand_filename, f
!!!  USE globals, ONLY: optical_property
  IMPLICIT NONE
  INTEGER  :: ik, iTrans, iv, ic
  CHARACTER(LEN=3) :: ADV_opt
  INTEGER :: read_status
  
!!!  IF (optical_property.EQ.0) THEN
!!!     WRITE(6,*) 'Calculating joint dos so not loading integrand'
!!!     f(:,:) = 1.d0
!!!  ELSE
  WRITE(6,'(A10,A80)') 'opening: ', integrand_filename
  OPEN(1, FILE = integrand_filename, STATUS = "old")
  READ(UNIT=1,FMT=*) units_factor
  WRITE(6,*) 'Overall factor : ', units_factor
  DO ik = 1, kMax
!!!        WRITE(6,*) ik
     DO iv = 1, nVal
        DO ic = nVal+1, nMax
           IF (ic==nMax) THEN
              ADV_opt = "YES"
           ELSE
              ADV_opt = "NO"
           END IF
           iTrans=ind_Trans(iv,ic)
           ! WRITE(6,*) ik, iTrans
           IF (ic==nMax) THEN
              READ(UNIT=1,FMT='(E15.7)',ADVANCE="YES",&
                   IOSTAT=read_status) f(ik,iTrans)
           ELSE
              READ(UNIT=1,FMT='(E15.7)',ADVANCE="NO",&
                   IOSTAT=read_status) f(ik,iTrans)
           END IF
!           READ(UNIT=1,FMT='(E15.7)',ADVANCE=ADV_opt,&
!                IOSTAT=read_status) f(ik,iTrans)
           IF (read_status.GT.0) THEN
              WRITE(6,*) 'Error in Load_integrand'
              WRITE(6,*) read_status, ik, ic, iv, iTrans
              STOP
           ELSE IF(read_status==-1) THEN
              WRITE(6,*) ik, iv, ic
              WRITE(6,*) 'End of file', integrand_filename
           ELSE IF(read_status.LT.-1) THEN
              WRITE(6,*) read_status
              WRITE(6,*) 'Strange Error: Load Integrand'
              STOP
           END IF
           ! WRITE(6,*) ik, iTrans, f(ik,iTrans)
        END DO
     END DO
  END DO
  CLOSE(1)
  !  END IF
  WRITE(6,*) 'Load_Integrand DONE'
!!!**************************
END SUBROUTINE Load_Integrand
!!!**************************

!!!########################
SUBROUTINE Load_Energy_Mesh
!!!########################
  USE globals
  IMPLICIT NONE
  REAL(DP) :: eStepsize
  INTEGER :: i
  eStepsize = (energy_Max-energy_Min)/(energy_Steps-1)
  DO i=1, energy_Steps
     energy_Out(i) = DFLOAT(i-1)*eStepSize
  END DO
  WRITE(6,*) 'Load_Energy_Mesh DONE'
!!!****************************
END SUBROUTINE Load_Energy_Mesh
!!!****************************

!!!#########
PROGRAM LATM
!!!#########
  USE globals
!!!  USE energy_mesh
  IMPLICIT NONE
  INTEGER :: j, iTmp1
!!! variables for final output
  INTEGER :: iEnergy
  REAL(DP) :: energy
  REAL(DP), ALLOCATABLE :: pp(:,:)
! 
  REAL(DP), ALLOCATABLE :: preTotal(:,:)
  REAL(DP) :: total
  REAL(DP) :: cornTrans(4), fh
  INTEGER :: flag1, ibug1
  REAL*8 :: p(4)
  REAL*8 :: e21, e31, e42, de
! pp is the total contribution at one energy for one transition.
  CHARACTER(LEN=4) :: int2cha
  REAL*8 :: S1, S2, S3, S4
  INTEGER :: iTetra, iTrans, ic, iv
  INTEGER :: iostat
  
  CALL getarg(1,paramFile)
  
  CALL readParamFile
  CALL Allocate_Global_Arrays
  CALL Transition_Index
  CALL Load_Tetrahedra_Data
  CALL Which_Tetrahedra
  WRITE(6,*) 'Calling Load_Transition_Energies'
  CALL Load_Transition_Energies
  CALL Which_Transitions
  CALL Load_Integrand
  CALL Load_Energy_Mesh
  
  WRITE(6,*) 'Total transitions= ', nTrans
  WRITE(*,*) 'tetrahedron MAIN started'
  WRITE(6,*) 'infront: ', units_factor
  
  ALLOCATE(pp(energy_Steps,nMax*nMax))
  pp = 0.d0  ! initialize pp !!!!! R E A L L Y   I M P O R T A N T
  ALLOCATE(preTotal(energy_Steps,nTrans))
  
! cycle over valence and conduction levels
  
  DO iv = 1, nVal
     DO ic = nVal+1, nMax
        iTrans = ind_Trans(iv,ic)
        IF (includeTransition(iTrans)) THEN
           WRITE(6,*)'only this valence ', iv,' and this conduction ',ic,' are included '
!           WRITE(6,*) iTrans, Transition_Energy(1:kMax,iTrans)
  
! cycle over tetrahedra
           DO iTetra = 1, nTetra
  
! this determines if we want to include this tetrahedron in the
! calculation
              IF (includeTetrahedron(iTetra)) THEN
                
! Store corner energy transistions in cornTrans
! Get integrand at corners of tetrahedron
!                 WRITE(6,*) tetCorner(iTetra,1:4)
!                 WRITE(6,*) tetCorner(iTetra,:)
                 DO j = 1, 4
                    iTmp1 = tetCorner(iTetra,j)
                    cornTrans(j) = Transition_Energy(iTmp1,iTrans)
                    p(j) = f(tetCorner(iTetra,j), iTrans)
!!!!! For joint density of states uncomment the next line
!!!!!! and make p(j)=1.d0
!!!                   p(j) = 1.d0
                 END DO
!                 WRITE(6,*) p(:)
!                
! Sort energies at corners into ascending order.
! Also, simultaneously rearrange the function values.
!                
                 CALL piksort(4,cornTrans,p)
                 
! Write some information - not necessary
!                 IF (cornTrans(1).LT.0.33d0) THEN
!                    WRITE(6,*) iTetra
!                    WRITE(6,*) cornTrans
!                    WRITE(6,*) p
!                    WRITE(6,*) tetrahedronVolume(iTetra)
!                    WRITE(6,*) ' '
!                    p=0.d0
!                    PAUSE
!                 END IF
                 
                 DO ienergy=1, energy_Steps
                    energy = energy_Out(ienergy)
              IF (energy.GE.cornTrans(1).AND.energy.LT.cornTrans(2)) THEN
! in region I
                pp(ienergy,iTrans)=pp(ienergy,iTrans) &
                     + S1(energy,cornTrans,p)*tetrahedronVolume(iTetra)
             ELSE IF (energy.GE.cornTrans(2).AND.energy.LT.cornTrans(3)) THEN
! in region II
                pp(ienergy,iTrans)=pp(ienergy,iTrans) &
                     + S3(energy,cornTrans,p)*tetrahedronVolume(iTetra)
             ELSE IF (energy.GE.cornTrans(3).AND.energy.LT.cornTrans(4)) THEN
! in region III
                pp(ienergy,iTrans)=pp(ienergy,iTrans) &
                     + S4(energy,cornTrans,p)*tetrahedronVolume(iTetra)
             END IF
                 END DO     ! iEnergy
!                  END IF  ! Energy restriction
              END IF     ! includeTetrahedron
           END DO     ! iTetra
        END IF     ! includeTransition
     END DO    ! ic
  END DO     ! iv
! output
  WRITE(6,*) 'opening: ', spectrum_filename
  OPEN(77,FILE=spectrum_filename)
  DO ienergy=1, energy_Steps
     energy = energy_Out(ienergy)
     WRITE(77,99) energy, SUM(pp(ienergy,1:nMax*nMax))*units_factor
99   FORMAT(2E15.7)
  END DO
  CLOSE(77)
!***************
END PROGRAM LATM
!***************

!##############################
FUNCTION S1(energy,cornTrans,p)
!##############################
!
! Case 1. e1<e<e2
!
  USE constants
  IMPLICIT NONE
  REAL(DP) :: S1, energy, cornTrans(4), p(4)
  REAL(DP) :: de, e21, e31, e41, fh
  de = energy - cornTrans(1)
  e21 = cornTrans(2) - cornTrans(1)
  e31 = cornTrans(3) - cornTrans(1)
  e41 = cornTrans(4) - cornTrans(1)
! choice 1
  fh = 3*p(1) + (p(2)-p(1))*de/e21 + (p(3)-p(1))*de/e31 + (p(4)-p(1))*de/e41
  fh = fh/3.d0
! choice 2
!  fh = 0.25d0*SUM(p)
  S1 = 3.d0*fh*de**2/(e21*e31*e41)
  RETURN
!**************
END FUNCTION S1
!**************

!##############################
FUNCTION S2(energy,cornTrans,p)
!##############################
  USE constants
  IMPLICIT NONE
  REAL(DP) :: S2, energy, cornTrans(4), p(4)
  REAL(DP) :: de, e21, e32, e42, fh
  de = energy - cornTrans(2)
  e21 = cornTrans(2) - cornTrans(1)
  e32 = cornTrans(3) - cornTrans(2)
  e42 = cornTrans(4) - cornTrans(2)
!!!!!!!!!!!!!!!!!!!!!!!!!! CHECK THIS LINE !!!!!!!!!!!!!!!!!!!!!!!!
!fh = 3*p(2) + (p(1)-p(2))*de/e12 + (p(3)-p(2))*de/e32 + (p(4)-p(2))*de/e42
!  fh =      p(1) + (p(2)-p(1))*(energy-cornTrans(1))/e21
!  fh = fh + p(2) + (p(3)-p(2))*de/e32
!  fh = fh + p(2) + (p(4)-p(2))*de/e42
!  fh = fh/3.d0
  fh = 0.25d0*SUM(p)
  S2 = 3.d0*fh*de**2/(e21*e32*e42)
!  If (S2.GT.10.d0) pause
  RETURN
!**************
END FUNCTION S2
!**************

!##############################
FUNCTION S3(energy,cornTrans,p)
!##############################
!
! Case 2. e1<e<e2
!
  USE constants
  IMPLICIT NONE
  REAL(DP) :: S1, S2, S3, energy, cornTrans(4), p(4)
  REAL(DP) :: e, e1, e2, e3, e4, ee1234, e1234 
  REAL(DP) :: f1, f2, f3, f4
  REAL(DP) :: de, de1, de2, de3, de4
  REAL(DP) :: tmp, tmp1, tmp2
  REAL(DP) :: fh
  REAL(DP) :: numerator, denominator
  
! Choice one: explicit formula for the area
  e = energy
  e1 = cornTrans(1)
  e2 = cornTrans(2)
  e3 = cornTrans(3)
  e4 = cornTrans(4)
!  ee1234 = e*e*(e1+e2-e3-e4)
!  e1234  = e*(e1*e2-e3*e4)
! 
!  fh = 0.25d0*SUM(p)
!  numerator = -e1*e2*e3 - e1*e2*e4 + e1*e3*e4 + e2*e3*e4
!  numerator = numerator + 2.d0*e1234 - ee1234
!  denominator = -(e3-e1)*(e4-e1)*(e3-e2)*(e4-e2)
!  S3 = 3.d0*fh*numerator/denominator
  
! Choice two: the difference of two areas
!  S3 = S1(energy,cornTrans,p) - S2(energy,cornTrans,p)
!  If you use choice two, you must uethe average of the function
!  over the entire tetrahedron!!!!!

! Choice three: Bernd and Claudia's version
  
  f1 = p(1)
  f2 = p(2)
  f3 = p(3)
  f4 = p(4)
  
  de  = e - e2
  de1 = e3 - e1
  de2 = e4 - e2
  de3 = e3 - e2
  de4 = e2 - e1
  
  tmp = de*(e3-e)/(de2*de1*de3)
  tmp1 = tmp*( f1 + 2.d0*f2 + (f3-f1)*de4/de1 + de*(    &
         (f3-f1)/de1 + (f4-f2)/de2 + (f3-f2)/de3) )
  
  de  = e - e1
!  de1 = e3 - e1  ! same as above
!  de2 = e4 - e2  ! same as above
  de3 = e4 - e1
!  de4 = e2 - e1  ! same as above
  
  tmp = de*(e4-e)/(de2*de1*de3)
  tmp2 = tmp*( f2 + 2.d0*f1 + (f2-f4)*de4/de2 + de*(     &
         (f3-f1)/de1 + (f4-f2)/de2 + (f4-f1)/de3) )
  
  S3 = tmp1 + tmp2
  
  RETURN
!**************
END FUNCTION S3
!**************

!##############################
FUNCTION S4(energy,cornTrans,p)
!##############################
!
! Case 3. e3<e<e4
!
  USE constants
  IMPLICIT NONE
  REAL(DP) :: S4, energy, cornTrans(4), p(4)
  REAL(DP) :: de, e14, e24, e34, fh
  de = energy - cornTrans(4)
  e14 = cornTrans(1) - cornTrans(4)
  e24 = cornTrans(2) - cornTrans(4)
  e34 = cornTrans(3) - cornTrans(4)
! choice 1
  fh = 3*p(4) + (p(1)-p(4))*de/e14 + (p(2)-p(4))*de/e24 + (p(3)-p(4))*de/e34
  fh = fh/3.d0
! choice 2
!  fh = 0.25d0*SUM(p)
  S4 = 3.d0*fh*de**2/DABS(e14*e24*e34)
  RETURN
!**************
END FUNCTION S4
!**************
