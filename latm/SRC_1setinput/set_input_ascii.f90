!#BMSVer3.0d
! ??Linear resposne and INJECTION CURRENT MUST BE CODIFIED ANEW!!!!!
! Linear response DONE sep/7/2014
! Injection Current ????
!#BMSVer3.0u
!#########################################################
! January, 2005
! 
!(Additions by Ben Wilson, August 7, 2003)
!
! Modified for calligraphic P.
! 
! THE FOLLOWING DESCRIPTION NEEDS TO BE UPDATED
! 
! To do:
! Include tolerance option to remove the contribution when the
! tolerance threshold is violated.
! 
! Purpose:
! 
! The sole purpose of this porgram is to create input files
! for my tetrahedron code tetra_method
! 
! Takes raw momentum matrix elements, and generates matrix
! elements for position, generalized derivative of position,
! and energy transitions.
!#BMSVer3.0d
! and layered velocity matrix elements
!#BMSVer3.0u
! 
! Applies scissor shift to the energies
! 
! Creates input file for LATM code
! 
! Uses function position.f                                    
! Uses function genderiv.f                                   
! 
! Control file determines which spectrum integrand is
! calculated.
!                                                                 
! For each transition there is an output file for both the       
! derivative and generalized derivative.                         
! For the position file on each line there is the                
!   kpoint index, r^x, r^y, r^z                                  
! For the generalized derivative file the data is grouped like   
!   kpoint index, r^x;x, r^y;x, r^z;x                            
!   kpoint index, r^x;y, r^y;y, r^z;y                            
!   kpoint index, r^x;z, r^y;z, r^z;z                            
! 
!!!##############
PROGRAM set_input
!!!##############
  USE inparams, ONLY : checkCommandLineInputs
  USE inparams, ONLY : readParamFile, readSpectrumFile
  USE inparams, ONLY : nSpinor
  USE inparams, ONLY : paramFile, spectrumFile
  USE inparams, ONLY : debug
  USE arrays, ONLY : DP, DPC, nVal, nMax, nSym, kMax, scissor
  USE arrays, ONLY : energy_data_filename, energys_data_filename
  USE arrays, ONLY : pmn_data_filename, rmn_data_filename
  USE arrays, ONLY : der_data_filename, smn_data_filename
  USE arrays, ONLY : rhomm_data_filename
  USE arrays, ONLY : allocateArrays, deallocateArrays
  USE symmetryOperations, ONLY : initializeSymOps
  USE file_control, ONLY : openOutputDataFiles
  USE file_control, ONLY : closeOutputDataFiles
  USE file_control, ONLY : writeOutputDataFileHeaders
  USE arrays, ONLY : momMatElem, posMatElem, derMatElem, Delta, spiMatElem
  USE arrays, ONLY : GenDerCalPosition
!!! BMS/FN
  USE arrays, ONLY : calrho
  USE arrays, ONLY : calDelta
  USE arrays, ONLY : calMomMatElem, cal_data_filename
!#BMSVer3.0d
  USE arrays, ONLY : cfMatElem,cfmn_data_filename
  USE arrays, ONLY : gdVlda
  USE arrays, ONLY : gdVsig
  USE arrays, ONLY : gdf
  USE arrays, ONLY : vldaMatElem
  USE arrays, ONLY : gdcalVlda
  USE arrays, ONLY : gdcalVS
  USE arrays, ONLY : gdcalVsig
  USE arrays, ONLY : calVsig
  USE arrays, ONLY : f
!#BMSVer3.0u
  !!!!!!!!!!
  Use arrays, ONLY : calPosMatElem
  USE arrays, ONLY : efe
  !!!!!!!!!!
  !#BMSVer3.0d
  USE inparams, ONLY : vnlkss
  !#BMSVer3.0u
!  USE arrays, ONLY : denMatElem, den_data_filename
  USE arrays, ONLY : curMatElem, cur_data_filename
  USE arrays, ONLY : layeredCalculation
  USE arrays, ONLY : ndotCalculation
  USE arrays, ONLY : spinCalculation
  USE arrays, ONLY : layeredInjectionCurrent
!  USE arrays, ONLY : microscopicDensityCalculation
!  USE arrays, ONLY : microscopicCurrentCalculation
!!! BMS/FN
  USE arrays, ONLY : energy, energys, band, readenergyfile, scissorenergies
  USE arrays, ONLY : oldStyleScissors
  USE functions, ONLY : position, genderiv
  USE functions, ONLY : GenDerCalPositionf
  !#BMSVer3.0d
  USE functions, ONLY : calVlda,calVscissors
  !#BMSVer3.0u
  USE integrands, ONLY : calculateintegrands
  USE functions, ONLY : calposition
  IMPLICIT NONE
  
  INTEGER :: i, ii, ij, iii, l
  INTEGER :: ik, iv, ic
  INTEGER :: vp,cp
  INTEGER :: io_status
  REAL(DP) :: matTemp(6)
  REAL(DP) :: matTemp3(3)
  !#BMSVer3.0d
  REAL(DP) :: matTemp2(3)
  COMPLEX (DPC) :: t1,t2
  !#BMSVer3.0u
  COMPLEX (DPC) :: tmpm
  COMPLEX(DPC) :: ci
  REAL (DP) :: scissorFactor
  INTEGER :: checkflag
  LOGICAL :: writeoutMEdata   ! flag controlling whether matrix element output
  !                             is written or not. Much slower if outputting.
  !-------------------------------------------------------------------
  call system("rm -f endWELLpmn")
  writeoutMEdata = .false.
  oldStyleScissors = .false.
  IF (oldStyleScissors) THEN
     WRITE(*,*) 'Using Hughes and Sipe version of scissors correction'
  END IF
  
  CALL getarg(1,paramFile)
  CALL getarg(2,spectrumFile)
  
  
  CALL checkCommandLineInputs
  CALL readParamFile
  CALL readSpectrumFile

  ! Test opening and closing files
  IF (debug) WRITE(6,*) "Testing opening and closing files"
  CALL openOutputDataFiles
  CALL closeOutputDataFiles
  IF (debug) WRITE(6,*) "Testing opening and closing files succeeded"
  
  !#BMSVer3.0d
  ! debug
  !if(vnlkss)then
  !   write(*,*)'vnl will be included'
  !else
  !   write(*,*)'vnl not included'
  !end if
  !stop 'set_input_ascii.f90:stop'
  !#BMSVer3.0u
!  write(*,*)"%%%%%%%%%%%%%%%%%%%%%%"

  OPEN(11, FILE=pmn_data_filename, STATUS='OLD', IOSTAT=io_status)
  IF (io_status /= 0) THEN
     WRITE(6,*) "Error occured trying to open:", pmn_data_filename
     WRITE(6,*) "Error status returned is:", io_status
     WRITE(6,*) "Stopping"
     STOP "Stopping: error with momentum matrix element file"
!  else
!     WRITE(6,*) "no problem opening:", pmn_data_filename
  END IF
  
!!!BMS
!  IF (nSpinor == 2) THEN
  INQUIRE(FILE=smn_data_filename, EXIST=spinCalculation)
  if ( spinCalculation) then
!     WRITE(*,*) "Found file ", TRIM(smn_data_filename), " => Performing spin calculation"
     OPEN(41, FILE=smn_data_filename)
!  ELSE
!     WRITE(6,*) "no smn_data_filename => no-spin calculation"
  END IF
!  END IF

  INQUIRE(FILE=cal_data_filename, EXIST=layeredCalculation)
  IF ( layeredCalculation ) THEN
!     WRITE(*,*) "Found file ", TRIM(cal_data_filename), " => Performing layer P calculation"
     OPEN(16, FILE=cal_data_filename,IOSTAT=io_status)
!  else
!     WRITE(6,*) "no cal_data_filename => no-caligraphic P calculation"
  END IF

  !#BMSVer3.0d
    INQUIRE(FILE=cfmn_data_filename, EXIST=layeredCalculation)
  IF ( layeredCalculation ) THEN
!     WRITE(*,*) "Found file ", TRIM(cfmn_data_filename), " => Performing layer P calculation"
     OPEN(53, FILE=cfmn_data_filename,IOSTAT=io_status)
!  else
!     WRITE(6,*) "no cfmn_data_filename => no-caligraphic P calculation"
  END IF
  !#BMSVer3.0u

  INQUIRE(FILE=cur_data_filename, EXIST=layeredInjectionCurrent)
  IF ( layeredInjectionCurrent ) THEN
!     WRITE(*,*) "Found file ", TRIM(cur_data_filename), " => Performing layer dotJ calculation"
     OPEN(17, FILE=cur_data_filename,IOSTAT=io_status)
!  else
!     WRITE(6,*) "no cur_data_filename => no-caligraphic dotJ calculation"
  END IF
  
  INQUIRE(FILE=rhomm_data_filename, EXIST=ndotCalculation)
  IF ( ndotCalculation ) THEN
!     WRITE(*,*) "Found file ", TRIM(rhomm_data_filename), " => Performing ndot layered calculation"
     OPEN(69, FILE=rhomm_data_filename,IOSTAT=io_status)
!  else
!     WRITE(6,*) "no rhomm_data_filename => no-ndot calculation"
  END IF
  
!!!
!     INQUIRE(FILE=den_data_filename,EXIST=microscopicDensityCalculation)
!     IF ( microscopicDensityCalculation ) THEN
!        OPEN(16, FILE=den_data_filename,IOSTAT=io_status)
!        IF (io_status /= 0) THEN
!           WRITE(6,*) "Error occured trying to open:", den_data_filename
!           WRITE(6,*) "Error status returned is:", io_status
!           WRITE(6,*) "Stopping"
!           STOP "Stopping: error with density matrix element file"
!        END IF
!     END IF
!!!
!     INQUIRE(FILE=cur_data_filename,EXIST=microscopicCurrentCalculation)
!     IF ( microscopicCurrentCalculation ) THEN
!        OPEN(17, FILE=cur_data_filename,IOSTAT=io_status)
!        IF (io_status /= 0) THEN
!           WRITE(6,*) "Error occured trying to open:", cur_data_filename
!           WRITE(6,*) "Error status returned is:", io_status
!           WRITE(6,*) "Stopping"
!           STOP "Stopping: error with current matrix element file"
!        END IF
!     END IF
!!!
!!!BMS
!  write(*,*)"%%%%%%%%%%%%%%%%%%%%%%"
  
  CALL allocateArrays
  CALL readenergyfile
  CALL scissorenergies
  
  IF (writeoutMEdata) THEN
     OPEN(12, FILE=rmn_data_filename, STATUS='UNKNOWN')
     OPEN(13, FILE=der_data_filename, STATUS='UNKNOWN')
  END IF
  
  ! Open additional files
  
  CALL openOutputDataFiles
  
  CALL initializeSymOps
  
  CALL writeOutputDataFileHeaders
  
  DO ik = 1, kMax
     IF ((ik<11).OR.(MOD(ik,50).EQ.0).OR.(ik.EQ.kMax)) THEN
        WRITE(6,*)'set_input ik= ',ik
     END IF
     
     ! Use unscissored band energies to calculate rmn
     band(1:nMax) = energy(ik,1:nMax)
     ! iv.eq.ic and iv.ne.ic
     DO iv = 1, nMax
        !------------------------
        ! DO ic = 1, nMax
        !------------------------
        !   reduced input begin
        !------------------------
        DO ic = iv, nMax
           !---------------------
           ! reduced input end
           !---------------------
           READ(11,*) (matTemp(l), l=1,6)

101        FORMAT(2I3,6E15.7)
           !#BMSVer3.0d
           ! for option -n v^\nl is included in momMatElem
           !#BMSVer3.0u
           momMatElem(1,iv,ic) = matTemp(1) + (0.0d0,1.0d0)*matTemp(2)
           momMatElem(2,iv,ic) = matTemp(3) + (0.0d0,1.0d0)*matTemp(4)
           momMatElem(3,iv,ic) = matTemp(5) + (0.0d0,1.0d0)*matTemp(6)

           ! reduced input begin
           IF (ic.NE.iv) THEN
              DO ii=1,3
                 momMatElem(ii,ic,iv) = CONJG(momMatElem(ii,iv,ic))
              END DO
           END IF
           ! reduced input end

!!!BMS     
!           IF (nSpinor==2) THEN
! calculates S_{cc'} only!
           IF ( spinCalculation ) THEN
              if( (iv .ge. nval+1).and.(ic .ge. nval+1) ) then 
                 READ(41, *) (matTemp(l), l=1,6)
                 spiMatElem(1,iv,ic) = matTemp(1) + (0.d0,1.d0)*matTemp(2)
                 spiMatElem(2,iv,ic) = matTemp(3) + (0.d0,1.d0)*matTemp(4)
                 spiMatElem(3,iv,ic) = matTemp(5) + (0.d0,1.d0)*matTemp(6)
                 IF (ic.NE.iv) THEN
                    DO ii=1,3
                       spiMatElem(ii,ic,iv) = CONJG(spiMatElem(ii,iv,ic))
                    END DO
                 END IF
              end if
           END IF
!!!BMS
     
!!!BMS
              IF ( ndotCalculation ) then
!!! rho_{mm} is only diagonal and real
!                 if (iv.eq.ic) then
!                    READ(69,*) matTemp(1)
!!! rho_{cc'} is complex (see arrays.f90)
                 if( (iv .ge. nval+1).and.(ic .ge. nval+1) ) then 
                    READ(69,*) matTemp(1),matTemp(2)
                    IF(io_status.NE.0) THEN
                       WRITE(*,*) "ERROR: Couldn't read matTemp: rhomm layered calculation. Stopping"
                       WRITE(*,*) "Error number ", io_status
                       STOP "COULD NOT READ matTemp for rhomm"
                    ELSE
!!! rho_{mm} is real
!                       calrho(iv) = matTemp(1)
!!! rho_{cc'} is complex (see arrays.f90)
                       calrho(iv,ic) = matTemp(1)+(0.d0,1.d0)*matTemp(2)
                       IF (ic.NE.iv) THEN
                          calrho(ic,iv) = CONJG(calrho(iv,ic))
                       END IF
                    END IF
                 end if !iv=ic
              END IF
!!! 
!!! FN
           IF ( layeredCalculation ) then
              READ(16,*) (matTemp(l),l=1,6)
              IF(io_status.NE.0) THEN
                 WRITE(*,*) "ERROR: Could not read matTemp for layered calculation. Stopping"
                 WRITE(*,*) "Error number ", io_status
                 STOP "COULD NOT READ matTemp"
              ELSE
                 calMomMatElem(1,iv,ic) = matTemp(1) + (0.0d0,1.0d0)*matTemp(2)
                 calMomMatElem(2,iv,ic) = matTemp(3) + (0.0d0,1.0d0)*matTemp(4)
                 calMomMatElem(3,iv,ic) = matTemp(5) + (0.0d0,1.0d0)*matTemp(6)
              END IF
              IF (ic.NE.iv) THEN
                 DO ii=1,3
                   calMomMatElem(ii,ic,iv) = CONJG(calMomMatElem(ii,iv,ic))
                 END DO
              END IF
           END IF
!#BMSVer3.0d
           IF ( layeredCalculation ) then
              READ(53,*) (matTemp2(l),l=1,2)
              IF(io_status.NE.0) THEN
                 WRITE(*,*) "ERROR: Could not read matTemp2 for layered calculation. Stopping"
                 WRITE(*,*) "Error number ", io_status
                 STOP "COULD NOT READ matTemp"
              ELSE
                 cfMatElem(iv,ic) = matTemp2(1) + (0.0d0,1.0d0)*matTemp2(2)
              END IF
              IF (ic.NE.iv) THEN
                 DO ii=1,3
                   cfMatElem(ic,iv) = CONJG(cfMatElem(iv,ic))
                 END DO
              END IF
           END IF
!#BMSVer3.0u

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!#BMSVer3.0d
! below shouldn't be needed
          !! Calculate the calPosMatElem matrix elements 
          !! despues de calcular los calMomMatElem 
           IF ( layeredCalculation ) then
              DO ii=1,3
                 calPosMatElem(ii,iv,ic) = calPosition(ii,iv,ic,ik)
                 calPosMatElem(ii,ic,iv) = conjg(calPosMatElem(ii,iv,ic))
              END DO
           end if
!#BMSVer3.0u
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! jl 


!!! 
!           IF ( microscopicDensityCalculation ) THEN
!              IF ( iv .EQ. ic ) THEN
!                 READ(16,*) (matTemp(l),l=1,2)
!                 denMatElem(iv) = matTemp(1) + (0.0d0,1.0d0)*matTemp(2)
!              END IF
!           END IF
!!!     
        IF ( layeredInjectionCurrent ) THEN
           if ( iv .eq. ic ) then
              READ(17,*) (matTemp3(l),l=1,3)
              curMatElem(1,iv) = matTemp3(1) 
              curMatElem(2,iv) = matTemp3(2) 
              curMatElem(3,iv) = matTemp3(3) 
           END IF
        END IF
!!! FN
        !#BMSVer3.0d
        ! for option -n v^\nl is included in momMatElem
        ! and so is also included in r_{nm}
        !#BMSVer3.0u
           ! Calculate the position matrix elements
           DO ii=1,3
              posMatElem(ii,iv,ic) = position(ii,iv,ic,ik)
              posMatElem(ii,ic,iv) = conjg(posMatElem(ii,iv,ic))
           END DO

102        FORMAT(6E15.7)
           
           ! Write out the position matrix elements
           IF (writeoutMEdata) THEN
              WRITE(12,102) REAL(posMatElem(1,iv,ic)),IMAG(posMatElem(1,iv,ic)) &
                   , REAL(posMatElem(2,iv,ic)),IMAG(posMatElem(2,iv,ic)) &
                   , REAL(posMatElem(3,iv,ic)),IMAG(posMatElem(3,iv,ic))
           END IF
        END DO !ic=iv,nMax
     END DO !iv=1,nMax

     ! Check hermiticity of position matrix elements
     CALL check_hermiticity(ik)
     
     ! SET THE IMAGINARY PARTS OF THE DIAGONAL COMPONENTS TO ZERO BY HAND
     DO iv = 1, nMax
        momMatElem(1,iv,iv)= REAL(momMatElem(1,iv,iv)) + 0.d0*(0.d0,1.d0)
        momMatElem(2,iv,iv)= REAL(momMatElem(2,iv,iv)) + 0.d0*(0.d0,1.d0)
        momMatElem(3,iv,iv)= REAL(momMatElem(3,iv,iv)) + 0.d0*(0.d0,1.d0)
        IF ( layeredCalculation ) then
           calMomMatElem(1,iv,iv)= REAL(calMomMatElem(1,iv,iv)) + 0.d0*(0.d0,1.d0)
           calMomMatElem(2,iv,iv)= REAL(calMomMatElem(2,iv,iv)) + 0.d0*(0.d0,1.d0)
           calMomMatElem(3,iv,iv)= REAL(calMomMatElem(3,iv,iv)) + 0.d0*(0.d0,1.d0)
        END IF
     END DO

     !#BMSVer3.0d
     DO iv = 1, nVal
        DO ic = nVal+1, nMax
           ! Calculate Delta(m,n) Eq. {delta} NOT LAYERED
           ! for -n option the contribution from v^\nl is included
           ! recall that Delta has zero contribution from
           ! the scissors operator
           Delta(1,ic,iv) = momMatElem(1,ic,ic)-momMatElem(1,iv,iv)
           Delta(2,ic,iv) = momMatElem(2,ic,ic)-momMatElem(2,iv,iv)
           Delta(3,ic,iv) = momMatElem(3,ic,ic)-momMatElem(3,iv,iv)
        END DO
     END DO
     !#BMSVer3.0u

     IF (oldStyleScissors) THEN !set to false
        band(1:nMax) = energys(ik,1:nMax) !this is not longer used
     END IF
     !#BMSVer3.0d
     ! Calculate the generalized derivative of the position matrix elements
     ! for -n option the contribution from v^\nl is included
     ! however, neglecting \tau^{ab}_{nm}. Eq {c-na_rgendevn}
     !#BMSVer3.0u
     DO iv = 1, nMax
        DO ic = 1, nMax
           DO ii=1,3
              DO iii=1,3
                 derMatElem(iii,ii,iv,ic) = genderiv(iii,ii,iv,ic,ik)
              END DO
           END DO
           
           IF (writeoutMEdata) THEN
              WRITE(13,102) REAL(derMatElem(1,1,iv,ic)),IMAG(derMatElem(1,1,iv,ic)), &
                   REAL(derMatElem(2,1,iv,ic)),IMAG(derMatElem(2,1,iv,ic)), &
                   REAL(derMatElem(3,1,iv,ic)),IMAG(derMatElem(3,1,iv,ic))
              WRITE(13,102) REAL(derMatElem(1,2,iv,ic)),IMAG(derMatElem(1,2,iv,ic)), &
                   REAL(derMatElem(2,2,iv,ic)),IMAG(derMatElem(2,2,iv,ic)), &
                   REAL(derMatElem(3,2,iv,ic)),IMAG(derMatElem(3,2,iv,ic)) 
              WRITE(13,102) REAL(derMatElem(1,3,iv,ic)),IMAG(derMatElem(1,3,iv,ic)), &
                   REAL(derMatElem(2,3,iv,ic)),IMAG(derMatElem(2,3,iv,ic)), &
                   REAL(derMatElem(3,3,iv,ic)),IMAG(derMatElem(3,3,iv,ic))
           END IF
        END DO
     END DO
!
! calculates the {\cal F}^{\rma\rmb}_{nm} Eq. (A7) of Cabellos et al., PRB 80, 155205 (2009)
!

    ci=cmplx(0.d0,1.d0)

     DO iv = 1, nVal
        DO ic = nVal+1, nMax
           DO ii=1,3
              DO iii=1,3
                 efe(iv,ic,ii,iii)=cmplx(0.d0,0.d0)
                 tmpm=cmplx(0.d0,0.d0)
                 do vp=1,nVal
                    if (vp.ne.iv) then
                       tmpm=tmpm+posMatElem(iii,ic,vp)*posMatElem(ii,vp,iv)
                    end if
                 end do
                 do cp=nVal+1,nMax
                    if (cp.ne.ic) then
                       tmpm=tmpm-posMatElem(ii,ic,cp)*posMatElem(iii,cp,iv)
                    end if
                 end do
                 efe(ic,iv,ii,iii)=-scissor*(ci*tmpm+derMatElem(iii,ii,ic,iv)) 
                 efe(iv,ic,ii,iii)=-conjg(efe(ic,iv,ii,iii))
              end do
           end do
        end do
     end do

!!!#BMSVer3.0d
!!!This is not longer required
!!! Calculate the generalized derivative of the layered position matrix elements
!!! Eq (97), i.e. \eqref{rgkcal} shg-layer.tex
!!$     IF ( layeredCalculation ) then
!!$        DO iv = 1, nMax
!!$           DO ic = 1, nMax
!!$              DO ii=1,3
!!$                 DO iii=1,3
!!$                    GenDerCalPosition(iii,ii,iv,ic) = GenDerCalPositionf(iii,ii,iv,ic,ik)
!!$                 END DO
!!$              END DO
!!$           END DO
!!$        END DO
!!$     END IF
!!!#BMSVer3.0u
     !#BMSVer3.0d
     ! v^\lda diagonal terms before scissors correction
     ! is implemented in momMatElem
     ! Eq. (c-chon.98) v^\gs_{nn}=v^\lda_{nn}
     ! If -n option is given, then the momMatElem already
     ! contain the v^nl contribution.
     do ic=1,nMax
        do ii=1,3
           vldaMatElem(ii,ic,ic) = momMatElem(ii,ic,ic)
        end do
     end do
     !#BMSVer3.0u
     !#BMSVer3.0d
     !f(v)=1 and f(c)=0
     do iv=1,nVal
        f(iv)=1.d0
     end do
     do ic=nVal+1,nMax
        f(ic)=0.d0
     end do
     !#BMSVer3.0u
!!!#BMSVer3.0d
!!! Now renormalize (scissor) the momentum matrix elements     
!!! so the scissor correction is properly included
!!!#BMSVer3.0u
     ! iv .ne. ic
     DO iv = 1, nVal
        DO ic = nVal + 1, nMax
!!!
!!! BMS jan/29/2013: THIS IS CORRECT FOR SHG IN THE VELOCITY GAUGE.
!!! FOR OTHER RESPONSES WHERE ONE USES THE MOMENTUM 
!!! INSTEAD OF THE POSITION MATRIX ELEMENTS
!!! ONE HAS YET TO CHECK THE CORRECT IMPLEMENTATION OF THE SCISSOR SHIFT
!!!
!!!#BMSVer3.0d
!!! If -n option is given, then the momMatElem already
!!! contain the v^nl contribution.
!!!
!!! Off-diagonal matrix elements are renormalized by scissorFactor
!!! diagonal matrix elements are NOT renormalized by scissorFactor
!!! diagonal matrix elements are coded above, however since
!!! the diagonal momMatElem are not affected by the scissors
!!! correction, they could be coded below as well.
!!! Notice that:
!!! \Sigma=scissor
!!! and
!!! \omega^\sigma_{nm}/\omega_{nm}=scissorFactor
!!! This far in the code the energies are still the LDA energies
!!! => they have not yet been scissored
!!!#BMSVer3.0u
           scissorFactor = 1.d0 + scissor / (band(ic)-band(iv))
!           write(70,*)'in set_input_ascii.f90:',scissor
           DO ii=1,3
              ! v^\lda=p/m + v^\nl
              ! where v^\nl could or could not be included
              ! depending on the -n flag
              ! off-diagonal terms
              ! Before scissors correction
              vldaMatElem(ii,iv,ic) = momMatElem(ii,iv,ic)
              vldaMatElem(ii,ic,iv) = momMatElem(ii,ic,iv)
              !with this momMatElem become 
              !v^\sigma=v^\lda+v^\scissor (c-chon.98)
              momMatElem(ii,iv,ic) = momMatElem(ii,iv,ic)*scissorFactor
              momMatElem(ii,ic,iv) = momMatElem(ii,ic,iv)*scissorFactor
              do iii=1,3
                 !Eq. c-a.3 off-diagonal terms
                 gdVlda(iii,ii,iv,ic)=(0.d0,1.d0)*(Delta(ii,iv,ic)*posMatElem(iii,iv,ic)&
                      +(band(iv)-band(ic))*derMatElem(iii,ii,iv,ic))
                 gdVlda(iii,ii,ic,iv)=conjg(gdVlda(iii,ii,iv,ic))
              end do
              IF ( layeredCalculation ) then
                 !#BMSVer3.0d
                 !if vnlkss is true: v^\nl is included
                 !and Eq. \ref{c-a.2} is calculated 
                 if(vnlkss)then
                    t1=calVlda(ii,iv,ic,ik)
                    if (scissor .gt. 0.d0) then
                       !The vc case is coded
                       t2=calVscissors(ii,iv,ic,ik)*scissor
                    else
                       t2=(0.d0,0.d0)
                    end if
                    calMomMatElem(ii,iv,ic) = t1+t2
                    calMomMatElem(ii,ic,iv) = conjg(t1+t2)
                 else
                    !if vnlkss is false: v^\nl is NOT included
                    !and Eq. \ref{eni.2} is used, which was
                    !computed in matrix elements and already stored
                    !in calMomMatElem
                    !we only add the scissors correction
                    !using new expressions c-a.3b or vs.cv
                    t1=calMomMatElem(ii,iv,ic)
                    if (scissor .gt. 0.d0) then
                       !The vc case is coded
                       t2=calVscissors(ii,iv,ic,ik)*scissor
                    else
                       t2=(0.d0,0.d0)
                    end if
                    calMomMatElem(ii,iv,ic) = t1+t2
                    calMomMatElem(ii,ic,iv) = conjg(t1+t2)
!!!above calMomMatElem is already the caligraphic velocity matrix elements, i.e.
!!!it contains both the v^\nl (-n option) and the scissors contribution 
!!!calMomMatElem->\calbv^{\gs,\ell}_{nm} (c-a.1)
!!!As explianed in shg-layer-nonlocal.pdf
!!!calMomMatElem ARE NOT SIMPLY RESCALED
!!!by scissorFactor, therefore
!!!these two were wrong
!!!calMomMatElem(ii,iv,ic) = calMomMatElem(ii,iv,ic)*scissorFactor
!!!calMomMatElem(ii,ic,iv) = calMomMatElem(ii,ic,iv)*scissorFactor
                 end if
              end IF
           END DO
        END DO !ic=nVal+1,nMax
     END DO !iv=1,nVal
     !#BMSVer3.0u
     !#BMSVer3.0d
     IF ( layeredCalculation ) then
        DO iv = 1, nVal
           DO ic = nVal+1, nMax
              ! Calculate Delta^\ell_{nm} Eq. {caldelta}  LAYERED
              ! for -n option the contribution from v^\nl is included
              ! the contribution from the scissors operator is included
              calDelta(1,ic,iv) = calMomMatElem(1,ic,ic)-calMomMatElem(1,iv,iv)
              calDelta(2,ic,iv) = calMomMatElem(2,ic,ic)-calMomMatElem(2,iv,iv)
              calDelta(3,ic,iv) = calMomMatElem(3,ic,ic)-calMomMatElem(3,iv,iv)
           END DO
        END DO
     END IF
     !#BMSVer3.0u
!!! 
     !#BMSVer3.0d
     ! calculation of c-a.3c diagonal terms
     do ic=1,nMax
        do ii=1,3
           do iii=1,3
              t1=(0.d0,0.d0)
              do iv=1,nMax
                 if ( iv .ne. ic) then
                    t2=(band(iv)-band(ic))&
                         *(posMatElem(ii,ic,iv)*posMatElem(iii,iv,ic)&
                         + posMatElem(iii,ic,iv)*posMatElem(ii,iv,ic))
                    t1=t1+t2
                 end if
              end do
              if ( ii .eq. iii) then
                 gdVlda(ii,iii,ic,ic)=1.d0-t1
              else
                 gdVlda(ii,iii,ic,ic)=-t1
              end if
           end do
        end do
     end do
     !#BMSVer3.0u
     !#BMSVer3.0d
     !calculation of c-a.7
     IF ( layeredCalculation ) then
        do ic=1,nMax
           do iv=ic,nMax
              do ii=1,3
                 t1=(0.d0,0.d0)
                 do l=1,nMax
                    if((l.ne.ic).and.(l.ne.iv)) then
                       t2=posMatElem(ii,ic,l)*cfMatElem(l,iv)&
                            -cfMatElem(ic,l)*posMatElem(ii,l,iv)
                       t1=t1+t2
                    end if
                 end do
                 if ( ic .ne. iv ) then
                    gdf(ii,ic,iv)=(0.d0,1.d0)*(t1+posMatElem(ii,ic,iv)&
                         *( cfMatElem(iv,iv)-cfMatElem(ic,ic) ) )
                 else
                    gdf(ii,ic,iv)=(0.d0,1.d0)*t1
                 end if
                 gdf(ii,iv,ic)=conjg(gdf(ii,ic,iv))
              end do
           end do
        end do
     end IF
     !#BMSVer3.0u
     !#BMSVer3.0d
     !Eq. c-a.2nn (\calbV^\lda);k
     IF ( layeredCalculation ) then
        do ic=1,nMax
           do iv=ic,nMax
              do ii=1,3
                 do iii=1,3
                    t1=(0.d0,0.d0)
                    do l=1,nMax
                       t2=gdVlda(ii,iii,ic,l)*cfMatElem(l,iv)&
                            +vldaMatElem(ii,ic,l)*gdf(iii,l,iv)&
                            +gdf(iii,ic,l)*vldaMatElem(ii,l,iv)&
                            +cfMatElem(ic,l)*gdVlda(ii,iii,l,iv)
                       t1=t1+t2
                    end do
                    gdcalVlda(ii,iii,ic,iv)=t1/2.d0
                    gdcalVlda(ii,iii,iv,ic)=conjg(t1)/2.d0
                 end do
              end do
           end do
        end do
     end IF
     !#BMSVer3.0u
     !#BMSVer3.0d
     !Eq. c-a.3bnn: (\calbV^\cals);k
     IF ( layeredCalculation ) then
        if (scissor .gt. 0.d0) then
           !dgvs.cv
           do ic=1,nMax
              do iv=ic,nMax
                 do ii=1,3
                    do iii=1,3
                       t1=(0.d0,0.d0)
                       do l=1,nMax !sum over q=l
                          t2=(f(l)-f(ic))&
                               *( derMatElem(ii,iii,ic,l)*cfMatElem(l,iv)&
                               +  posMatElem(ii,ic,l)*gdf(iii,l,iv) )&
                               + (f(iv)-f(l))&
                               *( gdf(iii,ic,l)*posMatElem(ii,l,iv)&
                               + cfMatElem(ic,l)*derMatElem(ii,iii,l,iv) )
                          t1=t1+t2
                       end do
                       gdcalVS(ii,iii,ic,iv)=(0.d0,1.d0)*scissor*t1/2.d0
                       gdcalVS(ii,iii,iv,ic)=conjg(gdcalVS(ii,iii,ic,iv))
                    end do
                 end do
              end do
           end do
        end if
        !Eq. c-a.1
        do ic=1,nMax
           do iv=ic,nMax
              do ii=1,3
                 do iii=1,3
                    if (scissor .gt. 0.d0) then
                       gdcalVsig(ii,iii,ic,iv)=gdcalVlda(ii,iii,ic,iv)&
                            +gdcalVS(ii,iii,ic,iv)
                    else
                       gdcalVsig(ii,iii,ic,iv)=gdcalVlda(ii,iii,ic,iv)
                    end if
                    gdcalVsig(ii,iii,iv,ic)=conjg(gdcalVlda(ii,iii,ic,iv))
                 end do
              end do
           end do
        end do
     end IF
     !#BMSVer3.0u
     !#BMSVer3.0d
     !Eq. c-a.1nn (v^sigma_{nm});k
     do ic=1,nMax
        do iv=ic,nMax
           do ii=1,3
              do iii=1,3
                 if ( scissor .gt. 0.d0 ) then
                    gdVsig(ii,iii,ic,iv)=gdVlda(ii,iii,ic,iv)&
                         +(0.d0,1.d0)*scissor*(f(iv)-f(ic))&
                         *derMatElem(ii,iii,ic,iv)
                 else
                    gdVsig(ii,iii,ic,iv)=gdVlda(ii,iii,ic,iv)
                 end if
                 gdVsig(ii,iii,iv,ic)=conjg(gdVsig(ii,iii,ic,iv))
              end do
           end do
        end do
     end do
     !#BMSVer3.0u
     !#BMSVer3.0d
     ! layer-to-bulk
     IF ( layeredCalculation ) then
        if ( ik .eq. 1 ) then 
           write(*,*)'********'
           write(*,*)'set_input_ascii.f90: layered calculation'
           write(*,*)'********'
        end if
        !########## MIMIC A BULK RESPONSE #######d
        calVsig=calMomMatElem !comment to check layered fromulas
        ! uncomment next two lines to check layered fromulas
        !calVsig=momMatElem !\calv^\gs -> v^\gs
        !gdcalVsig=gdVsig   !(\calv^\gs);k -> (v^\gs);k
        ! comment previous two lines for layered fromulas
        !########## MIMIC A BULK RESPONSE #######u
     else !bulk calculation
        if ( ik .eq. 1 ) then 
           write(*,*)'********'
           write(*,*)'set_input_ascii.f90: bulk calculation'
           write(*,*)'momMatElem->calVsig'
           write(*,*)'gdVsig->gdcalVsig'
           write(*,*)'********'
        end if
        calVsig=momMatElem !\calv^\gs -> v^\gs
        gdcalVsig=gdVsig !(\calv^\gs);k -> (v^\gs);k
     end IF
     ! Now, Eqs. c-calvimchiewn, c-calvimchie2wn, c-calvimchiwn
     !      and  c-calvimchi2wn
     ! ??ADD linear response and current injection responses??
     ! can be calculated as coded for a layered-response
     !#BMSVer3.0u
!!!
!!! Now use the scissored energy bands !!!!!!!!!!
!!!
     
     band(1:nMax) = energys(ik,1:nMax)

!!! Now calculate the integrands
    

     CALL calculateintegrands
     
  END DO !ik=1,kMax
  
  ! CALL closefiles
  CALL closeOutputDataFiles
  !
  ! Check that we are really at the end of the input pmn file
  !
  matTemp(1:6) = 0.d0
  READ(11,*,IOSTAT=io_status) (matTemp(l), l=1,6)
  IF (io_status.LE.0) THEN
     WRITE(6,*) 'End of matrix elements file reached'
     call system("touch endWELLpmn")
     CLOSE(11)
  ELSE IF(io_status.EQ.0) THEN
     WRITE(6,*) (matTemp(l), l=1,6)
     STOP 'pmn file contains more data than expected'
  ELSE
     STOP 'reading end of pmn file caused an error.'
  END IF
  
  IF (writeoutMEdata) THEN
     CLOSE(12)
     CLOSE(13)
  END IF
  
 ! IF (nSpinor==2) THEN
 !    CLOSE(41)
 ! END IF
  
! close the spin file 
  INQUIRE(FILE=smn_data_filename, EXIST=spinCalculation)
  if ( spinCalculation) then
     CLOSE(41)
  END IF
!

  CALL deallocateArrays
  
!!!******************
END PROGRAM set_input
!!!******************

!###############################
SUBROUTINE check_hermiticity(ik)
!###############################
  USE arrays, ONLY: DPC, momMatElem, nMax, posMatElem
  IMPLICIT NONE
  INTEGER :: ik
  INTEGER :: iv, ic, ii
  COMPLEX(DPC) :: ctmpa(3), ctmpb(3)
! check for reality
  DO iv=1,nMax
     ctmpa(1:3) = momMatElem(1:3,iv,iv)
     DO ii=1,3
        IF (IMAG(ctmpa(ii)).GT.3.d-5) THEN
! YES I KNOW 2d-5 IS VERY LARGE STILL
!           WRITE(6,*) iv, ctmpa(ii)
!           PAUSE
        END IF
     END DO
  END DO
! check for hermiticity
  DO iv=2, nMax-1
     DO ic=iv+1, nMax
        ctmpa(1:3) = momMatElem(1:3,iv,ic)
        ctmpb(1:3) = momMatElem(1:3,ic,iv)
        DO ii = 1, 3
           IF (ctmpa(ii).NE.CONJG(ctmpb(ii))) THEN
              WRITE(6,*) iv,ic,ik,ii,ctmpa(ii),ctmpb(ii)
              PAUSE 'pmn not hermitian'
105           FORMAT(4I5,4E17.7)
           END IF
        END DO
        ctmpa(1:3) = posMatElem(1:3,iv,ic)
        ctmpb(1:3) = posMatElem(1:3,ic,iv)
        DO ii = 1, 3
           IF (ctmpa(ii).NE.CONJG(ctmpb(ii))) THEN
              WRITE(6,*) iv,ic,ik,ii,ctmpa(ii),ctmpb(ii)
              PAUSE 'rmn not hermitian'
           END IF
        END DO
     END DO
  END DO

!  WRITE(6,*) 'End check for kpoint', ik
!*******************************
END SUBROUTINE check_hermiticity
!*******************************
