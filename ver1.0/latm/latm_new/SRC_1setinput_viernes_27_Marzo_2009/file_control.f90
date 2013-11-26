MODULE file_control
  USE inparams, ONLY : number_of_spectra_to_calculate, spectrum_info
  USE constants, ONLY : debug
!!!    TYPE spectrum
!!!     CHARACTER(LEN=80) :: integrand_filename
!!!     INTEGER :: integrand_filename_unit
!!!     INTEGER :: spectrum_type
!!!     !  1: chi1          !  2: Lambda        !  3: Gamma         ! 4: S
!!!     !  5: C             !  6: Ctilde        !  7: E             ! 8: Etilde
!!!     !  9: staticChi1    ! 10: staticS       ! 11: staticC       ! 12: staticCtilde
!!!     ! 13: staticE       ! 14: staticEtilde  ! 15: staticChi2i   ! 16: staticChi2e
!!!     ! 17:               ! 18:               ! 19:               ! 20:
!!!     ! 21:               ! 22:               ! 23:               ! 24:
!!!     LOGICAL :: compute_integrand
!!!     INTEGER, POINTER :: spectrum_tensor_component(:)
!!!     REAL(DP), POINTER :: transformation_elements(:)
!!!  END TYPE spectrum
  IMPLICIT NONE
  INTEGER :: i
CONTAINS
  !==================================================================
  SUBROUTINE openOutputDataFiles
    IMPLICIT NONE
    INTEGER :: istat
    
    DO i=1,number_of_spectra_to_calculate
       IF (spectrum_info(i)%compute_integrand) THEN
          WRITE(6,*) "Trying to open: ", spectrum_info(i)%integrand_filename
          OPEN(UNIT = spectrum_info(i)%integrand_filename_unit,&
               FILE = spectrum_info(i)%integrand_filename, &
               IOSTAT = istat)
          IF (istat/=0) THEN
             WRITE(6,*) ' '
             WRITE(6,*) 'Error opening', spectrum_info(i)%integrand_filename
             WRITE(6,*) 'Assigned unit', spectrum_info(i)%integrand_filename_unit
             WRITE(6,*) 'STOPPING'
             STOP
          ELSE IF (istat.EQ.0) THEN
             WRITE(6,*) 'File opened'
          END IF
       END IF
    END DO
  END SUBROUTINE openOutputDataFiles
  !==================================================================
  
  !==================================================================
  SUBROUTINE closeOutputDataFiles
    IMPLICIT NONE
    INTEGER :: istat
    
    DO i=1,number_of_spectra_to_calculate
       IF (spectrum_info(i)%compute_integrand) THEN
          CLOSE(spectrum_info(i)%integrand_filename_unit, IOSTAT = istat)
          IF (istat.EQ.0) THEN
             WRITE(6,*) 'Closing files'
          ELSE
             WRITE(6,*) ' '
             WRITE(6,*) 'Error closing', spectrum_info(i)%integrand_filename
             WRITE(6,*) 'Assigned unit', spectrum_info(i)%integrand_filename_unit
             WRITE(6,*) 'STOPPING'
             STOP
          END IF
       END IF
    END DO
  END SUBROUTINE closeOutputDataFiles
  !==================================================================
  
  SUBROUTINE writeOutputDataFileHeaders
    USE INPARAMS, ONLY : DP, spin_factor, spectrum_factor
    USE INPARAMS, ONLY : nSym, unit_cell_volume
    IMPLICIT NONE
    REAL(DP) :: rtmp, rtmp2
    
    IF (debug) WRITE(*,*) "Program Flow: entered writeOutputDataFileHeaders"
    
!!!    rtmp = spin_factor*(1.d0/unit_cell_volume)*(1.d0/DFLOAT(2*nSym))
!    rtmp = spin_factor*(1.d0/DFLOAT(2*nSym))
    rtmp = spin_factor*(1.d0/DFLOAT(nSym))
    
    WRITE(*,*) "rtmp ", rtmp
    
    DO i=1,number_of_spectra_to_calculate
       IF (spectrum_info(i)%compute_integrand) THEN
          rtmp2 = rtmp*spectrum_factor(spectrum_info(i)%spectrum_type)
          WRITE (6,*) "writeOutputDataFileHeaders", i, rtmp2
          WRITE(UNIT=spectrum_info(i)%integrand_filename_unit, FMT=*) rtmp2
       END IF
    END DO


    
  END SUBROUTINE writeOutputDataFileHeaders
  
END MODULE file_control
