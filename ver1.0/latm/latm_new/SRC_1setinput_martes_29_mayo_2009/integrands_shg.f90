!################
MODULE integrands
  !################
  USE constants, ONLY : DP, DPC
  USE inparams, ONLY : nVal, nMax, nSym
  USE inparams, ONLY : tol
  USE inparams, ONLY : crystal_class
  USE inparams, ONLY : number_of_spectra_to_calculate
  USE inparams, ONLY : spectrum_info
  USE arrays, ONLY: momMatElem, posMatElem, derMatElem, Delta, energy, band
  USE arrays, ONLY: calMomMatElem
  USE arrays, ONLY: spiMatElem
  USE arrays, ONLY: calDelta
  USE arrays, ONLY: calrho
  IMPLICIT NONE
  
  ! TYPE spectrum
  !    CHARACTER(LEN=60) :: integrand_filename
  !    INTEGER :: integrand_filename_unit
  !    INTEGER :: spectrum_type
  !    !  1 : chi1       !  2 : Lambda       !  3 : eta2        !  4 : S
  !    !  5 : C          !  6 : Ctilde       !  7 : E            !  8 : Etilde
  !    !  9 : staticChi1 ! 10 : staticS      ! 11 : staticC      ! 12 : staticCtilde
  !    ! 13 : staticE    ! 14 : staticEtilde ! 15 : staticChi2i  ! 16 : staticChi2e
  !    ! 17 : zeta spinPop ! 18 : mu spinCurrent  ! 19 : xi2          ! 20 : eta3
  !    ! 21 : shg1       ! 22 : shg2         ! 23 : LEO          ! 24 : calChi1 (layered)
  !    ! 25 : caleta2  (layered)
  !    ! 26 : n-dot-cc (layered)
  !    ! 27 : n-dot-vv (layered)
  !    N O T E: 17=>zeta and 18=> mu, WORK AS THEY ARE FOR LAYER-BY-LAYER ANALYSIS
  !    LOGICAL :: compute_integrand
  !    INTEGER, POINTER :: spectrum_tensor_component(:)
  !    REAL(DP), POINTER :: transformation_elements(:)
  ! END TYPE spectrum
  INTEGER :: iv, ic, ip, iq
  INTEGER :: ix, iy, iz, iw
  INTEGER :: i_spectra
  
CONTAINS
!!!#############################
  SUBROUTINE calculateintegrands
!!!###########################
    IMPLICIT NONE
    
    CHARACTER(LEN=3) :: ADV_opt
    REAL(DP) :: omegamp, omeganp, omegapn
    
    !--------------------------------------------------------------------------
    DO i_spectra = 1, number_of_spectra_to_calculate
       IF (spectrum_info(i_spectra)%compute_integrand) THEN
!!        WRITE(6,*) "Spectrum type", spectrum_info(i_spectra)%spectrum_type
          SELECT CASE(spectrum_info(i_spectra)%spectrum_type)
          CASE(1) ! linear response
             CALL ImChi1
          CASE(2)
             CALL ImLambda
          CASE(3)
             CALL eta2
          CASE(4)
             CALL ImS
          CASE(5)
             CALL ImC
          CASE(6)
             CALL ImCtilde
          CASE(7)
             CALL ImE
          CASE(8)
             CALL ImEtilde
          CASE(9)
!!!                 CALL staticChi1
          CASE(10)
!!!                 CALL staticS
          CASE(11)
!!!                 CALL staticC
          CASE(12)
!!!                 CALl staticCtilde
          CASE(13)
!!!                 CALL staticE
          CASE(14)
!!!                 CALL staticEtilde
          CASE(15)
!!!                 CALL staticChi2i
          CASE(16)
!!!                 CALL staticChi2e
          CASE(17)
             CALL zeta
          CASE(18)
             CALL mu
!!!BW!
          CASE(19)
             CALL xi2
          CASE(20)
             CALL eta3
!!!BW!
          CASE(21)
             CALL shg1
          CASE(22)
             CALL shg2
          CASE(23)
             CALL LEO
          CASE(24)
             CALL calImChi1
          CASE(25)
             CALL caleta2old
          CASE(26)
             CALL ndotcc
          CASE(27)
             CALL ndotvv
          CASE DEFAULT
             STOP 'Error in calculateIntegrands: spectrum_type not available'
          END SELECT
       END IF
    END DO
    
  END SUBROUTINE calculateintegrands
!!!==================================================================
  
!!!################
  SUBROUTINE ImChi1
!!!################
    !
    ! This computes the integrand of the imaginary part of
    ! the linear response Chi1
    !
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp
    REAL(DP) :: omegamn,omeganm,fsc,Delta
    REAL(DP) :: T2(3,3)
    
    T2(1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:9), (/3,3/) )
    
    read(69,*)Delta
    close(69)
!    write(*,*)'Delta=',Delta
!    stop
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          omegamn = band(iv) - band(ic)
          omeganm = band(ic) - band(iv)
!!! renormalization factor for momentum matrix elements
          fsc = omeganm/(omeganm-Delta)
          IF (DABS(omeganm).LT.tol) THEN
             ! NOTE: the position operator matrix elements are set to
             !                                   zero at functions.f90, se we don't do anything here
             ! Set matrix element to zero, 
             ! which is equivalent to take
             fsc=0.
             IF (iv.EQ.nVal.AND.ic.EQ.nVal+1) THEN
                write(6,*)'############################'
                write(6,*)'integrands.f90@ImChi1: Hold on! The tol value is bigger than the gap'
                WRITE(6,*)'matrix elements set to zero'
                !             STOP
             END IF
          end IF
!!!
          DO ix=1,3
             DO iy=1,3
! written in terms of the position matrix elements
!                ctmp = ctmp + T2(ix,iy)*posMatElem(ix,ic,iv)*posMatElem(iy,iv,ic)
! written in terms of the momentum matrix elements
                ctmp = ctmp - T2(ix,iy)*fsc*momMatElem(ix,ic,iv)*fsc*momMatElem(iy,iv,ic)/(omegamn*omeganm)
             END DO
          END DO
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
       END DO
    END DO
104 FORMAT(E15.7)
    
!!!####################
  END SUBROUTINE ImChi1
!!!####################

!!!################
  SUBROUTINE calImChi1
!!!################
    !
    ! This computes the integrand of the imaginary part of
    ! the linear response Chi1
    ! for a layer-by-layer analysis
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp, ctmp1
    REAL(DP) :: omegamn
    REAL(DP) :: T2(3,3)
    
    T2(1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:9), (/3,3/) )
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          
          omegamn = band(iv) - band(ic)
          
          DO ix=1,3
             DO iy=1,3
                ctmp1 = posMatElem(ix,ic,iv)*calMomMatElem(iy,iv,ic)/(0.d0,1.d0)/omegamn
                ctmp = ctmp + T2(ix,iy)*ctmp1
             END DO
          END DO
!          write(94,92)iv,ic,real(ctmp),aimag(ctmp)
!92 format(2i5,6e14.5)     

          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
       END DO
    END DO
104 FORMAT(E15.7)
    
!!!#######################
  END SUBROUTINE calImChi1
!!!#######################

!!!################
  SUBROUTINE ndotcc
!!!################
    !
    ! This computes the integrand of the imaginary part of
    ! the linear response zeta1
    ! for a layer-by-layer analysis
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp, ctmp1,ctmp2
    REAL(DP) :: omegamn
    REAL(DP) :: T2(3,3)
    
    T2(1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:9), (/3,3/) )
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          omegamn = band(iv) - band(ic)
          DO ix=1,3
             DO iy=1,3
                ctmp2  = calrho(ic)
                ctmp1  = ctmp2*posMatElem(ix,ic,iv)*posMatElem(iy,iv,ic)
                ctmp   = ctmp + T2(ix,iy)*ctmp1
             END DO
          END DO
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
       END DO
    END DO
104 FORMAT(E15.7)
    
!!!#######################
  END SUBROUTINE ndotcc
!!!#######################

!!!################
  SUBROUTINE ndotvv
!!!################
    !
    ! This computes the integrand of the imaginary part of
    ! the linear response zeta1
    ! for a layer-by-layer analysis
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp, ctmp1,ctmp2
    REAL(DP) :: omegamn
    REAL(DP) :: T2(3,3)
    
    T2(1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:9), (/3,3/) )
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          omegamn = band(iv) - band(ic)
          DO ix=1,3
             DO iy=1,3
                ctmp2  = calrho(iv)
                ctmp1  = ctmp2*posMatElem(ix,ic,iv)*posMatElem(iy,iv,ic)
                ctmp   = ctmp + T2(ix,iy)*ctmp1
             END DO
          END DO
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
       END DO
    END DO
104 FORMAT(E15.7)
    
!!!#######################
  END SUBROUTINE ndotvv
!!!#######################

!!!##################
  SUBROUTINE ImLambda
!!!##################
    IMPLICIT NONE
    ! 
    ! THIS IS NOW MODIFIED TO BE THE LAMBDA FROM Nastos, Adolph and Sipe
    ! It is not the Lambda in Sipe's notes.  It differs from it by a factor
    ! of i.
    ! 
    ! The shift current tensor is just LambdaABC+LambdaACB now.
    !  
    COMPLEX(DPC) :: ctmp
    REAL(DP) :: omegamn
    REAL(DP) :: T3(3,3,3)
    
    T3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))    
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          omegamn=band(ic) - band(iv)
          ctmp = (0.d0, 0.d0)
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
                   ctmp = ctmp +                                       &
                        T3(ix,iy,iz)*derMatElem(iz,ix,iv,ic)*posMatElem(iy,ic,iv)
                END DO
             END DO
          END DO
          
          ctmp = -(0.d0,1.d0)*ctmp/2.d0
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)
    
!!!######################
  END SUBROUTINE ImLambda
!!!######################
  
!!!#################
  SUBROUTINE eta2
!!!#################
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp
    REAL(DP) :: T3(3,3,3)
    REAL(DP) :: omegamn,omeganm

    T3(1:3,1:3,1:3) = reshape(spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))    
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          omegamn = band(iv) - band(ic)
          omeganm = band(ic) - band(iv)
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
                   ctmp = ctmp                                     & 
                        +                                          &
                        T3(ix,iy,iz) *                             & 
                        Delta(ix,ic,iv)*                           &
!                        (momMatElem(ix,ic,iv))*  &
!                        (momMatElem(ix,ic,ic)-momMatElem(ix,iv,iv))*  &
                        (posMatElem(iz,ic,iv)*posMatelem(iy,iv,ic) &
                        -posMatElem(iy,ic,iv)*posMatelem(iz,iv,ic))
!                        momMatElem(iz,ic,iv)*momMatElem(iy,iv,ic)/(omegamn*omeganm)
                END DO
             END DO
          END DO
!          write(93,92)iv,ic,real(Delta(1,ic,iv)),aimag(Delta(1,ic,iv)) &
!               ,real(Delta(2,ic,iv)),aimag(Delta(2,ic,iv)) &
!               ,real(Delta(3,ic,iv)),aimag(Delta(3,ic,iv)) 
          ctmp = ctmp/4.0d0
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="YES") AIMAG(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="NO") AIMAG(ctmp)
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)
92 format(2i5,6e14.5)     
    
!!!#####################
  END SUBROUTINE eta2
!!!#####################

!!!#################
  SUBROUTINE caleta2
!!!#################
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp,ctmp1
    REAL(DP) :: omegamn
    REAL(DP) :: T2(3,3)
    INTEGER :: ia,ib,ic

    ia = spectrum_info(i_spectra)%spectrum_tensor_component(1)
    ib = spectrum_info(i_spectra)%spectrum_tensor_component(2)
    ic = spectrum_info(i_spectra)%spectrum_tensor_component(3)
    T2(1:3,1:3) = reshape(spectrum_info(i_spectra)%transformation_elements(1:9), (/3,3/) )
!    write(*,*)'in integrands.f90, calculating components \eta_2^{abc}: ',ia,ib,ic
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          omegamn = band(iv) - band(ic)

!          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
!!! correct formula?
                   ctmp = ctmp +                                   & 
!                        calmomMatElem(1,ic,ic)* &
                        calDelta(ia,ic,iv)*          &
                        T2(iy,iz)*(0.,1.)*(&
!                        posMatElem(iy,ic,iv)*(0.,1.)* &
!                        calMomMatElem(iz,iv,ic)/(0.d0,1.d0)/omegamn
                       (posMatElem(iz,ic,iv)*posMatelem(iy,iv,ic))  &
!                   + &
!                       (posMatElem(iy,ic,iv)*posMatelem(iz,iv,ic))  &
                   )
                END DO
             END DO
!          END DO
!          write(93,92)iv,ic,real(ctmp),aimag(ctmp)
          ctmp = ctmp/4.0d0
!          write(93,92)iv,ic,real(calDelta(1,ic,iv)),aimag(calDelta(1,ic,iv)) &
!               ,real(calDelta(2,ic,iv)),aimag(calDelta(2,ic,iv)) &
!               ,real(calDelta(3,ic,iv)),aimag(calDelta(3,ic,iv)) 
! IMAG is the correct choice!!
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="YES")AIMAG(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="NO")AIMAG(ctmp)
!!!              write(*,*)ctmp
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)
92 format(2i5,6e14.5)     

    
!!!#####################
  END SUBROUTINE caleta2
!!!#####################

!!!#################
  SUBROUTINE caleta2old
!!!#################
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp
    REAL(DP) :: omegamn
    REAL(DP) :: T3(3,3,3)

    T3(1:3,1:3,1:3) = reshape(spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))    
   
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          omegamn = band(iv) - band(ic)

          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
!!! correct formula?
                   ctmp = ctmp +                                   & 
                        T3(ix,iy,iz)*calDelta(ix,ic,iv)*           &
                        (posMatElem(iz,ic,iv)*posMatelem(iy,iv,ic) &
                        -posMatElem(iy,ic,iv)*posMatelem(iz,iv,ic))
                END DO
             END DO
          END DO
          ctmp = ctmp/4.0d0
! IMAG is the correct choice!!
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="YES")AIMAG(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="NO")AIMAG(ctmp)
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)
92 format(2i5,6e14.5)     
    
!!!#####################
  END SUBROUTINE caleta2old
!!!#####################

  
!!!#############
  SUBROUTINE ImS
!!!#############
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp
    REAL(DP) :: T3(3,3,3)
    REAL(DP) :: omegamn
    
    T3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))    
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          omegamn=band(ic) - band(iv)
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
                   ctmp = ctmp +                                  &
                        T3(ix,iy,iz)*derMatElem(iy,iz,ic,iv)*posMatElem(ix,iv,ic)
                END DO
             END DO
          END DO
          ctmp = ctmp/(2.d0*omegamn)
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="YES") -1.d0*IMAG(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="NO") -1.d0*IMAG(ctmp)
          END IF
          !The formula for S has an i in front. This i times
          !the i from the IMAG part gives a minus sign
       END DO
    END DO
!!!#################
  END SUBROUTINE ImS
!!!#################
  
!!!#############
  SUBROUTINE ImC
!!!#############
!    USE functions, ONLY : CfunctionTMP
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp, ctmp2, ctmp3
    REAL(DP) :: omegamp, omeganp
    REAL(DP) :: T3(3,3,3)
    
    T3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          DO ix=1,3
             DO iz=1,3
                ctmp2 = (0.d0, 0.d0)
                DO ip = 1, nMax
                   omegamp = band(ic) - band(ip)
                   IF (DABS(omegamp).GT.tol) THEN
                      ctmp2 = ctmp2 + posMatElem(ix,ic,ip)*posMatElem(iz,ip,iv)/omegamp
                   END IF
                   omeganp = band(iv) - band(ip)
                   IF (DABS(omeganp).GT.tol) THEN
                      ctmp2 = ctmp2 + posMatElem(iz,ic,ip)*posMatElem(ix,ip,iv)/omeganp
                   END IF
                END DO
                ctmp3 = (0.d0, 0.d0)
                DO iy=1,3
                   ctmp3 = ctmp3 +  T3(ix,iy,iz)*posMatElem(iy,iv,ic)
                END DO
                ctmp = ctmp +  ctmp3*ctmp2
                ctmp2 = (0.d0, 0.d0)
                ctmp3 = (0.d0, 0.d0)
             END DO
          END DO
          
          ctmp = -ctmp/4.d0
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="NO")  REAL(ctmp)
          END IF
       END DO
    END DO
!!!#################
  END SUBROUTINE ImC
!!!#################
  
!!!##################
  SUBROUTINE ImCtilde
!!!##################
!    USE functions, ONLY : rmprpnomp
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp, ctmp2, ctmp3
    REAL(DP) :: omegamn, omegamp, omeganp
    REAL(DP) :: T3(3,3,3)
    
    T3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          omegamn = band(ic) - band(iv)
          ctmp = (0.d0, 0.d0)
          DO ix=1,3
             DO iz=1,3
                ctmp2 = (0.d0, 0.d0)
                DO ip = 1, nMax
                   omegamp = band(ic) - band(ip)
                   IF (DABS(omegamp).GT.tol) THEN
                      ctmp2 = ctmp2 + posMatElem(ix,ic,ip)*posMatElem(iz,ip,iv)/omegamp**2
                   END IF
                   omeganp = band(iv) - band(ip)
                   IF (DABS(omeganp).GT.tol) THEN
                      ctmp2 = ctmp2 - posMatElem(iz,ic,ip)*posMatElem(ix,ip,iv)/omeganp**2
                   END IF
                END DO
                ctmp3 = (0.d0, 0.d0)
                DO iy=1,3
                   ctmp3 = ctmp3 +  T3(ix,iy,iz)*posMatElem(iy,iv,ic)
                END DO
                ctmp = ctmp +  ctmp3*ctmp2
                ctmp2 = (0.d0, 0.d0)
                ctmp3 = (0.d0, 0.d0)
             END DO
          END DO
          
          ctmp = -ctmp/4.d0*omegamn
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="NO")  REAL(ctmp)
          END IF
       END DO
    END DO
!!!######################
  END SUBROUTINE ImCtilde
!!!######################
  
!!!#############
  SUBROUTINE ImE
!!!#############
    USE functions, ONLY : rmprpnomp
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp
    REAL(DP) :: T3(3,3,3)
    REAL(DP) :: omegamn
    
    T3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          omegamn=band(ic) - band(iv)
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
                   ctmp = ctmp +                                 &
                        T3(ix,iy,iz) * posMatElem(ix,iv,ic)*     &
                        rmprpnomp(0,0,iy,iz,ic,iv,0)
                END DO
             END DO
          END DO
          ctmp = ctmp/(4.d0*omegamn)
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="NO") REAL(ctmp)
          END IF
       END DO
    END DO
!!!#################
  END SUBROUTINE ImE
!!!#################
  
!!!##################
  SUBROUTINE ImEtilde
!!!##################
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp, ctmp2
    REAL(DP) :: T3(3,3,3)
    REAL(DP) :: omegamn, omegamp, omegapn
    
    T3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))    
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          omegamn=band(ic) - band(iv)
          DO iy=1,3
             DO iz=1,3    
                ctmp2 = (0.d0, 0.d0)
                DO ip=1, nMax
                   omegamp = band(ic)-band(ip)
                   omegapn = band(ip)-band(iv)
                   ctmp2 = ctmp2                                             &
                        +  omegamp*posMatElem(iy,ic,ip)*posmatelem(iz,ip,iv) &
                        -  omegapn*posMatElem(iz,ic,ip)*posmatelem(iy,ip,iv)
                END DO
                DO ix=1,3
                   ctmp = ctmp + T3(ix,iy,iz)*posMatElem(ix,iv,ic)*ctmp2
                END DO
             END DO
          END DO
          
          ctmp = ctmp/(4.d0*omegamn**2)
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="NO") REAL(ctmp)
          END IF
          
       END DO
    END DO
    
!!!######################
  END SUBROUTINE ImEtilde
!!!######################
  
!!!###########
  SUBROUTINE zeta
!!!###########
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp,ctmp1
    REAL(DP) :: PT3(3,3,3)
    
    PT3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/) )
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          DO ip = nVal+1, nMax
             IF (DABS(band(ip)-band(ic)).LT.tol) THEN
                DO ix=1,3
                   DO iy=1,3
                      DO iz=1,3
                         ctmp1=(0.d0,0.d0)
                         ctmp1= PT3(ix,iy,iz) &
                              * ( spiMatElem(ix,ic,ip) &
                              *   posMatElem(iy,iv,ic)*posMatElem(iz,ip,iv) &
                              +   spiMatElem(ix,ip,ic) &
                              *   posMatElem(iy,iv,ip)*posMatElem(iz,ic,iv) )
                         ctmp1 = ctmp1*0.50d0*(band(ip)-band(iv))/(band(ic)-band(iv))
                         ctmp = ctmp + ctmp1
                      END DO
                   END DO
                END DO
             END IF
          END DO
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") DIMAG(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") DIMAG(ctmp)
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)
    
!!!###############
  END SUBROUTINE zeta
!!!###############
  
!!!BW Begin!
  
!!!#############
  SUBROUTINE xi2
!!!#############
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp
    COMPLEX(DPC) :: vxy, vzw
    REAL(DP) :: T4(3,3,3,3)
    REAL(DP) :: omegabarcv, omegacv

    T4(1:3,1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:81), (/3,3,3,3/) )
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          omegabarcv = (band(ic) + band(iv))/2.0d0
          omegacv = band(ic) - band(iv)
          DO ip = 1, nMax
             DO iq = 1, nMax
                DO ix=1,3
                   DO iy=1,3
                      DO iz=1,3
                         DO iw=1,3
                            vxy = (momMatElem(ix,iv,iq)*momMatElem(iy,iq,ic)                 &
                                   + momMatElem(iy,iv,iq)*momMatElem(ix,iq,ic))/2.0d0
                            vzw = (momMatElem(iz,ic,ip)*momMatElem(iw,ip,iv)                 &
                                   + momMatElem(iw,ic,ip)*momMatElem(iz,ip,iv))/2.0d0
                            
                            ctmp = ctmp + T4(ix,iy,iz,iw)*vxy*vzw                            &
                                   /((omegabarcv-band(iq))*(omegabarcv-band(ip))             &
                                    *(omegacv)**4)
                         END DO
                      END DO
                   END DO
                END DO
             END DO
          END DO
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
       END DO
    END DO
104 FORMAT(E15.7)
!!!#################
  END SUBROUTINE xi2
!!!#################
  
  
!!!##############
  SUBROUTINE eta3
!!!##############
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp, ctmp2
    COMPLEX(DPC) :: vyz
    REAL(DP) :: T4(3,3,3,3)
    REAL(DP) :: omegabarcv, omegacv
    
    T4(1:3,1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:81), (/3,3,3,3/) )
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0,0.d0)
          omegabarcv = (band(ic) + band(iv))/2.0d0
          omegacv = band(ic) - band(iv)
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
                   DO iw=1,3
                      vyz = (0.d0, 0.d0)
                      DO ip = 1, nMax
                         ctmp2 = (0.d0, 0.d0)
                         ctmp2 = (momMatElem(iy,iv,ip)*momMatElem(iz,ip,ic)         &
                              + momMatElem(iz,iv,ip)*momMatElem(iy,ip,ic))/2.0d0
                         vyz = vyz + ctmp2/(band(ip)-omegabarcv)
                      END DO
!! BMS 20/01/05
!! only electrons
!                     ctmp = ctmp + T4(ix,iy,iz,iw)*                         &
!                          (momMatElem(ix,ic,ic)*vyz*momMatElem(iw,ic,iv))
!! only holes
!                     ctmp = ctmp + T4(ix,iy,iz,iw)*                         &
!                          (-momMatElem(ix,iv,iv)*vyz*momMatElem(iw,ic,iv))
!! electrons and holes
                      ctmp = ctmp + T4(ix,iy,iz,iw)*                         &
                     ((momMatElem(ix,ic,ic)-momMatElem(ix,iv,iv))*vyz*momMatElem(iw,ic,iv))
                   END DO
                END DO
             END DO
          END DO
!! BMS 24/01/05
          ctmp = 8.d0*ctmp/(omegacv)**3
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO")  REAL(ctmp)
          END IF
       END DO
    END DO
104 FORMAT(E15.7)
!!!##################
  END SUBROUTINE eta3
!!!##################

!!! BW End!
  
!!!############
  SUBROUTINE mu
!!!############
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp
    REAL(DP) :: omegamn
    COMPLEX(DPC) :: Kab
    REAL(DP) :: PT4(3,3,3,3)
    
    PT4(1:3,1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:81), (/3,3,3,3/) )
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          DO iq = nVal+1, nMax
             IF (DABS(band(iq)-band(ic)).LT.tol) THEN
                Kab = (0.d0, 0.d0)
                DO ix=1,3
                   DO iy=1,3
                      DO ip = 1, nMax
!!!
!!! we symmetrize Kab for the layer-by-layer case, which also works
!!! for the normal case, indeed, it does two identical terms
!!! but it ain't a botleneck
!!! For the layer-by-layer case, spiMatElem are properly calculated with S(z)
!!!                      Kab = Kab + momMatElem(ix,ic,ip)*spiMatElem(iy,ip,iq)
!!!
                         Kab = Kab + (  momMatElem(ix,ic,ip)*spiMatElem(iy,ip,iq) &
                                      + spiMatElem(iy,ic,ip)*momMatElem(ix,ip,iq) )/2.
                      END DO
                      DO iz=1,3
                         DO iw=1,3
                            ctmp = ctmp + PT4(ix,iy,iz,iw)*Kab                &
                                 *(posMatElem(iz,iq,iv)*posMatElem(iw,iv,ic)  &
                                 + posMatElem(iw,iq,iv)*posMatElem(iz,iv,ic))
                         END DO
                      END DO
                   END DO
                END DO
             END IF
          END DO
          
          
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)
    
!!!################
  END SUBROUTINE mu
!!!################
  
!!!##################
  SUBROUTINE shg1
!!!##################
    !
    ! This computes the integrand of the imaginary part of
    ! the nonlinear response tensor Lambda
    ! using the longitudinal gauge

!    USE InputParametersMod, ONLY : nVal, nMax, nSym, tol
!    USE SpectrumParametersMod, ONLY : number_of_spectra_to_calculate
!    USE SpectrumParametersMod, ONLY : spectrum_info
!    USE ArraysMod, ONLY: momMatElem, posMatElem, derMatElem, Delta, energy, band
!    USE ArraysMod, ONLY: spiMatElem, magMatElem
!    USE PhysicalConstantsMod
    
    IMPLICIT NONE
    
    INTEGER :: iv, ic, ip, iq
    INTEGER :: ix, iy, iz, iw
    INTEGER :: ia, ib,  id
    INTEGER :: i_spectra
    CHARACTER(LEN=3) :: ADV_opt
    
    COMPLEX(DPC) :: ctmp_i, ctmp_e
    COMPLEX(DPC) :: ctmp, ctmp1, ctmp2, ctmp3, ctmp4
    REAL(DP) :: omegamn, omegapm, omeganp
    REAL(DP) :: T3(3,3,3)
    
    T3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))    
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          omegamn=band(ic) - band(iv)
          ctmp = (0.d0, 0.d0)
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
                   
                   ctmp_e = (0.d0, 0.d0)
                   ctmp_i = (0.d0, 0.d0)
                   
                   ctmp1 = (0.d0, 0.d0)
                   ctmp2 = (0.d0, 0.d0)
                   ctmp3 = (0.d0, 0.d0)
                   ctmp4 = (0.d0, 0.d0)
                   
                   DO ip = 1, nMax
                      
                      omegapm = band(ip) - band(ic)
                      omeganp = band(iv) - band(ip)

                         ctmp1 = ctmp1 + posMatElem(ix,iv,ip) * posMatElem(iy,ip,ic) / (omegamn - omegapm)
                         ctmp3 = ctmp3 + posMatElem(ix,iv,ip) * posMatElem(iz,ip,ic) / (omegamn - omegapm)


                         ctmp2 = ctmp2 + posMatElem(iy,iv,ip) * posMatElem(ix,ip,ic) / (omeganp - omegamn)
                         ctmp4 = ctmp4 + posMatElem(iz,iv,ip) * posMatElem(ix,ip,ic) / (omeganp - omegamn)

                      
                   END DO
                   
                   ctmp_e = posMatElem(iz,ic,iv)*(ctmp1 + ctmp2) + posMatElem(iy,ic,iv)*(ctmp3 + ctmp4)
                   ctmp_e = ctmp_e * (0.d0, -1.d0) / 2.d0
                   
                   ctmp1 = (0.d0, 0.d0)
                   ctmp2 = (0.d0, 0.d0)
                   ctmp3 = (0.d0, 0.d0)
                   ctmp4 = (0.d0, 0.d0)
                   
                   ctmp1 = derMatElem(ix,iz,iv,ic)*posMatElem(iy,ic,iv) &
                        +  derMatElem(ix,iy,iv,ic)*posMatElem(iz,ic,iv)
                   ctmp1 = ctmp1 / omegamn
                   ctmp1 = ctmp1 / (-2.d0)
                   
                   ctmp2 = posMatElem(iy,ic,iv)*Delta(iz,ic,iv) + posMatElem(iz,ic,iv)*Delta(iy,ic,iv)
                   ctmp2 = posMatElem(ix,iv,ic)*ctmp2
                   ctmp2 = ctmp2 / (omegamn ** 2)
                   ctmp2 = ctmp2 / (-2.d0)
             !      ctmp2 = ctmp2 * makeDouble(Hartree_eV)
                   
                   ctmp3 = derMatElem(iy,ix,iv,ic)*posMatElem(iz,ic,iv) &
                        +  derMatElem(iz,ix,iv,ic)*posMatElem(iy,ic,iv)
                   ctmp3 = ctmp3 / omegamn
                   ctmp3 = ctmp3 / 4.d0
                   ctmp3 = ctmp3
                   
                   ctmp_i = ctmp_i + ctmp1 + ctmp2 + ctmp3
                   
                   ctmp1 = (0.d0,0.d0)
                   ctmp2 = (0.d0,0.d0)
                   ctmp3 = (0.d0,0.d0)
                   
                   ctmp = ctmp + T3(ix,iy,iz)*(ctmp_i + ctmp_e)
                   
                END DO
             END DO
          END DO
          
          IF (ic==nMax) THEN
             ADV_opt = "YES"
          ELSE
             ADV_opt = "NO"
          END IF
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)
!!!##################
  END SUBROUTINE shg1
!!!##################
  
!!!##############
  SUBROUTINE shg2
!!!##############
        !
    ! This computes the integrand of the imaginary part of
    ! the nonlinear response SHG2
    ! using the longitudinal gauge
    !
!    USE InputParametersMod, ONLY : nVal, nMax, nSym, tol
!    USE SpectrumParametersMod, ONLY : number_of_spectra_to_calculate
!    USE SpectrumParametersMod, ONLY : spectrum_info
!    USE ArraysMod, ONLY: momMatElem, posMatElem, derMatElem, Delta, energy, band
!    USE ArraysMod, ONLY: spiMatElem, magMatElem
!    USE PhysicalConstantsMod
    
    IMPLICIT NONE
    
    INTEGER :: iv, ic, ip, iq
    INTEGER :: ix, iy, iz, iw
    INTEGER :: ia, ib,  id
    INTEGER :: i_spectra
    CHARACTER(LEN=3) :: ADV_opt
    
    COMPLEX(dpc) :: ctmp_i, ctmp_e
    COMPLEX(dpc) :: ctmp, ctmp1, ctmp2
    REAL(dp) :: omegamn, omegapn, omegamp
    REAL(dp) :: T3(3,3,3)
    
    T3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          omegamn=band(ic) - band(iv)
          ctmp = (0.d0, 0.d0)
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
                   
                   ctmp_e = (0.d0, 0.d0)
                   ctmp_i = (0.d0, 0.d0)
                   
                   ctmp1 = (0.d0, 0.d0)
                   
                   DO ip = 1, nMax
                      
                      ctmp2 = (0.d0, 0.d0)
                      
                      omegapn = band(ip) - band(iv)
                      omegamp = band(ic) - band(ip)
                      
                      ctmp2 = posMatElem(iy,ic,ip)*posMatElem(iz,ip,iv) + posMatElem(iz,ic,ip)*posMatElem(iy,ip,iv)


                         ctmp2 = ctmp2/(omegapn - omegamp)

                      
                      ctmp1 = ctmp1 + ctmp2
                      
                      ctmp2 = (0.d0, 0.d0)
                      
                   END DO
                   
                   ctmp_e = ctmp1 * posMatElem(ix,iv,ic) * (0.d0, 1.d0)
                   
                   ctmp1 = (0.d0, 0.d0)
                   ctmp2 = (0.d0, 0.d0)
                   
                   ctmp1 = posMatElem(ix,iv,ic)*(derMatElem(iy,iz,ic,iv) + derMatElem(iz,iy,ic,iv))
                   ctmp1 = -ctmp1 / omegamn
                   
                   ctmp2 = posMatElem(iy,ic,iv)*Delta(iz,ic,iv) + posMatElem(iz,ic,iv)*Delta(iy,ic,iv)
                   ctmp2 = ctmp2 * posMatElem(ix,iv,ic)
                   ctmp2 = ctmp2 / (omegamn ** 2)
                   ctmp2 = ctmp2 * 2.d0! * makeDouble(Hartree_eV)
                   
                   ctmp_i = ctmp1 + ctmp2
                   
                   ctmp = ctmp + T3(ix,iy,iz) * (ctmp_i + ctmp_e)
                   
                END DO
             END DO
          END DO
          
          IF (ic==nMax) THEN
             ADV_opt = "YES"
          ELSE
             ADV_opt = "NO"
          END IF
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)

!!!######################
  END SUBROUTINE shg2
!!!######################
  
!!!##################
  SUBROUTINE Leo
!!!##################
    IMPLICIT NONE
    !
    ! One part of the second-harmonic tensor
    !  
    COMPLEX(DPC) :: ctmp
    REAL(DP) :: omegamn
    REAL(DP) :: T3(3,3,3)
    
    STOP 'LEO is not implemented.  In fact, you should not reach this message'
    
    T3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))    
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          omegamn=band(ic) - band(iv)
          ctmp = (0.d0, 0.d0)
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
!                   ctmp = ctmp +                                       &
!                        T3(ix,iy,iz)*derMatElem(iz,ix,iv,ic)*posMatElem(iy,ic,iv)
                   ctmp = (0.d0, 0.d0)
                END DO
             END DO
          END DO
          
!          ctmp = -(0.d0,1.d0)*ctmp/2.d0
          
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)
!!!######################
  END SUBROUTINE Leo
!!!######################
  
!!!####################  
END MODULE integrands
!####################
