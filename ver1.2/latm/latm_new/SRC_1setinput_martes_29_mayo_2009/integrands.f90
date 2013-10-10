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
!!!--------------------------------June  2008
  USE arrays, ONLY: calPosMatElem
!!!--------------------------------03 Marzo 2009
  USE arrays, ONLY: kMax
  
  IMPLICIT NONE
  
  ! TYPE spectrum
  !    CHARACTER(LEN=60) :: integrand_filename
  !    INTEGER :: integrand_filename_unit
  !    INTEGER :: spectrum_type

  !!  1 : chi1                (W)       
  !!  2 :  Lambda       
  !!  3 : eta2        
  !!  4 : S
  !!  5 : C 
  !!  6 : Ctilde
  !!  7 : E
  !!  8 : Etilde
  !!  9 : staticChi1 
  !! 10 : staticS 
  !! 11 : staticC
  !! 12 : staticCtilde
  !! 13 : staticE    
  !! 14 : staticEtilde 
  !! 15 : staticChi2i  
  !! 16 : staticChi2e
  !! 17 : zeta_spin_bulk      (W)       
  !! 18 : zeta_spin_layered  
  !! 19 : xi2          
  !! 20 : eta3
  !! 21 : SHG1       
  !! 22 : SHG2
  !! 23 : LEO          
  !! 24 : calChi1  (layered)  (W)
  !! 25 : caleta2  (layered)  (W)
  !! 26 : n-dot-cc (layered)
  !! 27 : n-dot-vv (layered)
  !! 28 :
  !! 29 : calZeta1 (layered)  (W)
  !! 39 :
  !! 40 : zeta_cabellos       (W)
  !! 41 : zeta_abs            (W)
  !    LOGICAL :: compute_integrand
  !    INTEGER, POINTER :: spectrum_tensor_component(:)
  !    REAL(DP), POINTER :: transformation_elements(:)
  ! END TYPE spectrum
  INTEGER :: iv, ic,iq,ip
  INTEGER :: ix, iy, iz, iw
  INTEGER :: i_spectra
  !! spin indices&variable=above + below (feb.16.2009)
  INTEGER :: icp

CONTAINS
!!!#############################
!!! SUBROUTINE calculateintegrands
  SUBROUTINE calculateintegrands
!!!###########################
    IMPLICIT NONE
    
    CHARACTER(LEN=3) :: ADV_opt
    REAL(DP) :: omegamp, omeganp, omegapn
    integer :: comova
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
             CALL zeta_fred
          CASE(18)
             CALL zeta_layered_bms
!!!BW!
          CASE(19)
             CALL xi2
          CASE(20)
             CALL eta3
!!!BW!
          CASE(21)
             CALL SHG1
          CASE(22)
             CALL SHG2
          CASE(23)
             CALL LEO
          CASE(24)
             CALL calImChi1
          CASE(25)
             CALL caleta2
          CASE(26)
             CALL ndotcc
          CASE(27)
             CALL ndotvv
!!!!!!!!!!! right now we add new response to 17 Febrero 2009  jl !!!!!
          CASE(28)
             CALL zeta_fred
!!!!!!!!!!! right now we add new response to 23 Febrero 2009  jl !!!!!
!!!!!!!!!!! right now we add new response to 23 Febrero 2009  jl !!!!!
!!!!!!!!!!! right now we add new response to 23 Febrero 2009  jl !!!!!
!!!!!!!!!!! right now we add new response to 23 Febrero 2009  jl !!!!!
          CASE(29)
              call zeta_layered_bms
              !if ( comova.eq.1 ) then
              ! write(*,*)"Haciendo FILE: zeta_layered_bms veces ", comova
              ! call system("touch zeta_layered_bms") 
              !end if 
!!!!!!!!!!! right now we add new response to 20 Febrero 2009  jl !!!!!
          CASE(39) 
             CALL ImChi1_cabellos
          CASE(40)
             CALL zeta_cabellos
          CASE(41)
             CALL zeta_bulk  !!! esta como en las notas y usamos esta jl 
                             !!! para el calculo de spin bulk ....
          

          
          CASE DEFAULT
             STOP 'Error in calculateIntegrands: spectrum_type not available'
          END SELECT
       END IF
    END DO
    
  END SUBROUTINE calculateintegrands
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! below is $\zeta^{\ell,abc}_{\mathrm{i}}$
!!!! of Eq. \ref{zetaabci} (i.e. Eq. 11)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE zeta_layered_bms
    IMPLICIT NONE
    REAL(DP) :: PT3(3,3,3)
    REAL(DP) :: tmp,tmp1
    
    PT3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/) )

    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          tmp = 0.d0
         DO icp = nVal+1, nMax
             IF (DABS(band(icp)-band(ic)).LT.tol) THEN
                DO ix=1,3
                   DO iy=1,3
                      DO iz=1,3
                         tmp1=0.d0
                         tmp1= PT3(ix,iy,iz) &
                              *aimag(  spiMatElem(ix,icp,ic)*calposMatElem(iy,ic,iv)*posMatElem(iz,iv,icp) &
                                     + spiMatElem(ix,ic,icp)*calposMatElem(iy,icp,iv)*posMatElem(iz,iv,ic) )
                         tmp = tmp + tmp1
                      END DO
                   END DO
                END DO
             END IF
          END DO
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") tmp
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") tmp
          END IF
          
       END DO !!
    END DO !!
104 FORMAT(E15.7)
  END SUBROUTINE zeta_layered_bms
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><> Martes 20 Febrero 2009 <><><><><><><><><><><><>
!!! <><><><><><><><> responses #  <><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  SUBROUTINE zeta_cabellos 
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp,ctmp1
    REAL(DP) :: PT3(3,3,3)
    
    PT3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/) )
    
    !write(29,*)"tol=",tol

    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ip=ic
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
          
       END DO !!
    END DO !!
104 FORMAT(E15.7)
  END SUBROUTINE zeta_cabellos 
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><> Martes 17 Febrero 2009 <><><><><><><><><><><><>
!!! <><><><><><><><> responses # 28 <><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

  SUBROUTINE zeta_fred 
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp,ctmp1
    REAL(DP) :: PT3(3,3,3)
    
    PT3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/) )
    
    !write(29,*)"tol=",tol

    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ip=ic
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
          
       END DO !!
    END DO !!
104 FORMAT(E15.7)
  END SUBROUTINE zeta_fred
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><> Martes 17 Febrero 2009 <><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  SUBROUTINE ImChi1
    ! This computes the integrand of the imaginary part of
    ! the linear response Chi1
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
          fsc = (omeganm/(omeganm-Delta))**2
          IF (DABS(omeganm).LT.tol) THEN
             ! NOTE: the position operator matrix elements are set to
             ! zero at functions.f90, se we don't do anything here
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
       ctmp = ctmp - fsc*T2(ix,iy)*momMatElem(ix,ic,iv)*momMatElem(iy,iv,ic)/(omegamn*omeganm)
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
  END SUBROUTINE ImChi1
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><> Viernes 20 Febrero 2009 <><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  SUBROUTINE ImChi1_cabellos
    ! This computes the integrand of the imaginary part of
    ! the linear response Chi1
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
          fsc = (omeganm/(omeganm-Delta))**2
          IF (DABS(omeganm).LT.tol) THEN
             ! NOTE: the position operator matrix elements are set to
             ! zero at functions.f90, se we don't do anything here
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
       ctmp = ctmp - fsc*T2(ix,iy)*momMatElem(ix,ic,iv)*momMatElem(iy,iv,ic)/(omegamn*omeganm)
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
  END SUBROUTINE ImChi1_cabellos
!!!!<><><><><><><><><><><><><><><><><><><><><><>>



  SUBROUTINE calImChi1

    !
    !! This computes the integrand of the imaginary part of
    !! the linear response Chi1
    !! for a layer-by-layer analysis
    !! 1 dic 2008 cabellos 
    !! esta utiliza el prefactor de chi1: Chi1_factor
    !! en inparams.f90 
    !! chi1 factor de acuerdo con paper's sipe es: 
    !!  
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
    

  END SUBROUTINE calImChi1
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  SUBROUTINE ndotcc
    ! This computes the integrand of the imaginary part of
    ! the linear response zeta1
    ! for a layer-by-layer analysis
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp, ctmp1,ctmp2
    REAL(DP) :: T2(3,3)
    
    T2(1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:9), (/3,3/) )
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          DO ix=1,3
             DO iy=1,3
!                ctmp2  = calrho(ic)
!                ctmp1  = ctmp2*posMatElem(ix,ic,iv)*posMatElem(iy,iv,ic)
                ctmp1  = calposMatElem(ix,ic,iv)*posMatElem(iy,iv,ic)
                !!ctmp1  = posMatElem(ix,ic,iv)*calposMatElem(iy,iv,ic)
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
 END SUBROUTINE ndotcc
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  SUBROUTINE ndotcc_erres
!!!con las dos erres caligrafiados 
    ! This computes the integrand of the imaginary part of
    ! the linear response zeta1
    ! for a layer-by-layer analysis
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp, ctmp1,ctmp2
    REAL(DP) :: T2(3,3)
    
    T2(1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:9), (/3,3/) )
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          DO ix=1,3
             DO iy=1,3
!               ctmp2  = calrho(ic)
!               ctmp1  = ctmp2*posMatElem(ix,ic,iv)*posMatElem(iy,iv,ic)
                ctmp1  = calposMatElem(ix,ic,iv)*calposMatElem(iy,iv,ic)
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
  END SUBROUTINE ndotcc_erres
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
  SUBROUTINE ndotvv
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
  END SUBROUTINE ndotvv
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
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

    T3(1:3,1:3,1:3) = reshape(spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))    
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ctmp = (0.d0, 0.d0)
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
                   ctmp = ctmp                                     & 
                        +                                          &
                        T3(ix,iy,iz) *                             & 
                        Delta(ix,ic,iv) *                          &
                       (posMatElem(iz,ic,iv)*posMatelem(iy,iv,ic)  &
                        -posMatElem(iy,ic,iv)*posMatelem(iz,iv,ic))
                END DO
             END DO
          END DO
        !  write(93,92)iv,ic,real(Delta(1,ic,iv))&
        !       ,real(Delta(2,ic,iv))&
        !       ,real(Delta(3,ic,iv)) 
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
79  format(5i5,76e14.5)    
!!!#####################
  END SUBROUTINE eta2
!!!#####################

!!!#################
  SUBROUTINE caleta2
!!!#################
    IMPLICIT NONE
    REAL(DP) :: T3(3,3,3)
    REAL(DP) :: tmp
    T3(1:3,1:3,1:3) = reshape(spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))    
!!!!!!!!!!!!!!!!!
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          tmp = 0.d0
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                   tmp = tmp +  T3(ix,iy,iz)*Delta(ix,ic,iv)* &
                        aimag(posMatElem(iy,ic,iv)*calPosMatElem(iz,iv,ic))
                END DO
             END DO
          END DO
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="YES")tmp
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT="(E15.7)",ADVANCE="NO")tmp
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)
92  format(2i5,6e14.5)     
    
!!!#####################
  END SUBROUTINE caleta2
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! ESTA ZETA calcula zeta con (DOS DOS) rpmns caligraphics 
!!!! y usa   response_BMS_7_Agosto_2008.sh
!  SUBROUTINE zeta_layered 
!    IMPLICIT NONE
!    REAL(DP) :: PT3(3,3,3)
!    REAL(DP) :: tmp,tmp1
!    
!    PT3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/) )
    
!
!   DO iv = 1, nVal
!       DO ic = nVal+1, nMax
!          tmp = 0.d0
!         DO icp = nVal+1, nMax
!             IF (DABS(band(icp)-band(ic)).LT.tol) THEN
!                DO ix=1,3
!                   DO iy=1,3
!                      DO iz=1,3
!                         tmp1=0.d0
!                         tmp1= PT3(ix,iy,iz) &
!                              *aimag( spiMatElem(ix,ic,icp) &
!                                  *posMatElem(iy,icp,iv)*calposMatElem(iz,iv,ic) &
!                                 +spiMatElem(ix,icp,ic) &
!                                  *posMatElem(iy,ic,iv)*calposMatElem(iz,iv,icp) )
!                         tmp = tmp + tmp1
!                      END DO
!                   END DO
!                END DO
!             END IF
!          END DO
!          IF (ic==nMax) THEN
!             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
!                  FMT=104,ADVANCE="YES") tmp
!          ELSE
!             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
!                  FMT=104,ADVANCE="NO") tmp
!          END IF
          
!       END DO !!
!    END DO !!
!104 FORMAT(E15.7)
!  END SUBROUTINE zeta_layered
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><> 18 OCTUBRE 2008 <><><><><><><><><><><><><><><><><><> 
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!! ESTA ZETA calcula zeta con (UNA UNA) rpmns caligraphics 
!!!! y usa   response_BMS_7_Agosto_2008.sh
  SUBROUTINE zeta_UNA_R 
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp,ctmp1
    REAL(DP) :: PT3(3,3,3)
    
    PT3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/) )
    
    !write(29,*)"tol=",tol

    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ip=ic
          ctmp = (0.d0, 0.d0)
         DO ip = nVal+1, nMax
             IF (DABS(band(ip)-band(ic)).LT.tol) THEN
                DO ix=1,3
                   DO iy=1,3
                      DO iz=1,3
                         ctmp1=(0.d0,0.d0)
                         ctmp1= PT3(ix,iy,iz) &
                              * ( spiMatElem(ix,ic,ip) &
                              *  calposMatElem(iy,iv,ic)*posMatElem(iz,ip,iv) &
                              +   spiMatElem(ix,ip,ic) &
                              *  calposMatElem(iy,iv,ip)*posMatElem(iz,ic,iv) )
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
          
       END DO !!
    END DO !!
104 FORMAT(E15.7)
  END SUBROUTINE zeta_UNA_R
!!! <><><><><><><<<<<<<<< 01 MARZO 2009 <><>><><>
!!! <><><><><><><<<<<<<<< 01 MARZO 2009 <><>><><>
!!! <><><><><><><<<<<<<<< 01 MARZO 2009 <><>><><>
!!! <><><><><><><<<<<<<<< 01 MARZO 2009 <><>><><>
  SUBROUTINE zeta_DOS_R 
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp,ctmp1
    REAL(DP) :: PT3(3,3,3)
    
    PT3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/) )
    
    !write(29,*)"tol=",tol

    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ip=ic
          ctmp = (0.d0, 0.d0)
         DO ip = nVal+1, nMax
             IF (DABS(band(ip)-band(ic)).LT.tol) THEN
                DO ix=1,3
                   DO iy=1,3
                      DO iz=1,3
                         ctmp1=(0.d0,0.d0)
                         ctmp1= PT3(ix,iy,iz) &
                              * ( spiMatElem(ix,ic,ip) &
                              *  calposMatElem(iy,iv,ic)*calposMatElem(iz,ip,iv) &
                              +   spiMatElem(ix,ip,ic) &
                              *  calposMatElem(iy,iv,ip)*calposMatElem(iz,ic,iv) )
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
          
       END DO !!
    END DO !!
104 FORMAT(E15.7)
  END SUBROUTINE zeta_DOS_R
!!!!!!!!!!!!!!!!!!!!!!!!! 01 MARZO 2009 !!!!!!!!!!!!!!!!!!!!!!!!
  SUBROUTINE zeta_NINGUNA_R 
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp,ctmp1
    REAL(DP) :: PT3(3,3,3)
    
    PT3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/) )
    
    !write(29,*)"tol=",tol

    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ip=ic
          ctmp = (0.d0, 0.d0)
         DO ip = nVal+1, nMax
             IF (DABS(band(ip)-band(ic)).LT.tol) THEN
                DO ix=1,3
                   DO iy=1,3
                      DO iz=1,3
                         ctmp1=(0.d0,0.d0)
                         ctmp1= PT3(ix,iy,iz) &
                              * ( spiMatElem(ix,ic,ip) &
                              *  posMatElem(iy,iv,ic)*posMatElem(iz,ip,iv) &
                              +   spiMatElem(ix,ip,ic) &
                              *  posMatElem(iy,iv,ip)*posMatElem(iz,ic,iv) )
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
          
       END DO !!
    END DO !!
104 FORMAT(E15.7)
  END SUBROUTINE zeta_NINGUNA_R
!!!!!!!!!!!!!!!!!!!!!!!!! 01 MARZO 2009 !!!!!!!!!!!!!!!!!!!!!!!!




!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><> this zeta is good   <><><><><><><><><><><><><<><><><><>
!!! <><> 23 septiembre 2008 <><><><><><><><><><><><><><><><><><> 
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! hi simpre prueba esta zeta con el GaAs.
  SUBROUTINE zeta_good
    IMPLICIT NONE
    COMPLEX(DPC) :: ctmp,ctmp1
    REAL(DP) :: PT3(3,3,3)
    
    PT3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/) )
    
    !write(29,*)"tol=",tol

    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          ip=ic
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
          
       END DO !!
    END DO !!
104 FORMAT(E15.7)
  END SUBROUTINE zeta_good
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!!  below zeta is according to 
!!! $\zeta^{B,abc}_{\mathrm{i}}=\zeta^{B,abc}/2$ given in A4 of
!!!   Cabellos et al manuscript
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! hi simpre prueba esta zeta con el GaAs.
  SUBROUTINE zeta_bulk
    IMPLICIT NONE
    REAL(DP) :: tmp,tmp1
    REAL(DP) :: PT3(3,3,3)
    
    PT3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/) )
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          tmp = 0.d0
         DO icp = nVal+1, nMax
             IF (DABS(band(icp)-band(ic)).LT.tol) THEN
                DO ix=1,3
                   DO iy=1,3
                      DO iz=1,3
                         tmp1=0.d0
                         tmp1= PT3(ix,iy,iz) &
                              * aimag(  spiMatElem(ix,icp,ic)* posMatElem(iy,ic,iv)*posMatElem(iz,iv,icp) &
                                      + spiMatElem(ix,ic,icp)*posMatElem(iy,icp,iv)*posMatElem(iz,iv,ic) )
                         tmp = tmp + tmp1
                      END DO
                   END DO
                END DO
             END IF
          END DO
          IF (ic==nMax) THEN
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") tmp
          ELSE
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") tmp
          END IF
          
       END DO !!
    END DO !!
104 FORMAT(E15.7)
  END SUBROUTINE zeta_bulk
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
!!! <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
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
  SUBROUTINE Shg1
!!!##################
    IMPLICIT NONE
    !
    ! The part of the imaginary part of the SHG tensor which will turn on at the band gap.
    ! That is, the part associated with $delta(omega-omega_{mn}(\vec{k}))$
    !  
    COMPLEX(DPC) :: ctmp, ctmp2, ctmp3, cdiffOfMme1, cdiffOfMme2
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
                   ctmp2 = (0.d0, 0.d0)
                   DO ip = 1, nMax
                      omegapm = band(ip) - band(ic)
                      ctmp3 = (0.d0, 0.d0)
                      ctmp3 =         posMatElem(iy,ip,ic)*posMatElem(iz,ic,iv)
                      ctmp3 = ctmp3 + posMatElem(iz,ip,ic)*posMatElem(iy,ic,iv)
                      
                      ctmp2 = ctmp2 + posMatElem(ix,iv,ip)*ctmp3/(omegamn - omegapm)
                      
                      omeganp = band(iv) - band(ip)
                      ctmp3 = (0.d0, 0.d0)
                      ctmp3 =         posMatElem(iy,ic,iv)*posMatElem(iz,iv,ip)
                      ctmp3 = ctmp3 + posMatElem(iz,ic,iv)*posMatElem(iy,iv,ip)
                      
                      ctmp2 = ctmp2 + posMatElem(ix,ip,ic)*ctmp3/(omeganp - omegamn)
                      
                      ctmp3 = (0.d0, 0.d0)
                   END DO
                   
                   ctmp2 = ctmp2 * (0.d0, 1.d0) * 0.5d0
                   
                   ctmp3 = (0.d0, 0.d0)
                   ctmp3 =         derMatElem(ix,iz,iv,ic)*posMatELem(iy,ic,iv)
                   ctmp3 = ctmp3 + derMatElem(ix,iy,iv,ic)*posMatELem(iz,ic,iv)
                   ctmp2 = ctmp2 - ctmp3/omegamn
                   
                   cdiffOfMme1 = posMatElem(iy,ic,iv)*(momMatElem(iz,ic,ic) - momMatElem(iz,iv,iv))
                   cdiffOfMme2 = posMatElem(iz,ic,iv)*(momMatElem(iy,ic,ic) - momMatElem(iy,iv,iv))
                   ctmp3 = posMatElem(ix,iv,ic) * (cdiffOfMme1 + cdiffOfMme2)
                   ctmp2 = ctmp2 - ctmp3/omegamn**2
                   
                   ctmp3 = derMatElem(iy,ix,iv,ic)*posMatElem(iz,ic,iv) + &
                        derMatElem(iz,ix,iv,ic)*posMatElem(iy,ic,iv)
                   ctmp2 = ctmp2 + ctmp3/(2.d0*omegamn)
                   
                   ctmp = ctmp + T3(ix,iy,iz)*ctmp2
                   
                END DO
             END DO
          END DO
          ctmp = ctmp / 2.d0
          
          IF (ic==nMax) THEN
!!!  Temporarily commented since M.Lee's calc does not include inversion symmetry  
!!!             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
!!!                  FMT=104,ADVANCE="YES") 2.d0*REAL(ctmp)
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="YES") REAL(ctmp)
          ELSE
!!!             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
!!!                  FMT=104,ADVANCE="NO") 2.d0*REAL(ctmp)
             WRITE(UNIT=spectrum_info(i_spectra)%integrand_filename_unit, &
                  FMT=104,ADVANCE="NO") REAL(ctmp)
          END IF
          
       END DO
    END DO
104 FORMAT(E15.7)
    
!!!##################
  END SUBROUTINE Shg1
!!!##################
  
!!!##############
  SUBROUTINE Shg2
!!!##############
    IMPLICIT NONE
    ! 
    ! The part of the imaginary part of the SHG tensor which will turn on at half the band gap.
    ! That is, the part associated with $delta(omega-omega_{mn}(\vec{k})/2)$
    ! 
    COMPLEX(DPC) :: ctmp, ctmp2, ctmp3, cdiffOfMme1, cdiffOfMme2
    REAL(DP) :: omegamn, omegapn, omegamp
    REAL(DP) :: T3(3,3,3)
    
    T3(1:3,1:3,1:3) = reshape( spectrum_info(i_spectra)%transformation_elements(1:27), (/3,3,3/))
    
    DO iv = 1, nVal
       DO ic = nVal+1, nMax
          omegamn=band(ic) - band(iv)
          ctmp = (0.d0, 0.d0)
          DO ix=1,3
             DO iy=1,3
                DO iz=1,3
                   ctmp2 = (0.d0, 0.d0)
                   DO ip = 1, nMax
                      omegapn = band(ip) - band(iv)
                      omegamp = band(ic) - band(ip)
                      ctmp3 = (0.d0, 0.d0)
                      ctmp3 =         posMatElem(iy,ic,ip)*posMatElem(iz,ip,iv)
                      ctmp3 = ctmp3 + posMatElem(iz,ip,ic)*posMatElem(iy,ic,iv)
                      
                      ctmp2 = ctmp2 + ctmp3/(omegapn - omegamp)
                   END DO
                   ctmp2 = posMatElem(ix,iv,ic) * (0.d0, 1.d0) * 0.5d0 * ctmp2
                   
                   ctmp3 = derMatElem(iy,iz,ic,iv) + derMatElem(iz,iy,ic,iv)
                   ctmp3 = posMatElem(ix,iv,ic)*ctmp3/omegamn
                   ctmp2 = ctmp2 - ctmp3
                   
                   cdiffOfMme1 = posMatElem(iy,ic,iv)*(momMatElem(iz,ic,ic) - momMatElem(iz,iv,iv))
                   cdiffOfMme2 = posMatElem(iz,ic,iv)*(momMatElem(iy,ic,ic) - momMatElem(iy,iv,iv))
                   ctmp3 = posMatElem(ix,iv,ic) * (cdiffOfMme1 + cdiffOfMme2)
                   ctmp2 = ctmp2 - 2.d0*ctmp3/omegamn**2
                   
                   ctmp = ctmp + T3(ix,iy,iz)*ctmp2
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
    
!!!######################
  END SUBROUTINE Shg2
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
