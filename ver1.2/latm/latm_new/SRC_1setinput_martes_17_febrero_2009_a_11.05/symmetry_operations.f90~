!!! Module containing symmetry operations
MODULE symmetryOperations
  USE inparams, ONLY : DP, spectrum, nSym
  USE inparams, ONLY : debug
  USE inparams, ONLY : number_of_spectra_to_calculate, spectrum_info
  IMPLICIT NONE
  
  ! There is a type of matrix for each spectrum_type, and there
  ! is a matrix for each spectrum
  
!!!  TYPE spectrum
!!!     CHARACTER(LEN=60) :: integrand_filename
!!!     INTEGER :: integrand_filename_unit
!!!     INTEGER :: spectrum_type
!!!     !  1: Chi1          !  2: Lambda        !  3: Gamma
!!!     !  4: S             !  5: C             !  6: Ctilde
!!!     !  7: E             !  8: Etilde        !  9: staticChi1
!!!     ! 10: staticS       ! 11: staticC       ! 12: staticCtilde
!!!     ! 13: staticE       ! 14: staticEtilde  ! 15: staticChi2i
!!!     ! 16: staticChi2e   ! 17: spinPop       ! 18: spinCurrent 
!!!     ! 19: xi2           ! 20: eta3          ! 21: SHG-part1
!!!     ! 22: SHG-part2     ! 24: layered Chi1
!!!     ! 25 : caleta2 (layered)
!!!     ! 26 : n-dot-cc   (layered)
!!!     ! 27 : n-dot-vv   (layered)
!!!     LOGICAL :: compute_integrand
!!!     INTEGER, POINTER :: spectrum_tensor_component(:)
!!!     REAL(DP), POINTER :: transformation_elements(:)
!!!  END TYPE spectrum
  
  REAL(DP), ALLOCATABLE :: SymOp(:,:,:)
  REAL(DP), ALLOCATABLE :: dSymOp(:)
  
CONTAINS
!!!##########################
  SUBROUTINE initializeSymOps
!!!##########################
    USE inparams, ONLY : crystal_class
    IMPLICIT NONE
    INTEGER :: i_spectra
    
    LOGICAL :: developingFlag
    
    IF (debug) WRITE(*,*) "Program Flow: Entered initializeSymOps"
    
    developingFlag = .true.
    
    IF (developingFlag) THEN
       CALL getSymOpsFromFile
    ELSE
       
       SELECT CASE(crystal_class)
       CASE('znbl', 'td...')
          CALL znblSymOps
       CASE('wrtz', 'c6v..')
          CALL wrtzSymOps
       CASE('c6...')
          CALL c6_SymOps
       CASE DEFAULT
          WRITE(6,*) 'Error in initializeSymOps:'
          WRITE(6,*) '   crystal_class ',crystal_class,' is unknown to code'
          WRITE(6,*) '   Stopping'
          STOP 'Error in initializeSymOps: crystal_class is unknown to code'
       END SELECT
       
    END IF
    
    DO i_spectra=1,number_of_spectra_to_calculate
!       WRITE(6,*) "i_spectra", i_spectra
       SELECT CASE(spectrum_info(i_spectra)%spectrum_type)
       CASE(1,24,26,27)
          CALL transformationLinearResponse(i_spectra)
! BMS march-27-07 The old way of calculating the LbL (Layer-by-Layer) injection current \eta_2^{abc}
!                 needs the SecondOrderResponse kind-of-transformation 
       CASE(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,25)
          CALL transformationSecondOrderResponse(i_spectra)
! BMS march-27-07 However, the new way of calculating the LbL injection current \eta_2^{abc}
!                 only needs the LinearResponse kind-of-transformation, that we treat 
!                 separately, since is different from  transformationLinearResponse in
!                 that we need the last two indices of \eta_2^{abc} to construct the
!                 rotation matrices instead the first two as in linear response, i.e. \chi^{ab}
!                 To use the 'old-way' comment next two and put response 25 above! 
!       CASE(25)
!          CALL transformationLbLInjectionCurrent(i_spectra)
       CASE(17)
          CALL transformationOneBeamSpinInjection(i_spectra)
       CASE(71)
          CALL transformationOneBeamSpinInjection(i_spectra)
       CASE(18)
          CALL transformationOneBeamSpinCurrent(i_spectra)
!!!BW!!!
       CASE(19,20)
          CALL transformationCurrentInjection(i_spectra)
!!!BW!!!
       CASE(21,22)
          CALL transformationSecondOrderResponse(i_spectra)
       CASE DEFAULT
          WRITE(6,*) 'Error in initializeSymOps:'
          WRITE(6,*) '   No tranformation for case i_spectra ', i_spectra,' is coded yet'
          WRITE(6,*) '   Stopping'
          STOP 'Error in initializeSymOps: spectrum_type is not coded yet'
       END SELECT
    END DO
    
!!!##############################
  END SUBROUTINE initializeSymOps
!!!##############################
  
  
!!!#############################
  SUBROUTINE getSymOpsFromFile
!!!#############################
    IMPLICIT NONE
    INTEGER :: ios, iSym
    
    OPEN(UNIT=89, FILE="Symmetries.Cartesian",IOSTAT=ios)
    IF (ios.NE.0) THEN
       WRITE(*,*) "Could not open file Symmetries.Cartesian"
       WRITE(*,*) "Stopping"
       STOP "Stopping: Could not open file Symmetries.Cartesian"
    END IF
    READ(89,*) nSym
    WRITE(*,*) "Read number of symmetries from file to be: ", nSym
    
    ALLOCATE(SymOp(nSym,3,3))
    ALLOCATE(dSymOp(nSym))
    
    DO iSym = 1, nSym
       READ(89,*) SymOp(iSym,1,1:3)
       READ(89,*) SymOp(iSym,2,1:3)
       READ(89,*) SymOp(iSym,3,1:3)
       
       IF (debug) THEN
          WRITE(*,*) SymOp(iSym,1,1:3), SymOp(iSym,2,1:3), SymOp(iSym,3,1:3)
       END IF

       dSymOp(iSym) = SymOp(iSym,1,1) * ( SymOp(iSym,2,2)* SymOp(iSym,3,3)-SymOp(iSym,3,2)* SymOp(iSym,2,3) )&
                    - SymOp(iSym,1,2) * ( SymOp(iSym,2,1)* SymOp(iSym,3,3)-SymOp(iSym,3,1)* SymOp(iSym,2,3) )&  
                    + SymOp(iSym,1,3) * ( SymOp(iSym,2,1)* SymOp(iSym,3,2)-SymOp(iSym,3,1)* SymOp(iSym,2,2) )

       
    END DO
    
    CLOSE(89)
!!!#############################
  END SUBROUTINE getSymOpsFromFile
!!!#############################
  
  
!!!####################
  SUBROUTINE znblSymOps
!!!####################
    IMPLICIT NONE
    
    ALLOCATE(SymOp(24,3,3))
    ALLOCATE(dSymOp(24))
    
    SymOp(1,1,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    SymOp(1,2,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    SymOp(1,3,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    dSymOp(1) = 1.d0
    ! 
    SymOp(2,1,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    SymOp(2,2,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    SymOp(2,3,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    dSymOp(2) = 1.d0
    !
    SymOp(3,1,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    SymOp(3,2,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    SymOp(3,3,1:3) = (/-1.d0, 0.d0, 0.d0/)
    dSymOp(3) = 1.d0
    !
    SymOp(4,1,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    SymOp(4,2,1:3) = (/-1.d0, 0.d0, 0.d0/)
    SymOp(4,3,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    dSymOp(4) = 1.d0
    !
    SymOp(5,1,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    SymOp(5,2,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    SymOp(5,3,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    dSymOp(5) = 1.d0
    !
    SymOp(6,1,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    SymOp(6,2,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    SymOp(6,3,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    dSymOp(6) = -1.d0
    !
    SymOp(7,1,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    SymOp(7,2,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    SymOp(7,3,1:3) = (/-1.d0, 0.d0, 0.d0/)
    dSymOp(7) = -1.d0
    !
    SymOp(8,1,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    SymOp(8,2,1:3) = (/-1.d0, 0.d0, 0.d0/)
    SymOp(8,3,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    dSymOp(8) = -1.d0
    !
    SymOp(9,1,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    SymOp(9,2,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    SymOp(9,3,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    dSymOp(9) = -1.d0
    !
    SymOp(10,1,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    SymOp(10,2,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    SymOp(10,3,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    dSymOp(10) = 1.d0
    !
    SymOp(11,1,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    SymOp(11,2,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    SymOp(11,3,1:3) = (/-1.d0, 0.d0, 0.d0/)
    dSymOp(11) = -1.d0
    !
    SymOp(12,1,1:3) = (/-1.d0, 0.d0, 0.d0/)
    SymOp(12,2,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    SymOp(12,3,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    dSymOp(12) = 1.d0
    !
    SymOp(13,1,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    SymOp(13,2,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    SymOp(13,3,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    dSymOp(13) = -1.d0
    !
    SymOp(14,1,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    SymOp(14,2,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    SymOp(14,3,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    dSymOp(14) = -1.d0
    !
    SymOp(15,1,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    SymOp(15,2,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    SymOp(15,3,1:3) = (/-1.d0, 0.d0, 0.d0/)
    dSymOp(15) = 1.d0
    !
    SymOp(16,1,1:3) = (/-1.d0, 0.d0, 0.d0/)
    SymOp(16,2,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    SymOp(16,3,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    dSymOp(16) = -1.d0
    !
    SymOp(17,1,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    SymOp(17,2,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    SymOp(17,3,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    dSymOp(17) = -1.d0
    !
    SymOp(18,1,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    SymOp(18,2,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    SymOp(18,3,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    dSymOp(18) = -1.d0
    !
    SymOp(19,1,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    SymOp(19,2,1:3) = (/-1.d0, 0.d0, 0.d0/)
    SymOp(19,3,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    dSymOp(19) = -1.d0
    !
    SymOp(20,1,1:3) = (/-1.d0, 0.d0, 0.d0/)
    SymOp(20,2,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    SymOp(20,3,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    dSymOp(20) = -1.d0
    !
    SymOp(21,1,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    SymOp(21,2,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    SymOp(21,3,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    dSymOp(21) = 1.d0
    !
    SymOp(22,1,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    SymOp(22,2,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    SymOp(22,3,1:3) = (/ 1.d0, 0.d0, 0.d0/)
    dSymOp(22) = 1.d0
    !
    SymOp(23,1,1:3) = (/ 0.d0, 0.d0,-1.d0/)
    SymOp(23,2,1:3) = (/-1.d0, 0.d0, 0.d0/)
    SymOp(23,3,1:3) = (/ 0.d0, 1.d0, 0.d0/)
    dSymOp(23) = 1.d0
    !
    SymOp(24,1,1:3) = (/-1.d0, 0.d0, 0.d0/)
    SymOp(24,2,1:3) = (/ 0.d0,-1.d0, 0.d0/)
    SymOp(24,3,1:3) = (/ 0.d0, 0.d0, 1.d0/)
    dSymOp(24) = 1.d0
    !
!!!########################    
  END SUBROUTINE znblSymOps
!!!########################
  
!!!####################
  SUBROUTINE wrtzSymOps
!!!####################
    IMPLICIT NONE
    REAL(DP), PARAMETER :: t = 0.8660254037844385d0  ! sqrt(3)/2
    REAL(DP), PARAMETER :: h = 0.5d0                 ! 1/2
    REAL(DP), PARAMETER :: z = 0.d0
    REAL(DP), PARAMETER :: u = 1.d0
    
    ALLOCATE(SymOp(12,3,3))
    ALLOCATE(dSymOp(12))
    
    SymOp( 1,1,1:3) = (/-h,-t, z/)
    SymOp( 1,2,1:3) = (/ t,-h, z/)
    SymOp( 1,3,1:3) = (/ z, z, u/)
    dSymOp( 1) = 1.d0
    !
    SymOp( 2,1,1:3) = (/-h, t, z/)
    SymOp( 2,2,1:3) = (/ t, h, z/)
    SymOp( 2,3,1:3) = (/ z, z, u/)
    dSymOp( 2) =-1.d0
    !
    SymOp( 3,1,1:3) = (/-h,-t, z/)
    SymOp( 3,2,1:3) = (/-t, h, z/)
    SymOp( 3,3,1:3) = (/ z, z, u/)
    dSymOp( 3) =-1.d0
    !
    SymOp( 4,1,1:3) = (/-h, t, z/)
    SymOp( 4,2,1:3) = (/-t,-h, z/)
    SymOp( 4,3,1:3) = (/ z, z, u/)
    dSymOp( 4) = 1.d0
    !
    SymOp( 5,1,1:3) = (/ u, z, z/)
    SymOp( 5,2,1:3) = (/ z, u, z/)
    SymOp( 5,3,1:3) = (/ z, z, u/)
    dSymOp( 5) = 1.d0
    !
    SymOp( 6,1,1:3) = (/ u, z, z/)
    SymOp( 6,2,1:3) = (/ z,-u, z/)
    SymOp( 6,3,1:3) = (/ z, z, u/)
    dSymOp( 6) =-1.d0
    !
    SymOp( 7,1,1:3) = (/ h,-t, z/)
    SymOp( 7,2,1:3) = (/ t, h, z/)
    SymOp( 7,3,1:3) = (/ z, z, u/)
    dSymOp( 7) = 1.d0
    !
    SymOp( 8,1,1:3) = (/-u, z, z/)
    SymOp( 8,2,1:3) = (/ z, u, z/)
    SymOp( 8,3,1:3) = (/ z, z, u/)
    dSymOp( 8) =-1.d0
    !
    SymOp( 9,1,1:3) = (/ h, t, z/)
    SymOp( 9,2,1:3) = (/ t,-h, z/)
    SymOp( 9,3,1:3) = (/ z, z, u/)
    dSymOp( 9) =-1.d0
    !
    SymOp(10,1,1:3) = (/ h,-t, z/)
    SymOp(10,2,1:3) = (/-t,-h, z/)
    SymOp(10,3,1:3) = (/ z, z, u/)
    dSymOp(10) =-1.d0
    !
    SymOp(11,1,1:3) = (/-u, z, z/)
    SymOp(11,2,1:3) = (/ z,-u, z/)
    SymOp(11,3,1:3) = (/ z, z, u/)
    dSymOp(11) = 1.d0
    !
    SymOp(12,1,1:3) = (/ h, t, z/)
    SymOp(12,2,1:3) = (/-t, h, z/)
    SymOp(12,3,1:3) = (/ z, z, u/)
    dSymOp(12) = 1.d0
    !
!!!########################
  END SUBROUTINE wrtzSymOps
!!!########################
  
!!!########################
  SUBROUTINE c6_SymOps
!!!########################
    IMPLICIT NONE
    REAL(DP), PARAMETER :: t = 0.8660254037844385d0  ! sqrt(3)/2
    REAL(DP), PARAMETER :: h = 0.5d0                 ! 1/2
    REAL(DP), PARAMETER :: z = 0.d0
    REAL(DP), PARAMETER :: u = 1.d0
    
    ALLOCATE(SymOp(6,3,3))
    ALLOCATE(dSymOp(6))
    !
    SymOp( 1,1,1:3) = (/ u, z, z/)
    SymOp( 1,2,1:3) = (/ z, u, z/)
    SymOp( 1,3,1:3) = (/ z, z, u/)
    dSymOp( 1) = 1.d0
    !
    SymOp( 2,1,1:3) = (/-h,-t, z/)
    SymOp( 2,2,1:3) = (/ t,-h, z/)
    SymOp( 2,3,1:3) = (/ z, z, u/)
    dSymOp( 2) = 1.d0
    !
    SymOp( 3,1,1:3) = (/-h, t, z/)
    SymOp( 3,2,1:3) = (/-t,-h, z/)
    SymOp( 3,3,1:3) = (/ z, z, u/)
    dSymOp( 3) = 1.d0
    !
    SymOp( 4,1,1:3) = (/-u, z, z/)
    SymOp( 4,2,1:3) = (/ z,-u, z/)
    SymOp( 4,3,1:3) = (/ z, z, u/)
    dSymOp( 4) = 1.d0
    !
    SymOp( 5,1,1:3) = (/ h, t, z/)
    SymOp( 5,2,1:3) = (/-t, h, z/)
    SymOp( 5,3,1:3) = (/ z, z, u/)
    dSymOp( 5) = 1.d0
    !
    SymOp( 6,1,1:3) = (/ h,-t, z/)
    SymOp( 6,2,1:3) = (/ t, h, z/)
    SymOp( 6,3,1:3) = (/ z, z, u/)
    dSymOp( 6) = 1.d0
    !
!!!########################
  END SUBROUTINE c6_SymOps
!!!########################
  
  
!!!#################################################
  SUBROUTINE transformationLinearResponse(i_spectra)
!!!#################################################
    IMPLICIT NONE
    INTEGER :: i_spectra
    REAL(DP) :: T2(3,3)
    INTEGER :: ia, ib
    INTEGER :: ix, iy
    INTEGER :: i
    
    ia = spectrum_info(i_spectra)%spectrum_tensor_component(1)
    ib = spectrum_info(i_spectra)%spectrum_tensor_component(2)
    T2(1:3,1:3) = 0.d0
    DO ix = 1, 3
       DO iy = 1, 3
          DO i = 1, nSym
             T2(ix,iy) = T2(ix,iy) + SymOp(i,ia,ix)*SymOp(i,ib,iy)
          END DO
       END DO
    END DO
    
!!!    tmpArr = reshape((/T2(1:3,1:3)/),(/9/))
    spectrum_info(i_spectra)%transformation_elements(1:9) = reshape((/T2(1:3,1:3)/),(/9/))
    
    WRITE(99,*) "T2", ia, ib
    WRITE(99,*) " "
    WRITE(99,*) spectrum_info(i_spectra)%transformation_elements(1:9)
    DO ix = 1, 3
       DO iy = 1, 3
          WRITE(99,*)  ix, iy, T2(ix,iy)
       END DO
    END DO
    WRITE(99,*) " "
    
!!!############################################
  END SUBROUTINE transformationLinearResponse
!!!############################################

!!!#################################################
  SUBROUTINE transformationLbLInjectionCurrent(i_spectra)
!!!#################################################
    IMPLICIT NONE
    INTEGER :: i_spectra
    REAL(DP) :: T2(3,3)
    INTEGER :: ib, ic
    INTEGER :: ix, iy
    INTEGER :: i
    
    ib = spectrum_info(i_spectra)%spectrum_tensor_component(2)
    ic = spectrum_info(i_spectra)%spectrum_tensor_component(3)
    T2(1:3,1:3) = 0.d0
    DO ix = 1, 3
       DO iy = 1, 3
          DO i = 1, nSym
             T2(ix,iy) = T2(ix,iy) + SymOp(i,ib,ix)*SymOp(i,ic,iy)
          END DO
       END DO
    END DO
    
!!!    tmpArr = reshape((/T2(1:3,1:3)/),(/9/))
    spectrum_info(i_spectra)%transformation_elements(1:9) = reshape((/T2(1:3,1:3)/),(/9/))
    
    WRITE(99,*) "T2", ib, ic
    WRITE(99,*) " "
    WRITE(99,*) spectrum_info(i_spectra)%transformation_elements(1:9)
    DO ix = 1, 3
       DO iy = 1, 3
          WRITE(99,*)  ix, iy, T2(ix,iy)
       END DO
    END DO
    WRITE(99,*) " "
    
!!!############################################
  END SUBROUTINE transformationLbLInjectionCurrent
!!!############################################

  
!!!######################################################
  SUBROUTINE transformationSecondOrderResponse(i_spectra)
!!!######################################################
    IMPLICIT NONE
    INTEGER :: i_spectra
    REAL(DP) :: T3(3,3,3)
    INTEGER :: ia, ib, ic
    INTEGER :: ix, iy, iz
    INTEGER :: i
    
    ia = spectrum_info(i_spectra)%spectrum_tensor_component(1)
    ib = spectrum_info(i_spectra)%spectrum_tensor_component(2)
    ic = spectrum_info(i_spectra)%spectrum_tensor_component(3)
    
    T3(1:3,1:3,1:3) = 0.d0
    DO ix = 1, 3
       DO iy = 1, 3
          DO iz = 1, 3
             DO i = 1, nSym
                T3(ix,iy,iz) = T3(ix,iy,iz) + SymOp(i,ia,ix)*SymOp(i,ib,iy)*SymOp(i,ic,iz)
             END DO
          END DO
       END DO
    END DO
    
    spectrum_info(i_spectra)%transformation_elements(1:27) = reshape(T3,(/27/))    
    
    WRITE(99,*) "T3", ia, ib, ic
    WRITE(99,*) " "
    WRITE(99,*) spectrum_info(i_spectra)%transformation_elements(1:27)
    DO ix = 1, 3
       DO iy = 1, 3
          DO iz = 1, 3
             WRITE(99,*)  ix, iy, iz, T3(ix,iy,iz)
          END DO
       END DO
    END DO
    WRITE(99,*) " "
    
!!!###############################################
  END SUBROUTINE transformationSecondOrderResponse
!!!###############################################
  
!!!#######################################################
  SUBROUTINE transformationOneBeamSpinInjection(i_spectra)
!!!#######################################################
    IMPLICIT NONE
    INTEGER :: i_spectra
    REAL(DP) :: PT3(3,3,3)
    INTEGER :: ia, ib, ic
    INTEGER :: ix, iy, iz
    INTEGER :: i    
    
    ia = spectrum_info(i_spectra)%spectrum_tensor_component(1)
    ib = spectrum_info(i_spectra)%spectrum_tensor_component(2)
    ic = spectrum_info(i_spectra)%spectrum_tensor_component(3)
    
    
    PT3(1:3,1:3,1:3) = 0.d0
    DO ix = 1, 3
       DO iy = 1, 3
          DO iz = 1, 3
             DO i = 1, nSym
                PT3(ix,iy,iz) = PT3(ix,iy,iz) +                              &
                     SymOp(i,ia,ix)*SymOp(i,ib,iy)*SymOp(i,ic,iz)*dSymOp(i)
             END DO
          END DO
       END DO
    END DO
    
    spectrum_info(i_spectra)%transformation_elements(1:27) = reshape(PT3,(/27/))    
    
    WRITE(99,*) "PT3", ia, ib, ic
    WRITE(99,*) " "
    WRITE(99,*) spectrum_info(i_spectra)%transformation_elements(1:27)
    DO ix = 1, 3
       DO iy = 1, 3
          DO iz = 1, 3
             WRITE(99,*)  ix, iy, iz, PT3(ix,iy,iz)
          END DO
       END DO
    END DO
    WRITE(99,*) " "
    
!!!################################################
  END SUBROUTINE transformationOneBeamSpinInjection
!!!################################################


!!!#####################################################
  SUBROUTINE transformationOneBeamSpinCurrent(i_spectra)
!!!#####################################################
    IMPLICIT NONE
    INTEGER :: i_spectra
    REAL(DP) :: PT4(3,3,3,3)
    INTEGER :: ia, ib, ic, id
    INTEGER :: ix, iy, iz, iw
    INTEGER :: i    
    
    ia = spectrum_info(i_spectra)%spectrum_tensor_component(1)
    ib = spectrum_info(i_spectra)%spectrum_tensor_component(2)
    ic = spectrum_info(i_spectra)%spectrum_tensor_component(3)
    id = spectrum_info(i_spectra)%spectrum_tensor_component(4)
    
    PT4(1:3,1:3,1:3,1:3) = 0.d0
    DO ix = 1, 3
       DO iy = 1, 3
          DO iz = 1, 3
             DO iw = 1, 3
                DO i = 1, nSym
                   PT4(ix,iy,iz,iw) = PT4(ix,iy,iz,iw) +         &
                        SymOp(i,ia,ix)*SymOp(i,ib,iy)*SymOp(i,ic,iz)*SymOp(i,id,iw)*dSymOp(i)
                END DO
             END DO
          END DO
       END DO
    END DO
    
    spectrum_info(i_spectra)%transformation_elements(1:81) = reshape(PT4,(/81/))
    
    WRITE(99,*) "PT4", ia, ib, ic, id
    WRITE(99,*) " "
    WRITE(99,*) spectrum_info(i_spectra)%transformation_elements(1:81)
    DO ix = 1, 3
       DO iy = 1, 3
          DO iz = 1, 3
             DO iw = 1, 3
                WRITE(99,*)  ix, iy, iz, iw, PT4(ix,iy,iz,iw)
             END DO
          END DO
       END DO
    END DO
    WRITE(99,*) " "
    
!!!##############################################
  END SUBROUTINE transformationOneBeamSpinCurrent
!!!##############################################

!!!BW Begin!

!!!###################################################
  SUBROUTINE transformationCurrentInjection(i_spectra)
!!!###################################################
    IMPLICIT NONE
    INTEGER :: i_spectra
    REAL(DP) :: T4(3,3,3,3)
    INTEGER :: ia, ib, ic, id
    INTEGER :: ix, iy, iz, iw
    INTEGER :: i
    
    ia = spectrum_info(i_spectra)%spectrum_tensor_component(1)
    ib = spectrum_info(i_spectra)%spectrum_tensor_component(2)
    ic = spectrum_info(i_spectra)%spectrum_tensor_component(3)
    id = spectrum_info(i_spectra)%spectrum_tensor_component(4)

    T4(1:3,1:3,1:3,1:3) = 0.d0
    DO ix = 1,3
       DO iy = 1,3
          DO iz = 1,3
             DO iw = 1,3
                DO i = 1,nSym
                   T4(ix,iy,iz,iw) = T4(ix,iy,iz,iw) +         &
                        SymOp(i,ia,ix)*SymOp(i,ib,iy)*SymOp(i,ic,iz)*SymOp(i,id,iw)
                END DO
             END DO
          END DO
       END DO
    END DO
    
    spectrum_info(i_spectra)%transformation_elements(1:81) = reshape(T4,(/81/))
    
    WRITE(99,*) "T4", ia, ib, ic, id
    WRITE(99,*) spectrum_info(i_spectra)%transformation_elements(1:81)
    DO ix = 1,3
       DO iy = 1,3
          DO iz = 1,3
             DO iw = 1,3
                WRITE(99,*)  ix, iy, iz, iw, T4(ix,iy,iz,iw)
             END DO
          END DO
       END DO
    END DO
!!!############################################
  END SUBROUTINE transformationCurrentInjection
!!!############################################
  
!!!BW End!
  
!############################
END MODULE symmetryOperations
!############################
