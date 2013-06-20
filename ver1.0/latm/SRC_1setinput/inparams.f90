!!!############
MODULE inparams
!!!############
  USE constants, ONLY : DP, pi
  USE constants, ONLY : debug
  IMPLICIT NONE

  INTEGER :: nVal          ! number of valence bands
  INTEGER :: nMax          ! total number of bands
  INTEGER :: nVal_tetra    ! number of valence bands in tetra_method
  INTEGER :: nMax_tetra    ! total number of bands in tetra_method
  INTEGER :: kMax          ! number of kpoints
  INTEGER :: nTetra         ! number of tetrahedra
  INTEGER :: nTrans          ! number of transitions
  LOGICAL :: withSO           ! flag for whether spin-orbit is included or not
  INTEGER :: nSpinor=0        ! number of spinor components (1 or 2 only)
!!!                            ! if nSpinor==2 then will look for smn.d file
  INTEGER :: nSym                ! number of symmetry operations
  REAL(DP) :: actualBandGap=0.d0  ! actual badn gap in eV
  REAL(DP) :: scissor             ! scissor shift in eV
  REAL(DP) :: tol                 ! tolerance for degeneracy
  
  CHARACTER(LEN=10) :: crystal_class ! name of crystal type
  ! crystal_class can be zincblende or wurtzite
  
  CHARACTER(LEN=80) :: paramFile
  CHARACTER(LEN=80) :: spectrumFile
  
  INTEGER :: number_of_spectra_to_calculate
  TYPE spectrum
     CHARACTER(LEN=80) :: integrand_filename
     INTEGER :: integrand_filename_unit
     INTEGER :: spectrum_type
  ! TYPE spectrum
  !    CHARACTER(LEN=60) :: integrand_filename
  !    INTEGER :: integrand_filename_unit
  !    INTEGER :: spectrum_type
  !    !  1 : chi1       !  2 : Lambda       !  3 : eta2        !  4 : S
  !    !  5 : C          !  6 : Ctilde       !  7 : E            !  8 : Etilde
  !    !  9 : staticChi1 ! 10 : staticS      ! 11 : staticC      ! 12 : staticCtilde
  !    ! 13 : staticE    ! 14 : staticEtilde ! 15 : staticChi2i  ! 16 : staticChi2e
  !    ! 17 : zeta_spin_layered ! 18 : zeta_spin_bulk  ! 19 : xi2          ! 20 : eta3
  !    ! 21 : SHG1       ! 22 : SHG2         ! 23 : LEO          ! 24 : calChi1 (layered)
  !    ! 25 : caleta2  (layered)
  !    ! 26 : n-dot-cc (layered)
  !    ! 27 : n-dot-vv (layered)
  !    LOGICAL :: compute_integrand
  !    INTEGER, POINTER :: spectrum_tensor_component(:)
  !    REAL(DP), POINTER :: transformation_elements(:)
  ! END TYPE spectrum
     LOGICAL :: compute_integrand
     INTEGER, POINTER :: spectrum_tensor_component(:)
     REAL(DP), POINTER :: transformation_elements(:)
  END TYPE spectrum
  TYPE(spectrum), ALLOCATABLE :: spectrum_info(:)
  
  CHARACTER(LEN=80) ::  energy_data_filename  ! energy input file
  CHARACTER(LEN=80) ::  energys_data_filename ! scissored energy file
  CHARACTER(LEN=80) ::  half_energys_data_filename ! scissored energy times 0.5 file
  CHARACTER(LEN=80) ::  pmn_data_filename     ! momentum me input file
!!!FN
  CHARACTER(LEN=80) ::  cal_data_filename     !caligrpahic momentum matrix elements
  CHARACTER(LEN=80) ::  cur_data_filename     !caligrpahic current momentum matrix elements
!!!FN
  CHARACTER(LEN=80) ::  rmn_data_filename     ! position me output file
  CHARACTER(LEN=80) ::  der_data_filename     ! derivative me output file
  CHARACTER(LEN=80) ::  smn_data_filename     ! spin me output file
  CHARACTER(LEN=80) ::  rhomm_data_filename   ! layered rhomm output file
  
  REAL(DP) :: spin_factor = 0.d0
  REAL(DP) :: units_factor = 0.d0
  REAL(DP) :: unit_cell_volume = 0.d0
  
  ! FOLLOWING IS USED ONLY IN TETRAHEDRON METHOD
  !  INTEGER :: optical_property            ! The optical property to calculate
  
  ! FILES ONLY FOR TETRAHEDRON METHOD
  CHARACTER(LEN=80) :: tet_list_filename
  CHARACTER(LEN=80) :: integrand_filename
  CHARACTER(LEN=80) :: spectrum_filename
  
  ! ENERGY MESH
  REAL(DP) :: energy_Min
  REAL(DP) :: energy_Max
  INTEGER  :: energy_Steps
  REAL(DP), ALLOCATABLE :: energy_Out(:)
  
  ! Below, the minus signs come from the electron charge
  ! Chi1 -- used for the imaginary part of the linear response
  REAL(DP), PARAMETER :: Chi1_factor = pi*(27.21d0)**3
    !!! right now We have modificate the response NO. 24 in order to 
  !!! give the results in SI en Gaussian units X1(SI)=4\pi*X1(Gaussian)
  REAL(DP), PARAMETER :: Chi1_factor24 = (pi*(27.21d0)**3)
  ! Lambda -- used for shift currents
  REAL(DP), PARAMETER :: Lambda_factor = -pi*(27.21d0)**4*(2.4085d9)
  !!!!!!!!!!!&&&&&&
  ! Gamma -- used for bulk injection currents
  ! (\pi e^3/2\hbar^2) * (1/a_0^3) *   v    *    r     *     r     *   (1/w)
  ! prefactor          *  volume   *velocity* position *  position *delta-function
  ! -pi/2 * H^3 * e^3/\hbar^2 in cgs * 3.708833177694700d-15 => C^3/(J^2 s^2)
  REAL(DP), PARAMETER :: Gamma_factor = -(pi/2.d0)*(27.21d0)**3*(9.985d25)*3.7037d-15
  ! Gamma -- used for surface injection currents
  ! (\pi e^3/\hbar^2) * (1/a_0^3) *   v    *    r     *     r     *   (1/w)
  ! prefactor          *  volume   *velocity* position *  position *delta-function
  ! -pi * H^3 * e^3/\hbar^2 in cgs * 3.708833177694700d-15 => C^3/(J^2 s^2)
  ! surface differs from bulk by a factor of 2, since we use the Im[rr] in the surface
  ! term instead of [r,r] for the bulk term, and Im[rr]=[r,r]/2
  REAL(DP), PARAMETER :: caleta_factor = -pi*(27.21d0)**3*(9.985d25)*3.7037d-15
  !!!!!!!!!!!&&&&&&
  ! Rectification_factor -- used for rectification coefficients
  REAL(DP), PARAMETER :: rec_factor = -pi*(27.21d0)**4*(1.5875d-6)
  REAL(DP), PARAMETER :: Rectification_factor = -pi*(27.21d0)**4*(1.5875d-6)
  ! static Chi1 -- used for linear response
  REAL(DP), PARAMETER :: staticChi1_factor = (27.21d0)**3
  REAL(DP), PARAMETER :: staticRec_factor = -(27.21d0)**4*(1.5875d-6)
  ! one-beam spin population bulk 
  !! need to check the sign 
!  REAL(DP), PARAMETER :: spin_injection_factor_bulk =  -(pi/2.)*(27.21d0)**3
!  REAL(DP), PARAMETER :: spin_injection_factor_layered = -(pi/4.)*(27.21d0)**3
!!!!!!!!!!!!!!!!!! 26 Marzo 2009 
  REAL(DP), PARAMETER :: spin_injection_factor_bulk =  (pi/2.)*(27.21d0)**3
  REAL(DP), PARAMETER :: spin_injection_factor_layered = (pi/2)*(27.21d0)**3
!!!!!!!!!!!!!!!!!! 26 Marzo 2009 
! one-beam spin current
!  REAL(DP), PARAMETER :: one_beam_spin_current_factor = pi*(27.21d0)**3*(2.187676d8)
! The above factor is incorrect.  The factor of 0.5d0 below is for the spin
!  REAL(DP), PARAMETER :: one_beam_spin_current_factor = pi*(27.21d0)**2*(0.5d0)*(0.0095378)
! The above factor is totally incorrect.  The new correct factor is
  REAL(DP), PARAMETER :: one_beam_spin_current_factor = (pi/2.d0)*(27.21d0)**3*(2.191d8)
!!!BW!
  ! ****** 2 + 1 current injection ******
  ! two-photon density injection xi2 
  !in cgs
  REAL(DP), PARAMETER :: two_photon_density_injection_factor = 32.0d0*pi*(27.2114d0)**7  &
       *(0.52918d0)**4/((1.05457d0)*(4.80321d0)**2)*1.0d15
  ! BMS 21/01/05 gives xi2 
  ! in m/(s V^4)
!   REAL(DP), PARAMETER :: two_photon_density_injection_factor = 4.0d0*pi/(3.0d4)**3  &
!        *16.d0*pi*(0.52917d0)**5*(27.2114d0)**8/((4.8066d0)**4*6.5821d0)*1.0d16
!!!
! 2+1 current injection (eta3) in cgs
  REAL(DP), PARAMETER :: twoplusone_current_injection_factor = pi*(27.2114d0)**5  &
       *(1.05457d0)/((9.10939d0)**2*(4.80321d0)**2)*1.0d49
   ! BMS 21/01/05 gives eta3
   ! in (m C)/(s^2 V^3)
!   REAL(DP), PARAMETER :: twoplusone_current_injection_factor = (4.d0*pi/81.d0)*(27.2114d0)**7  &
!        *(0.52917d0)**4/((4.8066d0*6.5821d0)**2)*1.0d3
!!!BW!
  
  !REAL(DP), PARAMETER :: one_beam_current_injection_factor = -pi*(27.21d0)**3*(9.985d25)
!!!
  REAL(DP), PARAMETER :: shg1_factor = 1144690529.7519768d0/2.d0
  REAL(DP), PARAMETER :: shg2_factor = 1144690529.7519768d0/2.d0
!in principle above should be below term that appears in Cabellos tiniba version
!(pi*(4.803d-10)**3*(4.189d8) &
!  /((1.0546d-27)**2*(9.109d-28)**3*(1.519d15)**5*(5.291d-9)**3*(1.993d-19)**3))/2.

!!!
  REAL(DP), PARAMETER :: leo_factor = 0.d0  !! LEO not implemented
  
!!!FN
!!!  INTEGER, PARAMETER :: number_of_known_spectrum_types = 23
!!!  INTEGER, PARAMETER :: number_of_known_spectrum_types = 27
  !!!!! Martes 27 Febrero 2009 cab 
  INTEGER, PARAMETER :: number_of_known_spectrum_types = 50
  REAL(DP), PARAMETER :: uno=1.d0 
!!!FN
  ! TYPE spectrum
  !    CHARACTER(LEN=60) :: integrand_filename
  !    INTEGER :: integrand_filename_unit
  !    INTEGER :: spectrum_type
  !    !  1 : chi1       !  2 : Lambda       !  3 : eta2        !  4 : S
  !    !  5 : C          !  6 : Ctilde       !  7 : E            !  8 : Etilde
  !    !  9 : staticChi1 ! 10 : staticS      ! 11 : staticC      ! 12 : staticCtilde
  !    ! 13 : staticE    ! 14 : staticEtilde ! 15 : staticChi2i  ! 16 : staticChi2e
  !    ! 17 : zeta_spin_layered ! 18 : zeta_spin_bulk  ! 19 : xi2          ! 20 : eta3
  !    ! 21 : SHG1       ! 22 : SHG2         ! 23 : LEO          ! 24 : calChi1 (layered)
  !    ! 25 : caleta2  (layered)
  !    ! 26 : n-dot-cc (layered)
  !    ! 27 : n-dot-vv (layered)
  !    LOGICAL :: compute_integrand
  !    INTEGER, POINTER :: spectrum_tensor_component(:)
  !    REAL(DP), POINTER :: transformation_elements(:)
  ! END TYPE spectrum
  REAL(DP), PARAMETER :: spectrum_factor(number_of_known_spectrum_types)=       &
       !     1            2              3
       (/Chi1_factor, Lambda_factor, Gamma_factor,                              &
       !     4                       5 
       Rectification_factor, Rectification_factor,                              &
       !     6                       7                     8
       Rectification_factor, Rectification_factor, Rectification_factor,        &
       !     9                       10           11              12
       staticChi1_factor, staticRec_factor, staticRec_factor, staticRec_factor, &
       !     13                       14           15              16
       staticRec_factor, staticRec_factor, staticRec_factor, staticRec_factor,  &
       !     17                       18
       Chi1_factor,              Chi1_factor,                &
       !Chi1_factor,spin_injection_factor_bulk,                &
       
       !                19                                 20
       two_photon_density_injection_factor, twoplusone_current_injection_factor, &
!!!FN
       ! 21            22          23           24           25
       shg1_factor, shg2_factor, leo_factor, Chi1_factor24, caleta_factor, &
       ! 26            27           28         29 
       Chi1_factor,Chi1_factor, Chi1_factor, spin_injection_factor_layered, &
      ! Chi1_factor,Chi1_factor, Chi1_factor,  Chi1_factor, &
       ! 30     31    32    33   34   35   36   37   38   39           40
         uno,  uno,  uno,  uno, uno,  uno, uno, uno, uno, Chi1_factor, Chi1_factor, &
       ! 41                           42    43    44   45   46   47   48   49   50 
        spin_injection_factor_bulk ,  uno,  uno,  uno, uno,  uno, uno, uno, uno, uno/)
!!!FN
  
CONTAINS
  
!!!##################################
  SUBROUTINE checkCommandLineInputs
!!!##################################
    IMPLICIT NONE
    INTEGER :: ios
    LOGICAL :: fileExists
    
    IF ((TRIM(paramFile).EQ."").OR.(TRIM(spectrumFile).EQ."")) THEN
       WRITE(*,*) 'USAGE: setInput paramFile spectrumFile'
       WRITE(*,*) '  paramFile: the system parameter file'
       WRITE(*,*) '  spectrumFile: the file containing information on which spectrum to calcualte'
       STOP
    END IF
    
    INQUIRE(FILE=paramFile, EXIST=fileExists)
    IF (.NOT.fileExists) THEN
       WRITE(*,*) 'Cannot find file: ', TRIM(paramFile)
       STOP 'STOPPING: Cannot find parameter File'
    END IF
    
    INQUIRE(FILE=spectrumFile, EXIST=fileExists)
    IF (.NOT.fileExists) THEN
       WRITE(*,*) 'Cannot find file: ', TRIM(spectrumFile)
       STOP 'STOPPING: Cannot find spectrum File'
    END IF
    
!!!#######################################
  END SUBROUTINE checkCommandLineInputs
!!!#######################################
  
!!!#######################
  SUBROUTINE readParamFile
!!!#######################
    IMPLICIT NONE
    INTEGER :: ios
!!! common input data
    NAMELIST/INDATA/crystal_class
    NAMELIST/INDATA/nVal, nMax, nVal_tetra, nMax_tetra
    NAMELIST/INDATA/kMax, nTetra, tol
!!!    NAMELIST/INDATA/nSym
    NAMELIST/INDATA/withSO, nSpinor
    NAMELIST/INDATA/actualBandGap
    NAMELIST/INDATA/scissor
    NAMELIST/INDATA/energy_data_filename, energys_data_filename
    NAMELIST/INDATA/half_energys_data_filename
!!! FN
    NAMELIST/INDATA/pmn_data_filename, rmn_data_filename, cal_data_filename
    NAMELIST/INDATA/cur_data_filename
!!!FN
    NAMELIST/INDATA/smn_data_filename, der_data_filename
    NAMELIST/INDATA/rhomm_data_filename
    NAMELIST/INDATA/unit_cell_volume
    NAMELIST/INDATA/tet_list_filename
    NAMELIST/INDATA/integrand_filename
    NAMELIST/INDATA/spectrum_filename
    NAMELIST/INDATA/energy_min, energy_max, energy_steps
    
    OPEN(UNIT=1, FILE=paramFile, FORM='FORMATTED', STATUS='OLD', IOSTAT=ios)
    IF (ios.NE.0) THEN
       WRITE(6,*) "Error opening file: ", TRIM(paramFile)
       WRITE(6,*) "This should be the parameter file and be the first argument."
       WRITE(6,*) "Stopping"
       STOP
!INFO
!   ELSE
!      WRITE(6,*) "Opening ", TRIM(paramFile)
    END IF
    
    READ(1,NML=indata,IOSTAT=ios)
    IF (ios.NE.0) THEN
       WRITE(6,*) ios
       WRITE(6,*) 'READ ERROR WITH ', TRIM(paramFile)
       STOP 'inparams.f90:STOPPING'
    END IF
    CLOSE(1)
    
    crystal_class = "blank"
    nTetra = 99999999
    nSym = 0
    
    IF (debug) WRITE(6,NML=INDATA)
    
    ! because I am paranoid:
    IF (debug) THEN
       WRITE(*,*) "crystal_class ", TRIM(crystal_class)
       WRITE(*,*) "nVal ", nVal
       WRITE(*,*) "nMax ", nMax
       WRITE(*,*) "nVal_tetra ", nVal_tetra
       WRITE(*,*) "nMax_tetra ", nMax_tetra
       WRITE(*,*) "kMax ", kMax
       WRITE(*,*) "nTetra ", nTetra
       WRITE(*,*) "tol ", tol
       WRITE(*,*) "nSym ", nSym
       WRITE(*,*) "withSO ", withSO
       WRITE(*,*) "nSpinor WHAT ", nSpinor
       WRITE(*,*) "actualBandgap ", actualBandgap
       WRITE(*,*) "scissor ", scissor
       WRITE(*,*) "energy_data_filename ", TRIM(energy_data_filename)
       WRITE(*,*) "energys_data_filename ", TRIM(energys_data_filename)
       WRITE(*,*) "half_energys_data_filename ", TRIM(half_energys_data_filename)
       WRITE(*,*) "pmn_data_filename ", TRIM(pmn_data_filename)
!!!FN
!!!OJO
       WRITE(*,*) "cal_data_filename ", TRIM(cal_data_filename)
       WRITE(*,*) "cur_data_filename ", TRIM(cur_data_filename)
!!!FN
       WRITE(*,*) "rmn_data_filename ", TRIM(rmn_data_filename)
       WRITE(*,*) "smn_data_filename ", TRIM(smn_data_filename)
       WRITE(*,*) "rhomm_data_filename ", TRIM(rhomm_data_filename)
       WRITE(*,*) "der_data_filename ", TRIM(der_data_filename)
       WRITE(*,*) "unit_cell_volume ", unit_cell_volume
       WRITE(*,*) "tet_list_filename ", TRIM(tet_list_filename)
       WRITE(*,*) "integrand_filename ", TRIM(integrand_filename)
       WRITE(*,*) "spectrum_filename ", TRIM(spectrum_filename)
       WRITE(*,*) "energy_min ", energy_min
       WRITE(*,*) "energy_max ", energy_max
       WRITE(*,*) "energy_steps ", energy_steps
    END IF
    nTrans = nVal*(nMax-nVal)
!INFO
   if(debug) WRITE(6,FMT='(A,I5)') "nTrans = ", nTrans
    
    IF ((nSpinor.NE.1).AND.(nSpinor.NE.2)) THEN
       WRITE(6,*) 'PROBLEM: nSpinor not equal to 1 or 2'
       STOP 'STOPPING: problem with variable nSpinor'
    END IF
    if(debug)    WRITE(*,*) "nspinor QUE", nSpinor 
    
    if(debug)    WRITE(*,*) "nSpinor WHAT ", nSpinor

    IF (withSO) THEN ! spin-orbit included
       spin_factor = 1.d0
       IF (nSpinor == 1) THEN
          WRITE(6,*) 'INCONSISTENCY IN INPUT FILE: nSpinor == 1, but with_SO = .true.'
          STOP
       END IF
    ELSE ! no spin-orbit included
       spin_factor = 2.d0
       IF (nSpinor == 2) THEN
          WRITE(6,*) 'INCONSISTENCY IN INPUT FILE: nSpinor == 2, but with_SO = .false.'
          STOP
       ELSE IF (nSpinor.NE.1) THEN
          WRITE(6,*) 'INCONSISTENCY IN INPUT FILE: nSpinor != 1, but with_SO = .false.'
          STOP
       END IF
    END IF
   
!     SELECT CASE (nspinor)
!     CASE(1) ! no spin-orbit included
!       spin_factor = 2.d0
!      CASE(2) ! spin-orbit included
!        spin_factor = 1.d0
!     CASE DEFAULT
!        STOP 'nspinor makes no sense'
!     END SELECT

    !!! added diciembre 10 2008  
    !!! added diciembre 10 2008  
    !!! added diciembre 10 2008  
     !SELECT CASE (nspinor)
     !CASE(1) ! no spin-orbit included
     !  nspinor = 2.d0
     ! CASE(2) ! spin-orbit included
     !   nspinor = 2.d0
     !CASE DEFAULT
     !   STOP 'nspinor makes no sense'
     !END SELECT
    !IF (spin_factor == 1) then 
    !    spin_factor = 2.d0
    !endif 

     !IF (nSpinor == 2) then 
     !   spin_factor = 1.d0
     !endif

    
    if(debug)    WRITE(6,'(A,F)') 'spin_factor (inparams.f90) paso   = ', spin_factor
    
!!!#############################
  END SUBROUTINE readParamFile
!!!#############################
  
!!!############################
  SUBROUTINE readSpectrumFile
!!!############################
!!!    USE file_control, ONLY : openOutputDataFiles, closeOutputDataFiles
    IMPLICIT NONE
    INTEGER :: i, dims, length
    INTEGER :: istat, iostat
    INTEGER, ALLOCATABLE :: tmpIntArr(:)
    CHARACTER(LEN=14) :: response_type



!!!
    if(debug)    WRITE(6,*) ' '
    if(debug)    WRITE(6,*) 'Opening ', spectrumFile
    OPEN(UNIT=1, FILE=spectrumFile, FORM='FORMATTED', STATUS='OLD', IOSTAT=istat)
    IF (istat.NE.0) THEN
       WRITE(6,*) 'Error opening file', spectrumFile
       WRITE(6,*) 'Stopping'
       STOP
    END IF
    
    READ(1,*) number_of_spectra_to_calculate
    if(debug) WRITE(6,*) "Number of spectra to calculate", number_of_spectra_to_calculate
    
    ALLOCATE (spectrum_info(number_of_spectra_to_calculate), STAT=istat)
    IF (istat.NE.0) THEN
       ! Could not allocate
       WRITE(6,*) "Could not allocate spectrum_info)"
       WRITE(6,*) "Tried to allocate size of ", number_of_spectra_to_calculate
       WRITE(6,*) "Stopping"
       STOP
    END IF
    
    DO i=1, number_of_spectra_to_calculate
       READ(1,*) &
            spectrum_info(i)%spectrum_type,            &
            spectrum_info(i)%integrand_filename,       &
            spectrum_info(i)%integrand_filename_unit,  &
            spectrum_info(i)%compute_integrand
       if(debug) then
          WRITE(*,*) "For spectrum number", i
          WRITE(*,*) "inparams.f90: Spectrum type is@ReadSpectrumFile ", spectrum_info(i)%spectrum_type
          WRITE(*,*) "Spectrum filename is ", spectrum_info(i)%integrand_filename
          WRITE(*,*) "Spectrum filename unit is ", spectrum_info(i)%integrand_filename_unit
          WRITE(*,*) "Whether integrand will be computed: ", spectrum_info(i)%compute_integrand
          WRITE(*,*) ' '
       end if
!!! bms oct 1 2010
!!! writes the response type in response_type file so we can
!!! choses \delta(w_{nm}- n*w) with n=1-omega or n=2-omega 
!!! for the corresponding term of the response
!!! in subroutine deltaFunction
!!! shg2 (case(22)) needs n=2
    OPEN(UNIT=2, FILE='response_type', FORM='FORMATTED')
    write(2,*)spectrum_info(i)%spectrum_type
       
       SELECT CASE(spectrum_info(i)%spectrum_type)
       CASE(1)
          WRITE(*,*) '       inparams.f90:imaginary part of Chi1'
          dims = 2
          length = 9
       CASE(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,25)
          WRITE(6,*) '       inparams.f90:second order response'
          dims = 3
          length = 27
       CASE(17)
          WRITE(6,*) '       inparams.f90:one-beam spin injection bulk'
          dims = 3
          length = 27
       CASE(71)
          WRITE(6,*) '       inparams.f90:one-beam spin injection caligraphic RPMNS '
          dims = 3
          length = 27
       CASE(18)
          WRITE(6,*) '       inparams.f90:one-beam spin injection bulk'
          dims = 3
          length = 27
       CASE(19,20)
          WRITE(6,*) "       inparams.f90:two-plus-one currentinjection"
          dims = 4
          length = 81
       CASE(21)
          WRITE(6,*) "       inparams.f90:Second-harmonic generation 1-omega"
          dims = 3
          length = 27
       CASE(22)
          WRITE(6,*) "       inparams.f90:Second-harmonic generation 2-omega"
          dims = 3
          length = 27
       CASE(23)
          ! LEO
          ! Not Implemented
          WRITE(6,*) "       inparams.f90:LEO is not implemeneted yet!"
          STOP
!!!FN
       CASE(24)
          WRITE(6,*) "       inparams.f90:Layered Chi1 calculation"
          dims = 2
          length = 9
!!!FN
!!!BMS
       CASE(26)
          WRITE(6,*) "       inparams.f90:Layered n-dot-cc calculation"
          dims = 2
          length = 9
       CASE(27)
          WRITE(6,*) "       inparams.f90:Layered n-dot-vv calculation"
          dims = 2
          length = 9
!!!BMS
       CASE(28)
          WRITE(6,*) '       inparams.f90:Spin Injection Martes 17 Febrero 2009'
          dims = 3
          length = 27
         CASE(29)
          WRITE(6,*) '       inparams.f90: Spin Injection Layered'
          dims = 3
          length = 27
       CASE(39)
          WRITE(*,*) '       inparams.f90:imaginary part of Chi1 cabellos'
          dims = 2
          length = 9
        CASE(40)
          WRITE(6,*) '       inparams.f90:Spin Injection Viernes 20 Febrero 2009'
          dims = 3
          length = 27
        CASE(41)
          WRITE(6,*) '       inparams.f90:Spin Injection abs Viernes 20 Febrero 2009'
          dims = 3
          length = 27
       CASE DEFAULT
          WRITE(6,*) "Error in input file:"
          WRITE(6,*) "Spectrum_type ", spectrum_info(i)%spectrum_type," unknown"
          WRITE(6,*) "Stopping"
          STOP
       END SELECT
       
       ALLOCATE(tmpIntArr(dims), STAT=istat)
       IF (istat.NE.0) THEN
          ! Could not allocate
          WRITE(6,*) "Could not allocate tmpIntArr"
          WRITE(6,*) "Tried to allocate size of ", dims
          WRITE(6,*) "Stopping"
          STOP
       END IF

       if(debug) WRITE(*,*) "DIMS = ", dims
       
       READ(1,*) tmpIntArr(1:dims)
       IF ((tmpIntArr(1).GE.1).AND.(tmpIntArr(1).LE.3)) THEN
          ALLOCATE(spectrum_info(i)%spectrum_tensor_component(dims), STAT=istat)
          IF (istat.NE.0) THEN
             ! Could not allocate
             WRITE(6,*) "Could not allocate spectrum_info(", i, ")%spectrum_tensor_component"
             WRITE(6,*) "Tried to allocate size of ", dims
             WRITE(6,*) "Stopping"
             STOP
          END IF
          spectrum_info(i)%spectrum_tensor_component(1:dims) = tmpIntArr(1:dims)
          if(debug) WRITE(6,*) 'components are ', spectrum_info(i)%spectrum_tensor_component(1:dims)
       ELSE
          STOP 'Error in input file: spectrum_tensor_components not equal to 1, 2, or 3'
       END IF
       
       DEALLOCATE(tmpIntArr, STAT=istat)
       IF (istat.NE.0) THEN
          WRITE(6,*) "Could not deallocate tmpIntArr"
          WRITE(6,*) "Stopping"
          STOP
       END IF
       
       ALLOCATE(spectrum_info(i)%transformation_elements(length), STAT=istat)
       IF (istat.NE.0) THEN
          WRITE(6,*) "Could not allocate spectrum_info_transformation_elements"
          WRITE(6,*) "Stopping"
          STOP
       END IF
       
    END DO
    
    CLOSE(1)
    
    ! SHOULD ADD CHECKS ON THE DATA HERE
    ! 1) THAT THE SPECTRUM TYPE EXISTS
    ! 2) THAT THERE ARE NOT TOO MANY OR NOT ENOUGH COMPONENTS ENTERED
    
!!!******************************
  END SUBROUTINE readSpectrumFile
!!!******************************

!!!******************************
  subroutine deltaFactor(deltaFunctionFactor)
!!!******************************
    implicit none
    INTEGER :: deltaFunctionFactor
    INTEGER :: caso
    CHARACTER(LEN=14) :: response_type
    OPEN(UNIT=2, FILE='response_type', FORM='FORMATTED',STATUS='OLD')
    read(2,*)caso
!!!
!!! for 1-omega response terms chose
!!! (default for most responses)
!!!
    deltaFunctionFactor=1
!!!
!!! or for particular cases chose the following
!!!
    SELECT CASE(caso)
    CASE(22)!shg 2-omega terms
       deltaFunctionFactor=2
    END SELECT
!!!    
    if(debug)    write(6,*)'inparams.f90: in deltaFactor',deltaFunctionFactor
!!!
!!!******************************
  end subroutine deltaFactor
!!!******************************


!!!****************
END MODULE inparams
!!!****************
