MODULE constants
  IMPLICIT NONE
  INTEGER, PARAMETER :: SP = KIND(1.0)            ! single precision
  INTEGER, PARAMETER :: DP = KIND(1.0D0)          ! double precision
  INTEGER, PARAMETER :: SPC = KIND((1.0,1.0))     ! single precision complex
  INTEGER, PARAMETER :: DPC = KIND((1.0D0,1.0D0)) ! double precision complex
  
  REAL(DP), PARAMETER :: pi = 3.1415926535897932384626433832795d0            ! pi
  
  LOGICAL, PARAMETER :: debug = .false.
  
!  REAL(DP), PARAMETER :: atePi3 = 8.d0*pi**3           ! eight times pi cubed
!  REAL(DP), PARAMETER :: hbar_cgs = 1.05457266d-27     ! hbar in cgs units
!  REAL(DP), PARAMETER :: echarge_cgs = 4.80653d-10     ! electron charge in cgs units
!  REAL(DP), PARAMETER :: emass_cgs = 9.1093897d-28     ! electron mass in cgs units
!  REAL(DP), PARAMETER :: abohr_cgs = 5.2917725d-9      ! bohr in cgs units
!  REAL(DP), PARAMETER :: eV_cgs = 1.6021733d-12        ! electron-volt in cgs units
!  REAL(DP), PARAMETER :: hbar_si = 1.05457d-34         ! hbar in SI units
!  REAL(DP), PARAMETER :: echarge_si = 1.6021733d-19    ! electron charge in SI units
!  REAL(DP), PARAMETER :: emass_si = 9.10939d-31        ! electron mass in SI units
!  REAL(DP), PARAMETER :: mom_fac_cgs = hbar_cgs/abohr_cgs
!  REAL(DP), PARAMETER :: infront1 = (echarge_cgs/emass_cgs*hbar_cgs)**2
!  REAL(DP), PARAMETER :: integrand_first = mom_fac_cgs**2/eV_cgs**3
END MODULE constants

