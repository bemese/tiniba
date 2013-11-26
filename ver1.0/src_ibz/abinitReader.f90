MODULE abinitReader
  ! 
  ! Code to read ABINIT WFK files
  ! 
  USE Global, ONLY : debug
  USE Symmetries, ONLY : nSym
  USE CommandLineArguments, ONLY : printSymmetries
  
  USE defs_basis, ONLY : dp, dpc
  USE defs_datatypes, ONLY : hdr_type
  
!!!!!!!!!!!!!!!!!!   F R O M   A B I N I T   C O D E  !!!!!!!!!!!!!!!!
!!$  type hdr_type
!!$  integer :: bantot        ! total number of bands (sum of nband on all kpts and spins)
!!$  integer :: date          ! starting date
!!$  integer :: headform      ! format of the header
!!$  integer :: intxc,ixc,natom,nkpt,npsp,nspden        ! input variables
!!$  integer :: nspinor,nsppol,nsym,ntypat,occopt       ! input variables
!!$  integer :: pertcase      ! the index of the perturbation, 0 if GS calculation
!!$  integer :: usepaw        ! input variable (0=norm-conserving psps, 1=paw)
!!$  integer :: ngfft(3)      ! input variable
!!$
!!$! This record is not a part of the hdr_type, although it is present in the
!!$! header of the files. This is because it depends on the kind of file
!!$! that is written, while all other information does not depend on it.
!!$! It was preferred to let it be initialized or defined outside of hdr_type.
!!$! integer :: fform         ! file descriptor (or file format)
!!$
!!$  integer, pointer :: istwfk(:)    ! input variable istwfk(nkpt)
!!$  integer, pointer :: lmn_size(:)  ! lmn_size(npsp) from psps
!!$  integer, pointer :: nband(:)     ! input variable nband(nkpt*nsppol)
!!$  integer, pointer :: npwarr(:)    ! npwarr(nkpt) array holding npw for each k point
!!$  integer, pointer :: pspcod(:)    ! pscod(npsp) from psps
!!$  integer, pointer :: pspdat(:)    ! psdat(npsp) from psps
!!$  integer, pointer :: pspso(:)     ! pspso(npsp) from psps
!!$  integer, pointer :: pspxc(:)     ! pspxc(npsp) from psps
!!$  integer, pointer :: so_typat(:)  ! input variable so_typat(ntypat)
!!$  integer, pointer :: symafm(:)    ! input variable symafm(nsym)
!!$  integer, pointer :: symrel(:,:,:)! input variable symrel(3,3,nsym)
!!$  integer, pointer :: typat(:)     ! input variable typat(natom)
!!$
!!$  real(dp) :: ecut                  ! input variable
!!$  real(dp) :: ecutdg                ! input variable (ecut for NC psps, pawecutdg for paw)
!!$  real(dp) :: ecutsm                ! input variable
!!$  real(dp) :: ecut_eff              ! ecut*dilatmx**2 (dilatmx is an input variable)
!!$  real(dp) :: etot,fermie,residm    ! EVOLVING variables
!!$  real(dp) :: qptn(3)               ! the wavevector, in case of a perturbation
!!$  real(dp) :: rprimd(3,3)           ! EVOLVING variables
!!$  real(dp) :: stmbias               ! input variable
!!$  real(dp) :: tphysel               ! input variable
!!$  real(dp) :: tsmear                ! input variable
!!$  real(dp), pointer :: kptns(:,:)   ! input variable kptns(3,nkpt)
!!$  real(dp), pointer :: occ(:)       ! EVOLVING variable occ(bantot)
!!$  real(dp), pointer :: tnons(:,:)   ! input variable tnons(3,nsym)
!!$  real(dp), pointer :: xred(:,:)    ! EVOLVING variable xred(3,natom)
!!$  real(dp), pointer :: zionpsp(:)   ! zionpsp(npsp) from psps
!!$  real(dp), pointer :: znuclpsp(:)  ! znuclpsp(npsp) from psps
!!$                                    ! Note the difference between znucl and znuclpsp !!
!!$  real(dp), pointer :: znucltypat(:)! znucltypat(ntypat) from alchemy
!!$
!!$  character(len=6) :: codvsn              ! version of the code
!!$  character(len=132), pointer :: title(:) ! title(npsp) from psps
!!$
!!$  type(hdr_rhoij_type), pointer :: atmrhoij(:) ! EVOLVING variable paw_ij(natom)%rhoij(lmn2_size,nspden), only for paw
!!$ end type hdr_type
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  IMPLICIT NONE
  
  REAL(dp) :: rprimd(3,3)
  INTEGER, ALLOCATABLE :: symrel(:,:,:), symrelbm(:,:,:)
  
  CHARACTER(LEN=132) :: title
  
  ! variables for the wavefunction coefficients
  ! INTEGER :: bantot, nspinor, nband
  INTEGER :: isppol, ikpt
  INTEGER :: npw
  INTEGER, ALLOCATABLE :: kg(:,:)
  INTEGER :: i_tmp, ii, iband, nbandwf
  INTEGER :: jband
  
CONTAINS
  
  SUBROUTINE abinitWFKReader
    IMPLICIT NONE
    
    INTEGER :: i, j, j_lo, j_hi
    CHARACTER(LEN=30) :: filename
    TYPE(hdr_type) :: hdr
    INTEGER :: fform0, rdwr
    
    CALL getarg(1,filename)
    IF ( debug ) WRITE(*,*) "Program Flow: Entered readAbinitFile"
    
    WRITE(*,*) "Enter ABINIT WFK filename to read."
    READ(*,*) filename
    WRITE(*,*) "Filename is ", TRIM(filename)
    
    ! filename = "GaAso_DS2_WFK"
    OPEN(UNIT=1, FILE=filename, FORM='unformatted', ACTION='read')
    
    rdwr = 1
    CALL hdr_io_int(fform0,hdr,rdwr,1)
    
!!! echo header
    rdwr = 4
    CALL hdr_io_int(fform0,hdr,rdwr,6)
    
    IF (debug) THEN
       
       WRITE(*,*) 'codevsn ', hdr%codvsn
       WRITE(*,*) 'headform ', hdr%headform
       WRITE(*,*) 'bantot ', hdr%bantot
       WRITE(*,*) 'date ', hdr%date
       WRITE(*,*) 'intxc ', hdr%intxc
       WRITE(*,*) 'ixc ', hdr%ixc
       WRITE(*,*) 'natom ', hdr%natom
       WRITE(*,*) 'ngfft(1:3) ', hdr%ngfft(1:3)
       WRITE(*,*) 'nkpt ', hdr%nkpt
       WRITE(*,*) 'nspden ', hdr%nspden
       WRITE(*,*) 'nspinor ', hdr%nspinor
       WRITE(*,*) 'nsppol', hdr%nsppol
       WRITE(*,*) 'nsym', hdr%nsym
       WRITE(*,*) 'npsp', hdr%npsp
       WRITE(*,*) 'ntypat', hdr%ntypat
       WRITE(*,*) 'occopt', hdr%occopt
       WRITE(*,*) 'ecut', hdr%ecut
       WRITE(*,*) 'ecutsm', hdr%ecutsm
       WRITE(*,*) 'ecut_eff', hdr%ecut_eff
       WRITE(*,'(A,3E20.13)') 'rprimd(1:3,1)', hdr%rprimd(1:3,1)
       WRITE(*,'(A,3E20.13)') 'rprimd(1:3,2)', hdr%rprimd(1:3,2)
       WRITE(*,'(A,3E20.13)') 'rprimd(1:3,3)', hdr%rprimd(1:3,3)
       WRITE(6,*) 'tphysel ', hdr%tphysel
       WRITE(6,*) 'tsmear ', hdr%tsmear
    END IF
    
    nSym = hdr%nsym
    ALLOCATE( symrel(3,3,nSym) )
    
    symrel = hdr%symrel
    rprimd = hdr%rprimd
    
    CLOSE(1)
    
  END SUBROUTINE abinitWFKReader
  
  SUBROUTINE convertAbinitDataToInternalData
    USE Global, ONLY : d1, d2, d3
    USE Symmetries, ONLY : G
    IMPLICIT NONE
    INTEGER :: iSym
    INTEGER :: istat
    DOUBLE PRECISION :: tempMat(3,3), tempMat2(3,3)
    DOUBLE PRECISION :: rbas(3,3), rbasTranspose(3,3), rbasTransposeInverse(3,3)
    DOUBLE PRECISION :: gbas(3,3)
    DOUBLE PRECISION :: a,b,c
!!! The symmetry Matrices
!!! SymMatsConv - The symmetry matrices in the conventional basis
!!! SymMatsCt - The symmetry matrices in the Cartesian basis
!!! SymMatsPl - The symmetry matrices in the direct lattice basis
!!! SymMatsRl - The symmetry matrices in the reciprocal lattice basis
    INTEGER, ALLOCATABLE :: symMatsConv(:,:,:)
    DOUBLE PRECISION, ALLOCATABLE :: symMatsCt(:,:,:)
    INTEGER, ALLOCATABLE :: symMatsPL(:,:,:)
    INTEGER, ALLOCATABLE :: symMatsRL(:,:,:)
    
    IF (debug) WRITE(*,*) "Program Flow: Entered convertAbinitDataToInternalData"
    
!!! Use rprimd(1:3,1:3)

!!! via WF    
!   d1(1:3) = rprimd(1:3,1)
!   d2(1:3) = rprimd(1:3,2)
!   d3(1:3) = rprimd(1:3,3)

!!! via BMS
     OPEN (UNIT=11,FILE="pvectors",STATUS="OLD",ACTION="READ",IOSTAT=istat)
     IF ( istat.NE.0 ) THEN
        WRITE(*,*) "Error: Cannot open file pvectors" 
        STOP "Error opening file pvectors"
     END IF
     READ(11,*) d1(1:3)
     READ(11,*) d2(1:3)
     READ(11,*) d3(1:3)
     READ(11,*) a,b,c
     d1 = a * d1
     d2 = b * d2
     d3 = c * d3
     rprimd(1:3,1) = d1(1:3)  
     rprimd(1:3,2) = d2(1:3) 
     rprimd(1:3,3) = d3(1:3)   
     
     
!!! BMS

!!! rbas is used for the transformations below.
    
    rbas = TRANSPOSE(rprimd)
    rbasTranspose = TRANSPOSE(rbas)
    CALL invert3by3 (rbas, gbas)
    rbasTransposeInverse = TRANSPOSE(gbas)
    
!!! via WF
!   DO iSym=1, nSym
!      symMatsPL(1:3,1:3,iSym) = symrel(1:3,1:3,iSym)
!      write(*,*)'SymmetryWF= ',iSym
!      write(*,*)symrel(1:3,1:3,iSym)
!   END DO
!!! via BMS
    OPEN (UNIT=10,FILE="sym.d",STATUS="OLD",ACTION="READ")
    READ(10,*)nSym
!!! Symmetry matrices
    ALLOCATE( symMatsCt(3,3,nSym) )
    ALLOCATE( symMatsPL(3,3,nSym) )
    ALLOCATE( symMatsRL(3,3,nSym) )
    ALLOCATE( G(nSym) )
    ALLOCATE( symrelbm(3,3,nSym) )
    DO iSym=1, nSym
       READ(10,*)symrelbm(1:3,1:3,iSym)
       symMatsPL(1:3,1:3,iSym) = symrelbm(1:3,1:3,iSym)
!      write(*,*)'SymmetryBM= ',iSym
!      write(*,*)symrelbm(1:3,1:3,iSym)
    END DO
    
!!!BMS
    DO iSym=1, nSym
       ! Symmetry matrices in Cartesian basis
       
       tempMat = REAL(symMatsPl(1:3,1:3,iSym))
       tempMat2 = MATMUL(tempMat,rbasTransposeInverse)
       symMatsCt(1:3,1:3,iSym) = MATMUL(rbasTranspose,tempMat2)
    END DO
    
    DO iSym=1, nSym
       G(iSym)%el(:,:) = symMatsRL(:,:,iSym)
    END DO
    
    DO iSym=1, nSym
       ! inverse Symmetry matrices in Reciprocal lattice basis
       tempMat(1:3,1:3) = symMatsPL(1:3,1:3,iSym)
       tempMat2 = TRANSPOSE(tempMat)
       symMatsRL(1:3,1:3,iSym) = tempMat2(1:3,1:3)
    END DO
    
!!! Write out the matrices
    IF (printSymmetries) THEN
       OPEN(UNIT=1,FILE="IBZsymmetries")
       WRITE(1,*) "The symmetry matrices in various coordinate systems"
       WRITE(1,*) "===========  Cartesian Basis ================="
       DO iSym=1,nSym
          WRITE(1,*) "Symmetry ", iSym
          WRITE(1,'(3F15.8)') symMatsCt(1,1:3,iSym)
          WRITE(1,'(3F15.8)') symMatsCt(2,1:3,iSym)
          WRITE(1,'(3F15.8)') symMatsCt(3,1:3,iSym)
       END DO
       WRITE(1,*) "===========  Direct Primitive Basis ================="
       DO iSym=1,nSym
          WRITE(1,*) "Symmetry ", iSym
          WRITE(1,*) symMatsPl(1,1:3,iSym)
          WRITE(1,*) symMatsPl(2,1:3,iSym)
          WRITE(1,*) symMatsPl(3,1:3,iSym)
       END DO
       WRITE(1,*) "===========  Reciprocal Primitive Basis ================="
       WRITE(1,*) "NOTE: These are the symmetry operations satisfied by"
       WRITE(1,*) "       the energy eigenvalues in the BZ "
       DO iSym=1,nSym
          WRITE(1,*) "Symmetry ", iSym
          WRITE(1,*) symMatsRl(1,1:3,iSym)
          WRITE(1,*) symMatsRl(2,1:3,iSym)
          WRITE(1,*) symMatsRl(3,1:3,iSym)
       END DO
       CLOSE(1)
    END IF
    
    IF (printSymmetries) THEN
       OPEN(UNIT=1, FILE="Symmetries.Cartesian")
       WRITE(1,*) nSym
       DO iSym=1,nSym
          WRITE(1,'(3F15.8)') symMatsCt(1,1:3,iSym)
          WRITE(1,'(3F15.8)') symMatsCt(2,1:3,iSym)
          WRITE(1,'(3F15.8)') symMatsCt(3,1:3,iSym)
       END DO
       CLOSE(1)
    END IF
    
    DO iSym=1, nSym
       G(iSym)%el(:,:) = symMatsRL(:,:,iSym)
    END DO
    
    DEALLOCATE (symMatsRL)
    DEALLOCATE (symMatsPL)
    DEALLOCATE (symMatsCt)
    
  END SUBROUTINE convertAbinitDataToInternalData
  
  
END MODULE abinitReader
