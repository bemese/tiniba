MODULE abinitReaderMod
  ! 
  ! Code to read ABINIT WFK files
  ! 
  USE DebugMod, ONLY : debug  
  USE defs_basis, ONLY : dp, dpc
  USE defs_datatypes, ONLY : hdr_type  
  USE CommandLineArgumentsMod, ONLY : filename
  USE CommandLineArgumentsMod, ONLY : ene, nor, mme, sme, den
  
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
!!$                                    ! Note the difference between znucl
!!$                                    ! and znuclpsp !!
!!$  real(dp), pointer :: znucltypat(:)! znucltypat(ntypat) from alchemy
!!$
!!$  character(len=6) :: codvsn              ! version of the code
!!$  character(len=132), pointer :: title(:) ! title(npsp) from psps
!!$
!!$  type(hdr_rhoij_type), pointer :: atmrhoij(:) ! EVOLVING variable
!!$                                        ! paw_ij(natom)%rhoij(lmn2_size,nspden),
!!$                                        ! only for paw
!!$ end type hdr_type
!!$!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  IMPLICIT NONE
  
  PUBLIC
  
  REAL(dp) :: rprimd(3,3)
  REAL(dp) :: vol
  REAL(dp) :: G1(3), G2(3), G3(3)
  
  INTEGER :: nsppol
  INTEGER :: nkpt
  INTEGER :: ikpt
  INTEGER :: nspinor
  INTEGER :: nSym
  
  INTEGER :: npw
  INTEGER :: nbandwf
  INTEGER :: nVal
  
  INTEGER, ALLOCATABLE :: kg(:,:)
  REAL(dp), ALLOCATABLE :: eigen(:,:), occwf(:,:), kpt(:,:)
  INTEGER, ALLOCATABLE :: symrel(:,:,:)
  
  REAL(dp), PRIVATE, ALLOCATABLE :: cgs(:,:)
  COMPLEX(dp), PRIVATE, ALLOCATABLE :: cg(:,:,:)
  REAL(dp), PRIVATE :: acell(3)

  CHARACTER(LEN=20) :: filename1
  
CONTAINS
  
  SUBROUTINE engine()
    INTEGER :: isppol
    IF (debug) WRITE(*,*) "Program Flow: Entered engine"
    CALL openWFKfile()
    CALL readWFKhead()
    CALL checkForConsistency()
    CALL calculateVolume()
    CALL calculateReciprocalLatticeVectors() ! must calculate volume first
    CALL openOutputFiles()
    
    DO isppol=1, nsppol
       DO ikpt=1, nkpt
          IF (debug) WRITE(*,*) "kpt ", ikpt
          
          CALL readWaveFunctionCoefficientsAtKpoint()
          
          IF (nor) THEN
             CALL calculateNormalizations()
          END IF
          
          IF (mme) THEN
             CALL calculateMomentumMatrixElements()
          END IF
          
          IF (sme) THEN
             CALL calculateSpinorMatrixElements()
          END IF
          
          IF (den) THEN
             CALL calculateDensityAtKpoint()
          END IF
          
          CALL deallocateWFCoeffArrays()
          
       END DO
    END DO
    CALL closeOutputFiles()
    CALL closeWFKfile()
    
  END SUBROUTINE engine
  
  SUBROUTINE openWFKfile
    IF (debug) WRITE(*,*) "Program Flow: Entered openWFKfile"
    OPEN(UNIT=1, FILE=filename, FORM='unformatted', ACTION='read')
  END SUBROUTINE openWFKfile
  
  SUBROUTINE closeWFKfile
    IF (debug) WRITE(*,*) "Program Flow: Entered closeWFKfile"
    CLOSE(1)
  END SUBROUTINE closeWFKfile
  
  SUBROUTINE setAcell
    WRITE(*,*) "Make sure acell is properly in the code"
    PAUSE
    !#########################################################
    !####### MAKE SURE THESE ARE RELEVANT TO YOUR CASE #######
    acell(1) = rprimd(1,1)+rprimd(1,2)+rprimd(1,3)
    acell(2) = rprimd(2,1)+rprimd(2,2)+rprimd(2,3)
    acell(3) = rprimd(3,1)+rprimd(3,2)+rprimd(3,3)
    !#########################################################    
  END SUBROUTINE setAcell
  
  SUBROUTINE allocateWFCoeffArrays
    IF (debug) WRITE(*,*) "Program Flow: Entered allocateWFCoeffArrays"
    ALLOCATE(cg(nbandwf,nspinor,npw))
  END SUBROUTINE allocateWFCoeffArrays
  
  SUBROUTINE deallocateWFCoeffArrays
    IF (debug) WRITE(*,*) "Program Flow: Entered deallocateWFCoeffArrays"
    DEALLOCATE(cg)
    DEALLOCATE(kg)
  END SUBROUTINE deallocateWFCoeffArrays
  
  SUBROUTINE readWFKhead
    IMPLICIT NONE
    
    INTEGER :: i, j, j_lo, j_hi
    TYPE(hdr_type) :: hdr
    INTEGER :: fform0, rdwr
    
    IF ( debug ) WRITE(*,*) "Program Flow: Entered readAbinitFile"
    
    rdwr = 1
    CALL hdr_io_int(fform0,hdr,rdwr,1)
    
    rdwr = 4
    CALL hdr_io_int(fform0,hdr,rdwr,6)
    
    IF (debug) THEN
       WRITE(*,*)
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
       WRITE(*,*) 'tphysel ', hdr%tphysel
       WRITE(*,*) 'tsmear ', hdr%tsmear
       WRITE(*,*) 'kpts', hdr%kptns(1:3,1:hdr%nkpt)
    END IF
    
    nkpt = hdr%nkpt
    ALLOCATE(kpt(3,nkpt))
    kpt(1:3,1:nkpt) = hdr%kptns(1:3,1:hdr%nkpt)
    
    nSym = hdr%nsym
    ALLOCATE( symrel(3,3,nSym) )
    symrel = hdr%symrel
    rprimd = hdr%rprimd
    nsppol = hdr%nsppol
    IF (2==nsppol) THEN
       WRITE(*,*)
       WRITE(*,*) "ERROR: Code not tested for nsppol=2"
       STOP " ERROR: Code not tested for nsppol=2"
    END IF
    nspinor = hdr%nspinor
    
  END SUBROUTINE readWFKhead
  
  SUBROUTINE checkForConsistency
    IF (sme .AND. (nspinor.NE.2)) THEN
       WRITE(*,*) "Error: user requested -sme but nspinor is not equal to 2."
       WRITE(*,*) "Stopping"
       STOP
    END IF
    
    IF (nsppol .EQ. 2) THEN
       WRITE(*,*) "Error: Code is not set to handle nsppol equal to 2."
       WRITE(*,*) "Stopping"
       STOP
    END IF
  END SUBROUTINE checkForConsistency
  
  SUBROUTINE calculateVolume
    IF (debug) WRITE(*,*) "Program Flow: Entered calculateVolume"
    vol =       rprimd(1,1)*(rprimd(2,2)*rprimd(3,3) - rprimd(3,2)*rprimd(2,3))
    vol = vol + rprimd(2,1)*(rprimd(3,2)*rprimd(1,3) - rprimd(1,2)*rprimd(3,3))
    vol = vol + rprimd(3,1)*(rprimd(1,2)*rprimd(2,3) - rprimd(2,2)*rprimd(1,3))
    
    WRITE(*,*) "Volume of unit cell = ", vol
    
  END SUBROUTINE calculateVolume
  
  SUBROUTINE calculateReciprocalLatticeVectors
    
    IMPLICIT NONE

    DOUBLE PRECISION, PARAMETER :: PI=3.14159265358979323846d0
    
    G1(1) = 2*pi/vol*(rprimd(2,2)*rprimd(3,3) - rprimd(3,2)*rprimd(2,3))
    G1(2) = 2*pi/vol*(rprimd(3,2)*rprimd(1,3) - rprimd(1,2)*rprimd(3,3))
    G1(3) = 2*pi/vol*(rprimd(1,2)*rprimd(2,3) - rprimd(2,2)*rprimd(1,3))
    
    G2(1) = 2*pi/vol*(rprimd(2,3)*rprimd(3,1) - rprimd(3,3)*rprimd(2,1))
    G2(2) = 2*pi/vol*(rprimd(3,3)*rprimd(1,1) - rprimd(1,3)*rprimd(3,1))
    G2(3) = 2*pi/vol*(rprimd(1,3)*rprimd(2,1) - rprimd(2,3)*rprimd(1,1))
    
    G3(1) = 2*pi/vol*(rprimd(2,1)*rprimd(3,2) - rprimd(3,1)*rprimd(2,2))
    G3(2) = 2*pi/vol*(rprimd(3,1)*rprimd(1,2) - rprimd(1,1)*rprimd(3,2))
    G3(3) = 2*pi/vol*(rprimd(1,1)*rprimd(2,2) - rprimd(2,1)*rprimd(1,2))
    
  END SUBROUTINE calculateReciprocalLatticeVectors
  
  SUBROUTINE readWaveFunctionCoefficientsAtKPoint
    IMPLICIT NONE
    INTEGER :: i, j, jj, j_lo, j_hi, ispinor, ipw
    INTEGER :: nspinor_this
    INTEGER :: iband
    INTEGER :: nVal_this, nbandwf_this
    IF (debug) WRITE(*,*) "Program Flow: Entered readWaveFunctionCoefficientsAtKPoint"
    READ(unit=1) npw,nspinor_this,nbandwf_this  !   <-- for each k point
    
    IF (ikpt.EQ.1) THEN
       nbandwf = nbandwf_this
    ELSE
       IF (nbandwf_this.NE.nbandwf) THEN
          WRITE(*,*) "Problem with counting the number of bands.  Exiting"
          STOP "Problem with the counting the number of bands"
       END IF
    END IF
    
    IF(debug) WRITE(6,*) 'npw', npw
    IF(debug) WRITE(6,*) 'nspinor for this kpoint', nspinor_this
    IF(debug) WRITE(6,*) 'nbandwf', nbandwf
    
    IF (nspinor .NE. nspinor_this) THEN
       WRITE(*,*) "nspinor      ", nspinor
       WRITE(*,*) "nspinor_this ", nspinor_this
       WRITE(*,*) "These should be the same.  Exiting"
       STOP "nspinor and nspsinor_this are different when they should be the same."
    END IF
    
    ALLOCATE(kg(3,npw))
    READ(UNIT=1) kg(1:3,1:npw)     !   <-- plane wave reduced coordinates
    !IF(debug) WRITE(6,*) kg(1:3,1:npw)
    
    ALLOCATE(eigen(nkpt,nbandwf))
    ALLOCATE(occwf(nkpt,nbandwf))
    
    READ(unit=1) eigen(ikpt,1:nbandwf), occwf(ikpt,1:nbandwf)
    
    nVal_this = INT(SUM(occwf(ikpt,1:nbandwf)))
    IF (ikpt.EQ.1) THEN
!       nVal = nVal_this
       IF(nspinor == 1)  nVal = nVal_this/2
       IF(nspinor == 2)  nVal = nVal_this
    ELSE
       IF(nspinor == 1)  nVal_this = nVal_this/2
       IF (nVal_this.NE.nVal) THEN
          WRITE(*,*) "Problem with counting the number of valence bands.  Exiting"
          STOP "Problem with the counting the number of valence bands"
       END IF
    END IF
    
    ! write out band info to energy file
    IF ((ikpt.EQ.1).AND.(ene)) THEN
       WRITE(UNIT=51,FMT=*) nVal
       WRITE(UNIT=51,FMT=*) nbandwf-nVal
    END IF
    
    ! write out the energies to a separate file
    IF (ene) THEN
       WRITE(UNIT=51,FMT=102) ikpt, 27.2113834d0*eigen(ikpt,1:nbandwf)
102    FORMAT(I6,500F21.16)
    END IF
    
    IF (nbandwf.GT.500) THEN
       WRITE(6,*) 'WF READER IS SET TO ONLY WRITE OUT 500 BANDS MAXIMUM'
       WRITE(6,*) '             GO CHANGE READER CODE'
       STOP
    END IF
    DEALLOCATE(eigen)
    DEALLOCATE(occwf)
    
    ALLOCATE(cgs(2,nbandwf*npw*nspinor))
    CALL allocateWFCoeffArrays()
    
    DO iband=1,nbandwf
       j_lo = 1+(iband-1)*npw*nspinor
       j_hi = iband*npw*nspinor
       READ(unit=1) ((cgs(i,j),i=1,2),j=j_lo,j_hi)
       !IF(debug) WRITE(6,*) ((cgs(i,j),i=1,2),j=j_lo,j_hi)
    END DO
    
    IF (nspinor==2) THEN
       DO iband=1,nbandwf
          DO ispinor=1,nspinor
             DO ipw=1,npw
                IF (ispinor==1) j=nspinor*(iband-1)*npw+ipw
                IF (ispinor==2) j=nspinor*(iband-1)*npw+npw+ipw 
                cg(iband,ispinor,ipw) = cgs(1,j) + (0.d0,1.d0)*cgs(2,j)
                !IF(debug) WRITE(6,*) cg(iband,ispinor,ipw)
             END DO ! ipw
          END DO ! ispinor
       END DO ! iband
    ELSE IF (nspinor==1) THEN
       DO iband=1,nbandwf
          DO ispinor=1,nspinor
             DO ipw=1,npw
                j = (iband-1)*npw+ipw
                cg(iband,ispinor,ipw) = cgs(1,j) + (0.d0,1.d0)*cgs(2,j)
                !IF(debug) WRITE(6,*) cg(iband,ispinor,ipw)
             END DO ! ipw
          END DO ! ispinor
       END DO ! iband 
    END IF
    
    DEALLOCATE(cgs)
    
  END SUBROUTINE readWaveFunctionCoefficientsAtKPoint
  
  SUBROUTINE openOutputFiles
    IF (ene) THEN
       OPEN(UNIT=51,FILE='energy.d')
       WRITE(51,*) nkpt
    END IF
    
    IF (nor) THEN
       OPEN(UNIT=55,FILE='normalizations.d')
    END IF
    
    IF (mme) THEN
       OPEN(UNIT=50,FILE='pmn.d')
    END IF
    
    IF (sme) THEN
       OPEN(UNIT=61,FILE='smn.d')
    END IF
  END SUBROUTINE openOutputFiles
  
  
  SUBROUTINE calculateNormalizations
    COMPLEX(dp) :: norm, upup, dndn
    INTEGER :: iband, jband, ii
    IF (debug) WRITE(*,*) "Calculating normalizations for this k-point."
    DO iband = 1, nbandwf
       DO jband = 1, nbandwf
          
          norm = 0.d0
          
          DO ii = 1, npw
             IF (nspinor==1) THEN
                
                norm = norm + REAL(CONJG(cg(iband,1,ii))*cg(jband,1,ii))
                
             ELSE IF (nspinor==2) THEN
                
                upup = CONJG(cg(iband,1,ii))*cg(jband,1,ii)
                dndn = CONJG(cg(iband,2,ii))*cg(jband,2,ii)
                norm = norm + REAL(upup + dndn)
                
             ELSE
                STOP 'nspinor is incorrect'
             END IF
          END DO
          
          IF (iband==jband) THEN
             IF (ABS(norm-1.d0) .GT. 1d-12) THEN
                
                WRITE(*,*) "Error: normalization incorrect"
                WRITE(*,*) "Error at band ", iband
                WRITE(*,*) "Stopping"
                STOP
                
             END IF
          ELSE
             IF (ABS(norm) .GT. 1.d-12) THEN

                WRITE(*,*) "Error: normalization incorrect"
                WRITE(*,*) "Error at bands ", iband, jband
                WRITE(*,*) "Stopping"
                STOP
             END IF
          END IF
          
          WRITE(UNIT=55,FMT=104) norm
104       FORMAT(E16.8)
          
       END DO
    END DO
  END SUBROUTINE calculateNormalizations
  
  
  SUBROUTINE calculateMomentumMatrixElements
    COMPLEX(dp) :: pmnx_red, pmny_red, pmnz_red, pmnx, pmny, pmnz
    COMPLEX(dp) :: upup, dndn, updn, dnup
    INTEGER :: iband, jband, ii
    IF (debug) WRITE(*,*) "Program Flow: Entered calculateMomentumMatrixElements"
    DO iband = 1, nbandwf
       DO jband = 1, nbandwf
          pmnx_red = (0.d0,0.d0)
          pmny_red = (0.d0,0.d0)
          pmnz_red = (0.d0,0.d0)
          IF (iband == jband) THEN
             pmnx_red = kpt(1,ikpt) 
             pmny_red = kpt(2,ikpt)
             pmnz_red = kpt(3,ikpt)
          END IF
          DO ii = 1, npw
             IF (nspinor==1) THEN
                
                upup = CONJG(cg(iband,1,ii))*cg(jband,1,ii)
                pmnx_red = pmnx_red + REAL(kg(1,ii))*upup
                pmny_red = pmny_red + REAL(kg(2,ii))*upup
                pmnz_red = pmnz_red + REAL(kg(3,ii))*upup
                
             ELSE IF (nspinor==2) THEN
                
                upup = CONJG(cg(iband,1,ii))*cg(jband,1,ii)
                dndn = CONJG(cg(iband,2,ii))*cg(jband,2,ii)
                updn = CONJG(cg(iband,1,ii))*cg(jband,2,ii)
                dnup = CONJG(cg(iband,2,ii))*cg(jband,1,ii)
                
                pmnx_red = pmnx_red + REAL(kg(1,ii))*(upup + dndn)
                pmny_red = pmny_red + REAL(kg(2,ii))*(upup + dndn)
                pmnz_red = pmnz_red + REAL(kg(3,ii))*(upup + dndn)
                
             ELSE
                STOP 'nspinor is incorrect'
             END IF
          END DO
          
          ! convert to Cartesian coordinates
          pmnx = pmnx_red*G1(1) + pmny_red*G2(1) + pmnz_red*G3(1)
          pmny = pmnx_red*G1(2) + pmny_red*G2(2) + pmnz_red*G3(2)
          pmnz = pmnx_red*G1(3) + pmny_red*G2(3) + pmnz_red*G3(3)
          
          IF (jband.GE.iband) THEN
             WRITE(UNIT=50,FMT=103) pmnx, pmny, pmnz
103          FORMAT(6E16.8)
          END IF
          
       END DO
    END DO
  END SUBROUTINE calculateMomentumMatrixElements
  
  SUBROUTINE calculateSpinorMatrixElements
    COMPLEX(dp) :: smnx, smny, smnz, upup, dndn, updn, dnup
    COMPLEX(dp) :: spinmod
    INTEGER :: iband, jband, ii
    IF (debug) WRITE(*,*) "Calculating spin matrix elements for this k-point."
    DO iband = 1, nbandwf
       DO jband = 1, nbandwf
          smnx = (0.d0,0.d0)
          smny = (0.d0,0.d0)
          smnz = (0.d0,0.d0)
          DO ii = 1, npw
             IF (nspinor==1) THEN
                !  STOP 'Have not coded nspinor=1 yet'
                STOP 'SHOULD NOT BE HERE WITH NSPINOR=2'
             ELSE IF (nspinor==2) THEN
                upup = CONJG(cg(iband,1,ii))*cg(jband,1,ii)
                dndn = CONJG(cg(iband,2,ii))*cg(jband,2,ii)
                updn = CONJG(cg(iband,1,ii))*cg(jband,2,ii)
                dnup = CONJG(cg(iband,2,ii))*cg(jband,1,ii)
                
                smnx = smnx + (updn + dnup)
                smny = smny + (0.d0,1.d0)*(-updn + dnup)
                smnz = smnz + (upup - dndn)
                
             ELSE
                STOP 'nspinor is incorrect'
             END IF
          END DO
          
          IF (jband.GE.iband) THEN
             WRITE(UNIT=61,FMT=105) smnx, smny, smnz
105          FORMAT(6E16.8)
             
             !spinmod = (0.d0,0.d0)
             !spinmod = spinmod + CONJG(smnx)*smnx +  CONJG(smny)*smny + CONJG(smnz)*smnz
             !WRITE(*,*) "FN ", iband, jband, spinmod
             !IF ((REAL(spinmod)).GE.(1.0d0+1d-6)) THEN
             !   PAUSE
             !END IF
          END IF
          
       END DO
    END DO
  END SUBROUTINE calculateSpinorMatrixElements
  
  SUBROUTINE calculateDensityAtKpoint
    !!
    !!  Needs to be generalized beyond cubic crystals
    !!
    INTEGER :: Ndiv
    INTEGER :: aaa, bbb, ccc
    INTEGER :: ii, iband
    REAL(dp), ALLOCATABLE :: density(:,:,:,:)
    REAL(dp) :: rX, rY, rZ
    REAL(dp) :: GdotR
    REAL(dp) :: divlength
    COMPLEX(dp) :: uwavefunction, uwavefunctionUp, uwavefunctionDown
    
    CALL setAcell()
    
    Ndiv = 51
    IF (ikpt == 1) THEN
       ALLOCATE(density(Ndiv,Ndiv,Ndiv,nbandwf))
       
       divLength = 1.d0/REAL(Ndiv-1)
       
       DO aaa=1,Ndiv
          WRITE(*,*) aaa-1
          rX = REAL(aaa-1)*divLength*acell(1)
          DO bbb=1,Ndiv
             rY = REAL(bbb-1)*divLength*acell(2)
             DO ccc=1,Ndiv
                rZ = REAL(ccc-1)*divLength*acell(3)
                
                DO iband = 1, nbandwf
                   
                   uwavefunction = (0.d0, 0.d0)
                   uwavefunctionUp = (0.d0, 0.d0)
                   uwavefunctionDown = (0.d0, 0.d0)
                   GdotR = 0.d0
                   
                   DO ii = 1, npw
                      
                      GdotR = 0.d0
                      GdotR = GdotR + &
                           rX *( G1(1)*kg(1,ii) + G2(1)*kg(2,ii) + G3(1)*kg(3,ii) )
                      GdotR = GdotR + &
                           rY *( G1(2)*kg(1,ii) + G2(2)*kg(2,ii) + G3(2)*kg(3,ii) )
                      GdotR = GdotR + &
                           rZ *( G1(3)*kg(1,ii) + G2(3)*kg(2,ii) + G3(3)*kg(3,ii) )
                      
                      IF (nspinor==1) THEN
                         
                         uwavefunctionUp = uwavefunctionUp + cg(iband,1,ii)*EXP((0.d0,1.d0)*GdotR)
                         uwavefunctionDown = (0.d0, 0.d0)
                         
                      ELSE IF (nspinor==2) THEN
                         
                         uwavefunctionUp = uwavefunctionUp + cg(iband,1,ii)*EXP((0.d0,1.d0)*GdotR)
                         uwavefunctionDown = uwavefunctionDown + cg(iband,2,ii)*EXP((0.d0,1.d0)*GdotR)
                         
                      ELSE
                         STOP 'nspinor is incorrect'
                      END IF
                   END DO
                   
                   density(aaa,bbb,ccc,iband) = CONJG(uwavefunctionUp)*uwavefunctionUp + &
                        CONJG(uwavefunctionDown)*uwavefunctionDown
                   
                END DO
             END DO
          END DO
       END DO
       
       ! Output some typical band
       filename1="density8.d"
!       CALL writeDXcubes(Ndiv,density(1:Ndiv,1:Ndiv,1:Ndiv,8), "density8.dx")
       CALL writeDXcubes(Ndiv,density(1:Ndiv,1:Ndiv,1:Ndiv,8),filename1)
       
       DEALLOCATE(density)
    END IF
    
  END SUBROUTINE calculateDensityAtKpoint
  
  SUBROUTINE writeDXcubes(Ndiv,inDensity, filename)
    INTEGER, INTENT(IN) :: Ndiv
    REAL(DP), INTENT(IN) :: inDensity(Ndiv,Ndiv,Ndiv)
    CHARACTER(LEN=20), INTENT(IN) :: filename
    REAL(DP) :: divLength
    INTEGER :: aaa, bbb, ccc, counter
    INTEGER :: cubeCount, corn(8)
    REAL(DP) :: rX, rY, rZ
    INTEGER, ALLOCATABLE :: weights(:)
    
    
    OPEN(UNIT=101,FILE=filename)
    WRITE(101,'(A,I6,A)') "object  1 class array type float rank 1 shape 3 items ", &
         Ndiv**3, " data follows"
    
    divLength = 1.d0/REAL(Ndiv-1)
    
    ALLOCATE (weights(Ndiv**3))
    weights(1:Ndiv**3) = 0
    
    DO aaa=1,Ndiv
       rX = (aaa-1)*divLength*acell(1)
       DO bbb=1,Ndiv
          rY = (bbb-1)*divLength*acell(2)
          DO ccc=1,Ndiv
             rZ = (ccc-1)*divLength*acell(3)
             WRITE(UNIT=101,FMT='(3F12.6)') rX, rY, rZ
          END DO
       END DO
    END DO
    
    WRITE(101,'(A,I8,A)') "object 2 class array type int rank 1 shape 8 items", &
         (Ndiv-1)**3, " data follows"
    
    cubeCount = 0
    counter = 0
    DO aaa=0,Ndiv-2
       DO bbb=0,Ndiv-2
          DO ccc=0,Ndiv-2
             
             counter =  ccc + (bbb * Ndiv) + (aaa * Ndiv * Ndiv) + 1
             corn(1) = counter
             corn(2) = counter + 1
             corn(3) = counter + Ndiv
             corn(4) = counter + Ndiv + 1
             corn(5) = counter + Ndiv*Ndiv
             corn(6) = counter + Ndiv*Ndiv + 1
             corn(7) = counter + Ndiv*Ndiv + Ndiv
             corn(8) = counter + Ndiv*Ndiv + Ndiv + 1
             
             WRITE(UNIT=101,FMT='(8I8)') corn(1:8)-1
             
             weights(corn(1:8)) = weights(corn(1:8)) + 1
             
          END DO
       END DO
    END DO
    
    WRITE(99,*) weights(1:Ndiv**3)
    
    WRITE(101,*) 'attribute "element type" string "cubes"'
    WRITE(101,*) 'attribute "ref" string "positions"'
    
    WRITE(101,'(A,I6,A)') "object 3 class array type float rank 0 items ", &
         Ndiv**3, " data follows"
    
    DO aaa=1,Ndiv
       DO bbb=1,Ndiv
          DO ccc=1,Ndiv
             WRITE(UNIT=101,FMT='(E12.6)') inDensity(aaa,bbb,ccc)
          END DO
       END DO
    END DO
    
    WRITE(101,*) 'attribute "dep" string "positions"'
    WRITE(101,*) 'object "density data on positions" class field'
    
    WRITE(101,*) 'component "positions" value 1'
    WRITE(101,*) 'component "connections" value 2'
    WRITE(101,*) 'component "data" value 3'
    
    WRITE(101,*) "end"
    
    CLOSE(101)
  END SUBROUTINE writeDXcubes
  
  SUBROUTINE closeOutputFiles
    IF (ene) THEN
       CLOSE(51)
    END IF
    
    IF (nor) THEN
       CLOSE(55)
    END IF
    
    IF (mme) THEN
       CLOSE(50)
    END IF
    
    IF (sme) THEN
       CLOSE(61)
    END IF
  END SUBROUTINE closeOutputFiles
  
END MODULE abinitReaderMod
