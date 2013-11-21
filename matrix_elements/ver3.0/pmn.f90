! Program pnm.f90 in Fortran 90
! To calculate momentum matrix elements 
! It uses the plane wave expansion for the wavefunctions
! It uses the  wavefunction output from the abinit code

PROGRAM matrixElements

  USE readAbinitWF, ONLY : readWFheader,wavefunctionFilename
  USE readAbinitWF, ONLY : xred, kpt, rprimd, natom, nkpt
  USE readAbinitWF, ONLY : nsppol, nspinor
  USE geometry,     ONLY : volume, areaofrectangle, r2k, rrtorc, krtokc

  IMPLICIT NONE

  INTEGER :: numberOfCommandLineArguments
  DOUBLE PRECISION , ALLOCATABLE :: zeta(:),delta(:),deltab(:),deltaf(:)
  DOUBLE PRECISION :: Lslab
  DOUBLE PRECISION :: lamb1,lamb2
  DOUBLE PRECISION :: x,y,z
  INTEGER :: Nval
  INTEGER :: Nlayers,u_log,unitS
  INTEGER :: i,j,k,l,m,n,p,q,ii,ij
  INTEGER :: iband,ipw,ik,ispinor,ikpt,jband,jspinor,isppol,ir
  INTEGER :: icomp,iversion
  INTEGER :: npw,nbandk
  INTEGER :: npwnspinor
  INTEGER :: nsymrel,nsym
  INTEGER :: unit1,unit2
  INTEGER :: u_mme,u_spin,u_vme
  INTEGER :: zmin,ifrho
  INTEGER :: zmax
  DOUBLE PRECISION , ALLOCATABLE :: zmesh(:)
  DOUBLE PRECISION :: pmn1,pmn2,Mp1,Mp2,Mp3,Mpmn
  DOUBLE PRECISION :: pcv,pvc,gapk,kpkg
  DOUBLE PRECISION :: modpsi2,ucvol,ucarea
  DOUBLE PRECISION :: pi,twopi

  DOUBLE PRECISION, PARAMETER :: HaeV=27.2113961d0

  INTEGER, ALLOCATABLE :: kg(:,:),symrel(:)

  DOUBLE PRECISION :: kp(3)

  DOUBLE PRECISION, ALLOCATABLE :: occ(:)
  DOUBLE PRECISION, ALLOCATABLE :: rp(:),rcart(:)
  DOUBLE PRECISION, ALLOCATABLE :: kgr(:)
  DOUBLE PRECISION, ALLOCATABLE :: kpcart(:,:)
  DOUBLE PRECISION, ALLOCATABLE :: kgcart(:)
  DOUBLE PRECISION, ALLOCATABLE :: kgc(:,:)
  DOUBLE PRECISION, ALLOCATABLE :: pmn(:,:),cgs(:,:)
  DOUBLE PRECISION, ALLOCATABLE :: ps(:,:,:)
  DOUBLE PRECISION, ALLOCATABLE :: mn(:),mns(:,:)
  DOUBLE PRECISION, ALLOCATABLE :: cg(:,:,:,:)
  DOUBLE PRECISION, ALLOCATABLE :: pmnk(:,:,:,:)
  DOUBLE PRECISION, ALLOCATABLE :: eigen(:),occk(:)
  DOUBLE COMPLEX,   ALLOCATABLE :: cpmn(:,:,:)
  DOUBLE COMPLEX,   ALLOCATABLE :: cSzmn(:,:)

  DOUBLE PRECISION :: b(3,3)
  DOUBLE PRECISION :: b1(3), b2(3), b3(3)

  DOUBLE PRECISION :: redX, redY, redZ

  DOUBLE PRECISION :: acell,bcell,ccell

  CHARACTER(LEN=20) :: buffer
  Logical :: mesa,layered
  INTEGER :: orho
  Logical :: oem,opmn,opmm,olpmn,olpmm,orhomm,osccp,olsccp
!!! constants
  pi = dacos(-1.d0)
  twopi= 2.0d0*pi
!!!  
  numberOfCommandLineArguments = IARGC()
  IF (numberOfCommandLineArguments==0) THEN
     WRITE(*,*)
     WRITE(*,*) "Usage: (no option shows this menu) "
     WRITE(*,*) "        rpmns.xeon/itanium/quad wavefunctionFile options"
     WRITE(*,*) " where options are either 'true' or 'false' for the following functions: " 
     write(*,*) "   rho      rho(z) for a set of k-points in ${red}case${NC}.klist_rho"
     write(*,*) "   em       Energies E_{m}(k)"
     write(*,*) "   pmn      Momentum Matrix Elements p_{mn}(k), includes m=n"
     write(*,*) "   rhomm    Layered ndot Matrix Elements rho_{mm}(l;k)"
     write(*,*) "   lpmn     Layered Momentum Matrix Elements calp_{mn}(k)"
     write(*,*) "   lpmm     Layered Diagonal Momentum Matrix Elements calp_{mm}(k)"
     write(*,*) "   sccp     Spin Matrix Elements S_{cc'}(k)"
     write(*,*) "   lsccp    Layered Spin Matrix Elements calS_{cc'}(k)"
     WRITE(*,*) 
     STOP
  END IF
!!! reads the wavefunction name
  CALL GETARG(1, wavefunctionFilename)
!!! reads the header of the wavefunction  
  CALL readWFheader
!!! reads the options  
  IF (numberOfCommandLineArguments == 9) THEN
     CALL GETARG(2,buffer)
     read(buffer,*)orho
     CALL GETARG(3,buffer)
     read(buffer,*)oem
     CALL GETARG(4,buffer)
     read(buffer,*)opmn
     CALL GETARG(5,buffer)
     read(buffer,*)orhomm
     CALL GETARG(6,buffer)
     read(buffer,*)olpmn
     CALL GETARG(7,buffer)
     read(buffer,*)olpmm
     CALL GETARG(8,buffer)
     read(buffer,*)osccp
     CALL GETARG(9,buffer)
     read(buffer,*)olsccp
     mesa=.FALSE.
     if(orho.eq.1)mesa=.TRUE.
     write(*,*)"options@pmn.f90: ","rho=",mesa,"em=",oem,"pmn=",opmn,"rhomm=",orhomm,"CalPmn=",olpmn,"CalPmm=",olpmm,"Sccp=",osccp,"CalSccp=",olsccp
  else
     write(*,*)"you must give the following 8 options"
     WRITE(*,*) 
     write(*,*) "   rho      rho(z) for a set of k-points in ${red}case${NC}.klist_rho"
     write(*,*) "   em       Energies E_{m}(k)"
     write(*,*) "   pmn      Momentum Matrix Elements p_{mn}(k), includes m=n"
     write(*,*) "   rhomm    Layered ndot Matrix Elements rho_{mm}(l;k)"
     write(*,*) "   lpmn     Layered Momentum Matrix Elements calp_{mn}(k)"
     write(*,*) "   lpmm     Layered Diagonal Momentum Matrix Elements calp_{mm}(k)"
     write(*,*) "   sccp     Spin Matrix Elements S_{cc'}(k)"
     write(*,*) "   lsccp    Layered Spin Matrix Elements calS_{cc'}(k)"
     WRITE(*,*) 
     STOP
  END IF
!!!
!!! works each option separately           
!!!
!!! make  layered option false
  layered = .FALSE.
  if(olpmn.or.olpmm.or.orhomm.or.olsccp)layered=.TRUE.
!!!Calculate  unit cell volume/area
  ucvol=volume(rprimd(1:3,1),rprimd(1:3,2),rprimd(1:3,3))
  ucarea=areaofrectangle(rprimd(1:3,1),rprimd(1:3,2))
!!! WRITE(*,*) "unit cell volume ", ucvol
!!! WRITE(*,*) "unit cell area", ucarea
!!! Calculate reciprocal vectors units

  CALL r2k(rprimd,b)

  b1(:)=b(1,:)
  b2(:)=b(2,:)
  b3(:)=b(3,:)

!!! READ FROM the wavefunction file END
!!! OPEN NEW FILES
  u_mme=29 !momentum matrix elements
  u_vme=11 !velocity matrix elements
  u_spin=28!spin matrix elements
!!!
  OPEN(UNIT=27, FILE="kcart.d")   ! k-points in cartesian coord.
  OPEN(UNIT=8, FILE="kpoints_reduced.d")   ! k-points in reduced coordinates 
  IF ( opmn ) THEN
     OPEN(UNIT=u_mme,FILE="pmnhalf.d") ! <m|p|n> half=only upper diagonal since its hermitian
     OPEN(UNIT=u_vme,FILE="pnn.d") ! <n|p|n> velocity matrix elements
  END IF

  IF (osccp) THEN
     IF(nspinor==2) THEN
        OPEN(UNIT=u_spin,FILE="spinmn.d") ! <n|spin|m>
     ELSE
        write(*,*)"This is not a spin-orbit calculation, nspinor= ",nspinor
     END IF
  END IF

!!! conversion of k points from reduced to cartesian coordiantes
  ALLOCATE(kpcart(3,nkpt))
!!!
  DO ik=1,nkpt
     CALL krtokc(kpt(1:3,ik),b1,b2,b3,kpcart(1:3,ik))
     WRITE(27,"(3f15.8)")(kpcart(i,ik),i=1,3)
     WRITE(8,"(3f15.8)")kpt(1:3,ik)
  END DO !ik

!!! Cycle over k points

  IF ( oem ) THEN
     OPEN(UNIT=10, FILE="eigen.d")
  END IF

!!! reads the number of valence bands for S_{cc'}
  OPEN(UNIT=2, FILE='.fnval', FORM='FORMATTED')
  read(2,*)Nval
  close(unit=2)
!!!

  ! WRITE(*,*) nsppol, nkpt
  DO isppol=1,nsppol
     DO ik=1,nkpt

!!! reads the G vectors
        READ(1)npw,nspinor,nbandk      ! for each k point
        ALLOCATE(kg(3,npw))
        READ(1)((kg(ii,ij),ii=1,3),ij=1,npw)

        ALLOCATE(eigen(nbandk),occk(nbandk))
        READ(1)(eigen(iband),iband=1,nbandk),& 
             &(occk(iband),iband=1,nbandk)
        IF ( oem ) THEN
           WRITE(10,"(1i4,500f18.12)")ik,eigen(1:nbandk)*HaeV
        END IF
        DEALLOCATE(eigen,occk)

!!! reads the wavefunction coefficients
        ALLOCATE(cgs(2,nbandk*npw*nspinor),cg(2,nbandk,nspinor,npw))
        DO iband=1,nbandk
           READ(1)((cgs(ii,ij),ii=1,2),&
                &ij=1+(iband-1)*npw*nspinor,iband*npw*nspinor)
!!! cgs(:,j)->cg(:,nbandk,nspinor,npw) 
           DO ispinor=1,nspinor
              DO ipw=1,npw
                 IF(ispinor==1) j=nspinor*(iband-1)*npw+ipw
                 IF(ispinor==2) j=nspinor*(iband-1)*npw+npw+ipw 
                 cg(:,iband,ispinor,ipw)= cgs(:,j)
              END DO !ipw
           END DO ! ispinor
        END DO ! iband
        DEALLOCATE(cgs)

!!! check Normalization of the wavefunction for given k
!!!        IF(nspinor==1) then
!!!           CALL normawfk(ik,npw,nbandk,nspinor,cg)
!!!        END IF
        kp(1:3)=kpt(1:3,ik)

        IF (osccp) THEN
!!! calculate  <m|S^i|n>  spin_matrix_elements
!!!       in case nspinor=2
           IF (nspinor==2) THEN
              write(*,*)"***"
              write(*,*)"S_{cc'} for k= ",ik," of ",nkpt	
              call  sccp(u_spin,ik,nbandk,nspinor,npw,cg,nkpt,nval)
           ELSE
              write(*,*)"nspinor= ",nspinor," => no-spin orbit, is it ok?"
           END IF
        END IF
!!! Calculate Pmn = <m|p^a|n>
!!! output file -> pmn.d or pmnhalf.d
        IF (opmn) THEN
           write(*,*)"***"
           Write(*,*)"standard P_{mn} for ik= ",ik," of ",nkpt	
           CALL mme(u_mme,u_vme,ik,nbandk,nspinor,npw,kp,kg,b1,b2,b3,cg,nkpt)
           write(*,*)"***"
        END IF
!!!     
        IF (layered) THEN
!!! reads layered info from file
           OPEN(unit=98,file="fort.98")
           OPEN(unit=99,file="fort.99")
           read(98,*)Lslab,Nlayers,zmin,ifrho
           if (Nlayers .NE. 0 ) then
              ALLOCATE(zeta(Nlayers),delta(Nlayers),deltab(Nlayers),deltaf(Nlayers))
              do i=1,Nlayers
                 read(99,*)zeta(i),deltaf(i),deltab(i)
              end do
              CLOSE(98)
              CLOSE(99)
!!!
              u_log = 38
              unitS = 40
!!!
!!!{\cal P}_{mn} via <m|(PS + SP)|n> = <m|PS|n> + <m|SP|n>
!!!        and integartion by parts of <m|PS|n>
!!!        => direct calculation, no need of extra sum over complete states
!!!
!!! no spin-orbit interaction
              if ( nspinor == 1 ) then
!!! CalPmn(k)
                 if (olpmn) then
                    write(*,*)"***"
                    write(*,*)'CalP_{mn}: spinor=',nspinor,' for ik= ',ik," of ",nkpt
                    write(*,*)"***"
                    call lpmn(unitS,u_log,ik,nbandk,nspinor,npw&
                         &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
                 end if
!!! CalPmm(k)
                 if (olpmm) then
                    write(*,*)"***"
                    write(*,*)'CalP_{mm}: spinor=',nspinor,' for ik= ',ik," of ",nkpt
                    write(*,*)"***"
                    unitS = 40+Nlayers*2
                    call lpmm(unitS,u_log,ik,nbandk,nspinor,npw&
                         &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
                 end if
!!! rhomm(l;k)
                 if (orhomm) then
                    write(*,*)"***"
                    write(*,*)'rho_{mm}: spinor=',nspinor,' for ik= ',ik," of ",nkpt
                    write(*,*)"***"
! If you may want to run several matrix elements
! which is not recommended, modify next line so
! the different matrix elements do not overlap
! in the fort.* files
!                    unitS = 40+Nlayers*2
                    call rhomm(unitS,u_log,ik,nbandk,nspinor,npw&
                         &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
                 end if
!!!
              end if !if ( nspinor == 1 ) 
!!! spin-orbit interaction
              if ( nspinor == 2 ) then
!!! CalPmn(k)
                 if (olpmn) then
                    write(*,*)"***"
                    write(*,*)'CalP_{mn}: spinor=',nspinor,' for ik= ',ik," of ",nkpt
                    write(*,*)"***"
                    call slpmn(unitS,u_log,ik,nbandk,nspinor,npw&
                         &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
                 end if
!!! CalPmm(k)
                 if (olpmm) then
                    write(*,*)"***"
                    write(*,*)'CalP_{mm}: spinor=',nspinor,' for ik= ',ik," of ",nkpt
                    write(*,*)"***"
                    unitS = 40+Nlayers*2
                    call slpmm(unitS,u_log,ik,nbandk,nspinor,npw&
                         &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
                 end if
!!! rhomm(k)
                 if (orhomm) then
                    write(*,*)"***"
                    write(*,*)'rho_{mm}: spinor=',nspinor,' for ik= ',ik," of ",nkpt
                    write(*,*)"***"
! If you may want to run several matrix elements
! which is not recommended, modify next line so
! the different matrix elements do not overlap
! in the fort.* files
!                    unitS = 40+Nlayers*2
                    call srhoccp(unitS,u_log,ik,nbandk,nspinor,npw&
                         &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt,nval)
                 end if
!!! CalScc'(k)
                 if (olsccp) then
                    unitS = 40+Nlayers*2
                    write(*,*)'CalS_{cc''}:',nspinor,' for ik= ',ik," of ",nkpt
                    call lsccp(unitS,u_log,ik,nbandk,nspinor,npw&
                         &,kg,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt,nval)
                 end if
              end if
           end if ! Nlayers .NE. 0
!!!
        end if !if(layered)
!!!              
!!!
!!!           
        IF (orho.eq.1) THEN
           OPEN(unit=91,file="zmesh.d")
           read(91,*)zmax
           allocate(zmesh(zmax))
           do i=1,zmax
              read(91,*)zmesh(i)
           end do
           CLOSE(91)
           u_log = 38
           unitS = 39
           CALL rhoz(unitS,u_log,ik,nbandk,nspinor,npw&
                &,kg,b1,b2,b3,cg,zmax,zmesh)
           deallocate(zmesh)
        END IF
!!!           
        DEALLOCATE(cg)
        DEALLOCATE(kg)

        IF (layered) DEALLOCATE(zeta,delta,deltab,deltaf)

!!!
!!! Finishes loops over k-points and spin-components
     END DO !ik=1,nkpt
  END DO  !isppol

  IF ( opmn ) THEN
     CLOSE(u_mme)
     CLOSE(u_vme)
  END IF

  call system("touch pmnTermino")

!!!#######################
END PROGRAM matrixElements
!!!#######################
