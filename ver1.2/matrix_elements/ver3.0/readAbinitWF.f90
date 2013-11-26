MODULE readAbinitWF
  IMPLICIT NONE
!!! This module contains subroutines to read the wavefunctions
!!! from different versions of the Abinit code.
  
  PRIVATE
  
  INTEGER, PARAMETER :: DP = KIND(1.0D0)          ! double precision
  
  
  CHARACTER(LEN=80), PUBLIC :: wavefunctionFilename
  REAL(DP), ALLOCATABLE, PUBLIC :: kpt(:,:)
  REAL(DP), ALLOCATABLE, PUBLIC :: xred(:,:)
  REAL(DP), PUBLIC :: rprimd(3,3)
  INTEGER, PUBLIC :: natom, nkpt, nsppol, nspinor
  
  
  CHARACTER*6 :: codvsn
  INTEGER :: headform,fform, stmbias
  INTEGER :: bantot,date,intxc,ixc,ngfft(3), &
       nspden,nsym,ntypat,occopt,pertcase,usepaw
  REAL(DP) :: acell(3),ecut,ecutdg,ecutsm,ecut_eff,qptn(3),&
       tphysel,tsmear
  
  INTEGER, ALLOCATABLE :: istwfk(:),nband(:),npwarr(:),so_typat(:),&
       & symafm(:), symrel(:,:,:), typat(:)
  REAL(DP), ALLOCATABLE :: occ(:),tnons(:,:),znucltypat(:)
  CHARACTER(LEN=132) :: title
  REAL(DP) :: znuclpsp,zionpsp
  INTEGER :: npsp, pspso, pspdat, pspcod, pspxc, lmax, lloc, mmax
  REAL(DP) :: residm, etotal, fermie
  INTEGER :: ipsp, iost
  
  PUBLIC :: readWFheader
  
CONTAINS
  
  SUBROUTINE readWFheader
    IMPLICIT NONE
    ! reads header information
    OPEN(UNIT=1, FILE=wavefunctionFilename, iostat=iost, STATUS="old", FORM="UNFORMATTED")
    IF (iost .NE. 0) THEN
       WRITE(*,*) " "
       WRITE(*,*) "Error opening wavefunction file."
       WRITE(*,*) "Iostat error =", iost
       WRITE(*,*) " "
       STOP
    END IF
    READ(1) codvsn,headform,fform
    
    WRITE(*,*) "Code version: ", codvsn
    
    SELECT CASE (codvsn)
    CASE("4.0.1", "4.0.2", "4.0.3") 
!       CALL readwfv40
    CASE("4.1.1", "4.1.2", "4.1.3")
!       CALL readwfv41
    CASE("4.4.1", "4.4.2", "4.4.3")
       CALL readwfv44
    CASE("4.6.5")
       CALL readwfv46
    CASE DEFAULT
       STOP "I do not know what to do with this version.  Sorry."
    END SELECT
    
  END SUBROUTINE readWFheader
  
  SUBROUTINE readwfv46
    IMPLICIT NONE
    
    READ(unit=1) bantot,date,intxc,ixc,natom,ngfft(1:3),&
         nkpt,nspden,nspinor,nsppol,nsym,npsp,ntypat,occopt,pertcase,usepaw,&
         ecut,ecutdg,ecutsm,ecut_eff,qptn(1:3),rprimd(1:3,1:3),stmbias,tphysel,tsmear
    
    ALLOCATE( istwfk(nkpt), nband(nkpt*nsppol), npwarr(nkpt) )
    ALLOCATE( so_typat(ntypat), symafm(nsym), symrel(3,3,nsym), typat(natom) )
    ALLOCATE( kpt(3,nkpt), occ(bantot), tnons(3,nsym), znucltypat(ntypat) )
    ALLOCATE( xred(3,natom) )
    
    READ(unit=1) istwfk(1:nkpt),nband(1:nkpt*nsppol),&
         npwarr(1:nkpt),so_typat(1:ntypat),symafm(1:nsym),symrel(1:3,1:3,1:nsym),typat(1:natom),&
         kpt(1:3,1:nkpt),occ(1:bantot),tnons(1:3,1:nsym),znucltypat(1:ntypat)
    DO ipsp=1,npsp
       ! (npsp lines, 1 for each pseudopotential ; npsp=ntypat, except if alchemical pseudo-atoms)
       READ(unit=1) title,znuclpsp,zionpsp,pspso,pspdat,pspcod,pspxc
    ENDDO
    !(final record: residm, coordinates, total energy, Fermi energy)
    READ(unit=1) residm,xred(1:3,1:natom),etotal,fermie
    
  END SUBROUTINE readwfv46

  SUBROUTINE readwfv44
    IMPLICIT NONE
    
    READ(unit=1) bantot,date,intxc,ixc,natom,ngfft(1:3),&
         nkpt,nspden,nspinor,nsppol,nsym,npsp,ntypat,occopt,pertcase,usepaw,&
         ecut,ecutdg,ecutsm,ecut_eff,qptn(1:3),rprimd(1:3,1:3),stmbias,tphysel,tsmear
    
    ALLOCATE( istwfk(nkpt), nband(nkpt*nsppol), npwarr(nkpt) )
    ALLOCATE( so_typat(ntypat), symafm(nsym), symrel(3,3,nsym), typat(natom) )
    ALLOCATE( kpt(3,nkpt), occ(bantot), tnons(3,nsym), znucltypat(ntypat) )
    ALLOCATE( xred(3,natom) )
    
    READ(unit=1) istwfk(1:nkpt),nband(1:nkpt*nsppol),&
         npwarr(1:nkpt),so_typat(1:ntypat),symafm(1:nsym),symrel(1:3,1:3,1:nsym),typat(1:natom),&
         kpt(1:3,1:nkpt),occ(1:bantot),tnons(1:3,1:nsym),znucltypat(1:ntypat)
    DO ipsp=1,npsp
       ! (npsp lines, 1 for each pseudopotential ; npsp=ntypat, except if alchemical pseudo-atoms)
       READ(unit=1) title,znuclpsp,zionpsp,pspso,pspdat,pspcod,pspxc
    ENDDO
    !(final record: residm, coordinates, total energy, Fermi energy)
    READ(unit=1) residm,xred(1:3,1:natom),etotal,fermie
    
  END SUBROUTINE readwfv44
  
  SUBROUTINE readwfv40
    
    IMPLICIT NONE
    INTEGER :: i,j,ii,ij,ik
    INTEGER :: nknsp, iatom
    INTEGER :: nxfh,ixfh
    
    OPEN(UNIT=26, FILE="ab.d")
    
    READ(1)bantot,date,intxc,ixc,natom,(ngfft(ii),ii=1,3),&
         &nkpt,nspden,nspinor,nsppol,nsym,npsp,ntypat,occopt,&
         &ecut,ecutsm,ecut_eff,&
         &((rprimd(ii,ij),ii=1,3),ij=1,3),tphysel,tsmear
    
    DO j=1,3
       WRITE(26,*)(rprimd(i,j),i=1,3) 
    END DO
    
    nknsp=nkpt*nsppol
    
    ALLOCATE(istwfk(nkpt),nband(nknsp),npwarr(nkpt),&
         &so_typat(ntypat),symafm(nsym),symrel(3,3,nsym),&
         &typat(natom),occ(bantot),&
         &tnons(3,nsym),znucltypat(ntypat))
    READ(1)(istwfk(ii),ii=1,nkpt),(nband(ii),ii=1,nknsp),&
         &(npwarr(ii),ii=1,nkpt),&
         &(so_typat(ii),ii=1,ntypat),&
         &(symafm(ii),ii=1,nsym),&
         &symrel(1:3,1:3,1:nsym),&
         &(typat(ii),ii=1,natom),&
         &((kpt(ii,ij),ii=1,3),ij=1,nkpt),&
         &occ(1:bantot),&
         &((tnons(ii,ij),ii=1,3),ij=1,nsym),&
         &znucltypat(1:ntypat)
    
    deallocate(istwfk,npwarr,nband,so_typat,symafm,symrel,&
         &typat,occ,tnons,znucltypat)
    
    do ipsp=1,npsp 
       read(1)title,znuclpsp,zionpsp,pspso,pspdat,pspcod,pspxc
    end do
    
    ALLOCATE( xred(3,natom) )
    
    read(1)residm,((xred(ii,ij),ii=1,3),ij=1,natom),&
         &etotal,fermie     
    
    CALL printOutVariables
  END SUBROUTINE readwfv40
!!!######################################################
  
  SUBROUTINE readwfv41
    IMPLICIT NONE
    
    INTEGER :: iatom,i,j,ii,ij,ik
    
    DOUBLE PRECISION, DIMENSION(3) :: qptn
    DOUBLE PRECISION, DIMENSION(3,3) :: rprimd
    INTEGER :: itype,isppol,ikpt,iband,ibantot,nbandk
    INTEGER :: nknsp
    INTEGER :: nxfh,ixfh
    
    DOUBLE PRECISION, DIMENSION(3,3) :: b
    
    !###  BEGIN
    
    OPEN(UNIT=1,FILE="out_WFK",iostat=iost,&
         &STATUS="old",FORM="UNFORMATTED")

    OPEN(UNIT=26, FILE="ab.d")
    
    READ(1)bantot,date,intxc,ixc,natom,(ngfft(ii),ii=1,3),&
         &nkpt,nspden,nspinor,nsppol,nsym,npsp,ntypat,occopt,&
         &pertcase,ecut,ecutsm,ecut_eff,qptn(1:3),&
         &((rprimd(ii,ij),ii=1,3),ij=1,3),tphysel,tsmear
    
    ALLOCATE( xred(3,natom) )
    
    DO j=1,3
       WRITE(26,*)(rprimd(i,j),i=1,3) 
    END DO
    
    nknsp=nkpt*nsppol
    
    ALLOCATE(istwfk(nkpt),nband(nknsp),npwarr(nkpt),&
         &so_typat(ntypat),symafm(nsym),symrel(1:3,1:3,1:nsym),&
         &typat(natom),occ(bantot),&
         &tnons(3,nsym),znucltypat(ntypat))
    
    READ(1)(istwfk(ii),ii=1,nkpt),(nband(ii),ii=1,nknsp),&
         &(npwarr(ii),ii=1,nkpt),&
         &(so_typat(ii),ii=1,ntypat),&
         &(symafm(ii),ii=1,nsym),&
         &symrel(1:3,1:3,1:nsym),&
         &(typat(ii),ii=1,natom),&
         &((kpt(ii,ij),ii=1,3),ij=1,nkpt),&
         &(occ(ii),ii=1,bantot),&
         &((tnons(ii,ij),ii=1,3),ij=1,nsym),&
         &(znucltypat(ii),ii=1,ntypat)
    
    DEALLOCATE(istwfk,npwarr,nband,so_typat,symafm,symrel,&
         &typat,occ,tnons,znucltypat)
    
    DO itype=1,npsp 
       READ(1)title,znuclpsp,zionpsp,pspso,pspdat,pspcod,pspxc
    END DO
    
    READ(1)residm,((xred(ii,ij),ii=1,3),ij=1,natom),etotal,fermie     
    
    CALL  printOutVariables
    
  END SUBROUTINE readwfv41

!!!#####################################################3  
  
  SUBROUTINE printOutVariables
    IMPLICIT NONE
    INTEGER :: i, ii
    OPEN(UNIT=9, FILE="parameters.d")
    write(9,*)'########################'
    write(9,*)'bantot    ',bantot
    write(9,*)'date   ',date
    write(9,*)'intxc  ',intxc
    write(9,*)'ixc    ',ixc
    write(9,*)'natom  ',natom
    write(9,*)'nkpt   ',nkpt
    write(9,*)'nspden   ',nspden
    write(9,*)'nspinor   ',nspinor
    write(9,*)'nsppol    ',nsppol
    write(9,*)'nsym   ',nsym
    write(9,*)'npsp   ',npsp
    write(9,*)'ntypat   ',ntypat
    write(9,*)'occopt   ',occopt
    write(9,*)'pertcase   ',pertcase
    write(9,*)'ecut   ',ecut
    write(9,*)'ecutsm   ',ecutsm
    write(9,*)'ecut_eff   ',ecut_eff
    write(9,*)'tphysel   ',tphysel
    write(9,*)'tsmear   ',tsmear
    write(9,*)' '
    write(9,*)'ngfft(1:3) '
    write(9,"(3I6)")ngfft(1:3)
    write(9,*)'qptn(1:3)'
    write(9,"(3F18.12)")qptn(1:3)
    write(9,*)' '
    write(9,*)'rprimd(1:3,1:3)'
    write(9,*)'i,rprimd(i,1),rprimd(i,2),rprimd(i,3),'
    do i=1,3
       write(9,"(I4,3F18.12)")i,rprimd(i,1:3)
    end do
    write(9,*)' '
    
    write(9,*)'typat(1:natom)'
    write(9,"(50I6)")typat(1:natom)
    write(9,*)' '
    write(9,*)'so_typat(1:ntypat)'
    write(9,"(50I6)")so_typat(1:ntypat)
    write(9,*)' '
    write(9,*)'znucltypat(1:ntypat)'
    write(9,"(50F8.2)")znucltypat(1:ntypat)
    write(9,*)' '
    write(9,*)'symrel(1:3,1:3,1:nsym)'
    write(9,*)symrel(1:3,1:3,1:nsym)
    write(9,*)' '
    write(9,*)'(symafm(nsym),ii=1,nsym)'
    write(9,*)(symafm(ii),ii=1,nsym)
    write(9,*)' '
    
    ! write(9,*)'for all bands'
    write(9,*)'nband(1:nknsp)'
    write(9,*)nband(:)
    
    write(9,*)'#### for Pseudopotential'
    !  write(9,*)'title,znuclpsp,zionpsp,pspso,pspdat,pspcod,pspxc',' ->&
    !  write(9,*)'for n npsp=ntypat(except if alchemical-pspatoms) lines'
    ! write(9,*)''
    
    do ipsp=1,npsp 
       write(9,*)'type',ipsp
       write(9,*)'  title   ',title
       write(9,*)'  znuclpsp   ',znuclpsp
       write(9,*)'  zionpsp   ',zionpsp
       write(9,*)'  pspso   ',pspso
       write(9,*)'  pspdat   ',pspdat
       write(9,*)'  pspcod   ',pspcod
       write(9,*)'  pspxc   ',pspxc
       write(9,*)' '
    end do
    
    write(9,*)'########################'
    write(9,*)'residm   ',residm
    write(9,*)'etotal   ',etotal
    write(9,*)'fermie   ',fermie
    write(9,*)' '
    write(9,*)'xred(1:3,1:natom)'
    write(9,*)'natom, x,y,z'
    do i=1,natom
       write(9,"(1I6,3F18.12)")natom,xred(1:3,i)
    end do
    write(9,*)' '
    
    CLOSE(9)    
    
  END SUBROUTINE printOutVariables
  
END MODULE readAbinitWF
