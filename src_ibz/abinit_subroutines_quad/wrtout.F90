!{\src2tex{textfont=tt}}
!!****f* ABINIT/wrtout
!! NAME
!! wrtout
!!
!! FUNCTION
!! Organizes the sequential or parallel version of the write intrinsic
!! Also allows to treat correctly the write operations for
!! Unix (+DOS) and MacOS.
!!
!! COPYRIGHT
!! Copyright (C) 1998-2004 ABINIT group (DCA, XG, GMR)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~ABINIT/Infos/copyright
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~ABINIT/Infos/contributors .
!!
!! INPUTS
!!  unit=unit number for writing
!!  mode_paral='COLL' if all procs are calling the routine with the same message
!!           to be written once only
!!          or 'PERS' if the procs are calling the routine with different mesgs
!!           each to be written, or if one proc is calling the routine
!!
!! OUTPUT
!!  (only writing)
!!
!! SIDE EFFECTS
!!  message=(character(len=500)) message to be written
!!
!! PARENTS
!!      WffOpen,abinit,acfd_dyson,acfd_intexact,anaddb,append_cml,append_cml2
!!      asria9,asrif9,berryphase,berryphase_new,besjm,bigbx9,bldgrp,blok8,bonds
!!      bound,brdmin,canat9,ccfft,cgwf,cgwf3,chkdilatmx,chkdpr,chkexi,chkgrp
!!      chki8,chkilwf,chkin9,chkinp,chkint,chkneu,chknm8,chkorthsy,chkph3
!!      chkprimit,chkr8,chkrp9,chneu9,clnup1,clnup2,cmpar8,constrf
!!      contract_dp_ge_val,contract_int_ge_val,contract_int_le_val
!!      contract_int_list,cvxclda,d3output,ddkten,delocint,der_int,diel9
!!      dielmore9,dielmt,dieltcel,distrb2,dos_hdr_write,dotprod_vn,dotprodm_vn
!!      driver,drivexc,dsksta,dyfnl3,dyson_sc,eig1fixed,elast9,electrooptic
!!      eltfrxc3,energy,ewald,ewald2,ewald3,ewald4,ewald9,fconv,fftpac,fftw
!!      filterpot,findmin,fixsym,fourdp,fourwf,ftgam,ftgkk,ftiaf9,fxphas
!!      gensymshub,gensymshub4,gensymspgr,get_full_kgrid,get_g_tiny,get_tetra
!!      getattribute,getcut,getdim_nloc,getfreqsus,getghc,getgsc,getkgrid
!!      getlambda,getmpw,getnel,getng,getsc,getshell,getspinrot,gstate,gtblk9
!!      gtdyn9,handle_err_netcdf,hartre,hartre1,hartrestr,hdr_check,hdr_init
!!      hdr_io,hdr_io_netcdf,hermit,importcml,inarray,incomprs,ingeo,ingeobld
!!      init8,initang,initberry,initmpi_fft,initmv,initro,initwf,inkpts,inprep8
!!      instr9,instrng,insy3,int2char,int2char4,intagm,inupper,invars0,invars1
!!      invars1m,invars2,invars9,invcb,inwffil,inwffil3,ioarr,ioddb8,iofn1
!!      iofn2,irrzg,isfile,klocal,kpgio,kpgsph,kpgstr,kxc_alda,kxc_eok
!!      leave_new,leave_test,linemin,listkk,loop3dte,loper3,lwf,matcginv
!!      mati3inv,matrginv,matrixelmt_g,mean_fftr,meanvalue_g,memana,memkss
!!      memorf,memory,metcon,metric,metstr,mkcor3,mkcore,mkdenpos,mkeuler
!!      mkffnl,mkfilename,mkifc9,mklocl,mkrho,mkrho3,mkvxc3,moddiel,moldyn,move
!!      mrgddb,mv_3dte,nanal9,newfermie1,newkpt,newocc,newsp,newvtr,newvtr3
!!      nhatgrid,nonlinear,nonlop,nonlop_ylm,nselt3,nstdy3,nstwf3,occeig,operat
!!      opernl2,opernl3,opernl4b,opernlb_ylm,out1dm,outkss,outqmc,outvars,outwf
!!      overlap_g,pareigocc,pawdens,pawdij,pawenergy,pawmix,pawovlp,pawprt
!!      pawrhoij1,pawrhoij2,pawsphpot,pawxc,pawxcden,pawxcdenm,pawxcm,ph1d3d
!!      phfrq3,piezo9,poisson,polcart,prcref,precon,print_ij,projbd,prt_cml
!!      prt_cml2,prteigrs,prtene,prtene3,prtocc,prtph3,prtrhomxmn,prtspgroup
!!      prttagm,prtxf,prtxvf,psddb8,psp1cc,psp1in,psp1nl,psp2in,psp3in,psp3nl
!!      psp4cc,psp5in,psp5nl,psp6in,psp7in,psp7nl,psp8cc,psp8in,pspatm,pspini
!!      pstate,q0dy3,ramansus,randac,rdddb9,rdnpw,readeig,relaxpol,respfn
!!      rhofermi3,rhohxc,rhohxc_coll,rsiaf9,rwwan,rwwf,scalewf_nonlop,scfcge
!!      scfcv,scfcv3,scfeig,scfopt,scprqt,setmesh,setshells,setup1,setup2
!!      sg_ctrig,sg_fft,sg_fftpad,sg_fftpx,sg_fftrisc,sg_fftx,sg_ffty,sg_fftz
!!      sg_fourwf,smallprim,smatrix,smpbz,sphere,sphereboundary,sphericaldens
!!      status,stress,strnps,suscep,suscep_dyn,suscep_stat,suskmm,suskmm_dyn
!!      sym_gkk,symanal,symatm,symbrav,symdet,symdm9,symfind,symg,symkchk
!!      symkpt,symmultsg,symph3,symq3,symrelrot,symrhg,symspgr,tddft,testkgrid
!!      tetrahedron,thm9,timab,timana,timein,transgrid,uderiv,vlocalstr,vtorho
!!      vtorho3,vtorhotf,vtowfk,vtowfk3,wfconv,wffile,wfkfermi3,wfsinp,wght9
!!      wrtloctens,xcacfd,xcden,xchcth,xchelu,xcpot,xcpzca,xcspol,xctetr,xcwign
!!      xcxalp,xfpack,xredxcart
!!
!! CHILDREN
!!      mpi_comm_rank,timab
!!
!! SOURCE

 subroutine wrtout(unit,message,mode_paral)

 use defs_basis

 implicit none

#          if defined MPI
           include 'mpif.h'
#          endif
!Arguments ------------------------------------
 integer,intent(in) :: unit
 character(len=4),intent(in) :: mode_paral
 character(len=500),intent(inout) :: message

!Local variables-------------------------------
 integer,save :: iexit=0,master,ncomment=0,nwarning=0
 integer :: rtnpos
 real(dp) :: tsec(2)
 character(len=7) :: tag
 character(len=12) :: form,strfmt
 character(len=500) :: messtmp,string
 logical :: test_mpi
!no_abirules
#          if defined MPI
          !Variables introduced for MPI version
           integer :: ierr,me
#          endif

!******************************************************************
!BEGIN EXECUTABLE SECTION

 call timab(49,1,tsec)

!Be careful with the coding  of the parallel case ...
 test_mpi=.false.
#          if defined MPI
           test_mpi=.true.
          !Determine who I am
           call timab(48,1,tsec)
           call MPI_COMM_RANK(MPI_COMM_WORLD,me,ierr)
           call timab(48,2,tsec)
           if(mode_paral=='COLL') then
            if(me==master) then
#          endif

 if(message/=' ') then
  messtmp=message
! Here, split the message, according to the char(10)
! characters (carriage return). This technique is
! portable accross different OS.
  rtnpos=index(messtmp,ch10)
  do while(rtnpos/=0)
   string=messtmp(1:rtnpos-1)
   write(unit, '(a)' ) trim(string)
   messtmp=messtmp(rtnpos+1:len(message))
   rtnpos=index(messtmp,ch10)
  end do
  write(unit, '(a)' ) trim(messtmp)
 else
  write(unit,*)
 end if

 if( index(trim(message),'BUG') /= 0 )then
  write(unit, '(a)' ) '  Action : contact ABINIT group.'
  write(unit,*)
 end if

 if( index(trim(message),'BUG') /= 0   .or. &
&    index(trim(message),'Calculation completed') /= 0 )then
  if(nwarning<10000 .and. ncomment<1000)then
   write(unit, '(a,i5,a,i4,a)' ) &
&    ' Delivered',nwarning,' WARNINGs and',ncomment,' COMMENTs to log file.'
  else
   write(unit, '(a,i6,a,i6,a)' ) &
&    ' Delivered',nwarning,' WARNINGs and',ncomment,' COMMENTs to log file.'
  end if
  if(iexit/=0)then
   write(unit, '(a)' ) ' Note : exit requested by the user.'
  end if
 end if

 if( index(trim(message),'Exit') /= 0 )then
  iexit=1
 end if

!Count the number of warnings and comments. Only take into
!account unit 6, in order not to duplicate these numbers.
 if( index(trim(message),'WARNING') /= 0 .and. unit==6 )then
   nwarning=nwarning+1
 end if
 if( index(trim(message),'COMMENT') /= 0 .and. unit==6 )then
   ncomment=ncomment+1
 end if

#          if defined MPI
             end if
           elseif(mode_paral=='PERS') then
             if(me<10) then
               write(tag,'("-P-000",i1)') me
             elseif(me<100) then
               write(tag,'("-P-00",i2)') me
             elseif(me<1000) then
               write(tag,'("-P-0",i3)') me
             elseif(me<10000) then
               write(tag,'("-P-",i4)') me
             else
               tag=' ######'
             end if
             rtnpos=index(message,ch10)
#          endif
!            It is not possible to use CPP, due to the // signs
             if(test_mpi)then
              do while(rtnpos/=0)
               string=tag//' '//message(1:rtnpos-1)
               write(unit, '(a)' ) trim(string)
               message=message(rtnpos+1:len(message))
               rtnpos=index(message,ch10)
              end do
              string=tag//' '//message
             end if
#          if defined MPI
             write(unit, '(a)' ) trim(string)
           elseif(mode_paral=='INIT') then
	     master=unit
           end if
#          endif

 call timab(49,2,tsec)

 end subroutine wrtout
!!***
