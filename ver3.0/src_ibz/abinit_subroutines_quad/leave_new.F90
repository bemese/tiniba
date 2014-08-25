!{\src2tex{textfont=tt}}
!!****f* ABINIT/leave_new
!! NAME
!! leave_new
!!
!! FUNCTION
!! Routine for clean exit of f90 code, taking into account possible
!! parallelization.
!!
!! COPYRIGHT
!! Copyright (C) 1998-2004 ABINIT group (DCA, XG, GMR, NCJ)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~ABINIT/Infos/copyright
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~ABINIT/Infos/contributors .
!!
!! INPUTS
!!  mode_paral=
!!   'COLL' if all procs are calling the routine with the same message to be
!!     written once only or
!!   'PERS' if the procs are calling the routine with different mesgs
!!     each to be written, or if one proc is calling the routine
!!
!! OUTPUT
!!  (only writing, then stop)
!!
!! NOTES
!! By default, it uses "call exit(1)", that is not completely
!! portable.
!!
!! PARENTS
!!      WffOpen,abinit,acfd_dyson,acfd_intexact,anaddb,appdig,append_cml
!!      append_cml2,asria9,asrif9,berryphase,berryphase_new,besjm,bigbx9,bldgrp
!!      blok8,bonds,bound,brdmin,canat9,ccfft,cgwf,cgwf3,chkdilatmx,chkdpr
!!      chkexi,chkgrp,chki8,chkilwf,chkin9,chkinp,chkint,chkneu,chknm8
!!      chkorthsy,chkprimit,chkr8,chkrp9,clsopn,cmpar8,constrf
!!      contract_dp_ge_val,contract_int_ge_val,contract_int_le_val
!!      contract_int_list,cvxclda,ddkten,delocint,der_int,diel9,dielmt,dieltcel
!!      distrb2,dotprod_vn,dotprodm_vn,driver,drivexc,dyfnl3,dyson_sc,eig1fixed
!!      eltfrxc3,ewald,ewald2,ewald3,ewald4,ewald9,fappnd,fftpac,fftw,filterpot
!!      findmin,fixsym,fourdp,fourwf,ftgam,ftgkk,ftiaf9,fxphas,gensymshub
!!      gensymshub4,gensymspgr,get_full_kgrid,get_g_tiny,get_tetra,getattribute
!!      getcut,getfreqsus,getghc,getkgrid,getlambda,getnel,getng,getsc,getshell
!!      gstate,gtblk9,handle_err_netcdf,hartre,hartre1,hartrestr,hdr_check
!!      hdr_init,hdr_io,hdr_io_netcdf,hermit,importcml,inarray,ingeo,ingeobld
!!      init8,initang,initberry,initmpi_fft,initmv,inkpts,inprep8,inread
!!      instrng,insy3,int2char,int2char4,intagm,inupper,invars0,invars1,invars2
!!      invars9,invcb,inwffil,inwffil3,ioarr,ioddb8,iofn1,iofn2,irrzg,isfile
!!      klocal,kpgio,kpgsph,kpgstr,kxc_alda,kxc_eok,linemin,listkk,loper3,lwf
!!      matcginv,mati3inv,matrginv,matrixelmt_g,mean_fftr,meanvalue_g,memana
!!      metcon,metric,metstr,mkcor3,mkcore,mkeuler,mkffnl,mkfilename,mklocl
!!      mkvxc3,moddiel,moldyn,move,mrgddb,nanal9,newkpt,newocc,newsp,newvtr
!!      newvtr3,nhatgrid,nonlinear,nonlop,nonlop_ylm,nstdy3,nstwf3,occeig
!!      operat,opernl2,opernl3,opernl4b,opernlb_ylm,out1dm,outqmc,outwf
!!      overlap_g,pawdens,pawdij,pawenergy,pawovlp,pawrhoij1,pawrhoij2
!!      pawsphpot,pawxc,pawxcden,pawxcdenm,pawxcm,ph1d3d,poisson,prcref
!!      prteigrs,prtocc,prtph3,prtrhomxmn,prtspgroup,prttagm,psddb8,psp1cc
!!      psp1in,psp1nl,psp2in,psp3nl,psp4cc,psp5in,psp5nl,psp7in,psp7nl,psp8cc
!!      psp8in,pspatm,pspini,q0dy3,ramansus,rdddb9,rdnpw,readeig,relaxpol
!!      respfn,rhofermi3,rhohxc,rhohxc_coll,rsiaf9,rwwan,rwwf,scalewf_nonlop
!!      scfcge,scfcv,scfcv3,scfeig,scfopt,scprqt,setshells,setup1,sg_ctrig
!!      sg_fft,sg_fftpad,sg_fftpx,sg_fftrisc,sg_fftx,sg_ffty,sg_fftz,sg_fourwf
!!      smallprim,smatrix,smpbz,sphere,sphereboundary,sphericaldens,status
!!      suskmm,suskmm_dyn,sym_gkk,symanal,symatm,symbrav,symdet,symdm9,symfind
!!      symg,symkchk,symkpt,symrelrot,symrhg,symspgr,tddft,testkgrid,thm9,timab
!!      timein,transgrid,uderiv,vlocalstr,vtorho,vtorho3,vtowfk,vtowfk3,wfconv
!!      wffile,wfkfermi3,wfsinp,wght9,xcacfd,xcden,xchcth,xchelu,xcpot,xcpzca
!!      xcspol,xctetr,xcwign,xcxalp,xfpack,xredxcart
!!
!! CHILDREN
!!      leave_myproc,mpi_allreduce,mpi_barrier,mpi_comm_rank,mpi_finalize
!!      wrtout
!!
!! SOURCE

 subroutine leave_new(mode_paral)

 use defs_basis
!no_abirules
#if defined NAGf95
 use f90_unix
#endif

 implicit none

#          if defined MPI
           include 'mpif.h'
#          endif
!Arguments ------------------------------------
 character(len=4),intent(in) :: mode_paral

!Local variables-------------------------------
 integer :: gl_check_bit,my_check_bit
 character(len=500) :: message
!no_abirules
#          if defined MPI
           integer :: ierr,me
#          endif

! **********************************************************************
 write(message, '(a,a)' ) ch10,' leave_new : decision taken to exit ...'
 call wrtout(06,message,'PERS')

#          if defined MPI
          !Determine who I am
           call MPI_COMM_RANK(MPI_COMM_WORLD,me,ierr)
           if(mode_paral=='COLL') then
            call MPI_FINALIZE(ierr)
#          endif

 call leave_myproc

#          if defined MPI
           elseif(mode_paral=='PERS') then

          !  Synchronize
             call MPI_BARRIER(MPI_COMM_WORLD,ierr)
             write(message,'(a)' ) ' leave_new : synchronization done...'
             call wrtout(06,message,'PERS')

          !  There is a problem for me
             my_check_bit=1

          !  Tell it to the others
             call MPI_ALLREDUCE(my_check_bit,gl_check_bit,1,MPI_INTEGER,&
          &    MPI_SUM,MPI_COMM_WORLD,ierr)

          !  Finally exit
             write(message, '(a)' ) ' leave_new : exiting...'
             call wrtout(06,message,'PERS')

             call MPI_FINALIZE(ierr)
             call leave_myproc

           end if
#          endif

 end subroutine leave_new
!!***
