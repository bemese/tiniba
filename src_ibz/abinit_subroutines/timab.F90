!{\src2tex{textfont=tt}}
!!****f* ABINIT/timab
!! NAME
!! timab
!!
!! FUNCTION
!! Timing subroutine.  Calls machine-dependent "timein" which
!! returns elapsed cpu and wall clock times in sec.
!! Also return the number of times the counter has been called
!!
!! Depending on value of "option" routine will:
!! (0) zero all accumulators
!! (1) start with new incremental time slice for accumulator n
!!   also increase by one the counter for this accumulator
!! (2) stop time slice; add time to accumulator n
!! (3) report accumulated time for accumulator n
!!   and number of time that the routine has been called
!! (4) report time slice for accumlator n (not full time accumlated)
!! (5) option to suppress timing (nn should be 0) or reenable it (nn /=0)
!!
!! If, on first entry, subroutine is not being initialized, it
!! will automatically initialize as well as rezero accumulator n.
!! However, initialization SHOULD be done explicitly by the user
!! so that it can be done near the top of his/her main routine.
!!
!!
!! COPYRIGHT
!! Copyright (C) 1998-2004 ABINIT group (DCA, XG, GMR)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~ABINIT/Infos/copyright
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~ABINIT/Infos/contributors .
!!
!! INPUTS
!!  nn=index of accumulator (distinguish what is being timed);
!!   not used if option=0
!!  option=see comment above
!!
!! OUTPUT
!!  on option=3 :
!!    tottim(2,nn)=accumulated time for accumulator nn; otherwise
!!     tottim is a dummy variable.
!!    option gives the number of times that the
!!     accumulator has been incremented
!!
!! PARENTS
!!      abinit,acfd_dyson,acfd_intexact,bestwfs,cchi0,cgwf,cgwf3,csigme,dielmt
!!      dieltcel,dotprod_g,dotprod_v,dotprod_vn,dotprodm_v,dotprodm_vn,driver
!!      dyfnl3,eltfrhar3,eltfrkin3,eltfrloc3,eltfrnl3,eltfrxc3,eneres3,energy
!!      filterpot,fourdp,fourwf,getghc,getgsc,gstate,hartre,hartre1,initylmg
!!      inkpts,invars2,inwffil,inwffil3,iofn1,iofn2,kpgio,kpgsph,leave_test
!!      loop3dte,loper3,matrixelmt_g,mean_fftr,meanvalue_g,mkcore,mkffnl,mklocl
!!      mkresi,mkrho,mkrho3,mkvxc3,newkpt,newocc,newvtr,newvtr3,nonlinear
!!      nonlop,nonlop_ylm,nstdy3,nstwf3,orthon,outkss,outwf,pareigocc,pawrhoij2
!!      pawxc,pawxcm,projbd,pspini,respfn,rhofermi3,rhohxc_coll,rwwf,scfcv
!!      scfcv3,screening,setsym,setvtr,sigma,sqnorm_g,sqnorm_v,sqnormm_v,status
!!      stress,strhar,strnps,suscep,suscep_dyn,suscep_stat,susk,susk_dyn
!!      susk_dyn_pgg,suskmm,suskmm_dyn,symrhg,symsgcube,tddft,timana,vresfo
!!      vtorho,vtorho3,vtorhotf,vtowfk,vtowfk3,wfkfermi3,wfsinp,wrtout,xcden
!!      xcpot
!!
!! CHILDREN
!!      leave_new,timein,wrtout
!!
!! SOURCE

 subroutine timab(nn,option,tottim)

 use defs_basis
 
 implicit none
 
!Arguments ------------------------------------
 integer,intent(in) :: nn
 integer,intent(inout) :: option
 real(dp),intent(out) :: tottim(2)
 
!Local variables-------------------------------
!mtim determines the maximum number of "timing slots" available
 integer,parameter :: mtim=599
 integer,save :: timopt=1
 real(dp) :: cpu,wall
 character(len=500) :: message
 integer,save :: ncount(mtim)
 real(dp),save :: acctim(2,mtim),tzero(2,mtim)
 
! *************************************************************************

! DEBUG
!write(6,*)' timab : enter with  nn, option, timopt',nn,option,timopt
!if(entry==5)stop
! ENDDEBUG

 if (option==5) then
!DEBUG
! write(6,*)' timab : option=5, nn=',nn
!ENDDEBUG
  timopt=nn
 end if

!If timopt was set to zero by a call with option=5, suppress
!all action of this routine (might as well return at this point !)
 if(timopt/=0 .and. option/=5)then
!
! Check that nn lies in sensible bounds
  if (nn<0.or.nn>mtim) then
   write(message, '(a,a,a,a,i6,a,i8,a)' ) ch10,&
&    ' timab: BUG -',ch10,&
&    '  dim mtim=',mtim,' but input nn=',nn,'.'
   call wrtout(6,message,'PERS')
   call leave_new('PERS')
  end if
!
  if (option==0) then
!  Zero out all accumulators of time and init timers
   acctim(:,:)=0.0d0
   tzero(:,:)=0.0d0
   ncount(:)=0
  else if (option==1) then
!  initialize timab for nn
!  write(6,*)' timab : enter timein '
   call timein(cpu,wall)
!  write(6,*)' timab : exit timein '
   tzero(1,nn)=cpu
   tzero(2,nn)=wall
  else if (option==2) then
!  accumulate time for nn
!  write(6,*)' timab : enter timein '
   call timein(cpu,wall)
!  write(6,*)' timab : exit timein '
   acctim(1,nn)=acctim(1,nn)+cpu -tzero(1,nn)
   acctim(2,nn)=acctim(2,nn)+wall-tzero(2,nn)
   ncount(nn)=ncount(nn)+1
  else if (option==3) then
!  return accumulated time for nn
   tottim(1)=acctim(1,nn)
   tottim(2)=acctim(2,nn)
   option=ncount(nn)
  else if (option==4) then
!  return elapsed time for nn (do not accumulate)
   call timein(cpu,wall)
   tottim(1)=cpu-tzero(1,nn)
   tottim(2)=wall-tzero(2,nn)
  else
   write(message, '(a,a,a,a,i10,a)' ) ch10,&
&    ' timab: BUG -',ch10,&
&    '  Input option not valid, =',option,'.'
   call wrtout(6,message,'PERS')
   call leave_new('PERS')
  end if

 end if

! DEBUG
!write(6,*)' timab : exit '
! ENDDEBUG

 end subroutine timab
!!***
