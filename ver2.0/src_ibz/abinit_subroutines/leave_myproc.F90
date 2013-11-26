!{\src2tex{textfont=tt}}
!!****f* ABINIT/leave_myproc
!! NAME
!! leave_myproc
!!
!! FUNCTION
!! Routine for clean exit of f90 code by one processor
!!
!! COPYRIGHT
!! Copyright (C) 1998-2004 ABINIT group (DCA, XG, GMR, NCJ)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~ABINIT/Infos/copyright
!! or http://www.gnu.org/copyleft/gpl.txt .
!! For the initials of contributors, see ~ABINIT/Infos/contributors .
!!
!! INPUTS
!!  (nothing)
!!
!! OUTPUT
!!  (only writing, then stop)
!!
!! NOTES
!! By default, it uses "call exit(1)", that is not completely
!! portable.
!!
!! PARENTS
!!      leave_new,leave_test
!!
!! CHILDREN
!!      exit
!!
!! SOURCE

 subroutine leave_myproc

 use defs_basis
!no_abirules
#if defined NAGf95
 use f90_unix
#endif
 
 implicit none
 
!Arguments ------------------------------------
 
!Local variables-------------------------------
 
! **********************************************************************

#if defined ibm
 stop 1
#elif defined macosx
 stop 1
#elif defined NAGf95
 call exit(-1)
#elif defined __IFC
 stop 1
#else
 call exit(1)
#endif

 end subroutine leave_myproc
!!***
