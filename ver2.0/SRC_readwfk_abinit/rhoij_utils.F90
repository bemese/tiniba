!{\src2tex{textfont=tt}}
!!****f* ABINIT/rhoij_utils
!! This module contains functions used to manipulate
!! variables of structured datatype pawrhoij_type.
!! pawrhoij_type variables are rhoij occupancies
!! matrixes used within PAW formalism
!!
!!***

!!****f* ABINIT/rhoij_alloc
!! NAME
!! rhoij_alloc
!!
!! FUNCTION
!! Initialize and allocate a pawrhoij datastructure
!!
!! COPYRIGHT
!! Copyright (C) 2007-2009 ABINIT group (MT)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~ABINIT/Infos/copyright
!! or http://www.gnu.org/copyleft/gpl.txt .
!!
!! INPUTS
!! cplex=1 if rhoij is REAL,2 if COMPLEX
!! nlmn(:)=array of (l,m,n) sizes for rhoij
!! nspden=number of spin-components for rhoij
!! nsppol=number of independant spin-components for rhoij
!! typat(:)=types of atoms
!! ngrhoij=number of gradients to be allocated (OPTIONAL, default=0)
!! nlmnmix=number of rhoij elements to be mixed during SCF cycle (OPTIONAL, default=0)
!! use_rhoij_=1 if pawrhoij(:)%rhoij_ has to be allocated (OPTIONAL, default=0)
!! use_rhoijres=1 if pawrhoij(:)%rhoijres has to be allocated (OPTIONAL, default=0)
!!
!! SIDE EFFECTS
!! pawrhoij(:)<type(pawrhoij_type)>= rhoij datastructure
!!
!! PARENTS
!!
!! CHILDREN
!!
!! OUTPUT
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

subroutine rhoij_alloc(cplex,nlmn,nspden,nsppol,pawrhoij,typat,&      ! Mandatory arguments
&                      ngrhoij,nlmnmix,use_rhoij_,use_rhoijres) ! Optional arguments

 use defs_basis
 use defs_datatypes

 implicit none

!Arguments ------------------------------------
!scalars
 integer,intent(in) :: cplex,nspden,nsppol
 integer,intent(in),optional :: ngrhoij,nlmnmix,use_rhoij_,use_rhoijres
!arrays
 integer,intent(in) :: nlmn(:),typat(:)
 type(pawrhoij_type),intent(inout) :: pawrhoij(:)

!Local variables-------------------------------
!scalars
 integer :: irhoij,itypat,lmn2_size,nn1,nn2,nrhoij

! *************************************************************************

 nrhoij=size(pawrhoij);nn1=size(nlmn);nn2=size(typat)
 if (nrhoij/=nn2.or.maxval(typat)>nn1) stop "Error in rhoij_alloc: wrong sizes ! "

 do irhoij=1,nrhoij

  itypat=typat(irhoij)
  lmn2_size=nlmn(itypat)*(nlmn(itypat)+1)/2

! Scalars initializations
  pawrhoij(irhoij)%cplex=cplex
  pawrhoij(irhoij)%lmn_size=nlmn(itypat)
  pawrhoij(irhoij)%lmn2_size=lmn2_size
  pawrhoij(irhoij)%nspden=nspden
  pawrhoij(irhoij)%nsppol=nsppol
  pawrhoij(irhoij)%nrhoijsel=0
  pawrhoij(irhoij)%lmnmix_sz=0
  pawrhoij(irhoij)%ngrhoij=0
  pawrhoij(irhoij)%use_rhoij_=0
  pawrhoij(irhoij)%use_rhoijres=0

! Mandatory pointers allocations
  allocate(pawrhoij(irhoij)%rhoijselect(lmn2_size))
  allocate(pawrhoij(irhoij)%rhoijp(cplex*lmn2_size,nspden))

! Optional pointers allocations
  if (present(ngrhoij)) then
   if (ngrhoij>0) then
    pawrhoij(irhoij)%ngrhoij=ngrhoij
    allocate(pawrhoij(irhoij)%grhoij(ngrhoij,cplex*lmn2_size,nspden))
   end if
  end if
  if (present(nlmnmix)) then
   if (nlmnmix>0) then
    pawrhoij(irhoij)%lmnmix_sz=nlmnmix
    allocate(pawrhoij(irhoij)%kpawmix(nlmnmix))
   end if
  end if
  if (present(use_rhoij_)) then
   if (use_rhoij_>0) then
    pawrhoij(irhoij)%use_rhoij_=use_rhoij_
    allocate(pawrhoij(irhoij)%rhoij_(cplex*lmn2_size,nspden))
   end if
  end if
  if (present(use_rhoijres)) then
   if (use_rhoijres>0) then
    pawrhoij(irhoij)%use_rhoijres=use_rhoijres
    allocate(pawrhoij(irhoij)%rhoijres(cplex*lmn2_size,nspden))
   end if
  end if

! Intializations to zero (to avoid overflow)
  pawrhoij(irhoij)%rhoijselect(:)=0
  pawrhoij(irhoij)%rhoijp(:,:)=zero

 end do

end subroutine rhoij_alloc
!!***

!!****f* ABINIT/rhoij_free
!! NAME
!! rhoij_free
!!
!! FUNCTION
!! Destroy a pawrhoij datastructure
!!
!! COPYRIGHT
!! Copyright (C) 2007-2009 ABINIT group (MT)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~ABINIT/Infos/copyright
!! or http://www.gnu.org/copyleft/gpl.txt .
!!
!! SIDE EFFECTS
!! pawrhoij(:)<type(pawrhoij_type)>= rhoij datastructure
!!
!! PARENTS
!!
!! CHILDREN
!!
!! INPUTS
!!
!! OUTPUT
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

subroutine rhoij_free(pawrhoij)

 use defs_basis
 use defs_datatypes

 implicit none

!Arguments ------------------------------------
!arrays
 type(pawrhoij_type),intent(inout) :: pawrhoij(:)

!Local variables-------------------------------
!scalars
 integer :: irhoij,nrhoij

! *************************************************************************

 nrhoij=size(pawrhoij)
 do irhoij=1,nrhoij
  if (associated(pawrhoij(irhoij)%rhoijp)) deallocate(pawrhoij(irhoij)%rhoijp)
  if (associated(pawrhoij(irhoij)%rhoijselect)) deallocate(pawrhoij(irhoij)%rhoijselect)
  if (pawrhoij(irhoij)%ngrhoij>0) deallocate(pawrhoij(irhoij)%grhoij)
  if (pawrhoij(irhoij)%lmnmix_sz>0) deallocate(pawrhoij(irhoij)%kpawmix)
  if (pawrhoij(irhoij)%use_rhoij_>0) deallocate(pawrhoij(irhoij)%rhoij_)
  if (pawrhoij(irhoij)%use_rhoijres>0) deallocate(pawrhoij(irhoij)%rhoijres)
 end do

end subroutine rhoij_free
!!***

!!****f* ABINIT/rhoij_copy
!! NAME
!! rhoij_copy
!!
!! FUNCTION
!! Copy one pawrhoij datastructure into another
!! Can take into accound changes of dimensions
!!
!! COPYRIGHT
!! Copyright (C) 2007-2009 ABINIT group (MT)
!! This file is distributed under the terms of the
!! GNU General Public License, see ~ABINIT/Infos/copyright
!! or http://www.gnu.org/copyleft/gpl.txt .
!!
!! INPUTS
!! keep_cplex= optional argument (logical, default=.TRUE.)
!!             if .TRUE. pawrhoij_out(:)%cplex is NOT MODIFIED,
!!             even if different from pawrhoij_in(:)%cplex
!! keep_nspden= optional argument (logical, default=.TRUE.)
!!              if .TRUE. pawrhoij_out(:)%nspden is NOT MODIFIED,
!!              even if different from pawrhoij_in(:)%nspden
!! pawrhoij_in(:)<type(pawrhoij_type)>= input rhoij datastructure
!!
!! SIDE EFFECTS
!! pawrhoij_out(:)<type(pawrhoij_type)>= output rhoij datastructure
!!
!! PARENTS
!!
!! CHILDREN
!!
!! OUTPUT
!!
!! SOURCE

#if defined HAVE_CONFIG_H
#include "config.h"
#endif

subroutine rhoij_copy(pawrhoij_in,pawrhoij_out, &
&                     keep_cplex,keep_nspden) ! optional arguments

 use defs_basis
 use defs_datatypes

 implicit none

!Arguments ------------------------------------
!scalars
 logical,intent(in),optional :: keep_cplex,keep_nspden
!arrays
 type(pawrhoij_type),intent(in) :: pawrhoij_in(:)
 type(pawrhoij_type),intent(inout) :: pawrhoij_out(:)

!Local variables-------------------------------
!scalars
 integer :: cplex_in,cplex_out,dplex_in,dplex_out,i_in,i_out,ilmn,irhoij,ispden
 integer :: lmn2_size_out,lmnmix,ngrhoij,nrhoij_in,nrhoij_out,nselect,nspden_in
 integer :: nspden_out,use_rhoij_,use_rhoijres
 logical :: change_dim,keep_cplex_,keep_nspden_

! *************************************************************************

!Test on sizes
 nrhoij_in=size(pawrhoij_in);nrhoij_out=size(pawrhoij_out)
 if (nrhoij_in/=nrhoij_out) stop "Error in rhoij_copy: wrong sizes ! "

!Init flags
 keep_cplex_=.true.
 if (present(keep_cplex)) keep_cplex_=keep_cplex
 keep_nspden_=.true.
 if (present(keep_nspden)) keep_nspden_=keep_nspden

!Loop on rhoij components
 do irhoij=1,nrhoij_in

  lmn2_size_out=pawrhoij_in(irhoij)%lmn2_size
  cplex_in=pawrhoij_in(irhoij)%cplex
  cplex_out=cplex_in;if(keep_cplex_)cplex_out=pawrhoij_out(irhoij)%cplex
  nspden_in=pawrhoij_in(irhoij)%nspden
  nspden_out=nspden_in;if(keep_nspden_)nspden_out=pawrhoij_out(irhoij)%nspden

  change_dim=(pawrhoij_out(irhoij)%cplex/=cplex_out.or. &
&  pawrhoij_out(irhoij)%lmn2_size/=lmn2_size_out.or.&
&  pawrhoij_out(irhoij)%nspden/=nspden_out)
  dplex_in=cplex_in-1;dplex_out=cplex_out-1

! Scalars
  nselect=pawrhoij_in(irhoij)%nrhoijsel
  pawrhoij_out(irhoij)%cplex=cplex_out+0
  pawrhoij_out(irhoij)%nspden=nspden_out+0
  pawrhoij_out(irhoij)%lmn2_size=lmn2_size_out+0
  pawrhoij_out(irhoij)%lmn_size=pawrhoij_in(irhoij)%lmn_size+0
  if(.not.keep_nspden_) pawrhoij_out(irhoij)%nsppol=pawrhoij_in(irhoij)%nsppol+0
  pawrhoij_out(irhoij)%nrhoijsel=nselect+0

! Mandatory pointer: indexes for non-zero elements selection
  if (change_dim) then
   deallocate(pawrhoij_out(irhoij)%rhoijselect)
   allocate(pawrhoij_out(irhoij)%rhoijselect(lmn2_size_out))
  end if
  pawrhoij_out(irhoij)%rhoijselect(1:nselect)=pawrhoij_in(irhoij)%rhoijselect(1:nselect)+0
  if (nselect<lmn2_size_out) pawrhoij_out(irhoij)%rhoijselect(nselect+1:lmn2_size_out)=0

! Mandatory pointer: non-zero elements of rhoij
  if (change_dim) then
   deallocate(pawrhoij_out(irhoij)%rhoijp)
   allocate(pawrhoij_out(irhoij)%rhoijp(cplex_out*lmn2_size_out,nspden_out))
  end if
  if (cplex_out==cplex_in.and.nspden_out==nspden_in) then
   do ispden=1,nspden_out
    pawrhoij_out(irhoij)%rhoijp(1:cplex_out*nselect,ispden)=pawrhoij_in(irhoij)%rhoijp(1:cplex_out*nselect,ispden)+zero
    if (nselect<lmn2_size_out) pawrhoij_out(irhoij)%rhoijp(cplex_out*nselect+1:cplex_out*lmn2_size_out,ispden)=zero
   end do
  else
   pawrhoij_out(irhoij)%rhoijp(:,:)=zero
   if (nspden_out==1) then
    if (nspden_in==2) then
     do ilmn=1,nselect
      i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
      pawrhoij_out(irhoij)%rhoijp(i_out,1)=pawrhoij_in(irhoij)%rhoijp(i_in,1) &
&                                         +pawrhoij_in(irhoij)%rhoijp(i_in,2)+zero
     end do
    else ! nspden_in==1 or nspden_in=4
     do ilmn=1,nselect
      i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
      pawrhoij_out(irhoij)%rhoijp(i_out,1)=pawrhoij_in(irhoij)%rhoijp(i_in,1)+zero
     end do
    end if
   else if (nspden_out==2) then
    if (nspden_in==1) then
     do ilmn=1,nselect
      i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
      pawrhoij_out(irhoij)%rhoijp(i_out,1)=half*pawrhoij_in(irhoij)%rhoijp(i_in,1)+zero
      pawrhoij_out(irhoij)%rhoijp(i_out,2)=pawrhoij_in(irhoij)%rhoijp(i_out,1)
     end do
    else if (nspden_in==2) then
     do ilmn=1,nselect
      i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
      pawrhoij_out(irhoij)%rhoijp(i_out,1:2)=pawrhoij_in(irhoij)%rhoijp(i_in,1:2)+zero
     end do
    else ! nspden_in==4
     do ilmn=1,nselect
      i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
      pawrhoij_out(irhoij)%rhoijp(i_out,1)=half*(pawrhoij_in(irhoij)%rhoijp(i_in,1) &
&                                               +pawrhoij_in(irhoij)%rhoijp(i_in,4))+zero
      pawrhoij_out(irhoij)%rhoijp(i_out,2)=half*(pawrhoij_in(irhoij)%rhoijp(i_in,1) &
&                                               -pawrhoij_in(irhoij)%rhoijp(i_in,4))+zero
     end do
    end if
   else if (nspden_out==4) then
    if (nspden_in==1) then
     do ilmn=1,nselect
      i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
      pawrhoij_out(irhoij)%rhoijp(i_out,1)=pawrhoij_in(irhoij)%rhoijp(i_in,1)+zero
     end do
    else if (nspden_in==2) then
     do ilmn=1,nselect
      i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
      pawrhoij_out(irhoij)%rhoijp(i_out,1)=pawrhoij_in(irhoij)%rhoijp(i_in,1) &
&                                         +pawrhoij_in(irhoij)%rhoijp(i_in,2)+zero
      pawrhoij_out(irhoij)%rhoijp(i_out,4)=pawrhoij_in(irhoij)%rhoijp(i_in,1) &
&                                         -pawrhoij_in(irhoij)%rhoijp(i_in,2)+zero
     end do
    else ! nspden_in==4
     do ilmn=1,nselect
      i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
      pawrhoij_out(irhoij)%rhoijp(i_out,1:4)=pawrhoij_in(irhoij)%rhoijp(i_in,1:4)+zero
     end do
    end if
   end if
  end if

! Optional pointer: indexes of rhoij to be mixed
  lmnmix=pawrhoij_in(irhoij)%lmnmix_sz
  if (pawrhoij_out(irhoij)%lmnmix_sz/=lmnmix) then
   if (pawrhoij_out(irhoij)%lmnmix_sz>0) deallocate(pawrhoij_out(irhoij)%kpawmix)
   if (lmnmix>0) allocate(pawrhoij_out(irhoij)%kpawmix(lmnmix))
   pawrhoij_out(irhoij)%lmnmix_sz=lmnmix
  end if
  if (lmnmix>0) pawrhoij_out(irhoij)%kpawmix(1:lmnmix)=pawrhoij_in(irhoij)%kpawmix(1:lmnmix)

! Optional pointer: gradients of rhoij
  ngrhoij=pawrhoij_in(irhoij)%ngrhoij
  if (pawrhoij_out(irhoij)%ngrhoij/=ngrhoij) then
   if (pawrhoij_out(irhoij)%ngrhoij>0) deallocate(pawrhoij_out(irhoij)%grhoij)
   if (ngrhoij>0) allocate(pawrhoij_out(irhoij)%grhoij(ngrhoij,cplex_out*lmn2_size_out,nspden_out))
   pawrhoij_out(irhoij)%ngrhoij=ngrhoij
  end if
  if (ngrhoij>0) then
   if (change_dim) then
    deallocate(pawrhoij_out(irhoij)%grhoij)
    allocate(pawrhoij_out(irhoij)%grhoij(ngrhoij,cplex_out*lmn2_size_out,nspden_out))
   end if
   if (cplex_out==cplex_in.and.nspden_out==nspden_in) then
    do ispden=1,nspden_out
     do ilmn=1,cplex_out*lmn2_size_out
      pawrhoij_out(irhoij)%grhoij(1:ngrhoij,ilmn,ispden)=pawrhoij_in(irhoij)%grhoij(1:ngrhoij,ilmn,ispden)
     end do
    end do
   else
    pawrhoij_out(irhoij)%grhoij(:,:,:)=zero
    if (nspden_out==1) then
     if (nspden_in==2) then
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%grhoij(1:ngrhoij,i_out,1)=pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,1) &
&       +pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,2)
      end do
     else ! nspden_in==4
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%grhoij(1:ngrhoij,i_out,1)=pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,1)
      end do
     end if
    else if (nspden_out==2) then
     if (nspden_in==1) then
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%grhoij(1:ngrhoij,i_out,1)=half*(pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,1) &
&       +pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,2))
       pawrhoij_out(irhoij)%grhoij(1:ngrhoij,i_out,2)=pawrhoij_out(irhoij)%grhoij(1:ngrhoij,i_out,1)
      end do
     else ! nspden_in==4
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%grhoij(1:ngrhoij,i_out,1)=half*(pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,1) &
&       +pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,4))
       pawrhoij_out(irhoij)%grhoij(1:ngrhoij,i_out,2)=half*(pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,1) &
&       -pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,4))
      end do
     end if
    else if (nspden_out==4) then
     if (nspden_in==1) then
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%grhoij(1:ngrhoij,i_out,1)=pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,1)
      end do
     else ! nspden_in==2
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%grhoij(1:ngrhoij,i_out,1)=half*(pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,1) &
&       +pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,2))
       pawrhoij_out(irhoij)%grhoij(1:ngrhoij,i_out,4)=half*(pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,1) &
&       -pawrhoij_in(irhoij)%grhoij(1:ngrhoij,i_in,2))
      end do
     end if
    end if
   end if
  end if

! Optional pointer: residuals of rhoij
  use_rhoijres=pawrhoij_in(irhoij)%use_rhoijres
  if (pawrhoij_out(irhoij)%use_rhoijres/=use_rhoijres) then
   if (pawrhoij_out(irhoij)%use_rhoijres>0) deallocate(pawrhoij_out(irhoij)%rhoijres)
   if (use_rhoijres>0) allocate(pawrhoij_out(irhoij)%rhoijres(cplex_out*lmn2_size_out,nspden_out))
   pawrhoij_out(irhoij)%use_rhoijres=use_rhoijres
  end if
  if (use_rhoijres>0) then
   if (change_dim) then
    deallocate(pawrhoij_out(irhoij)%rhoijres)
    allocate(pawrhoij_out(irhoij)%rhoijres(cplex_out*lmn2_size_out,nspden_out))
   end if
   if (cplex_out==cplex_in.and.nspden_out==nspden_in) then
    do ispden=1,nspden_out
     do ilmn=1,cplex_out*lmn2_size_out
      pawrhoij_out(irhoij)%rhoijres(ilmn,ispden)=pawrhoij_in(irhoij)%rhoijres(ilmn,ispden)
     end do
    end do
   else
    pawrhoij_out(irhoij)%rhoijres(:,:)=zero
    if (nspden_out==1) then
     if (nspden_in==2) then
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoijres(i_out,1)=pawrhoij_in(irhoij)%rhoijres(i_in,1) &
&       +pawrhoij_in(irhoij)%rhoijres(i_in,2)
      end do
     else ! nspden_in==4
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoijres(i_out,1)=pawrhoij_in(irhoij)%rhoijres(i_in,1)
      end do
     end if
    else if (nspden_out==2) then
     if (nspden_in==1) then
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoijres(i_out,1)=half*(pawrhoij_in(irhoij)%rhoijres(i_in,1) &
&       +pawrhoij_in(irhoij)%rhoijres(i_in,2))
       pawrhoij_out(irhoij)%rhoijres(i_out,2)=pawrhoij_out(irhoij)%rhoijres(i_out,1)
      end do
     else ! nspden_in==4
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoijres(i_out,1)=half*(pawrhoij_in(irhoij)%rhoijres(i_in,1) &
&       +pawrhoij_in(irhoij)%rhoijres(i_in,4))
       pawrhoij_out(irhoij)%rhoijres(i_out,2)=half*(pawrhoij_in(irhoij)%rhoijres(i_in,1) &
&       -pawrhoij_in(irhoij)%rhoijres(i_in,4))
      end do
     end if
    else if (nspden_out==4) then
     if (nspden_in==1) then
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoijres(i_out,1)=pawrhoij_in(irhoij)%rhoijres(i_in,1)
      end do
     else ! nspden_in==2
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoijres(i_out,1)=half*(pawrhoij_in(irhoij)%rhoijres(i_in,1) &
&       +pawrhoij_in(irhoij)%rhoijres(i_in,2))
       pawrhoij_out(irhoij)%rhoijres(i_out,4)=half*(pawrhoij_in(irhoij)%rhoijres(i_in,1) &
&       -pawrhoij_in(irhoij)%rhoijres(i_in,2))
      end do
     end if
    end if
   end if
  end if

! Optional pointer: non-symmetrized rhoij
  use_rhoij_=pawrhoij_in(irhoij)%use_rhoij_
  if (pawrhoij_out(irhoij)%use_rhoij_/=use_rhoij_) then
   if (pawrhoij_out(irhoij)%use_rhoij_>0) deallocate(pawrhoij_out(irhoij)%rhoij_)
   if (use_rhoij_>0) allocate(pawrhoij_out(irhoij)%rhoij_(cplex_out*lmn2_size_out,nspden_out))
   pawrhoij_out(irhoij)%use_rhoij_=use_rhoij_
  end if
  if (use_rhoij_>0) then
   if (change_dim) then
    deallocate(pawrhoij_out(irhoij)%rhoij_)
    allocate(pawrhoij_out(irhoij)%rhoij_(cplex_out*lmn2_size_out,nspden_out))
   end if
   if (cplex_out==cplex_in.and.nspden_out==nspden_in) then
    do ispden=1,nspden_out
     do ilmn=1,cplex_out*lmn2_size_out
      pawrhoij_out(irhoij)%rhoij_(ilmn,ispden)=pawrhoij_in(irhoij)%rhoij_(ilmn,ispden)
     end do
    end do
   else
    pawrhoij_out(irhoij)%rhoij_(:,:)=zero
    if (nspden_out==1) then
     if (nspden_in==2) then
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoij_(i_out,1)=pawrhoij_in(irhoij)%rhoij_(i_in,1) &
&       +pawrhoij_in(irhoij)%rhoij_(i_in,2)
      end do
     else ! nspden_in==4
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoij_(i_out,1)=pawrhoij_in(irhoij)%rhoij_(i_in,1)
      end do
     end if
    else if (nspden_out==2) then
     if (nspden_in==1) then
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoij_(i_out,1)=half*(pawrhoij_in(irhoij)%rhoij_(i_in,1) &
&       +pawrhoij_in(irhoij)%rhoij_(i_in,2))
       pawrhoij_out(irhoij)%rhoij_(i_out,2)=pawrhoij_out(irhoij)%rhoij_(i_out,1)
      end do
     else ! nspden_in==4
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoij_(i_out,1)=half*(pawrhoij_in(irhoij)%rhoij_(i_in,1) &
&       +pawrhoij_in(irhoij)%rhoij_(i_in,4))
       pawrhoij_out(irhoij)%rhoij_(i_out,2)=half*(pawrhoij_in(irhoij)%rhoij_(i_in,1) &
&       -pawrhoij_in(irhoij)%rhoij_(i_in,4))
      end do
     end if
    else if (nspden_out==4) then
     if (nspden_in==1) then
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoij_(i_out,1)=pawrhoij_in(irhoij)%rhoij_(i_in,1)
      end do
     else ! nspden_in==2
      do ilmn=1,lmn2_size_out
       i_in=cplex_in*ilmn-dplex_in;i_out=cplex_out*ilmn-dplex_out
       pawrhoij_out(irhoij)%rhoij_(i_out,1)=half*(pawrhoij_in(irhoij)%rhoij_(i_in,1) &
&       +pawrhoij_in(irhoij)%rhoij_(i_in,2))
       pawrhoij_out(irhoij)%rhoij_(i_out,4)=half*(pawrhoij_in(irhoij)%rhoij_(i_in,1) &
&       -pawrhoij_in(irhoij)%rhoij_(i_in,2))
      end do
     end if
    end if
   end if
  end if

 end do ! irhoij

end subroutine rhoij_copy
!!***
