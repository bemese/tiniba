!##################### matrix elements subroutines below #################
!
!
!
!###### comments!
!
! The subroutine rhomm computes the carrier injection for valence and conduction states
! so far there is no response that needs it
!
! The subroutine srhoccp computes the carrier injection using rho_{ccp}
! that is needed for the D(egree of)S(pin)P(olarization)
!
!###  momentum matrix elements: momentum_me
!     and Modified momentum_me 
!#####   Calculate Pmn = <m|p^a|n>
!        it also may calculate <m|p^a S^i|n>
SUBROUTINE mme(u_mme,u_vme,ik,nbandk,nspinor,npw,kp,kg,b1,b2,b3,cg,nkpt)

  IMPLICIT NONE
  INTEGER :: i,ik,mband,nband,ipw
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: u_mme,u_vme
  INTEGER :: cmax,imax,icon,nkpt
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: hbar,m0,kpkg
  DOUBLE COMPLEX, ALLOCATABLE :: ps(:,:)
  INTEGER, DIMENSION(3,npw) :: kg
  DOUBLE PRECISION, DIMENSION(3) :: kp,b1,b2,b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX :: ci,cgmup,cgmdn,cgnup,cgndn
!!! constants
  m0=1.d0
  hbar=1.d0
  ci = cmplx(.0,1.)  

!!!   Pmn=<ms|p|ns'>
!!!   s,s' -> +,-  ->  1,2   ;   1:+    2:-
!!!   <ms|p^i|ns'> = Sum_s Sum_G [(k+G)_i * (C_{m,k+G,s}^* C_{n,k+G,s})]
!!!   <ms|K^{ix}|ns'> = hbar/2   Sum_G [(k+G)_i * ( C_{m,k+G,up}^* C_{n,k+G,dn}+C_{m,k+G,dn}^* C_{n,k+G,up})]
!!!   <ms|K^{iy}|ns'> = i*hbar/2 Sum_G [(k+G)_i * (-C_{m,k+G,up}^* C_{n,k+G,dn}+C_{m,k+G,dn}^* C_{n,k+G,up})]
!!!   <ms|K^{iz}|ns'> = hbar/2   Sum_G [(k+G)_i * ( C_{m,k+G,up}^* C_{n,k+G,up}-C_{m,k+G,dn}^* C_{n,k+G,dn})]
!!!       K^{ij}= v^i S^j (i,j = x,y,z)
!!! counter
  cmax = nbandk*(nbandk+1)/2
  imax = 0
  icon = 1
  call CPU_time (iatime)
  call CPU_time (itime)
!!! counter
  do mband=1,nbandk
     do nband=mband,nbandk
        allocate(ps(3,4))
        do i=1,3
!!!ps(direction 1=x 2=y 3=z ,regular p => 1
!!!                          K^{ix}    => 2 (the i-component goes in the first index)
!!!                          K^{iy}    => 3
!!!                          K^{iz}    => 4
           ps(i,:)=0.d0
           do ipw=1,npw
!!!
!!! (k + G)^i       
!!!
              kpkg= (kp(1)+kg(1,ipw))*b1(i) +&
                   &(kp(2)+kg(2,ipw))*b2(i) +&
                   &(kp(3)+kg(3,ipw))*b3(i)

!!!
!!! first define the spinor components in human language
!!!
              cgmup  = cg(1,mband,1,ipw) + ci * cg(2,mband,1,ipw)
              cgnup  = cg(1,nband,1,ipw) + ci * cg(2,nband,1,ipw)
              if(nspinor==2) then 
                 cgmdn  = cg(1,mband,2,ipw) + ci * cg(2,mband,2,ipw)
                 cgndn  = cg(1,nband,2,ipw) + ci * cg(2,nband,2,ipw)
              endif
              !no-spin
              if(nspinor==1) then 
                 ps(i,1) = ps(i,1) + hbar * conjg(cgmup)*cgnup * kpkg
              else !nspinor =2
                 ! pmn
                 ps(i,1) = ps(i,1) + hbar*( conjg(cgmup)*cgnup+conjg(cgmdn)*cgndn ) * kpkg
                 ! K^{ix}_mn
!!!                 ps(i,2) = ps(i,2) + hbar/(2.*m0)   *( conjg(cgmup)*cgndn+conjg(cgmdn)*cgnup ) * kpkg
                 ! K^{iy}_mn
!!!                 ps(i,3) = ps(i,3) + ci*hbar/(2.*m0)*(-conjg(cgmup)*cgndn+conjg(cgmdn)*cgnup ) * kpkg
                 ! K^{iz}_mn
!!!                 ps(i,4) = ps(i,4) + hbar/(2.*m0)   *( conjg(cgmup)*cgnup-conjg(cgmdn)*cgndn ) * kpkg
              end if
!!!
           end do ! ipw      
!!!
        end do ! i              
!!!
!        write(u_mme,"(6E18.8)")real(ps(1,1)),aimag(ps(1,1))&
!                             &,real(ps(2,1)),aimag(ps(2,1))&
!                             &,real(ps(3,1)),aimag(ps(3,1))
        write(u_mme,*)real(ps(1,1)),aimag(ps(1,1))&
                             &,real(ps(2,1)),aimag(ps(2,1))&
                             &,real(ps(3,1)),aimag(ps(3,1))
!!!
!        write(u_mme,"(3i4,6E18.8)")ik,mband,nband,real(ps(1,1)),aimag(ps(1,1))&
!             &,real(ps(2,1)),aimag(ps(2,1))&
!             &,real(ps(3,1)),aimag(ps(3,1))
!!! velocity matrix elements are real
        if(mband.eq.nband) then
!           write(u_vme,"(i,3E18.8)")mband,real(ps(1,1))&
!           write(u_vme,"(3E18.8)")real(ps(1,1))&
!                &,real(ps(2,1))&
!                &,real(ps(3,1))
           write(u_vme,*)real(ps(1,1))&
                &,real(ps(2,1))&
                &,real(ps(3,1))
        end if
        if(1.eq.2)then
        if(nspinor==2) then 
           ! K^{ix}_mn
           write(u_mme+1,"(6E18.8)")real(ps(1,2)),aimag(ps(1,2))&
                                  &,real(ps(2,2)),aimag(ps(2,2))&
                                  &,real(ps(3,2)),aimag(ps(3,2))
           ! K^{iy}_mn
           write(u_mme+2,"(6E18.8)")real(ps(1,3)),aimag(ps(1,3))&
                                  &,real(ps(2,3)),aimag(ps(2,3))&
                                  &,real(ps(3,3)),aimag(ps(3,3))
           ! K^{iz}_mn
           write(u_mme+3,"(6E18.8)")real(ps(1,4)),aimag(ps(1,4))&
                                  &,real(ps(2,4)),aimag(ps(2,4))&
                                  &,real(ps(3,4)),aimag(ps(3,4))
        end if
        endif
        deallocate(ps)
!!!
!!! counter
        imax = imax + 1
        if ( imax.eq.(int(icon*(cmax/5)))) then
           icon = icon + 1
           call CPU_time(ftime)
           ttime = ftime-itime
           write(*,"(A5, I7, A21, I7, A5, F10.2 )")'pmn: ',imax,' cycles of a total of',cmax,'. t=',ttime/60.
           call CPU_time(itime) 
        end if
!!! counter
!!! time estimate
        if((ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt/50
           write(*,*)"---"
           write(*,"(A37, F10.2, A12,I5,A9  )")'pmn: using 50 cylcles estimated time=',ttime/60.,' minutes for',nkpt,' k-points'
           write(*,*)"---"
        end if
!!!
     end do ! jband
  end do ! iband
!!! total time
  call CPU_time(fatime) 
  ttime = fatime-iatime
  write(*,"(A26,I5,A4,F10.2,A9)")'pmn: total time for ik= ',ik,' t=',ttime/60.,' minutes'
!!! total time
  
END SUBROUTINE mme

!###  spin matrix elements: spin_me
!
!#####    Calculate <m|S^i|n> for a given m,n,k
!         <m|s^i|n> -> (hbar/2)*<m|s^i|n>
SUBROUTINE sccp(u_spin,ik,nbandk,nspinor,npw,cg,nkpt,nval)
  
  IMPLICIT NONE
  INTEGER :: mband,nband,ipw
  INTEGER :: u_spin,ik,nbandk,nspinor,npw
  INTEGER :: cmax,imax,icon,nkpt
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE PRECISION :: hbar
  DOUBLE COMPLEX :: ci,cgnup,cgndn,cgmup,cgmdn
  DOUBLE COMPLEX :: sx,sy,sz
  INTEGER :: Nval

  hbar=1.0d0
  ci = cmplx(0.,1.)
!!!##### calculates only upper half spin_{cc'} matrix 
!!! counter
  cmax = (nbandk-nval)*(nbandk-nval+1)/2
  imax = 0
  icon = 1
  call CPU_time (iatime)
  call CPU_time (itime)
!!! counter
!!! We only need S_{cc'}. i.e. among the conduction bands only
  do nband=nval+1,nbandk
     do mband=nband,nbandk
        sx=cmplx(0.,0.)
        sy=cmplx(0.,0.)
        sz=cmplx(0.,0.)
        do ipw=1,npw
           cgmup  = cg(1,mband,1,ipw) + ci * cg(2,mband,1,ipw)
           cgmdn  = cg(1,mband,2,ipw) + ci * cg(2,mband,2,ipw)
           cgnup  = cg(1,nband,1,ipw) + ci * cg(2,nband,1,ipw)
           cgndn  = cg(1,nband,2,ipw) + ci * cg(2,nband,2,ipw)
!!! |m+>=cgmup, |m->=cgmdn, <ns|=(|ns>)*
!!!
!!!!!!!!!!!!!! 
!!! The factor (hbar/2) is leaved out.
!!! If the DSP is calculated, then (hbar/2) is properly taken into account
!!!                           in the expression for the DSP, and thus is not needed.
!!! If $\zeta^{abc}$ needs to be reported, then multiply by (hbar/2)
!!!                                        and convert to the appropriate S.I. units.!
!!!!!!!!!!!!!! 
!!! <n|S^x|m>= (1/2)*(<n+|m-> + <n-|m+>)
!           sx = sx + (hbar/2.d0)   *(  conjg(cgnup)*cgmdn + conjg(cgndn)*cgmup )
           sx = sx + (  conjg(cgnup)*cgmdn + conjg(cgndn)*cgmup )
!!! <n|S^y|m>= (I/2)*(-<n+|m-> + <n-|m+>)
!           sy = sy + (ci*hbar/2.d0)*( -conjg(cgnup)*cgmdn + conjg(cgndn)*cgmup )
           sy = sy + ci*( -conjg(cgnup)*cgmdn + conjg(cgndn)*cgmup )
!!! <n|S^z|m>= (1/2)*(<n+|m+> - <n-|m->)
!           sz = sz + (hbar/2.d0)   *(  conjg(cgnup)*cgmup - conjg(cgndn)*cgmdn )
           sz = sz + (  conjg(cgnup)*cgmup - conjg(cgndn)*cgmdn )
           !
        end do !ipw
!!!        write(u_spin,"(3i5,6E18.8)")ik,nband,mband,real(sx),aimag(sx),real(sy),aimag(sy),real(sz),aimag(sz)
!           write(u_spin,"(6E18.8)")real(sx),aimag(sx),real(sy),aimag(sy),real(sz),aimag(sz)
           write(u_spin,*)real(sx),aimag(sx),real(sy),aimag(sy),real(sz),aimag(sz)
!!! counter
        imax = imax + 1
        if ( imax.eq.(int(icon*(cmax/5)))) then
           icon = icon + 1
           call CPU_time(ftime)
           ttime = ftime-itime
           write(*,"(A5, I7, A21, I7, A5, F10.2 )")'sccp: ',imax,' cycles of a total of',cmax,'. t=',ttime/60.
           call CPU_time(itime) 
        end if
!!! counter
!!! time estimate
        if((ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt/50
           write(*,*)"---"
           write(*,"(A37, F10.2, A12,I5,A9  )")'sccp: using 50 cycles estimated time=',ttime/60.,' minutes for',nkpt,' k-points'
           write(*,*)"---"
        end if
!!!
     end do !mband
  end do !nband
!!! total time
  call CPU_time(fatime) 
  ttime = fatime-iatime
  write(*,"(A26,I5,A4,F10.2,A9)")'sccp: total time for ik= ',ik,' t=',ttime/60.,' minutes'
!!! total time
  
END SUBROUTINE sccp

!#####  cal P^a_mn = (1/2)*Sum_{G'} c*_{m,G'para,G'perp} Sum_Gperp c_{n,G'para,Gperp} 
!                   ( 2 k^a + 2 G'para ( \delta_{a,x} + \delta_{a,y} ) + ( Gperp + G'perp )\delta_{a,z} ) f(g-g')
!       G=G_parallel+ Gperp 
!       PRB, vol.63, 205406 (2001)
!       NEW Version, \delta_{G_parallel,G'_parallel} implicitely taken
!       12/02/05
SUBROUTINE lpmn(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
  IMPLICIT NONE
  INTEGER :: i,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  INTEGER :: cmax,imax,icon,nkpt
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: Lslab,Lg
  DOUBLE PRECISION, DIMENSION(Nlayers) :: delta,zeta,deltab,deltaf
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  DOUBLE PRECISION, DIMENSION(3) :: kp
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX, ALLOCATABLE :: cf(:,:)
  DOUBLE COMPLEX :: ci,cgn,cgm,cero
  DOUBLE COMPLEX :: ctmp
  DOUBLE COMPLEX :: laycut ! layered cut function
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp1
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp2 ! Only to check
  DOUBLE PRECISION, DIMENSION(3) :: kmg,kmg12
  ! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
!     write(u_log,*)ik,ix,iy,iz
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
!  write(u_log,*)-minx,maxx,-miny,maxy,-minz,maxz
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  end do
  allocate (cf(-minz-maxz:maxz+minz,Nlayers))
  do izeta = 1,Nlayers ! number of layers
!!! selects given planes
!!!     if (izeta.eq.9)  izeta=10
!!!     if (izeta.eq.10) izeta=14
!!!     if (izeta.eq.11) izeta=19
!!!
     do jz = -minz-maxz,maxz+minz  ! g-g'
        if ( jz==0 ) then
           cf(jz,izeta)=cmplx(( deltab(izeta)+deltaf(izeta) )/Lslab,0.)
        else
           Lg = Lslab* jz * b3(3) 
           cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab)) &
                -exp(ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab)))
        endif
     end do
  end do
  !
  ! for now for no spin cg(1:2,jband,ispinor,ipw)
  !
  do izeta = 1,Nlayers
!!! selects given planes
!!!     if (izeta.eq.9)  izeta=10
!!!     if (izeta.eq.10) izeta=14
!!!     if (izeta.eq.11) izeta=19
!!!
!!! writes only diagonal and upper diagonal \cal P are hermitian
!!! counter
     cmax = nbandk*(nbandk+1)/2
     imax = 0
     icon = 1
     call CPU_time (iatime)
     call CPU_time (itime)
!!! counter
     do iband=1,nbandk !n
        do jband=iband,nbandk !m
           ctmp1=cero
           ctmp2=cero
           laycut=cero
           do ipwp=1,npw
              ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
              realcg=cg(1,iband,1,ipwp) !iband => n
              imagcg=cg(2,iband,1,ipwp) !iband => n
              cgn=dcmplx(realcg,imagcg)
              ! only for b1(i) \cdot b2(i) =0?  b1(i) \cdot b3(i) = b2(i) \cdot b3(i) = 0 for sure
              kmg12 = 2.*( kp(1) + kg(1,ipwp) )*b1 + 2.*( kp(2) + kg(2,ipwp) )*b2
!!! sum over G_perp only
!!! it gives same as full sum
!!! tiny differences only when values are ~E-15 or smaller
!!!
              if ( 1 .eq. 1 ) then
                 do jz =-minz,maxz
                    ipw = arr(ix,iy,jz)
                    if(ipw.ne.0) then
                       realcg=cg(1,jband,1,ipw) !jband => m
                       imagcg=cg(2,jband,1,ipw) !jband => m
                       cgm=dcmplx(realcg,imagcg)
                       ! only for b1(i) \cdot b2(i) =0?  b1(i) \cdot b3(i) = b2(i) \cdot b3(i) = 0 for sure
                       kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
                       ctmp=CONJG(cgn)*cgm*cf(jz-iz,izeta)
                       ctmp1= ctmp1 + ctmp*kmg
                       laycut= laycut + ctmp
                    end if
                 end do ! jz (Gperp)
              endif
61            format(3i5,f8.4,8e13.5)
!!!!!!!!!!!!!!!!!!!!!! full sum
!!! activate if need to check with above
!!! sum over G_perp only
!!            if ( 1 .eq. 2 ) then
!!               do jz =1,npw
!!                  if((ix.eq.kg(1,jz)).and.(iy.eq.kg(2,jz))) then
!!                     ipw = arr(ix,iy,kg(3,jz))
!!                     realcg=cg(1,jband,1,ipw) !jband => m
!!                     imagcg=cg(2,jband,1,ipw) !jband => m
!!                     cgm=dcmplx(realcg,imagcg)
!!                     kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
!!                     ctmp=CONJG(cgn)*cgm*kmg*cf(kg(3,jz)-iz,izeta)
!!                     !ctmp=2.*CONJG(cgn)*cgm*cf(kg(3,jz)-iz,izeta)
!!                     ctmp1= ctmp1 + ctmp
!!                  end if
!!               end do ! jz (Gperp)
!!            endif
!!!!!!!!!!!!!!!!!!!!!!!!
           end do ! ipw  (Gpara,Gperp)
!!! with sum over G_perp only
!           write(unitS+izeta-1,"(6E18.8)")dreal(ctmp1(1)/2.),dimag(ctmp1(1)/2.)&
!                ,dreal(ctmp1(2)/2.),dimag(ctmp1(2)/2.)&
!                ,dreal(ctmp1(3)/2.),dimag(ctmp1(3)/2.) 
           write(unitS+izeta-1,*)dreal(ctmp1(1)/2.),dimag(ctmp1(1)/2.)&
                ,dreal(ctmp1(2)/2.),dimag(ctmp1(2)/2.)&
                ,dreal(ctmp1(3)/2.),dimag(ctmp1(3)/2.) 
           write(unitS+izeta+9,*)dreal(laycut),dimag(laycut)
!              write(31,63)ik,izeta,iband,jband,dreal(ctmp1(1)/2.),dimag(ctmp1(1)/2.)&
!                   ,dreal(ctmp1(2)/2.),dimag(ctmp1(2)/2.)&
!                   ,dreal(ctmp1(3)/2.),dimag(ctmp1(3)/2.) 
!63   format(4i5,6e13.5)

!!! with full
!              write(unitS+Nlayers+izeta-1,"(6E18.8)")dreal(ctmp2(1)/2.),dimag(ctmp2(1)/2.)&
!                ,dreal(ctmp2(2)/2.),dimag(ctmp2(2)/2.)&
!               ,dreal(ctmp2(3)/2.),dimag(ctmp2(3)/2.) 
!!!!!!!
!!! counter
           imax = imax + 1
           if ( imax.eq.(int(icon*(cmax/5)))) then
              icon = icon + 1
              call CPU_time(ftime)
              ttime = ftime-itime
              write(*,"(A14, I4, A2, I7, A22,I7,A5, F10.2 )")'calPmn: layer',izeta,'. ',imax,' cycles of a total of',cmax,'. t=',ttime/60.
              call CPU_time(itime) 
           end if
!!! counter
!!! time estimate
        if((izeta.eq.1).and.(ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt*Nlayers/50
           write(*,"(A24, F10.2, A13,I5,A13,I4  )")'calPmn: estimated time=',ttime/60.,' minutes for',nkpt,' k-points. L=',Nlayers
        end if
!!!
        end do !jband
     end do !iband
!!! total time
     call CPU_time(fatime) 
     ttime = fatime-iatime
     write(*,"(A12,F10.2,A9)")'CalPmn: t=',ttime/60,' minutes'
!!! total time
  end do !izeta (layers)
  !
END SUBROUTINE lpmn
!#####  cal P^a_mn = (1/2)*Sum_{G,G'} c*_{m,G'para,G'perp,s} Sum_Gperp c_{n,G'para,Gperp,s} 
!                   ( 2 k^a + 2 G'para ( \delta_{a,x} + \delta_{a,y} ) + ( Gperp + G'perp )\delta_{a,z} ) f(g-g')
!       G=G_parallel+ Gperp 
!       s = up or dn
!       PRB, vol.63, 205406 (2001)
!       NEW Version, \delta_{G_parallel,G'_parallel} implicitely taken
!       12/02/05
SUBROUTINE slpmn(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
  IMPLICIT NONE
  INTEGER :: i,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  INTEGER :: cmax,imax,icon,nkpt
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: Lslab,Lg
  DOUBLE PRECISION, DIMENSION(Nlayers) :: delta,zeta,deltab,deltaf
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  DOUBLE PRECISION, DIMENSION(3) :: kp
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX, ALLOCATABLE :: cf(:,:)
  DOUBLE COMPLEX :: ci,cero
  DOUBLE COMPLEX :: cgnup,cgndn,cgmup,cgmdn
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp,ctmp1
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp2 ! Only to check
  DOUBLE PRECISION, DIMENSION(3) :: kmg,kmg12
  ! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
!     write(u_log,*)ik,ix,iy,iz
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
!  write(u_log,*)-minx,maxx,-miny,maxy,-minz,maxz
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  enddo
  allocate (cf(-minz-maxz:maxz+minz,Nlayers))
  do izeta = 1,Nlayers ! number of layers
     do jz = -minz-maxz,maxz+minz  ! g-g'
        if ( jz==0 ) then
           cf(jz,izeta)=cmplx(( deltab(izeta)+deltaf(izeta) )/Lslab,0.)
        else
           Lg = Lslab* jz * b3(3) 
           cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab)) &
                -exp(ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab)))
        endif
     end do
  end do
!!!
  do izeta = 1,Nlayers
!!! writes only diagonal and upper diagonal \cal P are hermitian
!!! counter
     cmax = nbandk*(nbandk+1)/2
     imax = 0
     icon = 1
     call CPU_time (iatime)
     call CPU_time (itime)!!! counter
     do iband=1,nbandk !n
        do jband=iband,nbandk !m
           ctmp1=cero
           ctmp2=cero
           do ipwp=1,npw
              ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
              realcg=cg(1,iband,1,ipwp) !iband => n,up
              imagcg=cg(2,iband,1,ipwp) !iband => n,up
              cgnup=dcmplx(realcg,imagcg)
              realcg=cg(1,iband,2,ipwp) !iband => n,dn
              imagcg=cg(2,iband,2,ipwp) !iband => n,dn
              cgndn=dcmplx(realcg,imagcg)
              kmg12 = 2.*( kp(1) + kg(1,ipwp) )*b1 + 2.*( kp(2) + kg(2,ipwp) )*b2
!!!
!!! sum over G_perp only
!!! it gives same as full sum
!!! tiny differences only when values are ~E-15 or smaller
!!!
                 do jz =-minz,maxz
                    ipw = arr(ix,iy,jz)
                    if(ipw.ne.0) then
                       realcg=cg(1,jband,1,ipw) !jband => m,up
                       imagcg=cg(2,jband,1,ipw) !jband => m,up
                       cgmup=dcmplx(realcg,imagcg)
                       realcg=cg(1,jband,2,ipw) !jband => m,dn
                       imagcg=cg(2,jband,2,ipw) !jband => m,dn
                       cgmdn=dcmplx(realcg,imagcg)
                       kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
                       ctmp=(CONJG(cgnup)*cgmup + CONJG(cgndn)*cgmdn)*kmg*cf(jz-iz,izeta)
                       ctmp1= ctmp1 + ctmp
                    end if
                 end do ! jz (Gperp)
!!!
           end do ! ipw  (Gpara,Gperp)
!!! with sum over G_perp only
!           write(unitS+izeta-1,"(6E18.8)")dreal(ctmp1(1)/2.),dimag(ctmp1(1)/2.)&
!                ,dreal(ctmp1(2)/2.),dimag(ctmp1(2)/2.)&
!                ,dreal(ctmp1(3)/2.),dimag(ctmp1(3)/2.) 
           write(unitS+izeta-1,*)dreal(ctmp1(1)/2.),dimag(ctmp1(1)/2.)&
                ,dreal(ctmp1(2)/2.),dimag(ctmp1(2)/2.)&
                ,dreal(ctmp1(3)/2.),dimag(ctmp1(3)/2.) 
!!!
!!! counter
           imax = imax + 1
           if ( imax.eq.(int(icon*(cmax/5)))) then
              icon = icon + 1
              call CPU_time(ftime)
              ttime = ftime-itime
              write(*,"(A18, I4, A2, I7, A22,I7,A5, F10.2 )")'spincalPmn: layer',izeta,'. ',imax,' cycles of a total of',cmax,'. t=',ttime/60.
              call CPU_time(itime) 
           end if
!!! counter
!!! time estimate
        if((izeta.eq.1).and.(ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt*Nlayers/50
           write(*,"(A28, F10.2, A13,I5,A13,I4  )")'spincalPmn: estimated time=',ttime/60.,' minutes for',nkpt,' k-points. L=',Nlayers
        end if
!!!
        end do !jband
     end do !iband
!!! total time
     call CPU_time(fatime) 
     ttime = fatime-iatime
     write(*,"(A16,F10.2,A9)")'spincalPmn: t=',ttime/60,' minutes'
!!! total time
  end do !izeta (layers)
  !
END SUBROUTINE slpmn
!#####
!       cal rho_mn m=c n=c'   
!#####  cal rho_mn = Sum_{G,G'} c*_{m,G'para,G'perp,s} Sum_Gperp c_{n,G'para,Gperp,s}*f(g-g') 
!       G=G_parallel+ Gperp 
!       s = up or dn
!       see notes zeta-rho.pdf
!       NEW Version, \delta_{G_parallel,G'_parallel} implicitely taken
!       12/02/05
SUBROUTINE srhoccp(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt,nval)
  IMPLICIT NONE
  INTEGER :: i,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  INTEGER :: cmax,imax,icon,nkpt
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: Lslab,Lg
  DOUBLE PRECISION, DIMENSION(Nlayers) :: delta,zeta,deltab,deltaf
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  DOUBLE PRECISION, DIMENSION(3) :: kp
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX, ALLOCATABLE :: cf(:,:)
  DOUBLE COMPLEX :: ci,cero
  DOUBLE COMPLEX :: cgnup,cgndn,cgmup,cgmdn
  DOUBLE COMPLEX  :: ctmp,ctmp1
  INTEGER :: nval

  ! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
!     write(u_log,*)ik,ix,iy,iz
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  enddo
  allocate (cf(-minz-maxz:maxz+minz,Nlayers))
  do izeta = 1,Nlayers ! number of layers
     do jz = -minz-maxz,maxz+minz  ! g-g'
        if ( jz==0 ) then
           cf(jz,izeta)=cmplx(( deltab(izeta)+deltaf(izeta) )/Lslab,0.)
        else
           Lg = Lslab* jz * b3(3) 
           cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab)) &
                -exp(ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab)))
        endif
     end do
  end do
!!!
  do izeta = 1,Nlayers
!!!  writes only rho_cc'
!!! counter
     cmax = (nbandk-nval)*(nbandk-nval+1)/2
     imax = 0
     icon = 1
     call CPU_time (iatime)
     call CPU_time (itime)
!!!  rho_{cc'}. i.e. among the conduction bands only
        do iband=nval+1,nbandk
           do jband=iband,nbandk
!!!
        ctmp1=cero
        do ipwp=1,npw
           ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
           realcg=cg(1,iband,1,ipwp) !iband => n,up
           imagcg=cg(2,iband,1,ipwp) !iband => n,up
           cgnup=dcmplx(realcg,imagcg)
           realcg=cg(1,iband,2,ipwp) !iband => n,dn
           imagcg=cg(2,iband,2,ipwp) !iband => n,dn
           cgndn=dcmplx(realcg,imagcg)
!!!
!!! sum over G_perp only
!!! it gives same as full sum
!!! tiny differences only when values are ~E-15 or smaller
!!!
           do jz =-minz,maxz
              ipw = arr(ix,iy,jz)
              if(ipw.ne.0) then
                 realcg=cg(1,jband,1,ipw) !jband => m,up
                 imagcg=cg(2,jband,1,ipw) !jband => m,up
                 cgmup=dcmplx(realcg,imagcg)
                 realcg=cg(1,jband,2,ipw) !jband => m,dn
                 imagcg=cg(2,jband,2,ipw) !jband => m,dn
                 cgmdn=dcmplx(realcg,imagcg)
                 ctmp=(CONJG(cgnup)*cgmup + CONJG(cgndn)*cgmdn)*cf(jz-iz,izeta)
                 ctmp1= ctmp1 + ctmp
              end if
           end do ! jz (Gperp)
!!!
        end do ! ipw  (Gpara,Gperp)
!!! with sum over G_perp only
!        write(unitS+izeta-1,"(3E18.8)")real(ctmp1),aimag(ctmp1)
        write(unitS+izeta-1,*)real(ctmp1),aimag(ctmp1)
!!!
!!! counter
        imax = imax + 1
        if ( imax.eq.(int(icon*(cmax/5)))) then
           icon = icon + 1
           call CPU_time(ftime)
           ttime = ftime-itime
           write(*,"(A18, I4, A2, I7, A22,I7,A5, F10.2 )")'rhoccp: layer',izeta,'. ',imax,' cycles of a total of',cmax,'. t=',ttime/60.
           call CPU_time(itime) 
        end if
!!! counter
!!! time estimate
        if((izeta.eq.1).and.(ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt*Nlayers/50
           write(*,"(A28, F10.2, A13,I5,A13,I4  )")'rhoccp: estimated time=',ttime/60.,' minutes for',nkpt,' k-points. L=',Nlayers
        end if
!!! 
     end do !jband
  end do !iband
!!! total time
  call CPU_time(fatime) 
  ttime = fatime-iatime
  write(*,"(A16,F10.2,A9)")'rhoccp: t=',ttime/60,' minutes'
!!! total time
end do !izeta (layers)
!
END SUBROUTINE srhoccp
!####
SUBROUTINE slpmm(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
  IMPLICIT NONE
  INTEGER :: i,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  INTEGER :: cmax,imax,icon,nkpt
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: Lslab,Lg
  DOUBLE PRECISION, DIMENSION(Nlayers) :: delta,zeta,deltab,deltaf
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  DOUBLE PRECISION, DIMENSION(3) :: kp
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX, ALLOCATABLE :: cf(:,:)
  DOUBLE COMPLEX :: ci,cero
  DOUBLE COMPLEX :: cgnup,cgndn,cgmup,cgmdn
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp,ctmp1
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp2 ! Only to check
  DOUBLE PRECISION, DIMENSION(3) :: kmg,kmg12
  ! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  enddo
  allocate (cf(-minz-maxz:maxz+minz,Nlayers))
  do izeta = 1,Nlayers ! number of layers
     do jz = -minz-maxz,maxz+minz  ! g-g'
        if ( jz==0 ) then
           cf(jz,izeta)=cmplx(( deltab(izeta)+deltaf(izeta) )/Lslab,0.)
        else
           Lg = Lslab* jz * b3(3) 
           cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab)) &
                -exp(ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab)))
        endif
     end do
  end do
!!!
  do izeta = 1,Nlayers
!!! counter
     cmax = nbandk
     imax = 0
     icon = 1
     call CPU_time (iatime)
     call CPU_time (itime)!!! counter
     do iband=1,nbandk !n
!!! pmm only!!!
!        do jband=iband,nbandk !m
        jband=iband
        ctmp1=cero
        ctmp2=cero
        do ipwp=1,npw
           ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
           realcg=cg(1,iband,1,ipwp) !iband => n,up
           imagcg=cg(2,iband,1,ipwp) !iband => n,up
           cgnup=dcmplx(realcg,imagcg)
           realcg=cg(1,iband,2,ipwp) !iband => n,dn
           imagcg=cg(2,iband,2,ipwp) !iband => n,dn
           cgndn=dcmplx(realcg,imagcg)
           kmg12 = 2.*( kp(1) + kg(1,ipwp) )*b1 + 2.*( kp(2) + kg(2,ipwp) )*b2
!!!
!!! sum over G_perp only
!!! it gives same as full sum
!!! tiny differences only when values are ~E-15 or smaller
!!!
           do jz =-minz,maxz
              ipw = arr(ix,iy,jz)
              if(ipw.ne.0) then
                 realcg=cg(1,jband,1,ipw) !jband => m,up
                 imagcg=cg(2,jband,1,ipw) !jband => m,up
                 cgmup=dcmplx(realcg,imagcg)
                 realcg=cg(1,jband,2,ipw) !jband => m,dn
                 imagcg=cg(2,jband,2,ipw) !jband => m,dn
                 cgmdn=dcmplx(realcg,imagcg)
                 kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
                 ctmp=(CONJG(cgnup)*cgmup + CONJG(cgndn)*cgmdn)*kmg*cf(jz-iz,izeta)
                 ctmp1= ctmp1 + ctmp
              end if
           end do ! jz (Gperp)
!!!
        end do ! ipw  (Gpara,Gperp)
!!! with sum over G_perp only
!        write(unitS+izeta-1,"(6E18.8)")dreal(ctmp1(1)/2.),dreal(ctmp1(2)/2.),dreal(ctmp1(3)/2.)
        write(unitS+izeta-1,*)dreal(ctmp1(1)/2.),dreal(ctmp1(2)/2.),dreal(ctmp1(3)/2.)
!!!
!!! counter
        imax = imax + 1
        if ( imax.eq.(int(icon*(cmax/5)))) then
           icon = icon + 1
           call CPU_time(ftime)
           ttime = ftime-itime
           write(*,"(A18, I4, A2, I7, A22,I7,A5, F10.2 )")'slpmm: layer',izeta,'. ',imax,' cycles of a total of',cmax,'. t=',ttime/60.
           call CPU_time(itime) 
        end if
!!! counter
!!! time estimate
        if((izeta.eq.1).and.(ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt*Nlayers/50
           write(*,"(A28, F10.2, A13,I5,A13,I4  )")'slpmm: estimated time=',ttime/60.,' minutes for',nkpt,' k-points. L=',Nlayers
        end if
!!!
!!! pmm only!!!
!!!     end do !jband
  end do !iband
!!! total time
  call CPU_time(fatime) 
  ttime = fatime-iatime
  write(*,"(A16,F10.2,A9)")'slpmm: t=',ttime/60,' minutes'
!!! total time
end do !izeta (layers)
!
END SUBROUTINE slpmm
!####
!#####  
!!! (hbar/2)(S(z)sigma^x)_mn =  (hbar/2)*Sum_{G'}[ c*_{m,G'para,G'perp,up} Sum_Gperp c_{n,G'para,Gperp,dn}f(g-g') 
!!!                                               +c*_{m,G'para,G'perp,dn} Sum_Gperp c_{n,G'para,Gperp,up}f(g-g') ]
!!!
!!! (hbar/2)(S(z)sigma^y)_mn = i(hbar/2)*Sum_{G'}[ -c*_{m,G'para,G'perp,up} Sum_Gperp c_{n,G'para,Gperp,dn}f(g-g') 
!!!                                               + c*_{m,G'para,G'perp,dn} Sum_Gperp c_{n,G'para,Gperp,up}f(g-g') ]
!!!
!!! (hbar/2)(S(z)sigma^z)_mn =  (hbar/2)*Sum_{G'}[  c*_{m,G'para,G'perp,up} Sum_Gperp c_{n,G'para,Gperp,up}f(g-g') 
!!!                                               - c*_{m,G'para,G'perp,dn} Sum_Gperp c_{n,G'para,Gperp,dn}f(g-g') ]
!!! {\hat S} = (hbar/2) {\hat sigma}
!       G=G_parallel+ Gperp(=g)
!       NEW Version, \delta_{G_parallel,G'_parallel} implicitely taken
!       17/05/05
SUBROUTINE lsccp(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kg,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt,nval)
  IMPLICIT NONE
  INTEGER, DIMENSION(3,npw) :: kg
  DOUBLE PRECISION, DIMENSION(3) :: b3
  INTEGER :: i,ik,nband,mband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  INTEGER :: cmax,imax,icon,nkpt
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: hbar,Lslab,Lg
  DOUBLE PRECISION, DIMENSION(Nlayers) :: delta,zeta,deltab,deltaf
  DOUBLE COMPLEX :: cgnup,cgndn,cgmup,cgmdn
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX, ALLOCATABLE :: cf(:,:)
  DOUBLE COMPLEX :: ci,cero
  DOUBLE COMPLEX :: sx,sy,sz,ctmp
  INTEGER :: Nval

  IF ( nspinor==1 ) STOP 'What are you doing?  This only work with spinors!'
  ! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  hbar=1.
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
!     write(u_log,*)ik,ix,iy,iz
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
!  write(u_log,*)-minx,maxx,-miny,maxy,-minz,maxz
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  enddo
  allocate (cf(-minz-maxz:maxz+minz,Nlayers))
  do izeta = 1,Nlayers ! number of layers
     do jz = -minz-maxz,maxz+minz  ! g-g'
        if ( jz==0 ) then
           cf(jz,izeta)=cmplx(( deltab(izeta)+deltaf(izeta) )/Lslab,0.)
        else
           Lg = Lslab* jz * b3(3) 
           cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab)) &
                -exp(ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab)))
        endif
     end do
  end do
!!!
  do izeta = 1,Nlayers
!!!  writes only Scc'
!!! counter
     cmax = (nbandk-nval)*(nbandk-nval+1)/2
     imax = 0
     icon = 1
     call CPU_time (iatime)
     call CPU_time (itime)!
!!! We only need S_{cc'}. i.e. among the conduction bands only
     do nband=nval+1,nbandk !n
        do mband=nband,nbandk !m
           sx=cero
           sy=cero
           sz=cero
           do ipwp=1,npw
              ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
              cgnup  = cg(1,nband,1,ipwp) + ci * cg(2,nband,1,ipwp)
              cgndn  = cg(1,nband,2,ipwp) + ci * cg(2,nband,2,ipwp)
!!!
!!! sum over G_perp only
!!! it gives same as full sum
!!! tiny differences only when values are ~E-15 or smaller
!!!
              do jz =-minz,maxz
                 ipw = arr(ix,iy,jz)
                 if(ipw.ne.0) then
                    cgmup  = cg(1,mband,1,ipw) + ci * cg(2,mband,1,ipw)
                    cgmdn  = cg(1,mband,2,ipw) + ci * cg(2,mband,2,ipw)
!!!
!!!!!!!!!!!!!! 
!!! The factor (hbar/2) is leaved out.
!!! If the DSP is calculated, then (hbar/2) is properly taken into account
!!!                           in the expression for the DSP, and thus is not needed.
!!! If $\zeta^{abc}$ needs to be reported, then multiply by (hbar/2)
!!!                                        and convert to the appropriate S.I. units.!
!!!!!!!!!!!!!! 
!!! x-component
!                    if(jz.eq.iz)then
!                       ctmp =    (hbar/2.)*(  CONJG(cgnup)*cgmdn + CONJG(cgndn)*cgmup )*cf(jz-iz,izeta)
                       ctmp = (  CONJG(cgnup)*cgmdn + CONJG(cgndn)*cgmup )*cf(jz-iz,izeta)
                       sx= sx + ctmp
!                    endif
!!! y-component
!                    ctmp = ci*(hbar/2.)*( -CONJG(cgnup)*cgmdn + CONJG(cgndn)*cgmup )*cf(jz-iz,izeta)
                    ctmp = ci*( -CONJG(cgnup)*cgmdn + CONJG(cgndn)*cgmup )*cf(jz-iz,izeta)
                    sy = sy + ctmp
!!! z-component
!                    ctmp =    (hbar/2.)*(  CONJG(cgnup)*cgmup - CONJG(cgndn)*cgmdn )*cf(jz-iz,izeta)
                    ctmp = (  CONJG(cgnup)*cgmup - CONJG(cgndn)*cgmdn )*cf(jz-iz,izeta)
                    sz = sz + ctmp
                 end if
              end do ! jz (Gperp)
!!!
!!! for full sum see ncalPmns
!!!
              if ( 1 .eq. 2 ) then
                 do jz =1,npw
                    if((ix.eq.kg(1,jz)).and.(iy.eq.kg(2,jz))) then
                       if(iz.eq.kg(3,jz))then
                       ipw = arr(ix,iy,kg(3,jz))
                       cgmup  = cg(1,mband,1,ipw) + ci * cg(2,mband,1,ipw)
                       cgmdn  = cg(1,mband,2,ipw) + ci * cg(2,mband,2,ipw)
!                       ctmp =    (hbar/2.)*(  CONJG(cgnup)*cgmdn + CONJG(cgndn)*cgmup )
                       ctmp =    (  CONJG(cgnup)*cgmdn + CONJG(cgndn)*cgmup )
                       sx= sx + ctmp
                       end if
                    end if
                 end do ! jz (Gperp)
              endif
!!!
           end do ! ipw  (Gpara,Gperp)
!!! with sum over G_perp only
!           write(unitS+izeta-1,"(6E18.8)")real(sx),aimag(sx)&
!                                         ,real(sy),aimag(sy)&
!                                         ,real(sz),aimag(sz)
           write(unitS+izeta-1,*)real(sx),aimag(sx)&
                                         ,real(sy),aimag(sy)&
                                         ,real(sz),aimag(sz)
!!!
!!! counter
           imax = imax + 1
           if ( imax.eq.(int(icon*(cmax/5)))) then
              icon = icon + 1
              call CPU_time(ftime)
              ttime = ftime-itime
              write(*,"(A22, I4, A2, I7, A22,I7,A5, F10.2 )")'CalSccp: layer',izeta,'.',imax,' cycles of a total of',cmax,'. t=',ttime/60.
              call CPU_time(itime) 
           end if
!!! counter
!!! time estimate
        if((izeta.eq.1).and.(ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt*Nlayers/50
           write(*,"(A28, F10.2, A13,I5,A13,I4  )")'CalSccp: estimated time=',ttime/60.,' minutes for',nkpt,' k-points and L=',Nlayers
        end if
!!!
        end do !mband
     end do !nband
!!! total time
     call CPU_time(fatime) 
     ttime = fatime-iatime
     write(*,"(A10,F10.2,A9)")'CalSccp: t=',ttime/60.,' minutes'
!!! total time
!!!
  end do !izeta (layers)
!!!
END SUBROUTINE lsccp

!#####  \rho(z) = 1/L\sum_{G'_\|,G'_\perp} c*_{n,G\|',G'\perp} \sum_{G_\perp} c_{n\G'_\|G_\perp} 
!                & exp(i(G_\perp - G'_\perp))
!       G=G_\|+ G_\perp
!       NEW Version, \delta_{G_parallel,G'_parallel} implicitely taken
!       14/02/05
SUBROUTINE rhoz(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kg,b1,b2,b3,cg,zmax,zmesh)

  IMPLICIT NONE
  INTEGER :: i,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: zmax
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  DOUBLE PRECISION :: realcg,imagcg
  DOUBLE PRECISION, DIMENSION(3) :: kp
  INTEGER, DIMENSION(3,npw) :: kg
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(zmax) :: zmesh
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX :: ci,cgndn,cgnup,cgmdn,cgmup,ctmp,ctmp1,cero,ctmp2
!!! writes the header
  write(unitS,*)  '# band   zmesh      real(rho)   imaginary(rho)'
  write(unitS+1,*)'# band   zmesh      real(psi)   imaginary(psi)'
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     write(u_log,*)ik,ix,iy,iz
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
  write(u_log,*)-minx,maxx,-miny,maxy,-minz,maxz
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  end do
  !
  do iband=1,nbandk
     do izeta = 1,zmax !cycle over zmesh, that goes from 0 to Lslab
        ctmp1=cero
        ctmp2=cero
        do ipwp=1,npw
           ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
           realcg=cg(1,iband,1,ipwp) !iband => n 
           imagcg=cg(2,iband,1,ipwp) !iband => n
           cgnup=dcmplx(realcg,imagcg)
           if(nspinor==2)then
              realcg=cg(1,iband,2,ipwp) !iband => n,dn
              imagcg=cg(2,iband,2,ipwp) !iband => n,dn
              cgndn=dcmplx(realcg,imagcg)
           end if
! constructs the wave-function \psi_{n\bfk}(z)=\sum_\bfG c_{n\bfk)(\bfG)\gd_{\bfG,0}e^{i\bfG\cdot\bff}
           if ( (ix.eq.0).and.(iy.eq.0)) then
              ctmp2=ctmp2+cgnup*exp( ci*b3(3)*iz*zmesh(izeta) ) !\psi(z,nk)
           end if
!
           do jz =-minz,maxz
              ipw  = arr(ix,iy,jz)
              if(ipw.ne.0) then
                 realcg=cg(1,iband,1,ipw) !iband => n
                 imagcg=cg(2,iband,1,ipw) !iband => n
                 cgmup=dcmplx(realcg,imagcg)
                 if(nspinor==2)then
                    realcg=cg(1,iband,2,ipw) !iband => m,dn
                    imagcg=cg(2,iband,2,ipw) !iband => m,dn
                    cgmdn=dcmplx(realcg,imagcg)
                 end if
                 if(nspinor==1) then
                    ctmp=CONJG(cgnup)*cgmup*exp( ci*b3(3)*(jz-iz)*zmesh(izeta) )
                 end if
                 if(nspinor==2) then
                    ctmp=(CONJG(cgnup)*cgmup+CONJG(cgndn)*cgmdn)*exp( ci*b3(3)*(jz-iz)*zmesh(izeta) )
                 end if
                 ctmp1= ctmp1 + ctmp
              end if
           end do ! jz
        end do  ! ipw
!        write(unitS,69)iband,(Lslab+zmin)-zeta*Lslab,dreal(ctmp1) !assumes zmin < 0
        write(unitS,69)iband,zmesh(izeta),real(ctmp1),aimag(ctmp1) !assumes zmin < 0
        write(unitS+1,69)iband,zmesh(izeta),real(ctmp2),aimag(ctmp2) !assumes zmin < 0
     end do  !izeta
     write(unitS,69)
     write(unitS+1,69)
     write(unitS+1,69)
  end do !iband
69 format(i5,f11.4,2e14.5)
END SUBROUTINE rhoz

SUBROUTINE lpmm(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
  IMPLICIT NONE
  INTEGER :: i,j,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  INTEGER :: cmax,imax,icon,nkpt,ios
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: Lslab,Lg,gz
  DOUBLE PRECISION, DIMENSION(Nlayers) :: delta,zeta,deltab,deltaf
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  INTEGER, DIMENSION(256) :: iproper
  DOUBLE PRECISION, DIMENSION(3) :: kp
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX, ALLOCATABLE :: cf(:,:)
  DOUBLE COMPLEX :: ci,cgn,cgm,cero
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp,ctmp1
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp2 ! Only to check
  DOUBLE PRECISION, DIMENSION(3) :: kmg,kmg12
  CHARACTER(len=3) :: ifSym
  ! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
! finds if the slab is centrosymmetric
!!$  OPEN(UNIT=81, FILE="ifcentrosymmetric",status="old",IOSTAT=ios)
!!$  IF (ios.NE.0) THEN
!!$     WRITE(*,*) "*********"
!!$     STOP "Stopping: Could not open file ifcentrosymmetric"
!!$  END IF
!!$  READ(81,*) ifSym
!!$  ifSym=trim(ifSym)
!!$  if(ifSym.eq."no") WRITE(*,*)"slab is ",ifSym,"centrosymmetric"
!!$  if(ifSym.eq."yes") WRITE(*,*)"slab is centrosymmetric"

  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
!     write(u_log,*)ik,ix,iy,iz
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
!write(u_log,*)'min-max ',-minx,maxx,-miny,maxy,-minz,maxz
!write(u_log,*)'b1-b3 ',b1,b2,b3
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  end do
  allocate (cf(-minz-maxz:maxz+minz,Nlayers))
!!! build f_\ell(g_z) function
  do izeta = 1,Nlayers ! number of layers
!!! selects given planes
!!!     if (izeta.eq.9)  izeta=10
!!!     if (izeta.eq.10) izeta=14
!!!     if (izeta.eq.11) izeta=19
!!!
     do jz = -minz-maxz,maxz+minz  ! g-g'
        if ( jz==0 ) then
           cf(jz,izeta)=cmplx(( deltab(izeta)+deltaf(izeta) )/Lslab,0.)
        else
           gz = jz * b3(3) 
           Lg = Lslab* jz * b3(3) 
!           if(ifSym.eq."no") then
              cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab))  &
                                  -exp(ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab))) 
!           else
!              cf(jz,izeta)=cmplx(2./(Lslab*gz) * cos(gz*(zeta(izeta)+(deltaf(izeta)-deltab(izeta))/2.)) &
!                   * sin(gz*(deltaf(izeta)+deltab(izeta))/2.),0.)
!           end if
!           if(debug)write(u_log,*)izeta,jz,flgz(jz,izeta),real(cf(jz,izeta))
        endif
     end do
  end do
  !
  ! for now for no spin cg(1:2,jband,ispinor,ipw)
  !
!!! Loop over layers
  do izeta = 1,Nlayers
     cmax = nbandk
     imax = 0
     icon = 1
     call CPU_time (iatime)
     call CPU_time (itime)
!!! counter
     do iband=1,nbandk !n
!!! pmm only!!!
!        do jband=iband,nbandk !m
        jband=iband
           ctmp1=cero
           ctmp2=cero
           do ipwp=1,npw
              ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
              realcg=cg(1,iband,1,ipwp) !iband => n
              imagcg=cg(2,iband,1,ipwp) !iband => n
              cgn=dcmplx(realcg,imagcg)
              ! only for b1(i) \cdot b2(i) =0?  b1(i) \cdot b3(i) = b2(i) \cdot b3(i) = 0 for sure
              kmg12 = 2.*( kp(1) + kg(1,ipwp) )*b1 + 2.*( kp(2) + kg(2,ipwp) )*b2
!!! sum over G_perp only
!!! it gives same as full sum
!!! tiny differences only when values are ~E-15 or smaller
!!!
              if ( 1 .eq. 1 ) then
                 do jz =-minz,maxz
                    ipw = arr(ix,iy,jz)
                    if(ipw.ne.0) then
                       realcg=cg(1,jband,1,ipw) !jband => m
                       imagcg=cg(2,jband,1,ipw) !jband => m
                       cgm=dcmplx(realcg,imagcg)
                       ! only for b1(i) \cdot b2(i) =0?  b1(i) \cdot b3(i) = b2(i) \cdot b3(i) = 0 for sure
                       kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
!                       ctmp=CONJG(cgn)*cgm*kmg*(cf(jz-iz,izeta)-cf(iz-jz,izeta))
                       ctmp=CONJG(cgn)*cgm*kmg*cf(jz-iz,izeta)
                       ctmp1= ctmp1 + ctmp
                    end if
                 end do ! jz (Gperp)
              endif
61            format(3i5,f8.4,8e13.5)
!!!!!!!!!!!!!!!!!!!!!! full sum
!!! activate if need to check with above
!!! sum over G_perp only
              if ( 1 .eq. 2 ) then
                 do jz =1,npw
                    if((ix.eq.kg(1,jz)).and.(iy.eq.kg(2,jz))) then
                       ipw = arr(ix,iy,kg(3,jz))
                       realcg=cg(1,jband,1,ipw) !jband => m
                       imagcg=cg(2,jband,1,ipw) !jband => m
                       cgm=dcmplx(realcg,imagcg)
                       kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
                       ctmp=2.*CONJG(cgn)*cgm*cf(kg(3,jz)-iz,izeta)
                       ctmp1= ctmp1 + ctmp
                    end if
                 end do ! jz (Gperp)
              endif
!!!!!!!!!!!!!!!!!!!!!!!!
           end do ! ipw  (Gpara,Gperp)
!!! with sum over G_perp only
!!! writes for a given layer!
!           write(unitS+izeta-1,"(6E18.8)")dreal(ctmp1(1)/2.),dreal(ctmp1(2)/2.),dreal(ctmp1(3)/2.)
           write(unitS+izeta-1,*)dreal(ctmp1(1)/2.),dreal(ctmp1(2)/2.),dreal(ctmp1(3)/2.)
!           write(unitS+izeta-1,"(6E18.8)")dreal(ctmp1(1)/2.),dimag(ctmp1(1)/2.)&
!                ,dreal(ctmp1(2)/2.),dimag(ctmp1(2)/2.)&
!                ,dreal(ctmp1(3)/2.),dimag(ctmp1(3)/2.)
!!! with full
!           if (debug) then
!              write(unitS+Nlayers+izeta-1,"(6E18.8)")dreal(ctmp2(1)/2.),dimag(ctmp2(1)/2.)&
!                   ,dreal(ctmp2(2)/2.),dimag(ctmp2(2)/2.)&
!                   ,dreal(ctmp2(3)/2.),dimag(ctmp2(3)/2.) 
!           end if
!!!!!!!
!!! counter
           imax = imax + 1
           if ( imax.eq.(int(icon*(cmax/5)))) then
              icon = icon + 1
              call CPU_time(ftime)
              ttime = ftime-itime
              write(*,"(A14, I4, A6,I4, I7, A22,I7,A5, F10.2 )")'jnnlk: layer',izeta,' fort.',unitS+izeta-1,imax,' cycles of a total of',cmax,'. t=',ttime/60.
              call CPU_time(itime) 
           end if
!!! counter
!!! time estimate
        if((izeta.eq.1).and.(ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt*Nlayers/50
           write(*,"(A24, F10.2, A13,I5,A13,I4  )")'jnnlk: estimated time=',ttime/60.,' minutes for',nkpt,' k-points. L=',Nlayers
        end if
!!!
!!! pmm only!!!
!!!     end do !jband
  end do !iband
!!! total time
     call CPU_time(fatime) 
     ttime = fatime-iatime
     write(*,"(A12,F10.2,A9)")'jnnlk: t=',ttime/60,' minutes'
!!! total time
  end do !izeta (layers)

END SUBROUTINE lpmm

SUBROUTINE rhomm(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
  IMPLICIT NONE
  INTEGER :: i,j,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  INTEGER :: cmax,imax,icon,nkpt,ios
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: Lslab,Lg,gz
  DOUBLE PRECISION, DIMENSION(Nlayers) :: delta,zeta,deltab,deltaf
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  INTEGER, DIMENSION(256) :: iproper
  DOUBLE PRECISION, DIMENSION(3) :: kp
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX, ALLOCATABLE :: cf(:,:)
  DOUBLE COMPLEX :: ci,cgn,cgm,cero
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp,ctmp1
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp2 ! Only to check
  DOUBLE PRECISION, DIMENSION(3) :: kmg,kmg12
  CHARACTER(len=3) :: ifSym
!!! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  end do
  allocate (cf(-minz-maxz:maxz+minz,Nlayers))
!!! build f_\ell(g_z) function
  do izeta = 1,Nlayers ! number of layers
     do jz = -minz-maxz,maxz+minz  ! g-g'
        if ( jz==0 ) then
           cf(jz,izeta)=cmplx(( deltab(izeta)+deltaf(izeta) )/Lslab,0.)
        else
           gz = jz * b3(3) 
           Lg = Lslab* jz * b3(3) 
           cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab))  &
                -exp(ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab))) 
        endif
     end do
  end do
  !
  ! for now for no spin cg(1:2,jband,ispinor,ipw)
  !
!!! Loop over layers
  do izeta = 1,Nlayers
     cmax = nbandk
     imax = 0
     icon = 1
     call CPU_time (iatime)
     call CPU_time (itime)
!!! counter
     do iband=1,nbandk !n
!!! pmm only!!!
!        do jband=iband,nbandk !m
        jband=iband
           ctmp1=cero
           ctmp2=cero
           do ipwp=1,npw
              ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
              realcg=cg(1,iband,1,ipwp) !iband => n
              imagcg=cg(2,iband,1,ipwp) !iband => n
              cgn=dcmplx(realcg,imagcg)
!!! sum over G_perp only
!!! it gives same as full sum
!!! tiny differences only when values are ~E-15 or smaller
!!!
              if ( 1 .eq. 1 ) then
                 do jz =-minz,maxz
                    ipw = arr(ix,iy,jz)
                    if(ipw.ne.0) then
                       realcg=cg(1,jband,1,ipw) !jband => m
                       imagcg=cg(2,jband,1,ipw) !jband => m
                       cgm=dcmplx(realcg,imagcg)
                       ctmp=CONJG(cgn)*cgm*cf(jz-iz,izeta)
                       ctmp1= ctmp1 + ctmp
                    end if
                 end do ! jz (Gperp)
              endif
61            format(3i5,f8.4,8e13.5)
!!!!!!!!!!!!!!!!!!!!!! full sum
!!! activate if need to check with above
!!! sum over G_perp only
              if ( 1 .eq. 2 ) then
                 do jz =1,npw
                    if((ix.eq.kg(1,jz)).and.(iy.eq.kg(2,jz))) then
                       ipw = arr(ix,iy,kg(3,jz))
                       realcg=cg(1,jband,1,ipw) !jband => m
                       imagcg=cg(2,jband,1,ipw) !jband => m
                       cgm=dcmplx(realcg,imagcg)
                       kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
                       ctmp=2.*CONJG(cgn)*cgm*cf(kg(3,jz)-iz,izeta)
                       ctmp1= ctmp1 + ctmp
                    end if
                 end do ! jz (Gperp)
              endif
!!!!!!!!!!!!!!!!!!!!!!!!
           end do ! ipw  (Gpara,Gperp)
!!! with sum over G_perp only
!!! writes for a given layer!
!           write(unitS+izeta-1,"(6E18.8)")dreal(ctmp1(1)),dreal(ctmp1(2)),dreal(ctmp1(3))
           write(unitS+izeta-1,*)dreal(ctmp1(1)),dreal(ctmp1(2)),dreal(ctmp1(3))
!!! counter
           imax = imax + 1
           if ( imax.eq.(int(icon*(cmax/5)))) then
              icon = icon + 1
              call CPU_time(ftime)
              ttime = ftime-itime
              write(*,"(A14, I4, A6,I4, I7, A22,I7,A5, F10.2 )")'rhomm: layer',izeta,' fort.',unitS+izeta-1,imax,' cycles of a total of',cmax,'. t=',ttime/60.
              call CPU_time(itime) 
           end if
!!! counter
!!! time estimate
        if((izeta.eq.1).and.(ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt*Nlayers/50
           write(*,"(A24, F10.2, A13,I5,A13,I4  )")'rhomm: estimated time=',ttime/60.,' minutes for',nkpt,' k-points. L=',Nlayers
        end if
!!!
!!! rhomm only!!!
!!!     end do !jband
  end do !iband
!!! total time
     call CPU_time(fatime) 
     ttime = fatime-iatime
     write(*,"(A12,F10.2,A9)")'rhomm: t=',ttime/60,' minutes'
!!! total time
  end do !izeta (layers)

END SUBROUTINE rhomm

!##################### matrix elements subroutines above #################

!##################### service subroutines below #################
!###  conditions on wf/matrix elements: conditions_me
!
!#### Checks hermiticity condition  <m|A|n>=<n|A|m>
!
SUBROUTINE hermiticity(ik,nbandk,Amn)
  
  IMPLICIT NONE
  INTEGER :: i,ik,m,n
  INTEGER :: nbandk
  DOUBLE COMPLEX, DIMENSION(3,nbandk,nbandk) ::  Amn
  
  do m=1,nbandk
     do n=m,nbandk
        do i=1,3
           if(abs(Amn(i,m,n)-conjg(Amn(i,n,m)))<=0.001d0) then
              !       write(66,*)ik,i,m,n,'   Hermiticity .... OK'
           else
              write(*,*)'STOP: Hermiticity of Pmn is NOT satisfied'
              write(*,*)ik,i,m,n,abs(Amn(i,m,n)-conjg(Amn(i,n,m)))
              STOP
           end if
        end do !i       
     end do ! n
  end do !m
  
END SUBROUTINE hermiticity

!##### check Normalization of the wavefunction for given k
!      Sum_spin Sum_G |cg|^2 = <mskG|mskG> = 1
SUBROUTINE normawfk(ik,npw,nbandk,nspinor,cg)
  
  IMPLICIT NONE
  INTEGER :: ipw,ik,iband,ispinor
  INTEGER :: npw,nbandk,nspinor
  DOUBLE PRECISION :: Nm, modcg
  DOUBLE PRECISION, ALLOCATABLE :: modkg(:)
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  
  ! To debug: To calculate |kg|, 
  !     kgc should be added as input variable in the subroutine
  !
  ! allocate(modkg(npw))
  ! do ipw=1,npw
  !  modkg(ipw)= kgc(1,ipw)*kgc(1,ipw)+kgc(2,ipw)*kgc(2,ipw)+&
  !      & kgc(3,ipw)*kgc(3,ipw)
  ! end do !ipw
  
  IF ( nspinor==2) STOP 'What are you doing?  This does not work with spinors!'
  
  DO iband=1,nbandk
     Nm=0.d0
     DO ispinor=1,nspinor
        DO ipw=1,npw
           !     <m|m>=1
           modcg = cg(1,iband,ispinor,ipw)*cg(1,iband,ispinor,ipw)&
                &  +cg(2,iband,ispinor,ipw)*cg(2,iband,ispinor,ipw)    
           Nm = Nm + modcg
           
        END DO !ipw
     END DO !ispinor
     IF ( ABS(Nm - 1.0d0) .GT. 10d-7 ) THEN
        STOP " Normalizations are not correct."
     END IF
  END DO !iband
  
END SUBROUTINE  normawfk
!##################### service subroutines above #################

!##################### service subroutines not used below #################
!#####  Sz_mn = Sum_{G,G'} c*_{mG'} c_{nG} X
!                X delta_{G_parallel,G'_parallel} f(g-g')
!       G=G_parallel+ g ^z
!       PRB, vol.63, 205406 (2001)
!       NEW Version, \delta_{G_parallel,G'_parallel} implicitely taken
!       8/02/05
!       THIS SUBROUTINE IS NOT REQUIRED AND THUS IS NOT UPDATED
!       IT NEEDS cxpmn FROM mme SUBROUTINE so if you wanna have it by my guest
SUBROUTINE snSzmn(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,cxpmn)
  
  IMPLICIT NONE
  INTEGER :: i,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  DOUBLE PRECISION :: Lslab,Lg
  DOUBLE PRECISION, DIMENSION(Nlayers) :: delta,zeta,deltab,deltaf
  DOUBLE COMPLEX, DIMENSION(3) :: calPmn
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX, ALLOCATABLE :: cf(:,:),nSzmn(:,:)
  DOUBLE COMPLEX, DIMENSION(3,nbandk,nbandk) :: cxpmn
  DOUBLE COMPLEX :: ci,cgm,cgn,ctmp,ctmp1,cero
  ! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
!     write(u_log,*)ik,ix,iy,iz
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
!  write(u_log,*)-minx,maxx,-miny,maxy,-minz,maxz
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  enddo
  allocate (cf(-minz-maxz:maxz+minz,Nlayers))
  ! f(g-g') (g = G_z) Symmetric Layer
  ! do izeta = 1,Nlayers ! number of layers
  !  do jz = -minz-maxz,maxz+minz  ! g-g'
  !     if ( jz==0 ) then
  !      cf(jz,izeta)=cmplx(delta(izeta)/Lslab,0.)
  !     else
  !      Lg = Lslab* jz * b3(3) 
  !      cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + delta(izeta)/Lslab/2.)) &
  !                             -exp(ci*Lg*(zeta(izeta)/Lslab - delta(izeta)/Lslab/2.)))
  !     endif
  !    end do
  ! end do
  ! f(g-g') (g = G_z) arbitrary Layer
  do izeta = 1,Nlayers ! number of layers
     do jz = -minz-maxz,maxz+minz  ! g-g'
        if ( jz==0 ) then
           cf(jz,izeta)=cmplx(( deltab(izeta)+deltaf(izeta) )/Lslab,0.)
        else
           Lg = Lslab* jz * b3(3) 
           cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab)) &
                -exp(ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab)))
        endif
     end do
  end do
  ! for now for no spin cg(1:2,jband,ispinor,ipw)
  !
  allocate (nSzmn(nbandk,nbandk))
  do izeta = 1,Nlayers
     nSzmn=cero
     do iband=1,nbandk
        do jband=1,nbandk
           ctmp1=dcmplx(0.d0,0.d0)
           do ipwp=1,npw
              ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
              realcg=cg(1,iband,1,ipwp) !iband => n 
              imagcg=cg(2,iband,1,ipwp) !iband => n
              cgn=dcmplx(realcg,imagcg)
              do jz =-minz,maxz
                 !        do jz =1,npw
                 !         if((ix.eq.kg(1,jz)).and.(iy.eq.kg(2,jz))) then
                 ipw  = arr(ix,iy,jz)
                 !         ipw  = arr(ix,iy,kg(3,jz))
                 if(ipw.ne.0) then
                    realcg=cg(1,jband,1,ipw) !iband => m
                    imagcg=cg(2,jband,1,ipw) !iband => m
                    cgm=dcmplx(realcg,imagcg)
                    ctmp=CONJG(cgn)*cgm*cf(jz-iz,izeta)
                    !         ctmp=CONJG(cxcgp)*cxcg*cf(kg(3,jz)-iz,izeta)
                    !         else
                    !         ctmp = cero
                    !         endif
                    ctmp1= ctmp1 + ctmp
                 endif
              end do ! jz
           end do ! ipw
           nSzmn(iband,jband)=ctmp1
!           write(unitS+izeta-1,"(6E18.8)")dreal(ctmp1),dimag(ctmp1)
           write(unitS+izeta-1,*)dreal(ctmp1),dimag(ctmp1)
        end do !jband
     end do !iband
     !  write(*,*)'calculating calPmn'
     !  writes only diagonal and upper diagonal since S and \cal P are hermitian
     do iband=1,nbandk
        do jband=iband,nbandk
           calPmn=cero
           do sband=1,nbandk
!!!
!!! to check 'completness' in calP
!!!              do sband=1,14
!!!
              calPmn(:)=calPmn(:) + (1.d0/2.d0)*( nSzmn(iband,sband)*cxpmn(:,sband,jband)&
                   +cxpmn(:,iband,sband)*nSzmn(sband,jband))
           end do !sband
!           write(unitS+Nlayers+izeta-1,"(6E18.8)")dreal(calPmn(1)),dimag(calPmn(1))&
!                ,dreal(calPmn(2)),dimag(calPmn(2))&
!                ,dreal(calPmn(3)),dimag(calPmn(3))
           write(unitS+Nlayers+izeta-1,*)dreal(calPmn(1)),dimag(calPmn(1))&
                ,dreal(calPmn(2)),dimag(calPmn(2))&
                ,dreal(calPmn(3)),dimag(calPmn(3))
        end do !jband
     end do !iband
  end do !izeta
END SUBROUTINE snSzmn
!#####  cal P^a_mn = (1/2)*Sum_{G',s} c*_{m,G'para,G'perp,s} Sum_Gperp c_{n,G'para,Gperp,s} 
!                   ( 2 k^a + 2 G'para ( \delta_{a,x} + \delta_{a,y} ) + ( Gperp + G'perp )\delta_{a,z} ) f(g-g')
!       G=G_parallel+ Gperp 
!       s = up or dn
!       PRB, vol.63, 205406 (2001)
!       NEW Version, \delta_{G_parallel,G'_parallel} implicitely taken
!       12/02/05
SUBROUTINE Snn(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kg,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
  IMPLICIT NONE
  INTEGER :: i,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  INTEGER :: cmax,imax,icon,nkpt
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: Lslab,Lg
  DOUBLE PRECISION, DIMENSION(Nlayers) :: delta,zeta,deltab,deltaf
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(3) :: b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX, ALLOCATABLE :: cf(:,:)
  DOUBLE COMPLEX :: ci,cero
  DOUBLE COMPLEX :: cgnup,cgndn,cgmup,cgmdn
  DOUBLE COMPLEX :: ctmp,ctmp1
  ! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
!     write(u_log,*)ik,ix,iy,iz
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
!  write(u_log,*)-minx,maxx,-miny,maxy,-minz,maxz
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  enddo
  allocate (cf(-minz-maxz:maxz+minz,Nlayers))
  do izeta = 1,Nlayers ! number of layers
     do jz = -minz-maxz,maxz+minz  ! g-g'
        if ( jz==0 ) then
           cf(jz,izeta)=cmplx(( deltab(izeta)+deltaf(izeta) )/Lslab,0.)
        else
           Lg = Lslab* jz * b3(3) 
           cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab)) &
                -exp(ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab)))
        endif
     end do
  end do
!!!
  do izeta = 1,Nlayers
     !  writes only diagonal and upper diagonal \cal P are hermitian
!!! counter
     cmax = nbandk*(nbandk+1)/2
     imax = 0
     icon = 1
     call CPU_time (iatime)
     call CPU_time (itime)!
!!! counter
     do iband=1,nbandk !n
        do jband=iband,nbandk !m
           ! only does the diagonal part
           if(iband.eq.jband) then
              ctmp1=cero
              do ipwp=1,npw
                 ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
                 realcg=cg(1,iband,1,ipwp) !iband => n,up
                 imagcg=cg(2,iband,1,ipwp) !iband => n,up
                 cgnup=dcmplx(realcg,imagcg)
                 if(nspinor==2)then
                    realcg=cg(1,iband,2,ipwp) !iband => n,dn
                    imagcg=cg(2,iband,2,ipwp) !iband => n,dn
                    cgndn=dcmplx(realcg,imagcg)
                 end if
!!!
!!! sum over G_perp only
!!! it gives same as full sum
!!! tiny differences only when values are ~E-15 or smaller
!!!
                 do jz =-minz,maxz
                    ipw = arr(ix,iy,jz)
                    if(ipw.ne.0) then
                       realcg=cg(1,jband,1,ipw) !jband => m,up
                       imagcg=cg(2,jband,1,ipw) !jband => m,up
                       cgmup=dcmplx(realcg,imagcg)
                       if(nspinor==2)then
                          realcg=cg(1,jband,2,ipw) !jband => m,dn
                          imagcg=cg(2,jband,2,ipw) !jband => m,dn
                          cgmdn=dcmplx(realcg,imagcg)
                       end if
                       if(nspinor==1) then
                          ctmp=CONJG(cgnup)*cgmup*cf(jz-iz,izeta)
                       end if
                       if(nspinor==2) then
                          ctmp=(CONJG(cgnup)*cgmup + CONJG(cgndn)*cgmdn)*cf(jz-iz,izeta)
                       end if
                       ctmp1= ctmp1 + ctmp
                    end if
                 end do ! jz (Gperp)
!!!
              end do ! ipw  (Gpara,Gperp)
!!! with sum over G_perp only
!              write(unitS+izeta-1,"(6E18.8)")real(ctmp1),aimag(ctmp1)
              write(unitS+izeta-1,*)real(ctmp1),aimag(ctmp1)
!!!
           end if !iband=jband
!!! counter
           imax = imax + 1
           if ( imax.eq.(int(icon*(cmax/5)))) then
              icon = icon + 1
              call CPU_time(ftime)
              ttime = ftime-itime
              write(*,"(A18, I4, A2, I7, A22,I7,A5, F10.2 )")'Snn: layer',izeta,'.',imax,' cycles of a total of',cmax,'. t=',ttime/60.
              call CPU_time(itime) 
           end if
!!! counter
!!! time estimate
        if((izeta.eq.1).and.(ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt*Nlayers/50
           write(*,"(A22, F10.2, A13,I5,A13,I4  )")'Snn: estimated time=',ttime/60.,' minutes for',nkpt,' k-points. L=',Nlayers
        end if
!!!
        end do !jband
     end do !iband
!!! total time
     call CPU_time(fatime) 
     ttime = fatime-iatime
     write(*,"(A9,F10.2,A9)")'Snn: t=',ttime/60.,' minutes'
!!! total time
  end do !izeta (layers)
  !
END SUBROUTINE Snn
SUBROUTINE jnmlk_old(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kp,kg,b1,b2,b3,cg,Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt)
  IMPLICIT NONE
  INTEGER :: i,j,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  INTEGER :: cmax,imax,icon,nkpt
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: Lslab,Lg,gz
  DOUBLE PRECISION, DIMENSION(Nlayers) :: delta,zeta,deltab,deltaf
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  INTEGER, DIMENSION(256) :: iproper
  DOUBLE PRECISION, DIMENSION(3) :: kp
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(3) :: G1,G2,G1p,G2p
  DOUBLE PRECISION :: Gx,Gxp,Gy,Gyp,gtx,gty,eta
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE PRECISION, ALLOCATABLE :: flgz(:,:)
  DOUBLE COMPLEX, ALLOCATABLE :: cf(:,:)
  DOUBLE COMPLEX :: ci,cgn,cgm,cero
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp,ctmp1
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp2 ! Only to check
  DOUBLE PRECISION, DIMENSION(3) :: kmg,kmg12
  LOGICAL, PARAMETER :: debug = .true.
  INTEGER :: ios, iSym,nSym,jSym,flag,np,idirac
  INTEGER, PARAMETER :: DP = KIND(1.0D0)          ! double precision
  REAL(DP), ALLOCATABLE :: SymOp(:,:,:)
  REAL(DP), ALLOCATABLE :: dSymOp(:)
! eta for deltas with G tilde
  eta=1.e-1
  ! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  ! reads the symmetry matrices
!!!#############################
!!!  SUBROUTINE getSymOpsFromFile
!!!#############################
!!!  IMPLICIT NONE
!!!  INTEGER :: ios, iSym
    
  OPEN(UNIT=81, FILE="Symmetries.Cartesian",status="old",IOSTAT=ios)
  IF (ios.NE.0) THEN
     WRITE(*,*) "*********"
     STOP "Stopping: Could not open file Symmetries.Cartesian"
  END IF
  READ(81,*) nSym
  WRITE(*,*) "Read number of symmetries from file to be: ", nSym
!!! check if nSym is odd or even
  if(mod(nSym,2).eq.1) then
     write(*,*) "***************"
     write(*,*)'mod= ',mod(nSym,2)
     write(*,*)'nSym is odd: this case in not coded, try even nSym'  
     write(*,*) "Stopping since I roboto don't know what to do"
     write(*,*) "***************"
     stop
  end if
  IF (debug) THEN
     WRITE(*,*) "The matrices are: "
  END IF
  
  ALLOCATE(SymOp(nSym,3,3))
  ALLOCATE(dSymOp(nSym))
  
  DO iSym = 1, nSym
     READ(81,*) SymOp(iSym,1,1:3) ! first row  (xx xy xz)
     READ(81,*) SymOp(iSym,2,1:3) ! second row (yx yy yz)
     READ(81,*) SymOp(iSym,3,1:3) ! third row  (zx zx zz)
     
     IF (debug) THEN
        WRITE(*,*) SymOp(iSym,1,1:3), SymOp(iSym,2,1:3), SymOp(iSym,3,1:3)
     END IF
     
     dSymOp(iSym) = SymOp(iSym,1,1) * ( SymOp(iSym,2,2)* SymOp(iSym,3,3)-SymOp(iSym,3,2)* SymOp(iSym,2,3) )&
          - SymOp(iSym,1,2) * ( SymOp(iSym,2,1)* SymOp(iSym,3,3)-SymOp(iSym,3,1)* SymOp(iSym,2,3) )&  
          + SymOp(iSym,1,3) * ( SymOp(iSym,2,1)* SymOp(iSym,3,2)-SymOp(iSym,3,1)* SymOp(iSym,2,2) )
     
     
  END DO
!!! We have to determine all the symmetries not related by an inversion
  np=0
  do iSym = 1,nSym
     do jSym = iSym+1,nSym
        flag = 0
        do i=1,3
           do j=1,3
              if((SymOp(iSym,i,j).ne.0).and.(SymOp(jsym,i,j).ne.0)) then
                 if(SymOp(iSym,i,j).ne.-SymOp(jsym,i,j))flag=1
              end if
           end do
        end do
     if(flag.eq.0) then
        np = np + 1
        iproper(np)=iSym
        write(*,*)"matrices ",iSym,jSym," are related by inversion"
     end if
     end do
  end do
  IF (debug) THEN
     write(*,*) "there are ",np," proper matrices"
     write(*,*) "their indeces are: "
     write(*,26)iproper(1:np)
     WRITE(*,*) "End of matrices"
     WRITE(*,*) "***************"
  END IF
26 format(48i5)  
  CLOSE(89)
!!!#############################
!!!  END SUBROUTINE getSymOpsFromFile
!!!#############################
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
!     write(u_log,*)ik,ix,iy,iz
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
  if(debug)write(u_log,*)'min-max ',-minx,maxx,-miny,maxy,-minz,maxz
  if(debug)write(u_log,*)'b1-b3 ',b1,b2,b3
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  end do
  allocate (flgz(-minz-maxz:maxz+minz,Nlayers))
  allocate (cf(-minz-maxz:maxz+minz,Nlayers))
!!! build f_\ell(g_z) function
  do izeta = 1,Nlayers ! number of layers
!!! selects given planes
!!!     if (izeta.eq.9)  izeta=10
!!!     if (izeta.eq.10) izeta=14
!!!     if (izeta.eq.11) izeta=19
!!!
     do jz = -minz-maxz,maxz+minz  ! g-g'
        if ( jz==0 ) then
           flgz(jz,izeta)= (deltab(izeta)+deltaf(izeta))/Lslab
           cf(jz,izeta)=cmplx(( deltab(izeta)+deltaf(izeta) )/Lslab,0.)
        else
           gz = jz * b3(3) 
           Lg = Lslab* jz * b3(3) 
!           cf(jz,izeta)=-ci/Lg * ( exp(ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab))  &
!                                  -exp(ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab)) &
!                                  -exp(-ci*Lg*(zeta(izeta)/Lslab + deltaf(izeta)/Lslab)) &
!                                  +exp(-ci*Lg*(zeta(izeta)/Lslab - deltab(izeta)/Lslab)))/2.
           flgz(jz,izeta)=2./(Lslab*gz) * cos(gz*(zeta(izeta)+(deltaf(izeta)-deltab(izeta))/2.)) &
                                      * sin(gz*(deltaf(izeta)+deltab(izeta))/2.) 
!           if(debug)write(u_log,*)izeta,jz,flgz(jz,izeta),real(cf(jz,izeta))
        endif
     end do
  end do
  !
  ! for now for no spin cg(1:2,jband,ispinor,ipw)
  !
!!! Loop over symmetries
idirac=0
  do i=1,np
!!! if \alpha_{ab}=\delta_{ab} the Dirac's deltas are coded below     
     if  (     (abs(SymOp(iproper(i),1,1)).eq.1.).and.    (SymOp(iproper(i),1,2).eq.0.) &
          .and.    (SymOp(iproper(i),2,1) .eq.0.).and.(abs(SymOp(iproper(i),2,2)).eq.1.) ) then
        idirac = idirac + 1
        if(idirac.gt.1)write(*,*)"delta(Gx-Gx')delta(Gy-Gy') for matrix= ",iproper(i)," => already done"
        if(idirac.eq.1) then 
        write(*,*)"delta(Gx-Gx')delta(Gy-Gy') for matrix= ",iproper(i)," => standard calculation"
        write(*,*)"*********"
!!! Loop over layers
  do izeta = 1,Nlayers
!!! selects given planes
!!!     if (izeta.eq.9)  izeta=10
!!!     if (izeta.eq.10) izeta=14
!!!     if (izeta.eq.11) izeta=19
!!!
!!! writes only diagonal and upper diagonal \cal P are hermitian
!!! counter
     cmax = nbandk*(nbandk+1)/2
     imax = 0
     icon = 1
     call CPU_time (iatime)
     call CPU_time (itime)
!!! counter
     do iband=1,nbandk !n
        do jband=iband,nbandk !m
           ctmp1=cero
           ctmp2=cero
           do ipwp=1,npw
              ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
              realcg=cg(1,iband,1,ipwp) !iband => n
              imagcg=cg(2,iband,1,ipwp) !iband => n
              cgn=dcmplx(realcg,imagcg)
              ! only for b1(i) \cdot b2(i) =0?  b1(i) \cdot b3(i) = b2(i) \cdot b3(i) = 0 for sure
              kmg12 = 2.*( kp(1) + kg(1,ipwp) )*b1 + 2.*( kp(2) + kg(2,ipwp) )*b2
!!! sum over G_perp only
!!! it gives same as full sum
!!! tiny differences only when values are ~E-15 or smaller
!!!
              if ( 1 .eq. 1 ) then
                 do jz =-minz,maxz
                    ipw = arr(ix,iy,jz)
                    if(ipw.ne.0) then
                       realcg=cg(1,jband,1,ipw) !jband => m
                       imagcg=cg(2,jband,1,ipw) !jband => m
                       cgm=dcmplx(realcg,imagcg)
                       ! only for b1(i) \cdot b2(i) =0?  b1(i) \cdot b3(i) = b2(i) \cdot b3(i) = 0 for sure
                       kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
                       ctmp=CONJG(cgn)*cgm*kmg*flgz(jz-iz,izeta)
                       ctmp1= ctmp1 + ctmp
!                       ctmp=CONJG(cgn)*cgm*kmg*cf(jz-iz,izeta)
!                       ctmp2= ctmp2 + ctmp
                    end if
                 end do ! jz (Gperp)
              endif
61            format(3i5,f8.4,8e13.5)
!!!!!!!!!!!!!!!!!!!!!! full sum
!!! activate if need to check with above
!!! sum over G_perp only
              if ( 1 .eq. 2 ) then
                 do jz =1,npw
                    if((ix.eq.kg(1,jz)).and.(iy.eq.kg(2,jz))) then
                       ipw = arr(ix,iy,kg(3,jz))
                       realcg=cg(1,jband,1,ipw) !jband => m
                       imagcg=cg(2,jband,1,ipw) !jband => m
                       cgm=dcmplx(realcg,imagcg)
                       kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
                       ctmp=CONJG(cgn)*cgm*kmg*flgz(kg(3,jz)-iz,izeta)
                       !ctmp=2.*CONJG(cgn)*cgm*cf(kg(3,jz)-iz,izeta)
                       ctmp1= ctmp1 + ctmp
                    end if
                 end do ! jz (Gperp)
              endif
!!!!!!!!!!!!!!!!!!!!!!!!
           end do ! ipw  (Gpara,Gperp)
!!! with sum over G_perp only
!!! writes for a given layer
!           write(unitS+izeta-1,"(6E18.8)")dreal(ctmp1(1)/2.),dimag(ctmp1(1)/2.)&
!                ,dreal(ctmp1(2)/2.),dimag(ctmp1(2)/2.)&
!                ,dreal(ctmp1(3)/2.),dimag(ctmp1(3)/2.) 
           write(unitS+izeta-1,*)dreal(ctmp1(1)/2.),dimag(ctmp1(1)/2.)&
                ,dreal(ctmp1(2)/2.),dimag(ctmp1(2)/2.)&
                ,dreal(ctmp1(3)/2.),dimag(ctmp1(3)/2.) 
!!! with full
!           if (debug) then
!              write(unitS+Nlayers+izeta-1,"(6E18.8)")dreal(ctmp2(1)/2.),dimag(ctmp2(1)/2.)&
!                   ,dreal(ctmp2(2)/2.),dimag(ctmp2(2)/2.)&
!                   ,dreal(ctmp2(3)/2.),dimag(ctmp2(3)/2.) 
!           end if
!!!!!!!
!!! counter
           imax = imax + 1
           if ( imax.eq.(int(icon*(cmax/5)))) then
              icon = icon + 1
              call CPU_time(ftime)
              ttime = ftime-itime
              write(*,"(A14, I4, A2, I7, A22,I7,A5, F10.2 )")'jnmlk: layer',izeta,'. ',imax,' cycles of a total of',cmax,'. t=',ttime/60.
              call CPU_time(itime) 
           end if
!!! counter
!!! time estimate
        if((izeta.eq.1).and.(ik.eq.1).and.(imax.eq.50)) then
           call CPU_time(ftime) 
           ttime = (ftime-itime)*cmax*nkpt*Nlayers/50
           write(*,"(A24, F10.2, A13,I5,A13,I4  )")'jnmlk: estimated time=',ttime/60.,' minutes for',nkpt,' k-points. L=',Nlayers
        end if
!!!
        end do !jband
     end do !iband
!!! total time
     call CPU_time(fatime) 
     ttime = fatime-iatime
     write(*,"(A12,F10.2,A9)")'jnmlk: t=',ttime/60,' minutes'
!!! total time
  end do !izeta (layers)
end if ! idirac
end if ! \alpha_{ab}\sim\delta_{ab}

!!! *************************************************
!!! all cases for which \alpha_{ab}.ne.\sim\delta_{ab}
!!! *************************************************

if  (     (abs(SymOp(iproper(i),1,1)).ne.1.).or.    (SymOp(iproper(i),1,2).ne.0.) &
     .and.    (SymOp(iproper(i),2,1) .ne.0.).or.(abs(SymOp(iproper(i),2,2)).ne.1.) ) then
   write(*,*)'calculating for non-trivial matriz: ',iproper(i)
   do izeta = 1,Nlayers
      do iband=1,nbandk !n
         do jband=iband,nbandk !m
            do ipw=1,npw
               do ipwp=1,npw
                  G1 = kg(1,ipw) *b1 
                  G2 = kg(2,ipw) *b2 
                  G2p= kg(1,ipwp)*b1 
                  G2p= kg(2,ipwp)*b2
                  Gx=G1(1)+G2(1)
                  Gy=G1(2)+G2(2)
                  Gxp=G1p(1)+G2p(1)
                  Gyp=G1p(2)+G2p(2)
                  gtx=(Gx-Gxp)*SymOp(iproper(i),1,1)+(Gy-Gyp)*SymOp(iproper(i),2,1) 
                  gty=(Gx-Gxp)*SymOp(iproper(i),1,2)+(Gy-Gyp)*SymOp(iproper(i),2,2)
                  if(izeta.eq.1.and.iband.eq.1.and.jband.eq.1)then
                     if((abs(gtx).lt.eta).and.(abs(gty).lt.eta)) then
                        write(9+i,31)ipw,ipwp,gtx,gty,Gx,Gxp,Gy,Gyp
                     end if
                  end if
               end do
            end do
         end do
      end do
   end do
end if
31 format(2i5,6e15.5)
!!! *************************************************
end do !i (np symmetries)
  !
END SUBROUTINE jnmlk_old


!#####  j^a_nm(z,k) = (1/2)*Sum_{G,G'} c*_{kn,G'para,G'perp}  c_{km,G'para,Gperp} 
!                   ( 2 k^a + 2 G'para ( \delta_{a,x} + \delta_{a,y} ) + ( Gperp + G'perp )\delta_{a,z} ) exp[i(g-g')z]
!       G=G_parallel+ Gperp 
!       march/20/07
SUBROUTINE jnmzk(unitS,u_log,ik,nbandk,nspinor,npw&
     &,kp,kg,b1,b2,b3,cg,zmax,zmesh)
  IMPLICIT NONE
  INTEGER :: i,ik,iband,jband,ipw,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS,u_log
  INTEGER :: zmax
  INTEGER :: maxx,minx,maxy,miny,maxz,minz
  INTEGER :: ix,iy,iz
  INTEGER :: jz,izeta,Nlayers,sband
  REAL (kind=8) :: itime,ftime,ttime,iatime,fatime
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  DOUBLE PRECISION, DIMENSION(3) :: kp
  INTEGER, ALLOCATABLE :: arr(:,:,:)
  DOUBLE PRECISION, DIMENSION(zmax) :: zmesh
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX :: ci,cgn,cgm,cero
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp,ctmp1
  DOUBLE COMPLEX, DIMENSION(3) :: ctmp2 ! Only to check
  DOUBLE PRECISION, DIMENSION(3) :: kmg,kmg12
  ! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
  ! max and min values of G_i
  maxx=0 ; minx=0
  maxy=0 ; miny=0
  maxz=0 ; minz=0
  ! indexing G vectors
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
!     write(u_log,*)ik,ix,iy,iz
     if ( ix > maxx ) maxx = ix
     if ( ( ix < 0 ) .and. ( -ix > minx ) ) minx = -ix
     if ( iy > maxy ) maxy = iy
     if ( ( iy < 0 ) .and. ( -iy > miny ) ) miny = -iy
     if ( iz > maxz ) maxz = iz
     if ( ( iz < 0 ) .and. ( -iz > minz ) ) minz = -iz
  end do
!  write(u_log,*)-minx,maxx,-miny,maxy,-minz,maxz
  allocate(arr(-minx:maxx,-miny:maxy,-minz:maxz))
  arr = 0
  do ipw=1,npw
     ix = kg(1,ipw) ; iy = kg(2,ipw) ; iz = kg(3,ipw) 
     arr(ix,iy,iz) = ipw
  end do
!
     do iband=1,nbandk !n
        do jband=iband,nbandk !m
           do izeta = 1,zmax !cycle over zmesh, that goes from 0 to Lslab
              ctmp1=cero
              ctmp2=cero
              do ipwp=1,npw
                 ix = kg(1,ipwp) ; iy = kg(2,ipwp) ; iz = kg(3,ipwp) 
                 realcg=cg(1,iband,1,ipwp) !iband => n
                 imagcg=cg(2,iband,1,ipwp) !iband => n
                 cgn=dcmplx(realcg,imagcg)
                 ! only for b1(i) \cdot b2(i) = 0? b1(i) \cdot b3(i) = b2(i) \cdot b3(i) = 0 for sure
                 kmg12 = 2.*( kp(1) + kg(1,ipwp) )*b1 + 2.*( kp(2) + kg(2,ipwp) )*b2
!!! sum over G_perp only
!!! it gives same as full sum
!!! tiny differences only when values are ~E-15 or smaller
!!!
              if ( 1 .eq. 1 ) then
                 do jz =-minz,maxz
                    ipw = arr(ix,iy,jz)
                    if(ipw.ne.0) then
                       realcg=cg(1,jband,1,ipw) !jband => m
                       imagcg=cg(2,jband,1,ipw) !jband => m
                       cgm=dcmplx(realcg,imagcg)
                       ! only for b1(i) \cdot b2(i) = 0? b1(i) \cdot b3(i) = b2(i) \cdot b3(i) = 0 for sure
                       kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
                       ctmp=CONJG(cgn)*cgm*kmg*exp( ci*b3(3)*(jz-iz)*zmesh(izeta) )
                       ctmp1= ctmp1 + ctmp
                       ctmp=CONJG(cgn)*cgm*kmg*&
                            (exp( ci*b3(3)*(jz-iz)*zmesh(izeta) )+exp( -ci*b3(3)*(jz-iz)*zmesh(izeta) ))
                       ctmp2= ctmp2 + ctmp/2.
                    end if
                 end do ! jz (Gperp)
              end if
!!!!!!!!!!!!!!!!!!!!!! full sum
!!! activate if need to check with above
!!! sum over G_perp only
              if ( 1 .eq. 2 ) then
                 do jz =1,npw
                    if((ix.eq.kg(1,jz)).and.(iy.eq.kg(2,jz))) then
                       ipw = arr(ix,iy,kg(3,jz))
                       realcg=cg(1,jband,1,ipw) !jband => m
                       imagcg=cg(2,jband,1,ipw) !jband => m
                       cgm=dcmplx(realcg,imagcg)
                       kmg = kmg12 + (2.*kp(3) + jz + iz)*b3
                       ctmp=CONJG(cgn)*cgm*kmg*exp( ci*b3(3)*(kg(3,jz)-iz)*zmesh(izeta) )
                       ctmp1= ctmp1 + ctmp
                    end if
                 end do ! jz (Gperp)
              end if
!!!!!!!!!!!!!!!!!!!!!!!!
           end do ! ipw  (Gpara,Gperp)
!!
           write(unitS,61)iband,jband,zmesh(izeta),real(ctmp1(1)/2.),aimag(ctmp1(1)/2.)&
                ,real(ctmp2(1)/2.),aimag(ctmp2(1)/2.)&
                ,real(ctmp1(2)/2.),aimag(ctmp1(2)/2.)&
                ,real(ctmp2(2)/2.),aimag(ctmp2(2)/2.)&
                ,real(ctmp1(3)/2.),aimag(ctmp1(3)/2.)&
                ,real(ctmp2(3)/2.),aimag(ctmp2(3)/2.) 
!!
        end do !izeta
!!

        end do !jband
     end do !iband
61   format(2i5,f8.4,12e13.5)
  !
   END SUBROUTINE jnmzk



!#####  \rho_{nm}(r,k) = 1/V\sum_{G',G} c*_{n,k}(G') c_{m,k}(G) e^{i(G-G')\cdot r} 
!       \j^a_{nm}(r,k) = 1/(2V)\sum_{G',G} c*_{n,k}(G') c_{m,k}(G) (2k+G+G')_a e^{i(G-G')\cdot r} 
!       m=\hbar=1
!       15/02/05
   SUBROUTINE jmnrks(unitS,u_log,ik,nbandk,nspinor,npw&
        &,kp,kg,b1,b2,b3,cg,rprimd,acell,bcell,ccell,zmax,zmesh,lamb1,lamb2)
!!Lslab,zeta,delta,deltab,deltaf,Nlayers,nkpt&     
     IMPLICIT NONE
     INTEGER :: ik,iband,ipwp,i,ipw
     INTEGER :: nbandk,nspinor,npw
     INTEGER :: unitS,u_log,zmax,izeta
     DOUBLE PRECISION, DIMENSION(zmax) :: zmesh
     DOUBLE PRECISION, DIMENSION(3) :: kp,ka
     DOUBLE PRECISION :: realcg,imagcg
     DOUBLE PRECISION :: lamb1,lamb2
     DOUBLE PRECISION :: acell,bcell,ccell
     DOUBLE PRECISION :: gmm,gmp,caldp,caldm
     INTEGER, DIMENSION(3,npw) :: kg
     DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
     DOUBLE PRECISION, DIMENSION(3,3) :: rprimd
     DOUBLE PRECISION, DIMENSION(3) :: G,Gp
     DOUBLE PRECISION, DIMENSION(3) :: R,K
     DOUBLE COMPLEX, DIMENSION(3) :: ctmp
     DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
     DOUBLE COMPLEX :: ci,cero,cgm,cgmp,cexpoxy,cexpoz,ctmp1
!!! i,cero
     ci = cmplx(0.,1.)
     cero = cmplx(0.,0.)
!!! Only works if (x,y)\cdot z =0
!!! gets (x,y,z) from R= lambda_1*a_1 + lambda_2*a_2 and r = R + z

     R(:) = lamb1*rprimd(:,1)/acell + lamb2*rprimd(:,2)/bcell 
     write(*,*)R(1),R(2),R(3)
     ka(:)= kp(1)*b1(:) +kp(2)*b2(:) +kp(3)*b3(:)
     do iband=1,nbandk
!!!
        do izeta = 1,zmax !cycle over zmesh, that goes from 0 to Lslab
           ctmp(:)=cero
           ctmp1=cero
           do ipw=1,npw
              realcg=cg(1,iband,1,ipw) !iband => m 
              imagcg=cg(2,iband,1,ipw) !iband => m
              cgm=dcmplx(realcg,imagcg)
              G(:) = kg(1,ipw)*b1(:) +kg(2,ipw)*b2(:) +kg(3,ipw)*b3(:)
! constructs the wave-function \psi_{n\bfk}(z)=\sum_\bfG c_{n\bfk)(\bfG)e^{i\bfG\cdot\bfr}
              cexpoxy = exp(ci*( G(1)*R(1) + G(2)*R(2)))
              cexpoz = exp(ci*G(3)*zmesh(izeta) )
              ctmp1 = ctmp1 + cgm**cexpoxy*cexpoz !\psi(x,y,z,nk)
!              ctmp = ctmp + conjg(cgm)*cgm*(ka + G)
!              if(1.eq.2) then
              do ipwp=1,npw
                 realcg=cg(1,iband,1,ipwp) !iband => m 
                 imagcg=cg(2,iband,1,ipwp) !iband => m
                 cgmp=dcmplx(realcg,imagcg)
                 Gp(:) = kg(1,ipwp)*b1(:) +kg(2,ipwp)*b2(:) +kg(3,ipwp)*b3(:)
                 K(:) = 2.*ka(:) + G(:) + Gp(:)
                 gmm = (G(1)-Gp(1))*sqrt(3.)/2 - (G(2)-Gp(2))/2.
                 gmp = (G(1)-Gp(1))*sqrt(3.)/2 + (G(2)-Gp(2))/2.
                 if(gmm.ne.0.)then
                    caldm=sin(gmm/2.)/(gmm/2.)
                 else
                    caldm = 1.
                 end if
                 if(gmp.ne.0.)then
                    caldp=sin(gmp/2.)/(gmp/2.)
                 else
                    caldp = 1.
                 end if
!                 cexpoxy = exp(ci*( (G(1)-Gp(1))*R(1) + (G(2)-Gp(2))*R(2)))
                 cexpoz = exp(ci*(G(3)-Gp(3))*zmesh(izeta) )
!                 ctmp = ctmp + 0.5*K*conjg(cgmp)*cgm*cexpoxy*cexpoz
                 ctmp = ctmp + 0.5*K*conjg(cgmp)*cgm*caldp*caldm*cexpoz
!                 if((ipw.eq.8).and.(izeta.eq.1).and.(iband.eq.1))write(unitS+2,70)ipw,ipwp,G(1),Gp(1),G(2),Gp(2),caldm,caldp
              end do ! ipwp
!           end if
           end do ! ipw
        write(unitS,69)iband,zmesh(izeta),(real(ctmp(i)),i=1,3)
        write(unitS+1,69)iband,zmesh(izeta),real(ctmp1),aimag(ctmp1)
     end do ! zmesh
     write(unitS,*)
     write(unitS,*)
     write(unitS+1,*)
     write(unitS+1,*)
  end do !iband
69 format(i5,f9.4,3E12.4) 
70 format(2i5,6f9.4) 
   END SUBROUTINE jmnrks
   
   SUBROUTINE rhor(unitS,ik,nbandk,nspinor,npw&
     &,kg,b1,b2,b3,cg,x,y,z)

  IMPLICIT NONE
  INTEGER :: ik,iband,ipwp
  INTEGER :: nbandk,nspinor,npw
  INTEGER :: unitS
  DOUBLE PRECISION :: x,y,z
  DOUBLE PRECISION :: realcg,imagcg
  INTEGER, DIMENSION(3,npw) :: kg
  DOUBLE PRECISION, DIMENSION(3) :: b1,b2,b3
  DOUBLE PRECISION, DIMENSION(3) :: G
  DOUBLE PRECISION, DIMENSION(2,nbandk,nspinor,npw) :: cg
  DOUBLE COMPLEX :: ci,cero,cgn,ctmps,cexpo
!!! i,cero
  ci = cmplx(0.,1.)
  cero = cmplx(0.,0.)
!!!
  do iband=1,nbandk
     ctmps=cero
     do ipwp=1,npw
        realcg=cg(1,iband,1,ipwp) !iband => n 
        imagcg=cg(2,iband,1,ipwp) !iband => n
        cgn=dcmplx(realcg,imagcg)
        G(:) = kg(1,ipwp)*b1(:) +kg(2,ipwp)*b2(:) +kg(3,ipwp)*b3(:) 
        cexpo = exp( ci*( G(1)*x + G(2)*y + G(3)*z ) )
!!!      write(*,*)iband,ipwp,kg(:,ipwp),b1(:)
        ctmps = ctmps + cgn*cexpo
     end do  ! ipwp
     write(unitS,"(E18.8)")real(ctmps*conjg(ctmps))
  end do !iband
END SUBROUTINE rhor
!##################### service subroutines not used above #################




