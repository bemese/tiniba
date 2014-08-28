program i17
  IMPLICIT NONE
  INTEGER :: k,n,m,ridir,q
  INTEGER :: Nk,nbands
  DOUBLE COMPLEX, ALLOCATABLE :: px(:,:,:),py(:,:,:),pz(:,:,:),cf(:,:,:)
  DOUBLE COMPLEX :: auxx,auxy,auxz,sumx,sumy,sumz,ci
  DOUBLE PRECISION :: rpx,ipx,rpy,ipy,rpz,ipz
  DOUBLE PRECISION :: rcf,icf
  !constants
  ci=cmplx(0.,1.)
  !reads from the terminal
  read(*,*)Nk,nbands
  allocate (px(Nk,nbands,nbands),py(Nk,nbands,nbands),pz(Nk,nbands,nbands))
  allocate (cf(Nk,nbands,nbands))
  ! reads the arrays
  do k=1,Nk
     do n=1,nbands !n                                                        
        do m=n,nbands !m                                                 
           ! reads the momentum matrix elements
           read(1,*)rpx,ipx,rpy,ipy,rpz,ipz
           px(k,n,m)=rpx+ci*ipx
           px(k,m,n)=conjg(px(k,n,m))
           py(k,n,m)=rpy+ci*ipy
           py(k,m,n)=conjg(py(k,n,m))
           pz(k,n,m)=rpz+ci*ipz
           pz(k,m,n)=conjg(pz(k,n,m))
           !reads the cut-function matrix elements
           read(2,*)rcf,icf
           cf(k,n,m)=rcf+ci*icf
           cf(k,m,n)=conjg(cf(k,n,m))
        end do
     end do
  end do
! computes i17
  do k=1,Nk
     do n=1,nbands !n                                                        
        do m=n,nbands !m                                                 
           sumx=cmplx(0.,0.)
           sumy=cmplx(0.,0.)
           sumz=cmplx(0.,0.)
           do q=1,nbands
              auxx= cf(k,n,q)*px(k,q,m)+px(k,n,q)*cf(k,q,m)
              auxy= cf(k,n,q)*py(k,q,m)+py(k,n,q)*cf(k,q,m)
              auxz= cf(k,n,q)*pz(k,q,m)+pz(k,n,q)*cf(k,q,m)
              sumx=sumx+auxx
              sumy=sumy+auxy
              sumz=sumz+auxz
           end do
           write(3,*)real(sumx)/2.,imag(sumx)/2.&
                ,real(sumy)/2.,imag(sumy)/2.&
                ,real(sumz)/2.,imag(sumz)/2.
        end do
     end do
  end do
!!!
end program i17
