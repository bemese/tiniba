!###############
MODULE FUNCTIONS
!###############
  USE constants, ONLY : DP, DPC
  USE inparams, ONLY : nMax, nVal, tol
  USE arrays, ONLY : band, momMatElem, posMatElem, Delta, tolchoice
  USE arrays, ONLY : oldStyleScissors
  USE arrays, ONLY : calMomMatElem,calDelta,calPosMatElem
  !#BMSVer3.0d
  USE arrays, ONLY : cfMatElem
  USE arrays, ONLY : f
  USE arrays, ONLY : vldaMatElem
  !#BMSVer3.0u
  IMPLICIT NONE
CONTAINS
!#BMSVer3.0d
!############################################
  COMPLEX(DPC) FUNCTION calVldaf(alpha,iv,ic)
!############################################
    ! Finds \calv^{\lda}_{n,m} (c-a.2) for the given k-value.
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: alpha, iv, ic
    INTEGER :: q,ik
    COMPLEX(DPC) :: tmp,aux1,aux2
    tmp=(0.d0,0.d0)
    DO q = 1, nMax
       aux1=vldaMatElem(alpha,iv,q)*cfMatElem(q,ic)
       aux2=cfMatElem(iv,q)*vldaMatElem(alpha,q,ic)
       tmp=tmp+(aux1+aux2)/2.d0
    end DO
    calVldaf=tmp
!############################################
  end FUNCTION calVldaf
!############################################
!#BMSVer3.0u
!#BMSVer3.0d
!!!############################################
  COMPLEX(DPC) FUNCTION calVscissorso(alpha,iv,ic,ik)
!!!############################################
    ! Finds \calv^\cals_{nm} (vs.vc,vs.cc,vs.vv) for the given k-value.
    ! IF iv not equal to ic:
    ! \calv^\cals_{vc} (vs.vc) is calculated
    ! IF iv=ic=conduction
    ! \calv^\cals_{cc} (vs.cc) is calculated
    ! IF iv=ic=valence
    ! \calv^\cals_{vv} (vs.vv) is calculated
    !
    ! The value of the scissor correction (scissorFactor) is used in 
    ! set_input_ascii.f90 instead of here, just for convenience
    ! 
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: alpha, iv, ic
    INTEGER :: q,ik
    COMPLEX(DPC) :: tmp1,tmp2,aux
    tmp1=(0.d0,0.d0)
    tmp2=(0.d0,0.d0)
    if (ic.gt.iv) then !vc case
       DO q = 1, nVal
          aux=posMatElem(alpha,iv,q)*cfMatElem(q,ic)
          tmp1=tmp1+aux
       end DO
       DO q = nVal+1, nMax
          aux=cfMatElem(iv,q)*posMatElem(alpha,q,ic)
          tmp2=tmp2+aux
       end DO
       !Notice that the vc case is coded 
       calVscissorso=(0.d0,-1.d0)*(tmp1+tmp2)/2.d0
    end if
    if (ic.eq.iv) then
       if (iv.le.nVal) then !vv-case
          do q=nVal+1,nMax !sum over conduction states
             aux=posMatElem(alpha,iv,q)*cfMatElem(q,iv)
             tmp1=tmp1+aux             
          end do
          calVscissorso=cmplx(aimag(tmp1),0.d0)
       end if
       if (ic.gt.nVal) then !cc-case
          do q=1,nVal !sum over valence states
             aux=posMatElem(alpha,ic,q)*cfMatElem(q,ic)
             tmp1=tmp1+aux             
          end do
          calVscissorso=cmplx(-aimag(tmp1),0.d0)
       end if
    end if
!!!############################################
  end FUNCTION calVscissorso
!############################################
!#BMSVer3.0u
!#BMSVer3.0d
!!!############################################
  COMPLEX(DPC) FUNCTION calVscissors(alpha,n,m,ik)
!!!############################################
    ! Finds \calv^\cals_{nm} (c-a.3bb) for the given k-value.
    !
    ! The value of the scissor correction (scissor) is used in 
    ! set_input_ascii.f90 instead of here, just for convenience
    ! 
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: alpha, n, m
    INTEGER :: q,ik
    COMPLEX(DPC) :: tmp,aux
    tmp=(0.d0,0.d0)
    DO q = 1, nMax
       if ( q .eq. n )posMatElem(alpha,n,q)=(0.d0,0.d0)
       if ( q .eq. m )posMatElem(alpha,q,m)=(0.d0,0.d0)
       aux=(f(q)-f(n))*posMatElem(alpha,n,q)*cfMatElem(q,m)&
            +(f(m)-f(q))*cfMatElem(n,q)*posMatElem(alpha,q,m)
       tmp=tmp+aux
    end DO
    calVscissors=(0.d0,1.d0)*tmp/2.d0
!!!############################################
  end FUNCTION calVscissors
!############################################
!#BMSVer3.0u

!############################################
  COMPLEX(DPC) FUNCTION calPosition(alpha,iv,ic,ik)
!############################################
    ! Finds r^{\alpha}_{iv ic} for the given k-value.
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: alpha, iv, ic
    INTEGER :: ii,ik
    REAL(DP) :: omeganm
    REAL(DP) :: fsc
    COMPLEX(DPC) :: tmp
    COMPLEX(DPC) :: meavc
    
    tmp = (0.d0, 0.d0)
    IF (iv.NE.ic) THEN
       omeganm = band(iv) - band(ic)
       SELECT CASE(tolchoice)
       CASE(0)
          IF (DABS(omeganm).LT.tol) THEN
             ! Set matrix element to zero
             tmp = (0.d0, 0.d0)
             IF (iv.EQ.nVal.AND.ic.EQ.nVal+1) THEN
                write(6,*)'############################'
                write(6,*)'functions.f90@position: Hold on! The tol value is bigger than the gap'
                WRITE(6,*)'ik= ',ik,'valence= ',iv,'conduction= ',ic,'E= ',omeganm
                !             STOP
             END IF
          ELSE
             meavc = calMomMatElem(alpha,iv,ic)
             tmp = meavc/omeganm
             tmp = tmp/(0.d0, 1.d0)
          END IF
       CASE (1)
          IF (DABS(omeganm).LT.tol) THEN
             ! Set energy difference to the tolerance
             IF (omeganm.LT.0.d0) omeganm =-tol
             IF (omeganm.GT.0.d0) omeganm = tol
          END IF
          IF (DABS(omeganm).GT.0.d0) THEN
             meavc = calMomMatElem(alpha,iv,ic)
             tmp = meavc/omeganm
             tmp = tmp/(0.d0, 1.d0)
          ELSE
             tmp = (0.d0, 0.d0)
          END IF
       CASE DEFAULT
          STOP 'functions.f90: problem with tolchoice'
       END SELECT
    ELSE IF (iv.EQ.ic) THEN
       tmp = (0.d0, 0.d0)
    ELSE
       STOP 'PROBLEM IN functions.f90 SHOULD NOT BE HERE'
    END IF
    calPosition = tmp
    RETURN
!**********************
  END FUNCTION calPosition
!**********************

!############################################
  COMPLEX(DPC) FUNCTION position(alpha,iv,ic,ik)
!############################################
    ! Finds r^{\alpha}_{iv ic} for the given k-value.
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: alpha, iv, ic
    INTEGER :: ii,ik
    REAL(DP) :: omeganm
    REAL(DP) :: fsc
    COMPLEX(DPC) :: tmp
    COMPLEX(DPC) :: meavc
    
    tmp = (0.d0, 0.d0)
    IF (iv.NE.ic) THEN
       omeganm = band(iv) - band(ic)
!       fsc=omeganm/(omeganm-.5)
!       fsc=1.
       SELECT CASE(tolchoice)
       CASE(0)
          IF (DABS(omeganm).LT.tol) THEN
             ! Set matrix element to zero
             tmp = (0.d0, 0.d0)
             IF (iv.EQ.nVal.AND.ic.EQ.nVal+1) THEN
                write(6,*)'############################'
                write(6,*)'functions.f90@position: Hold on! The tol value is bigger than the gap'
                WRITE(6,*)'ik= ',ik,'valence= ',iv,'conduction= ',ic,'E= ',omeganm
                !             STOP
             END IF
          ELSE
             meavc = momMatElem(alpha,iv,ic)
!             meavc = momMatElem(alpha,iv,ic)*fsc
             tmp = meavc/omeganm
             tmp = tmp/(0.d0, 1.d0)
          END IF
       CASE (1)
          IF (DABS(omeganm).LT.tol) THEN
             ! Set energy difference to the tolerance
             IF (omeganm.LT.0.d0) omeganm =-tol
             IF (omeganm.GT.0.d0) omeganm = tol
          END IF
          IF (DABS(omeganm).GT.0.d0) THEN
             meavc = momMatElem(alpha,iv,ic)
!             meavc = momMatElem(alpha,iv,ic)*fsc
             tmp = meavc/omeganm
             tmp = tmp/(0.d0, 1.d0)
          ELSE
             tmp = (0.d0, 0.d0)
          END IF
       CASE DEFAULT
          STOP 'functions.f90: problem with tolchoice'
       END SELECT
    ELSE IF (iv.EQ.ic) THEN
       tmp = (0.d0, 0.d0)
    ELSE
       STOP 'PROBLEM IN functions.f90 SHOULD NOT BE HERE'
    END IF
    position = tmp
    RETURN
!**********************
  END FUNCTION position
!**********************
! genderiv2=genderiv However, genderiv has the same indices as the shg-layer.tex  
!#################################################
  COMPLEX(DPC) FUNCTION genderiv2(alpha,beta,iv,ic,ik)
!#################################################
    ! Finds r^{\alpha}_{iv ic ; beta}
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: alpha, beta, iv, ic
    INTEGER :: ip,ik
    REAL(DP) :: omegapm, omeganp, omeganm, omegamn
    COMPLEX(DPC) :: tmp1, tmp2, deltaa, deltab
    COMPLEX(DPC) :: ravc, rbvc, rbpc, rbvp, ravp, rapc
    
    tmp1 = (0.d0, 0.d0)
    tmp2 = (0.d0, 0.d0)
    IF (iv.NE.ic) THEN
       omeganm = band(iv) - band(ic)
       ! 
       ! since omeganm is always greater than thegap, we
       ! do not worry about the tolerance BMS this is wrong
       ! 
       IF (DABS(omeganm).GT.tol) THEN
          ravc = posMatElem(alpha,iv,ic)
          deltab = Delta(beta,iv,ic)
          tmp1 = tmp1 - ravc*deltab
          
          rbvc = posMatElem(beta,iv,ic)
          deltaa = Delta(alpha,iv,ic)
          tmp1 = tmp1 - rbvc*deltaa
          
          tmp1 = tmp1/omeganm
          
          DO ip = 1, nMax
             IF ((ip.NE.ic).AND.(ip.NE.iv)) THEN
                omeganp = band(iv) - band(ip)
                rbpc = posMatElem(beta,ip,ic)
                ravp = posMatElem(alpha,iv,ip)
                tmp2 = tmp2 + omeganp*ravp*rbpc*(0.d0,1.d0)
                omegapm = band(ip) - band(ic)
                rbvp = posMatElem(beta,iv,ip)
                rapc = posMatElem(alpha,ip,ic)
                tmp2 = tmp2 - omegapm*rbvp*rapc*(0.d0,1.d0)
             END IF
          END DO
          tmp2 = -tmp2/omeganm
       ELSE
          IF (iv.EQ.nVal.AND.ic.EQ.nVal+1) THEN
             write(6,*)'############################'
             write(6,*)'functions.f90@genderiv: Hold on! The tol value is bigger than the gap'
             WRITE(6,*)'ik= ',ik,'valence= ',iv,'conduction= ',ic, omeganm
             !             STOP
          END IF
       END IF
    END IF
    genderiv2 = tmp1 + tmp2
!**********************
  END FUNCTION genderiv2
!**********************

!############################################
  COMPLEX(DPC) FUNCTION genderiv(alpha,beta,n,m,k)
!############################################
!!!
!!! Finds (r^\alpha_{nm})_{;k^\beta} for the given k-value according to \ref{a_rgendevn}
!!! from shg-layer.tex, i.e. Eq. (E13)
!!!
!#BMSVer3.0d
! for -n option the contribution from v^\nl is included
! however, neglectinh \tau^{ab}_{nm}
!#BMSVer3.0u
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: alpha, beta,n,m,k
    INTEGER :: l
    REAL(DP) :: omegalm, omeganl, omeganm
    COMPLEX(DPC) :: tmp1, tmp2
    tmp1 = (0.d0, 0.d0)
    tmp2 = (0.d0, 0.d0)
    if(n.ne.m) then
!!!
       omeganm = band(n) - band(m)

       IF (DABS(omeganm).GT.tol) THEN
          tmp1 = tmp1 + posMatElem(alpha,n,m)*Delta(beta ,m,n) 
          tmp1 = tmp1 + posMatElem(beta ,n,m)*Delta(alpha,m,n) 
          tmp1 = tmp1/omeganm
          
          DO l = 1, nMax
             IF ((l.NE.m).AND.(l.NE.n)) THEN
                omegalm = band(l) - band(m)
                omeganl = band(n) - band(l)
                tmp2 = tmp2 + omegalm*posMatElem(beta ,n,l)*posMatElem(alpha,l,m)
                tmp2 = tmp2 - omeganl*posMatElem(alpha,n,l)*posMatElem(beta ,l,m)
             END IF
          END DO
          tmp2 = tmp2*(0.d0,1.d0)/omeganm
       ELSE
          IF (m.EQ.nVal.AND.n.EQ.nVal+1) THEN
             write(6,*)'############################'
             write(6,*)'functions.f90@genderiv: Hold on! The tol value is bigger than the gap'
             WRITE(6,*)'ik= ',k,'valence= ',m,'conduction= ',n, omeganm
             !             STOP
          END IF
       END IF
!!!
    end if
    genderiv = tmp1 + tmp2
    RETURN
!**********************
  END FUNCTION genderiv
!**********************

!############################################
  COMPLEX(DPC) FUNCTION GenDerCalPositionf(alpha,beta,n,m,k)
!############################################
!!!
!!! Finds ({\cal R}^\alpha_{nm})_{;k^\beta} for the given k-value according to \ref{rgkcal}
!!! from shg-layer.tex, i.e. Eq. (97)
!!!
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: alpha, beta,n,m,k
    INTEGER :: l
    REAL(DP) :: omegalm, omeganl, omeganm
    COMPLEX(DPC) :: tmp1, tmp2
    tmp1 = (0.d0, 0.d0)
    tmp2 = (0.d0, 0.d0)
    if(n.ne.m) then
!!!
       omeganm = band(n) - band(m)

       IF (DABS(omeganm).GT.tol) THEN
          tmp1 = tmp1 + calPosMatElem(alpha,n,m)*Delta(beta,m,n) 
          tmp1 = tmp1 + posMatElem(beta,n,m)*calDelta(alpha,m,n) 
          tmp1 = tmp1/omeganm
          
          DO l = 1, nMax
             IF ((l.NE.m).AND.(l.NE.n)) THEN
                omegalm = band(l) - band(m)
                omeganl = band(n) - band(l)
                tmp2 = tmp2 + omegalm*posMatElem(beta,n,l)*calPosMatElem(alpha,l,m)
                tmp2 = tmp2 - omeganl*calPosMatElem(alpha,n,l)*posMatElem(beta,l,m)
             END IF
          END DO
          tmp2 = tmp2*(0.d0,1.d0)/omeganm
       ELSE
          IF (m.EQ.nVal.AND.n.EQ.nVal+1) THEN
             write(6,*)'############################'
             write(6,*)'functions.f90@genderiv: Hold on! The tol value is bigger than the gap'
             WRITE(6,*)'ik= ',k,'valence= ',m,'conduction= ',n, omeganm
             !             STOP
          END IF
       END IF
!!!
    end if
    GenDerCalPositionf = tmp1 + tmp2
    RETURN
!**********************
  END FUNCTION GenDerCalPositionf
!**********************

!######################################################
  COMPLEX(DPC) FUNCTION Cfunctiontmp(alpha,gamma,ic,iv)
!######################################################
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: alpha, gamma, ic, iv
    REAL(DP) :: omegacp, omegavp
    COMPLEX(DPC) :: racp, rcpv, rccp, rapv
    COMPLEX(DPC) :: ctmp
    !COMPLEX(DPC) Cfunctiontmp
    INTEGER :: ip
    
    !Cfunctiontmp = (0.d0, 0.d0)
    ctmp = (0.d0,0.d0)
    
    DO ip = 1, nMax
       omegacp = band(ic) - band(ip)
       racp = posMatElem(alpha,ic,ip)
       rcpv = posMatElem(gamma,ip,iv)
       IF (DABS(omegacp).GE.tol) THEN
          !Cfuntiontmp = Cfuntiontmp + racp*rcpv/omegacp
          ctmp = ctmp + racp*rcpv/omegacp
       END IF
!       ELSE IF (omegacp.GE.0.d0) THEN
!          !Cfuntiontmp = Cfuntiontmp + racp*rcpv/tol
!          ctmp = ctmp + racp*rcpv/tol
!       ELSE IF (omegacp.LT.0.d0) THEN
!          !Cfuntiontmp = Cfuntiontmp + racp*rcpv/(-tol)
!          ctmp = ctmp + racp*rcpv/(-tol)
!       ELSE
!          STOP 'PROBLEM in Cfunctiontmp IF statement 1'
!       END IF
       
       omegavp = band(iv) - band(ip)
       rccp = posMatElem(gamma,ic,ip)
       rapv = posMatElem(alpha,ip,iv)
       IF (DABS(omegavp).GE.tol) THEN
          !Cfuntiontmp = Cfuntiontmp + rccp*rapv/omegavp
          ctmp = ctmp + rccp*rapv/omegavp
       END IF
!       ELSE IF (omegavp.GE.0.d0) THEN
!          !Cfuntiontmp = Cfuntiontmp + rccp*rapv/tol
!          ctmp = ctmp + rccp*rapv/tol
!       ELSE IF (omegavp.LT.0.d0) THEN
!          !Cfuntiontmp = Cfuntiontmp + rccp*rapv/(-tol)
!          ctmp = ctmp + rccp*rapv/(-tol)
!       ELSE
!          STOP 'PROBLEM in Cfunctiontmp IF statement 2'
!       END IF
    END DO
    Cfunctiontmp = ctmp
    RETURN
!##########################    
  END FUNCTION Cfunctiontmp
!##########################

!#######################################
  COMPLEX(DPC) FUNCTION rmprpnomp(index,index2,alpha,beta,ic,iv,pow)
!#######################################
! index chooses first or second term in this expression
!    index = 0 --> sum the two terms of index=1 and index=2
!    index = 1 --> calculates:
!       \sum_{ip} r^{\alpha}_{ic, ip} r^{\beta}_{ip, iv} \omega_{ic, ip}^{pow}
!    index = 2 --> calculates:
!     - \sum_{ip} r^{\beta}_{ic, ip} r^{\alpha}_{ip, iv} \omega_{ip, iv}^{pow}
! index2 chooses whether to sum ip over only conduction bands or only
! over valence bands or over both valence and conduction bands
!    index2 = 0 --> sum over all bands
!    index2 = 1 --> sum over valence bands
!    index2 = 2 --> sum over conduction bands
!   
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: index, index2, alpha, beta, iv, ic, pow
    INTEGER :: ip, ipLOW, ipMAX
    REAL(DP) :: omegacp, omegapv
    COMPLEX(DPC) :: racp, rbpv, rbcp, rapv
    COMPLEX(DPC) :: ttmp
!    COMPLEX(DPC) :: rmprpnomp
    
    rmprpnomp = (0.d0, 0.d0)
    SELECT CASE(index2)
    CASE(0)
!!! sum over all bands
       ipLOW = 1
       ipMAX = nMAX
    CASE(1)
!!! sum over valence
       ipLOW = 1
       ipMAX = nVal
    CASE(2)
!!! sum over conduction
       ipLOW = nVal+1
       ipMAX = nMax
    CASE DEFAULT
       STOP 'error index2 in function rmprpnomp'
      END SELECT
      
    DO ip = 1, nMax
       omegacp = band(ic) - band(ip)
       omegapv = band(ip) - band(iv)
       
       SELECT CASE(index)
       CASE(0)
          ! calculate the entire sum
          IF (ip.NE.ic) THEN
             racp = posMatElem(alpha,ic,ip)
             rbpv = posMatElem(beta,ip,iv)
             IF (pow.LT.0) THEN
                ! then one must worry about divergences
                SELECT CASE(tolchoice)
                CASE(0)
                   ! increase the energy difference if it is below the tolerance
                   IF (DABS(omegacp).LT.tol) THEN
                      IF(omegacp.LT.0.d0) omegacp = -tol
                      IF(omegacp.GT.0.d0) omegacp = tol
                   END IF
                   IF (omegacp.NE.0.d0) THEN
                      rmprpnomp = rmprpnomp + racp*rbpv*(omegacp**pow)
                   END IF
                CASE(1)
                   ! do not include term if the energy is below the tolerance
                   IF (DABS(omegacp).GT.tol) THEN
                      rmprpnomp = rmprpnomp + racp*rbpv*(omegacp**pow)
                   END IF
                CASE DEFAULT
                   STOP 'functions.f90: problems with tolchoice'
                END SELECT
             ELSE IF (pow>0) THEN
                ! then the divergence does not matter
                rmprpnomp = rmprpnomp + racp*rbpv*(omegacp**pow)
             END IF
          END IF
          IF (ip.NE.iv) THEN
             rbcp = posMatElem(beta,ic,ip)
             rapv = posMatElem(alpha,ip,iv)
             IF (pow.LT.0) THEN
                ! then we worry about divergences
                SELECT CASE(tolchoice)
                CASE(0)
                   ! if energy is too small set it to be the tolerance
                   IF (DABS(omegapv).LT.tol) THEN
                      IF(omegapv.LT.0.d0) omegapv = -tol
                      IF(omegapv.GT.0.d0) omegapv = tol
                   END IF
                   IF (omegapv.NE.0.d0) THEN
                      rmprpnomp = rmprpnomp - rbcp*rapv*(omegapv**pow)
                   END IF
                CASE(1)
                   ! do not include the term if the energy is below the tolerance
                   IF (DABS(omegapv).GT.tol) THEN
                      rmprpnomp = rmprpnomp - rbcp*rapv*(omegapv**pow)
                   END IF
                END SELECT
             ELSE IF (pow.GT.0) THEN
                ! then we do not worry about divergences
                rmprpnomp = rmprpnomp - rbcp*rapv*(omegapv**pow)
             END IF
          END IF
       CASE(1)
          ! calculate just the first term
          IF (ip.NE.ic) THEN
             racp = posMatElem(alpha,ic,ip)
             rbpv = posMatElem(beta,ip,iv)
             IF (pow.LT.0) THEN
                ! then we worry about divergences
                SELECT CASE(tolchoice)
                CASE(0)
                   ! increase the energy difference if it is below the tolerance
                   IF (DABS(omegacp).LT.tol) THEN
                      IF(omegacp.LT.0.d0) omegacp = -tol
                      IF(omegacp.GT.0.d0) omegacp = tol
                   END IF
                   IF (omegacp.NE.0.d0) THEN
                      rmprpnomp = rmprpnomp + racp*rbpv*(omegacp**pow)
                   END IF
                CASE(1)
                   ! do not include the term if the energy is below the tolerance
                   IF (DABS(omegacp).GT.tol) THEN
                      rmprpnomp = rmprpnomp + racp*rbpv*(omegacp**pow)
                   END IF
                CASE DEFAULT
                   STOP 'functions.f90: problem with tolchoice'
                END SELECT
             ELSE IF (pow>0) THEN
                ! then we do not worry about the divergences
                rmprpnomp = rmprpnomp + racp*rbpv*(omegacp**pow)
             END IF
          END IF
       CASE(2)
          ! calculate the second term
          IF (ip.NE.iv) THEN
             rbcp = posMatElem(beta,ic,ip)
             rapv = posMatElem(alpha,ip,iv)
             IF (pow.LT.0) THEN
                ! then we worry about divergences
                SELECT CASE(tolchoice)
                CASE(0)
                   ! increase the energy difference if it is below the tolerance
                   IF (DABS(omegapv).LT.tol) THEN
                      IF(omegapv.LT.0.d0) omegapv = -tol
                      IF(omegapv.GT.0.d0) omegapv = tol
                   END IF
                   IF (omegapv.NE.0.d0) THEN
                      rmprpnomp = rmprpnomp - rbcp*rapv*(omegapv**pow)
                   END IF
                CASE(1)
                   ! do not include term if the energy is below the tolerance
                   IF (DABS(omegapv).GT.tol) THEN
                      rmprpnomp = rmprpnomp - rbcp*rapv*(omegapv**pow)
                   END IF
                CASE DEFAULT
                   STOP 'functions.f90: Problems with functions.f90'
                END SELECT
             ELSE IF (pow.GT.0) THEN
                ! then we do not worry about divergences
                rmprpnomp = rmprpnomp - rbcp*rapv*(omegapv**pow)
             END IF
          END IF
       CASE DEFAULT
          WRITE(6,*) 'functions.f90: problem with index'; STOP
       END SELECT
    END DO
!***********************
  END FUNCTION rmprpnomp
!***********************
  
END MODULE FUNCTIONS
!!!
!!! Calculates {\cal R}^\alpha_{nm} according to \ref{rcal} from shg-layer.tex, i.e. Eq. (74)    
!!! Begin
!!$    calRanm = (0.d0, 0.d0)
!!$    IF (n.NE.m) THEN
!!$       omeganm = band(n) - band(m)
!!$       SELECT CASE(tolchoice)
!!$       CASE(0)
!!$          IF (DABS(omeganm).LT.tol) THEN
!!$             ! Set matrix element to zero
!!$             calRanm = (0.d0, 0.d0)
!!$             IF (m.EQ.nVal.AND.n.EQ.nVal+1) THEN
!!$                write(6,*)'############################'
!!$                write(6,*)'functions.f90@position: Hold on! The tol value is bigger than the gap'
!!$                WRITE(6,*)'ik= ',k,'valence= ',m,'conduction= ',n,'E= ',omeganm
!!$                !             STOP
!!$             END IF
!!$          ELSE
!!$             meavc = calMomMatElem(alpha,n,m)
!!$             calRanm = meavc/omeganm
!!$             calRanm = calRanm/(0.d0, 1.d0)
!!$          END IF
!!$       CASE (1)
!!$          IF (DABS(omeganm).LT.tol) THEN
!!$             ! Set energy difference to the tolerance
!!$             IF (omeganm.LT.0.d0) omeganm =-tol
!!$             IF (omeganm.GT.0.d0) omeganm = tol
!!$          END IF
!!$          IF (DABS(omeganm).GT.0.d0) THEN
!!$             meavc = calMomMatElem(alpha,n,m)
!!$             calRanm = meavc/omeganm
!!$             calRanm = calRanm/(0.d0, 1.d0)
!!$          ELSE
!!$             calRanm = (0.d0, 0.d0)
!!$          END IF
!!$       CASE DEFAULT
!!$          STOP 'functions.f90: problem with tolchoice'
!!$       END SELECT
!!$    ELSE IF (n.EQ.m) THEN
!!$       calRanm = (0.d0, 0.d0)
!!$    ELSE
!!$       STOP 'PROBLEM IN functions.f90 SHOULD NOT BE HERE'
!!$    END IF
