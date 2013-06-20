!###############
MODULE FUNCTIONS
!###############
  USE constants, ONLY : DP, DPC
  USE inparams, ONLY : nMax, nVal, tol
  USE arrays, ONLY : band, momMatElem, posMatElem, Delta, tolchoice
  USE arrays, ONLY : oldStyleScissors
  IMPLICIT NONE
CONTAINS

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
  
!#################################################
  COMPLEX(DPC) FUNCTION genderiv(alpha,beta,iv,ic,ik)
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
    genderiv = tmp1 + tmp2
!**********************
  END FUNCTION genderiv
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
