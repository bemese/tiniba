#!/bin/bash
## please keep this history.
## LAST MODIFICATION :  Febrero 18 2010 by Cabellos  a 16:52
## LAST MODIFICATION :  Febrero 18 2010 by Cabellos  a 18:06
## AUTHOR            :  J.L. Cabellos 
## REPORTING BUGS    :  Report bugs to <sollebac@gmail.com>.

      RED='\e[0;31m'
     BLUE='\e[0;34m'
      BLU='\e[1;34m'
     CYAN='\e[0;36m'
    GREEN='\e[0;32m'
      GRE='\e[1;32m'
   YELLOW='\e[1;33m'
       NC='\e[0m' # No Color

        WORKZPACE="workspace" 
          BASEDIR=`dirname $PWD`
           PARENT=`basename $BASEDIR`
             CASO=`basename $PWD`
 function Line {
      printf "\t${BLUE}=============================${NC}\n"
       }
     


 declare -a FALSEMACHINESpmn
 declare -a VIVOS
 declare -a MUERTOS
### hha this all the cluster jl
MAQ501[1]="node01";MAQ501[12]="node12";MAQ501[23]="quad04"
MAQ501[2]="node02";MAQ501[13]="node13";MAQ501[24]="quad05"
MAQ501[3]="node03";MAQ501[14]="node14";MAQ501[25]="quad06"
MAQ501[4]="node04";MAQ501[15]="node15";MAQ501[26]="quad07"
MAQ501[5]="node05";MAQ501[16]="itanium01";MAQ501[27]="quad08"
MAQ501[6]="node06";MAQ501[17]="itanium02";MAQ501[28]="quad09"
MAQ501[7]="node07";MAQ501[18]="itanium03";MAQ501[29]="quad10"
MAQ501[8]="node08";MAQ501[19]="itanium04";MAQ501[30]="quad11"
MAQ501[9]="node09";MAQ501[20]="quad01";MAQ501[31]="quad12"
MAQ501[10]="node10";MAQ501[21]="quad02";MAQ501[32]="quad13"
MAQ501[11]="node11";MAQ501[22]="quad03";MAQ501[33]="quad14"
MAQ501[34]="hexa1";MAQ501[35]="hexa2";MAQ501[36]="hexa3"
MAQ501[37]="hexa4";MAQ501[38]="hexa5";MAQ501[39]="hexa6"
MAQ501[40]="hexa7";MAQ501[41]="hexa8";MAQ501[42]="hexa9"
MAQ501[43]="hexa10";MAQ501[44]="hexa11";MAQ501[45]="hexa12"
MAQ501[46]="hexa13";MAQ501[47]="hexa14";MAQ501[48]="hexa15"
MAQ501[49]="hexa16";MAQ501[50]="hexa17";MAQ501[51]="hexa18"
MAQ501[52]="hexa19";MAQ501[53]="hexa20";MAQ501[54]="hexa21"
MAQ501[55]="hexa22";MAQ501[56]="hexa23";MAQ501[57]="hexa24"
MAQ501[58]="hexa25";MAQ501[59]="hexa26";MAQ501[60]="hexa27"
MAQ501[61]="hexa28";MAQ501[62]="hexa29";MAQ501[63]="hexa30"
MAQ501[64]="hexa31";MAQ501[65]="hexa32";MAQ501[66]="hexa33"
MAQ501[67]="hexa34";MAQ501[68]="hexa35";MAQ501[69]="hexa36"
IPES[1]="192.168.1.1";IPES[12]="192.168.1.12";IPES[23]="192.168.1.23"
IPES[2]="192.168.1.2";IPES[13]="192.168.1.13";IPES[24]="192.168.1.24"
IPES[3]="192.168.1.3";IPES[14]="192.168.1.14";IPES[25]="192.168.1.25"
IPES[4]="192.168.1.4";IPES[15]="192.168.1.15";IPES[26]="192.168.1.26"
IPES[5]="192.168.1.5";IPES[16]="192.168.1.16";IPES[27]="192.168.1.27"
IPES[6]="192.168.1.6";IPES[17]="192.168.1.17";IPES[28]="192.168.1.28"
IPES[7]="192.168.1.7";IPES[18]="192.168.1.18";IPES[29]="192.168.1.29"
IPES[8]="192.168.1.8";IPES[19]="192.168.1.19";IPES[30]="192.168.1.30"
IPES[9]="192.168.1.9";IPES[20]="192.168.1.20";IPES[31]="192.168.1.31"
IPES[10]="192.168.1.10";IPES[21]="192.168.1.21";IPES[32]="192.168.1.32"
IPES[11]="192.168.1.11";IPES[22]="192.168.1.22";IPES[33]="192.168.1.33"
IPES[34]="172.17.1.1"
IPES[35]="172.17.1.2"
IPES[36]="172.17.1.3"
IPES[37]="172.17.1.4"
IPES[38]="172.17.1.5"
IPES[39]="172.17.1.6"
IPES[40]="172.17.1.7"
IPES[41]="172.17.1.8"
IPES[42]="172.17.1.9"
IPES[43]="172.17.1.10"
IPES[44]="172.17.1.11"
IPES[45]="172.17.1.12"
IPES[46]="172.17.1.13"
IPES[47]="172.17.1.14"
IPES[48]="172.17.1.15"
IPES[49]="172.17.1.16"
IPES[50]="172.17.1.17"
IPES[51]="172.17.1.18"
IPES[52]="172.17.1.19"
IPES[53]="172.17.1.20"
IPES[54]="172.17.1.21"
IPES[55]="172.17.1.22"
IPES[56]="172.17.1.23"
IPES[57]="172.17.1.24"
IPES[58]="172.17.1.25"
IPES[59]="172.17.1.26"
IPES[60]="172.17.1.27"
IPES[61]="172.17.1.28"
IPES[62]="172.17.1.29"
IPES[63]="172.17.1.30"
IPES[64]="172.17.1.31"
IPES[65]="172.17.1.32"
IPES[66]="172.17.1.33"
IPES[67]="172.17.1.34"
IPES[68]="172.17.1.35"
IPES[69]="172.17.1.36"
function Line {
    printf "\t${BLUE}=============================${NC}\n"
}
function findMaq {
 ALLOWED="0"
 SALIDA="1000"
 local kk=1
 local NOMAQ501a=`echo ${#MAQ501[@]}`
   for ((kk=1;kk<=($NOMAQ501a); kk++));do       
       if [ "${MAQ501[$kk]}" == "$1" ];then 
          SALIDA="$kk"
          ALLOWED=1
       fi 
   done 
}
function findIndex {
 INDES="1000"
 local kk=1
 local NOMAQ501a=`echo ${#MAQ501[@]}`
   for ((kk=1;kk<=($NOMAQ501a); kk++));do       
       if [ "${MAQ501[$kk]}" == "$1" ];then 
          INDES="$kk"
       fi 
   done 
}
########### checks for nodes to be alive
#### first .machines_scf
aviso=0
if [ ! -e .machines_scf.original ];then
    Line
    printf "\t ${RED}There is not .machines_scf.original${NC} create one.\n"
    Line
        exit 0
else
    cp .machines_scf.original .machines_scf
 FALSEMACHINESscf=(`cat .machines_scf`)
  NOFALSEMACHINESscf=`echo ${#FALSEMACHINESscf[@]}`
   jj=0
    mm=0
     nn=0
      rm -f .machines_scf
       touch .machines_scf
         for ((hh=0;hh<=($NOFALSEMACHINESscf-1); hh++));do 
           findMaq ${FALSEMACHINESscf[$hh]}
            if [ "$ALLOWED" == "0" ];then
		printf "\t${BLUE}========================${NC}\n"
		printf "\t The node ${RED}${FALSEMACHINESscf[$hh]}${NC} does not exist, it has been eliminated\n"
		printf "\t and the new list .machines_scf  will be used \n"
		printf "\t Remove them if you wish from your .machines_scf.original file\n"
	    fi
            if [ "$ALLOWED" == "1" ];then 
             IPT=`nmap --max_rtt_timeout 20  -oG - -p 514  ${FALSEMACHINESscf[$hh]} | grep open | cut -d" " -f2`
               findIndex ${FALSEMACHINESscf[$hh]}
                 if [ "$IPT" == "${IPES[$INDES]}" ];then 
                   let jj++
                   let nn++ 
                      echo ${FALSEMACHINESscf[$hh]} >> .machines_scf
                 else 
                   let jj++
                   let mm++
                    MUERTOSSCF[$mm]=${FALSEMACHINESscf[$hh]}
                 fi 
           fi 
         done     
            NOMUERTOSSCF=`echo ${#MUERTOSSCF[@]}`
             if [ $NOMUERTOSSCF -gt 0 ];then 
		 aviso=1
		 printf "\t${BLUE}========================${NC}\n"
		 printf "\tYour original .machines_scf.original has $NOMUERTOSSCF dead nodes that have been eliminated\n"
		 printf "\t and the new list .machines_scf  will be used \n"
		 printf "\t Remove them if you wish from your .machines_scf.original file\n"
		 for ((hh=1;hh<=($NOMUERTOSSCF); hh++));do
                     printf "\t%4d%12s${RED}%7s${NC}\n" "$hh" "${MUERTOSSCF[$hh]}" "Dead" 
		 done 
             fi 
fi 
######
######

if [ ! -e .machines_scf ];then
        printf "\t ${RED}There is not .machines_scf${NC}\n"
        touch -f killme
        exit 0
else
       MACHINESscf=(`cat .machines_scf`)
     NOMACHINESscf=`echo ${#MACHINESscf[@]}`
fi
if [ $aviso -eq 0 ] 
then
Line
printf "\t${BLU} all the nodes in .machines_scf are working: congratulations!\n${NC}"
Line
fi
#### end scf
#### second .machines_pmn 
aviso=0
if [ ! -e .machines_pmn.original ];then
    printf "\t ${RED}There is not .machines_pmn.original${NC} create one.\n"
       # touch -f killme
        exit 0
else
    cp .machines_pmn.original .machines_pmn 
 FALSEMACHINESpmn=(`cat .machines_pmn`)
  NOFALSEMACHINESpmn=`echo ${#FALSEMACHINESpmn[@]}`
   jj=0
    mm=0
     nn=0
      rm -f .machines_pmn
       touch .machines_pmn
         for ((hh=0;hh<=($NOFALSEMACHINESpmn-1); hh++));do 
           findMaq ${FALSEMACHINESpmn[$hh]}
           if [ "$ALLOWED" == "0" ];then
	       printf "\t${BLUE}========================${NC}\n"
               printf "\t The node ${RED}${FALSEMACHINESpmn[$hh]}${NC} does not exist, it has been eliminated\n"
               printf "\t and the new list .machines_pmn  will be used \n"
               printf "\t Remove them if you wish from your .machines_pmn.original file\n"
	   fi
            if [ "$ALLOWED" == "1" ];then 
             IPT=`nmap --max_rtt_timeout 20  -oG - -p 514  ${FALSEMACHINESpmn[$hh]} | grep open | cut -d" " -f2`
               findIndex ${FALSEMACHINESpmn[$hh]}
                 if [ "$IPT" == "${IPES[$INDES]}" ];then 
                   let jj++
                   let nn++ 
                      echo ${FALSEMACHINESpmn[$hh]} >> .machines_pmn
                 else 
                   let jj++
                   let mm++
                    MUERTOS[$mm]=${FALSEMACHINESpmn[$hh]}
                 fi 
           fi 
         done     
            NOMUERTOS=`echo ${#MUERTOS[@]}`
             if [ $NOMUERTOS -gt 0 ];then 
		 aviso=1
		 printf "\t${BLUE}========================${NC}\n"
		 printf "\tYour original .machines_pmn.original has $NOMUERTOS dead nodes that have been eliminated\n"
               printf "\t and the new list .machines_pmn  will be used \n"
               printf "\t Remove them if you wish from your .machines_pmn.original file\n"
		 for ((hh=1;hh<=($NOMUERTOS); hh++));do
                     printf "\t%4d%12s${RED}%7s${NC}\n" "$hh" "${MUERTOS[$hh]}" "Dead" 
		 done 
             fi 
fi 
######
######

if [ ! -e .machines_pmn ];then
        printf "\t ${RED}There is not .machines_pmn${NC}\n"
        touch -f killme
        exit 0
else
       MACHINESpmn=(`cat .machines_pmn`)
     NOMACHINESpmn=`echo ${#MACHINESpmn[@]}`
fi
if [ $aviso -eq 0 ] 
then
Line
printf "\t${BLU} all the nodes in .machines_pmn are working: congratulations!\n${NC}"
Line
fi
#### end pmn

##################################################
########## WHERE WORK ############################
##################################################
   for ((hh=0;hh<=($NOMACHINESpmn-1); hh++));do        
           let kk++  
           CAZO=$CASO"_"$kk
           WHEREWORKREMOTE[$hh]="/data/$USER/$WORKZPACE/$PARENT/$CAZO"
            WHEREWORKLOCAL[$hh]="$PWD/$CAZO"      
   done

##################################################
####========REMOTE DIRECTORIOS====================
##################################################
    printf "\t${BLUE}========================${NC}\n"
    printf "\t${CYAN}Checking${NC} REMOTE directories \n"
     for ((hh=0;hh<=($NOMACHINESpmn-1); hh++));do
        REMOTESERVER=${MACHINESpmn[$hh]}
         DIRQ=${WHEREWORKREMOTE[$hh]}
         BASE=`dirname $DIRQ`
         DIRE=`basename $DIRQ`
        printf "\t[$REMOTESERVER]:$BASE/${BLUE}$DIRE${NC}  "          
        EXISTE=`ssh $REMOTESERVER 'test -d '$DIRQ'; echo $?'`
          if [ $EXISTE -eq 0 ] ;then
           printf " [${GREEN}exist${NC}]\n"
          else
           printf " [${GREEN}making${NC}]\n"
           ssh $REMOTESERVER "mkdir -p $DIRQ"
          fi
         sleep .1
      done
   printf "\t${BLUE}========================${NC}\n"

##################################################
####========REMOTE DIRECTORIOS====================
##################################################
    printf "\t${CYAN}Checking${NC} LOCAL directories \n"
     for ((hh=0;hh<=($NOMACHINESpmn-1); hh++));do
        REMOTESERVER=${MACHINESpmn[$hh]}
         DIRQ=${WHEREWORKLOCAL[$hh]}
         BASE=`dirname $DIRQ`
         DIRE=`basename $DIRQ`
        printf "\t$BASE/${BLUE}$DIRE${NC} "          
           if [ -e "$DIRQ"  ] ;then
            printf " [${GREEN}exist${NC}]\n"
           else
            printf " [${GREEN}making${NC}]\n"
            mkdir -p $DIRQ
          fi
         sleep .1
      done
   printf "\t${BLUE}========================${NC}\n"
 exit 0
