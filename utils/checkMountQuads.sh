#!/bin/bash
## NAME     : checkMountQuads.sh
## CHILDREN : NONE; great no childs 
## please keep this history modification 
## LAST MODIFICATION : 17 Febrero 2010 at 13:34 hrs by Cabellos JL 
## LAST MODIFICATION : 17 Febrero 2010 at 14:25 hrs by Cabellos JL  
## AUTHOR            : J.L. Cabellos 
## FUNCTION          : Check if all nodes area alive
## Report bugs or modifications to <sollebac@gmail.com>.
## This is free software; .There is NO warranty. jl
## HOW TO MOUNT 
## dsh -w NODE "mount hostNode:/dir2mount /TargetDir
## dsh -w quad08 "mount quad01:/opt /opt"
## dsh -w quad10 "mount quad01:/homeib /homeib"
## dsh -N QUADS "cd /data/$USER/; pwd"
## checar todos los data de los nodos

   RED='\e[0;31m'
   BLUE='\e[0;34m'
    BLU='\e[1;34m'
   CYAN='\e[0;36m'
  GREEN='\e[0;32m'
    GRE='\e[1;32m'
 YELLOW='\e[1;33m'
    MAG='\e[0;35m'
     NC='\e[0m' # No Color
### define my diccionary 
### define my diccionary 
### define my diccionary 
DICC[0]="quad";DESCRIPTION[0]="Check if nodes QUAD are alive and mounted .../homeib." 
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
function findMaq {
OUTA=1
 SALIDA="Not-Found"
  NOMAQ501a=`echo ${#MAQ501[@]}`
   for ((kk=1;kk<=($NOMAQ501a); kk++));do       
     MAQ502a=${MAQ501[$kk]}
       if [ "$MAQ502a" == "$1" ];then 
         OUTA=0
          SALIDA="$kk"
       fi 
   done 
}
function help {
 printf "\tUsage: `dirname $0`/${GREEN}`basename $0`${NC} [${GREEN}option${NC}]\n"
  printf "\tWhere [${GREEN}option${NC}] could be:\n"
   NODICCa=`echo ${#DICC[@]}`
    for ((hhj=0;hhj<=($NODICCa-1); hhj++));do
      LOOK=${DICC[$hhj]}
       DESC=${DESCRIPTION[$hhj]}
        printf "\t${GREEN}$LOOK${NC}  : $DESC\n"
    done
       printf "\t Stoping right now ...\n"
        exit 0
}
  if [ $# -eq 0 ];then
   help
  fi
   ARCH=`echo $1  | tr '[A-Z]' '[a-z]'`
    NODICC=`echo ${#DICC[@]}`
     OUT=1
      for ((hh=0;hh<=($NODICC-1); hh++));do
       LOOK=${DICC[$hh]}
        if [ "$ARCH" == "$LOOK" ];then
         OUT=0       
        fi
      done
         if [ "$OUT" == "1" ];then
          help 
         fi 
           if [ $ARCH == "xeon" ];then
            ARCH="node"
           fi
           jj=0
            NOMAQ501=`echo ${#MAQ501[@]}`
             for ((hh=1;hh<=($NOMAQ501); hh++));do       
               MAQ502=${MAQ501[$hh]}
                if [[ "$MAQ502" == "$ARCH"* ]]; then
                  IPT=`nmap --max_rtt_timeout 20  -oG - -p 514  $MAQ502 | grep open | cut -d" " -f2`
                   findMaq $MAQ502 
                    if [ "$IPT" == "${IPES[$SALIDA]}" ];then
                      let "jj=jj+1"
                       printf "\t$jj) $MAQ502    [${GREEN}Alive${NC}] "
                       #rsh $MAQ502 ""df -h | grep -E 'quad01|nfs' | awk '{ print $4 }'""
                       #rsh $MAQ502 ""df -h | grep -E 'quad01|nfs' | awk '{ print $1, $4 }'""
                       WHAT=`rsh $MAQ502 ""df -h | grep -E 'nfsmaster|master' | awk '{ print "\t["$1"]", $4 }'" "`
                       if [ -z "$WHAT" ];then
                           WHAT="[nfsmaster:/home]"
                       printf "${RED}$WHAT${NC}\n"
                         if [[ $UID -eq 0 ]]; then
                         rsh $MAQ502 "mount master:/home /home"
                       printf  "\tmounting master:/home\n"
                         fi 


                       else 
                       printf "$WHAT [${GREEN}ok${NC}]\n"
                       fi 
                    else 
                       let "jj=jj+1"
                        printf "\t$jj) ${RED}$MAQ502${NC}    [${RED}Dead${NC}]\n"
                    fi 
                fi 
             done
read -p  "Press any key to continue ..."

jj=0
            NOMAQ501=`echo ${#MAQ501[@]}`
             for ((hh=2;hh<=($NOMAQ501); hh++));do       
               MAQ502=${MAQ501[$hh]}
                if [[ "$MAQ502" == "$ARCH"* ]]; then
                  IPT=`nmap --max_rtt_timeout 20  -oG - -p 514  $MAQ502 | grep open | cut -d" " -f2`
                   findMaq $MAQ502 
                    if [ "$IPT" == "${IPES[$SALIDA]}" ];then
                      let "jj=jj+1"
                       printf "\t$jj) $MAQ502    [${GREEN}Alive${NC}] "
                       #rsh $MAQ502 ""df -h | grep -E 'quad01|nfs' | awk '{ print $4 }'""
                       #rsh $MAQ502 ""df -h | grep -E 'quad01|nfs' | awk '{ print $1, $4 }'""
                       #WHAT=`rsh $MAQ502 ""df -h | grep -E 'quad' | awk '{ print "\t["$1"]", $4 }'" "`
                       #WHAT=`rsh $MAQ502 ""df -h | grep -E 'homeib' | awk '{ print "\t["$1"]", $4 }'" "`
                       WHATo=`rsh $MAQ502 ""df -h | grep -E 'opt' | awk '{ print "\t["$1"]", $4 }'" "`
                        #   if [ -z "$WHAT" ];then
                       #    WHAT="[quad01:/homeib]"
                       #printf "missing ${RED}$WHAT${NC}\n"
                       #else 
                       #printf "$WHAT [${GREEN}ok${NC}]\n"
                       #fi
                       if [ -z "$WHATo" ];then
                           WHATo="missing : quad01:/opt  \n"
                           
                            if [[ $UID -eq 0 ]]; then
                             rsh $MAQ502 "mount quad01:/opt /opt"
                              printf  "\tmounting quad01:/opt\n"
                            fi
                        printf "missing ${RED}$WHATo${NC}\n"   
                       else 
                       printf " $WHATo [${GREEN}ok${NC}]\n"  
                          
                       fi
                       #echo $WHATo
                       
                       #if [ -z "$WHAT" ];then
                       #    WHAT="missing : quad01:/homeib pos montalos "
                       #     if [[ $UID -eq 0 ]]; then
                       #  rsh $MAQ502 "mount mount quad01ib:/homeib /homeib"
                       #printf  "\tmounting quad01:/homeib /homeib\n"
                       #  fi 
                      #fi
                      # echo $WHAT
                    else 
                       let "jj=jj+1"
                        printf "\t$jj) ${RED}$MAQ502${NC}    [${RED}Dead${NC}]\n"
                    fi 
                fi 
             done

read -p  "Press any key to continue ..."


 jj=0
            NOMAQ501=`echo ${#MAQ501[@]}`
             for ((hh=1;hh<=($NOMAQ501); hh++));do       
               MAQ502=${MAQ501[$hh]}
                if [[ "$MAQ502" == "$ARCH"* ]]; then
                  IPT=`nmap --max_rtt_timeout 20  -oG - -p 514  $MAQ502 | grep open | cut -d" " -f2`
                   findMaq $MAQ502 
                    if [ "$IPT" == "${IPES[$SALIDA]}" ];then
                      let "jj=jj+1"
                       printf "\t$jj) $MAQ502    [${GREEN}Alive${NC}] "
                       #rsh $MAQ502 ""df -h | grep -E 'quad01|nfs' | awk '{ print $4 }'""
                       #rsh $MAQ502 ""df -h | grep -E 'quad01|nfs' | awk '{ print $1, $4 }'""
                       #WHAT=`rsh $MAQ502 ""df -h | grep -E 'quad' | awk '{ print "\t["$1"]", $4 }'" "`
                       WHAT=`rsh $MAQ502 ""df -h | grep -E 'homeib' | awk '{ print "\t["$1"]", $4 }'" "`
                       WHATo=`rsh $MAQ502 ""df -h | grep -E 'opt' | awk '{ print "\t["$1"]", $4 }'" "`
                        #   if [ -z "$WHAT" ];then
                       #    WHAT="[quad01:/homeib]"
                       #printf "missing ${RED}$WHAT${NC}\n"
                       #else 
                       #printf "$WHAT [${GREEN}ok${NC}]\n"
                       #fi
                       if [ -z "$WHATo" ];then
                           WHATo="missing : quad01:/opt  "
                            if [[ $UID -eq 0 ]]; then
                             rsh $MAQ502 "mount quad01:/opt /opt"
                              printf  "\tmounting quad01:/opt\n"
                         fi 
                          
                       fi
                       echo $WHATo
                       
                       if [ -z "$WHAT" ];then
                           WHAT="missing : quad01:/homeib pos montalos "
                            if [[ $UID -eq 0 ]]; then
                         rsh $MAQ502 "mount mount quad01ib:/homeib /homeib"
                       printf  "\tmounting quad01:/homeib /homeib\n"
                         fi 
                       fi
                       echo $WHAT
                    else 
                       let "jj=jj+1"
                        printf "\t$jj) ${RED}$MAQ502${NC}    [${RED}Dead${NC}]\n"
                    fi 
                fi 
             done



