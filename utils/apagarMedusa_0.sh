#!/bin/bash
## FUNCTION : shotdown medusa  
## AUTHOR   : Cabellos  
## DATE     : 04 JUNIO 2008 
## ## Report bugs or modifications to <sollebac@gmail.com>.
## Last Modification: 25 Febrero 2009
## Last Modification: 10 Marzo de 2010 by jl cabellos  at 19:26 
  RED='\e[0;31m'
   BLUE='\e[0;34m'
    BLU='\e[1;34m'
   CYAN='\e[0;36m'
  GREEN='\e[0;32m'
    GRE='\e[1;32m'
 YELLOW='\e[1;33m'
    MAG='\e[0;35m'
     NC='\e[0m' # No Color
## you can use this but im not going to ...jl
## dsh -N XEON ps -u jl
## dsh -N QUAD ps -u jl
### define my diccionary 
### define my diccionary 
### define my diccionary 
DICC[0]="xeon";DESCRIPTION[0]="Check if nodes XEON are alive." 
DICC[1]="quad";DESCRIPTION[1]="Check if nodes QUAD are alive." 
DICC[2]="itanium";DESCRIPTION[2]="Check if nodes ITANIUM are alive."
DICC[3]="master";DESCRIPTION[2]="Check if nodes master are alive."
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

if [[ $UID -ne 0 ]]; then
    printf  "\t${GREEN}$0${NC}  \n\tyou must be ${RED}root${NC} in order to run this ...\n"
     exit 1
 else
        printf  "\tbe carrefully $USER you are going to turn off all cluster ... !!\n" 
   fi
     ### 


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


     
     if [ "$#" -eq 0 ];then
       printf "\tUSAGE:  ${GREEN}`basename $0`${NC} quad \n"
       printf "\tUSAGE:  ${GREEN}`basename $0`${NC} xeon \n"
       printf "\tUSAGE:  ${GREEN}`basename $0`${NC} itanium \n"
       printf "\tUSAGE:  ${GREEN}`basename $0`${NC} master \n"
       printf "\tStoping right now ....\n"
       exit 1
       else 
         INPUT=`echo $1 | tr "[:upper:]" "[:lower:]"` 
     fi 
     ###
     ## nasty way ....
       if [ "$INPUT" != "quad" ];then
         if [ "$INPUT" != "xeon" ];then
           if [ "$INPUT" != "itanium" ];then
             if [ "$INPUT" != "master" ];then
             printf "\tUSAGE: ${GREEN}`basename $0`${NC} quad \n"
             printf "\tUSAGE: ${GREEN}`basename $0`${NC} xeon \n"
             printf "\tUSAGE: ${GREEN}`basename $0`${NC} itanium \n"
             printf "\tUSAGE: ${GREEN}`basename $0`${NC} master\n"
             printf "\tStoping right now ....\n"
              exit 1        
             fi
           fi 
         fi 
       fi 
    ###         
    

 ARCH=$INPUT
    NODICC=`echo ${#DICC[@]}`
     OUT=1
      for ((hh=0;hh<=($NODICC-1); hh++));do
       LOOK=${DICC[$hh]}
        # echo $LOOK $ARCH
        if [ "$ARCH" == "$LOOK" ];then
         OUT=0       
        fi
      done
        
           
         if [ "$OUT" == "1" ];then
           ### ESTO ES PARANOIA CABELLOS NUNCA VA A LLEGAR AQUI ...
          printf "\tUSAGE:  `basename $0` quad \n"
             printf "\tUSAGE:  `basename $0` xeon \n"
             printf "\tUSAGE:  `basename $0` itanium \n"
             printf "\tStoping right now ....\n"
              exit 1        
          
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
                       printf "\t$jj) $MAQ502    [${GREEN}Alive${NC}] ...Turn off now ...\n"
                        #dsh -f -w $MAQ502 /sbin/poweroff
                        #dsh -f -w $MAQ502 hostname
                       sleep 5
                    else 
                       let "jj=jj+1"
                        printf "\t$jj) ${RED}$MAQ502${NC}    [${RED}Dead${NC}]\n"
                    fi 
                fi 
             done
### nothing under here jl
 if [ "$INPUT" == "master" ];then
   ## at the present: a la sigueinte vez quita esta dependencia "cluster_ping" ..jl
   NODESDOWN=`cluster_ping | grep "nodes down:" | awk '{print $3}'` 
    
      if [ $NODESDOWN -ne 33 ];then
        printf "\t Tienes : $NODESDOWN nodo  abajo  tienen que ser 33 ...${GREEN}apagalos todos${NC}\n"
        printf "\t Tienes primero que apagar las "
        printf "\t tres plataformas: quad,itanium and xeon\n"
         printf "\tUSAGE:  ${GREEN}`basename $0`${NC} quad \n"
         printf "\tUSAGE:  ${GREEN}`basename $0`${NC} xeon \n"
         printf "\tUSAGE:  ${GREEN}`basename $0`${NC} itanium \n" 
         printf "\tStoping right now ....\n"
         exit 1 
      else 
        printf "\t${GREEN}Apagando el master: bye bye ...${NC}\n" 
        printf "\t /sbin/poewroff \n"
        /sbin/poweroff
        exit 1
      fi 

  fi 

    
    #  printf "\t NOTA: 25 Febrero 2009  cabellos \n"
    #  printf "\t The configuration of groups are in:\n"
    #  printf "\t /usr/local/cia/config/node_groups/ \n"
    #          ls /usr/local/cia/config/node_groups/
    #  printf "\t -----------------------------------\n"
    #  printf "\t${MAG}check if somebody is working ...${NC}\n"
    #  #dsh -N XEON ps -u jl
    #   cluster_ping | grep "nodes down:" | awk '{print $3}'   
  
   
