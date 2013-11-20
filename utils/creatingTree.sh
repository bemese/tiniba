#!/bin/bash
## jl septiembre 21 2007 13:05
## LAST MODIFICAION : MARTES 02 Marzo 2010 by Cabellos 
## LAST MODIFICAION : LUNES  05 Abril 2010 by Cabellos 
## bueno recursivo 
      RED='\e[0;31m'
     BLUE='\e[0;34m'
      BLU='\e[1;34m'
     CYAN='\e[0;36m'
    GREEN='\e[0;32m'
      GRE='\e[1;32m'
   YELLOW='\e[1;33m'
  MAGENTA='\e[0;35m'
     MGTA='\e[0;35m'
       NC='\e[0m' # No Color

 function Line {
      printf "\t${BLUE}=============================${NC}\n"
       }
     

function ExistDir {
   if [ $# -eq 0 ];then 
    printf " Function: ExitDir() \n"
    printf " Usage: ExitDir dirQueBusco\n"
    exit 127
   fi 
   ###===============
   LOCALCASO=$1
   VAR=0
   for dir in `echo *`;do
       if [ -d "$dir" ];then 
         if [ "$dir" == "$LOCALCASO" ];then
            VAR=1
            return $VAR
            
         fi 
            
       fi 
   done 
  }

function StopMe {
#      printf "\t${RED}Stoping right now... ${NC} `basename $0`\n"
      exit 127
       }
function search {
for dir in `echo *`;do
  if [ -d "$dir" ];then 
  zz=0                   
  while [ $zz != $1 ];do
      printf "      |"
      zz=`expr $zz + 1`  
  done
      printf "   +---${BLUE}$dir${NC}\n" 
      numdirs=`expr $numdirs + 1` 
         if cd "$dir";then 
            search `expr $1 + 1` 
            cd ..
         fi
  fi
done
}

      if [ $# -eq 0 ] || [ $# -gt 1 ] ;then
	  Line
	  printf "\tUsage:\n"
	  printf "\t${GREEN}`basename $0`${NC} case\n"
	  Line
	  StopMe
      fi 




 CASO=$1
 
 COUNTER=0
 NEWCASO=$CASO
#while [ "$COUNTER" -lt "10" ]; do
#    
#    printf "\t$COUNTER\n"
#done 

VAR=0
SALIDA=0
HAZDIR=1
printf "\t=====================\n"
while [ "$SALIDA" -lt "10" ]; do
     ExistDir $NEWCASO        
     if [ "$VAR" == "0" ];then
       SALIDA=10
        #printf "\tCOUNTER =$COUNTER\n"
      if [ $COUNTER -gt "0" ];then
        printf "\t=====================\n"
        printf "\t${MAGENTA}Do you want to do a new case${NC}? :"
        printf " ${BLUE}$NEWCASO ${NC} (yes/no)  : "
        read ANSWER 
          if [ -z $ANSWER ];then
           HAZDIR=0
           printf "\t${RED}No Making${NC} tree to run abinis and wien2k\n"
           exit 127
          else 
           RESP=$(echo "$ANSWER" | tr '[A-Z]' '[a-z]')
            if  [ "$RESP" == "yes" ] || [ "$RESP" == "y" ];then 
             HAZDIR=1
            else
              HAZDIR=0
              printf "\t${RED}No Making${NC} tree to run abinis and wien2k\n"
              exit 127
            fi 
         
          fi ##ANSWER
        fi ## COUNTER 

      fi ##var
      
######################################
  
     if [ "$VAR" == "1" ];then
        let "COUNTER+=1"
        printf "\t$PWD/${BLUE}$NEWCASO${NC} ... [${GREEN}exist${NC}]\n"
         if [ "$COUNTER" -lt "10" ];then
           APELLIDO='0'$COUNTER
         else
           APELLIDO=$COUNTER
         fi 
         NEWCASO=$CASO'_'$APELLIDO
     fi 
done 

###############################
    if [ "$HAZDIR" == "1" ];then
       printf "\t=====================\n"
       printf "\t${GREEN}Making${NC} tree to run abinis and wien2k \n"
       printf "\t$PWD/$NEWCASO/abinit-$CASO/${BLUE}$CASO${NC}\n"
       printf "\t$PWD/$NEWCASO/wien2k-$CASO/${BLUE}$CASO${NC}\n" 
       printf "\t=====================\n"
       mkdir -p $NEWCASO/abinit-$CASO/$CASO
       mkdir -p $NEWCASO/wien2k-$CASO/$CASO
    else
       printf "\t${RED}No Making${NC} tree to run abinis and wien2k\n"
    fi
       printf "\n"
       printf "\t=====================\n"
       printf "$PWD/${BLUE}$NEWCASO${NC}\n"
       
       cd $NEWCASO/
        numdirs=0
        search 0
       cd ..
       printf "\t=====================\n"
###############################            
##jl
##StopMe
