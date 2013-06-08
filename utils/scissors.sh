#!/bin/bash
##05 DECEMBER 2006 at 14:10 hrs. 
##FUNCTION: 
##shift the band gap  
##PARENTS:
##None
##CHILDREN:
##scissors !! fortran program
## cabellos last modification  LUNES 23 Febrero 2009 a las 14:12 hrs.: 

RED='\e[0;31m'
BLUE='\e[0;34m'
BLU='\e[1;34m'
cyan='\e[0;36m'
GREEN='\e[0;32m'
GRE='\e[1;32m'
YELLOW='\e[1;33m'
NC='\e[0m' # No Color
##
function Line {
printf "\t${cyan}--------------------${NC}\n"
}
##
 declare -a INPUTARGS
 HOST=`hostname`
 DIR=$PWD
 USER=$USER
 BASEDIR=`dirname $PWD`
 CASO=`basename $PWD`
 PARENT=`basename $BASEDIR`
 CUAL=$CASO.xyz
 WHERE=$TINIBA/utils/SRC_scissors
 RUNXEON=$WHERE/rscissors_xeon
 RUNITAN=$WHERE/rscissors_itanium
 RUNQUAD=$WHERE/rscissors_quad
 if [[ "$HOST" == "master"* ]]; then
      RUNA=$RUNXEON
 fi      
 if [[ "$HOST" == "node"* ]]; then
      RUNA=$RUNXEON
 fi      
 if [[ "$HOST" == "itanium"* ]]; then
      RUNA=$RUNITAN
 fi      
 if [[ "$HOST" == "quad"* ]]; then
      RUNA=$RUNQUAD
 fi      
 if [ -e $RUNA ];then 
#  printf "\t$RUNA \n"
  nada=0
 else 
  printf "\tThere isnt : RUNA \n"
  touch killme
  exit 127 
 fi 


##----------------------------
 if [ ! -e  setUpAbinit_$CASO.in  ] || [ ! -e  $CASO.xyz  ]
     then 
        printf " \n"
        printf "\tCAUSE:  \n"
        printf "\t${RED}There is not file(s): \n"
        printf "\t${BLUE}1.-setUpAbinit_$CASO.in ${NC} OR \n"
        printf "\t${BLUE}2.-$CASO.xyz ${NC}\n"
        printf "\tcheck if both of them exist ....   \n"
        printf "\t${RED}Stoping right now ... ${NC}\n"
        printf " \n"
        exit 1
 fi

 if [ $# -eq 0 ] || [ $# -lt 2 ] || [ $# -gt 2 ] ; then

         printf "${BLUE}********************************${NC}\n"
         printf "\tUsage:`basename $0` ${NC} ${RED}eigen_?  scissors_shift ${NC}\n"
         printf "${BLUE}********************************${NC}\n"
         exit 1
 fi
##----------------------------
    grep nband2 setUpAbinit_$CASO.in > tmpk_0
    NMAX=`head -1 tmpk_0 | awk '{print $2}'`
##----------------------------
     index=1
          for arg in "$@"
           do
               INPUTARGS[$index]=$arg
               let "index+=1"
           done
#     Line
     printf "\teigen energies FILE is: ${BLUE}${INPUTARGS[1]} ${NC} \n"
     printf "\tscissors correction is: ${BLUE}${INPUTARGS[2]} ${NC} \n"
     echo ${INPUTARGS[2]} > tijeras
     Line
   
    ## CHECK FOR  $CASO'_check' coz  
    if [ ! -d   $CASO'_check' ]; then
     printf "\t There is not directory :${BLUE} $CASO'_check'${NC}\n"
     printf "\t In order to generate one you have to run:\n "
     printf "\t ~/abinit_shell/utils/abinit_check 1 \n"
     printf "\t ~/abinit_shell/utils/abinit_check 2 \n"
     printf "\t If this dosent make sense for you ...better ask.. \n"
     printf "\t${RED}Stoping right now ${NC}\n"
     exit 1
    fi 
    ##-----------------------------------
    grep -n 'occ ' $CASO'_check'/$CASO.out > tmpk_1
    IOCC=`awk -F: '{print $1}' tmpk_1`
    grep -n 'prtvol ' $CASO'_check'/$CASO.out > tmpk_2
    IPRTVOL=`awk -F: '{print $1}' tmpk_2`
    awk 'NR=='$IOCC',NR=='$IPRTVOL'' $CASO'_check'/$CASO.out > tmpk_3
    grep -o 1.000 tmpk_3 > tmpk_4
    NBANDV=`wc tmpk_4 | awk '{print $2}'`
    if [ $NBANDV == '0' ]
	then
	grep -o 2.000 tmpk_3 > tmpk_4
	NBANDV=`wc tmpk_4 | awk '{print $2}'`
    fi
    NBANDC=`expr $NMAX - $NBANDV`
    rm -rf tmpk_*  ## ERASE ALL TMP FILES 
    printf "\tINFO FROM: /$CASO"_check"/$CASO.out \n"
    printf "\t   Number of Valence Bands  : ${BLUE}$NBANDV ${NC}\n "
    printf "\tNumber of Conduction Bands  : ${BLUE}$NBANDC ${NC}\n "
    printf "\t     Number of total Bands  : ${BLUE}$NMAX   ${NC}\n "
    ##-----------------------------------
      
    if [ ! -e  ${INPUTARGS[1]}_ORIGINAL  ];then    
    printf "\tmake a backup "
    printf "of your original FILE: \n"             
    printf "\t${BLUE} ${INPUTARGS[1]}${NC} ===>${BLUE} ${INPUTARGS[1]}_ORIGINAL ${NC}\n" 
     cp  ${INPUTARGS[1]}  ${INPUTARGS[1]}_ORIGINAL   
     else
#    printf "\t${INPUTARGS[1]}_ORIGINAL ${GREEN}copied to${NC} ${INPUTARGS[1]}\n" 
     cp  ${INPUTARGS[1]}_ORIGINAL   ${INPUTARGS[1]}  
     fi 

   NUMBEROFROWS=`awk 'END{print NF}' ${INPUTARGS[1]}` 
   NUMBEROFLINES=`awk 'END{print NR}' ${INPUTARGS[1]}` 
   printf  "\t        Number of k-points  : ${BLUE}$NUMBEROFLINES${NC}\n"
#   printf  "\tNumber of Rows=$NUMBEROFROWS=Num. of Bands + 1\n"
#   printf "\t this info come from scissors.f90\n"
#   printf "\t-------------------------------\n"
   rm -rf fort.35
 #./scissors namefile  $NUMBEROFROWS  $NUMBEROFLINES $NBANDV shift_gap 


val="$(echo ${INPUTARGS[2]}| sed 's/0.//')"


if [ $val -eq 0 ];then 
#printf "\t Your Tijeras Correction is(ZERO):  ${INPUTARGS[2]} \n"
#printf "\t ${GREEN}I did nothing to a file ${INPUTARGS[1]} ${NC}\n"
nada=0
else
$RUNA ${INPUTARGS[1]}  $NUMBEROFROWS  $NUMBEROFLINES $NBANDV ${INPUTARGS[2]}
echo "$RUNA ${INPUTARGS[1]}  $NUMBEROFROWS  $NUMBEROFLINES $NBANDV ${INPUTARGS[2]}"
         rm -rf ${INPUTARGS[1]}
         printf "\t${BLUE}********************************${NC}\n"
         printf  "\tmoving fort.35 to ${INPUTARGS[1]}\n"
         mv fort.35  ${INPUTARGS[1]}
         printf "\t${BLUE}********************************${NC}\n"
fi 




