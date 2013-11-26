#!/bin/bash
## LAST MODIFICATION :: 17 Marzo 2010 by J.L. Cabellos a las 09:55 

      RED='\e[0;31m'
     BLUE='\e[0;34m'
      BLU='\e[1;34m'
     CYAN='\e[0;36m'
    GREEN='\e[0;32m'
      GRE='\e[1;32m'
   YELLOW='\e[1;33m'
  MAGENTA='\e[0;35m'
      MAG='\e[0;35m'
       NC='\e[0m' # No Color

##====== DEFINITIONS ===========   
clear       
     WHERE=$HOME/tiniba/ver1.2/src_ibz
#     printf "\t${BLUE}`dirname $0`/${GREEN}`basename $0`${NC}\n "
     #### great idea jl jajaja september 30 2007
       BASEDIR=`dirname $PWD`
          CASO=`basename $PWD`
        PARENT=`basename $BASEDIR`
	yo=`hostname`
	if [[ "$yo" == "hexa"* || "$yo" == "medusa" ]]
	then
            IIBBZZ=$WHERE/ibz.hexa
	else
            IIBBZZ=$WHERE/ibz."`hostname  | sed 's/[0-9]//g'`"
	fi
     #### great idea jl jajaja Marzo 17 2010
##==============================
declare -a malla
###<><><>><><><><><><><><><><><><><><><><><>
###<><><>><><><> functions  <><><><><><><><>
###<><><>><><><><><><><><><><><><><><><><><>
 function StopMe {
#      printf "\t${RED}Stoping right now... ${NC} `basename $0`\n"
      exit 127    
       }
 function IsThereError {
     if [ -e killme ]; then
      printf "\t ${RED}Stoping right now ... ${NC} `basename $0`\n"
      rm -rf killme
      exit 127
     fi
    }
 function Line {
      printf "\t${BLUE}=============================${NC}\n"
       }
 function isThereFile {
      if [ ! -e "$1" ];then
      printf "\t${RED}Hold on!${NC} There isnt FILE: "
      printf "$1\n"
      printf "\t ${RED}Stoping right now ... ${NC} `basename $0`\n"
      exit 127 
      fi 
      }
###<><><>><><><><><><><><><><><><><><><><><>
###<><><>><><><> begin code <><><><><><><><>
###<><><>><><><><><><><><><><><><><><><><><>
  if [ "$#" -ne 4 ];then
      Line
      printf "\tUsage:\n\t`basename $0`"
      printf " [${GREEN}xGrid${NC}-odd]"
      printf " [${GREEN}yGrid${NC}-odd] [${GREEN}zGrid${NC}-odd]"
      printf " [${GREEN}abinit${NC} or ${GREEN}wien2k${NC}]\n"
      printf "\tfor a surface zGrid = ${red}2${NC}  and for a Lx*Ly unit cell => Ny=Nx/(Lx*Ly)=odd\n"
      printf "\t${red}if the system is not centrosymmetric, ${blue}just say no=${RED}0${NC}\n"
      Line
      exit 1
  fi 
       ####
        xxG=$1        
        yyG=$2
        zzG=$3
       CUAL=$4

   if [ -z $xxG ];then
      xxG=10
      printf "\t${RED}Hold on${NC} the value of the grid X = empty\n"
      printf "\t${GREEN}But taking the fault${NC}  grid X = $xxG\n"
      #StopMe
     fi
     if [ -z $yyG ];then
      yyG=10
      printf "\t${RED}Hold on${NC} the value of the grid Y = empty\n"
      printf "\t${GREEN}But taking the fault${NC}  grid Y = $yyG\n"
      #StopMe
      
     fi
     if [ -z $zzG ];then
      zzG=10
      printf "\t${RED}Hold on${NC} the value of the grid Z = empty\n"
      printf "\t${GREEN}But taking the fault${NC}  grid Z = $zzG\n"
      #StopMe
     fi
     ##
     if [ -z $CUAL ];then
      CUAL="abinit"
     fi 

###<><><><><><><><><><><><><><><><><><><><>
###<><><><><><><><><><><><><><><><><><><><>
###<><><><><><><><><><><><><><><><><><><><>
     if [ ! -e $IIBBZZ ];then
	 Line
	 printf "\t There is no file: $IIBBZZ \n"
	 Line
	 StopMe
     fi 
     if [ ! -e symmetries/pvectors ];then
     printf "\t There isnt FILE: symmetries/pvectors \n"
     printf "\t Run first: abinit_check.sh \n"
     StopMe
     fi
     if [ ! -e symmetries/sym.d ];then
     printf "\t There isnt FILE: symmetries/sym.d \n"
     printf "\t Run first: abinit_check.sh \n"
     StopMe
     fi
 
     if [ $CUAL == "wien2k" ];then 
       if [ ! -e $CASO.struct ];then 
        printf "\t you need a FILE:  $CASO.struct \n"
        StopMe
       fi 
       if [ ! -d $CASO ];then
       mkdir $CASO
       fi 
       cd $CASO
       printf "\tMaking FILE: grid with: $xxG  $yyG $zzG\n" 
       rm -f grid 
       echo $xxG $yyG $zzG > grid
       cp ../symmetries/pvectors .
       cp ../symmetries/sym.d .
       cp ../$CASO.struct .
       #isThereFile $WHERE/ibz.xeon
       isThereFile $IIBBZZ
  rm -f killme 
#$WHERE/ibz.xeon -wien2k -tetrahedra -cartesian -symmetries -reduced -mesh 
$IIBBZZ -wien2k -tetrahedra -cartesian -symmetries -reduced -mesh 

  if [ -e killme ];then  
   printf "Error with $IIBBZZ\n"
   StopMe
  fi   
    rm -f $CASO.struct
    isThereFile kpoints.reciprocal
    NKPT=`wc kpoints.reciprocal | awk '{print $1}'`
     cp kpoints.reciprocal ../$CASO.klist_$NKPT
     cp kpoints.reciprocal $CASO.klist_$NKPT
    isThereFile kpoints.cartesian  
     cp kpoints.cartesian ../symmetries/$CASO.kcartesian_$NKPT
     cp kpoints.cartesian $CASO.kcartesian_$NKPT
    isThereFile tetrahedra
     cp tetrahedra ../symmetries/tetrahedra_$NKPT
     cp tetrahedra   tetrahedra_$NKPT
    isThereFile Symmetries.Cartesian
     cp Symmetries.Cartesian ../symmetries/Symmetries.Cartesian_$NKPT
     cp Symmetries.Cartesian Symmetries.Cartesian_$NKPT

     isThereFile kpoints.wien2k
     cp kpoints.wien2k ../$CASO.kpoints.wien2k_$NKPT
     cp kpoints.wien2k  $CASO.kpoints.wien2k_$NKPT 
     printf "\n"
     
     printf "\t files generated\n"
     printf "\twien2k: ${MAGENTA}$CASO.kpoints.wien2k_$NKPT${NC}\n"
     printf "\twien2k: ${MAGENTA}$CASO.kpoints.wien2k_$NKPT -->${NC} " 
     #printf "\tabinit: ${MAGENTA}$CASO.klist_$NKPT${NC}\n"
     cp $CASO.kpoints.wien2k_$NKPT ../$CASO.klist
  
  printf "${MAGENTA}$CASO.klist${NC}\n"   

 
     printf "\t${BLUE}symmetries${NC}/"
     printf "${MAGENTA}$CASO.kcartesian_$NKPT${NC}\n"

     printf "\t${BLUE}symmetries${NC}/"
     printf "${MAGENTA}tetrahedra_$NKPT ${NC}\n"

     printf "\t${BLUE}symmetries${NC}/"
     printf "${MAGENTA}Symmetries.Cartesian_$NKPT ${NC}\n"   
    printf "\tyou just generated Nk=${GREEN}$NKPT${NC} kpoints \n" 
  
   printf "\t I found the end ...${GREEN}ok${NC}\n"
    
   cd ..
   exit 127
   fi #### end  wien2k 
   
    if [ $CUAL == "abinit" ];then 
      
      if [ ! -d TMP ];then
       mkdir TMP
      fi 
       cd TMP
       printf "\tMaking FILE: grid with: $xxG  $yyG $zzG\n" 
       rm -f grid 
       echo $xxG $yyG $zzG > grid
       cp ../symmetries/pvectors .
       cp ../symmetries/sym.d .
       isThereFile $IIBBZZ
# check if the system was rendered no-centrosymmetric
# via an odd_rank.sh call
    yn=`awk '{print $1}' ../.ifcentrosymmetric`
    if [ $yn == 'odd_rank' ]
    then
	echo 0 > fort.83
    fi
    if [ $yn == 'no' ]
    then
	echo 2 > fort.83
    fi
    if [ $yn == 'yes' ]
    then
	echo 1 > fort.83
	echo $yn
    fi
#
# $WHERE/ibz.xeon -abinit -tetrahedra -cartesian -symmetries -reduced -mesh
  $IIBBZZ -abinit -tetrahedra -cartesian -symmetries -reduced -mesh
    isThereFile kpoints.reciprocal
    NKPT=`wc kpoints.reciprocal | awk '{print $1}'`
     mv kpoints.reciprocal ../$CASO.klist_$NKPT
    isThereFile kpoints.cartesian  
     mv kpoints.cartesian ../symmetries/$CASO.kcartesian_$NKPT
    isThereFile tetrahedra
     mv tetrahedra ../symmetries/tetrahedra_$NKPT
    isThereFile Symmetries.Cartesian
     mv Symmetries.Cartesian ../symmetries/Symmetries.Cartesian_$NKPT
     Line
     printf "\tyou just generated Nk=${GREEN}$NKPT${NC} kpoints in file: \n"    
     printf "\t${MAGENTA}$CASO.klist_$NKPT${NC}\n"
     printf "\tand the following files:\n"     
     printf "\t${BLUE}symmetries${NC}/"
     printf "${MAGENTA}$CASO.kcartesian_$NKPT${NC}\n"

     printf "\t${BLUE}symmetries${NC}/"
     printf "${MAGENTA}tetrahedra_$NKPT ${NC}\n"

     printf "\t${BLUE}symmetries${NC}/"
     printf "${MAGENTA}Symmetries.Cartesian_$NKPT ${NC}\n"
     Line  
     cd ..
     rm -rf TMP/
     StopMe
    fi   
    

  



