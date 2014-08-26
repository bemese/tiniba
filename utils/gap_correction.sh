#!/bin/bash

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
 function Line {
      printf "\t${BLUE}=============================${NC}\n"
       }
# Run the script for instructions.
# By Sean Anderson (sma@cio.mx)
clear       
case=`pwd | awk -F / '{print$NF}'`
EXP=$1

# counts the number of occupied states from acs_check/case.out file
    grep -n 'occ ' $case'_check'/$case.out > temp1
    iocc=`awk -F: '{print $1}' temp1`

    grep -n 'prtvol ' $case'_check'/$case.out > temp1
    iprtvol=`awk -F: '{print $1}' temp1`

    awk 'NR=='$iocc',NR=='$iprtvol'' $case'_check'/$case.out > temp2

# for spin-orbit each state has only one electron for a given spin
    grep -o 1.000 temp2 > temp3

    NVF=`wc temp3 | awk '{print $2}'`
    if [ $NVF == '0' ]
        then
        grep -o 2.000 temp2 > temp3
        NVF=`wc temp3 | awk '{print $2}'`
    fi

BN1=`expr $NVF + 1`
BN2=`expr $BN1 + 1`
echo 0 0 0 > $case.klist_G

if [ "$#" -eq 0 ]; then
    Line
    printf "\tThis utility will determine the energy gap (Eg) for gamma.\n\tSpecify the experimental gap and the utility will give you\n\tthe approximate scissors correction.\n\n\tUsage:\n\tgap_correction.sh [${RED}experimental gap (eV)${NC}]\n"
    Line
else
    if [ ! -a .machines_pmn.backup ]; then
    mv .machines_pmn.original .machines_pmn.backup
    fi
    head -1 .machines_pmn.backup > .machines_pmn.original
    run_tiniba.sh -r run -k G -N 0 -x 2 -e
    mv .machines_pmn.backup .machines_pmn.original
    cp .machines_pmn.original .machines_pmn
    clear
    printf "Band energies calculated for gamma.\n"
    EIG=`ls eigen_G*`
    GAP=`awk '{print $'$BN2'-$'$BN1'}' $EIG`
    SCI=`echo "$EXP - $GAP" | bc`
    Line
    printf "\tEnergy gap (Exp)= $EXP eV\n\tEnergy gap (LDA)= $GAP eV\n\tScissors correction = $SCI eV\n" > scissors_gamma
    cat scissors_gamma
    echo "tij=$SCI" > .scissors_gamma
    Line
    rm temp*
fi
