#!/bin/bash
##
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
NC='\e[0m' # No Color
##
    function Line {
	printf "\t${BLUE}=============================${NC}\n"
    }
##
clear
##
if [ "$#" -eq 0 ]
    then
    Line
    printf "\tGathers the results for DSP calculation \n"
    printf "\t${BLUE}using the kk files only!${NC}\n"
    printf "\tRun in ${RED}res${NC} directory\n"
    printf "\tusage:\n"
    printf "\t${blue}dsp.sh${NC} [${red}full/Layer${NC}] [${red}scissors${NC}] [${red}Nc${NC}]\n"
    Line
    exit 1
fi
if [ ! "$#" -eq 0 ]
    then
## gets the las name from calChi1.sm*middle*
    if [ "$1" = "full" ]
    then
	f1=`ls chi1.kk*scissor*$2*Nc*$3`
	f2=`ls zeta.kk*scissor*$2*Nc*$3`
	awk '{print $1,$3,$5}' $f1 > hoy1
	awk '{print $2}' $f2 > hoy2
    else
	f1=`ls ndotccp.kk*_$1_*scissor*$2*Nc*$3`
	f2=`ls calZeta.kk*_$1_*scissor*$2*Nc*$3`
	awk '{print $1,$2,$3}' $f1 > hoy1
	awk '{print $2}' $f2 > hoy2
    fi
    paste hoy1 hoy2 > dsp.kk-$1-scissors-$2-Nc-$3
    Line    
    printf "\toutput:${blue}dsp.kk-$1-scissors-$2-Nc-$3${NC}\n"
    Line
#    rm hoy*
fi
