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
if [ "$#" -eq 0 ]; then
    Line
    printf "\tGathers results for DSP calculation in a single file.\n"
    printf "\t${red}Remember to use unsmoothed (kk) files only.${NC}\n"
    printf "\tFor full DSP:\n"
    printf "\t${blue}dsp.sh${NC} [${red}chi1.kk_...${NC}] [${red}zeta.kk_...${NC}] [${red}zeta component column number${NC}]\n"
    printf "\tor for layered DSP:\n"
    printf "\t${blue}dsp.sh${NC} [${red}ndotccp.kk_...${NC}] [${red}calzeta.kk_...${NC}] [${red}zeta component column number${NC}]\n"
    Line
    exit 1
else
	CHI=$1
	ZET=$2
    CCN="\$$3"
    SFX=`echo "$ZET" | sed 's/^.*scissor/scissor/'`
    OUT=dsp.kk-"$SFX"

    awk '{print $1,$3,$5}' $CHI > values.chi
    awk '{print '$CCN'}' $ZET > values.zeta
    paste values.chi values.zeta > $OUT
    Line    
    printf "\toutput: ${blue}dsp.kk-$SFX${NC}\n"
    printf "\tYou can plot the DSP with gnuplot:\n"
    printf "\t${BLUE}p \""$OUT"\" u 1:(2*\$4/(\$2+\$3+1.e-10)) w l${NC}\n"
    Line
    rm values.*
fi
