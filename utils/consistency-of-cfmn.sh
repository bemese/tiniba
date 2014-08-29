#!/bin/bash
##
## output
# standard: file_ii_components_Nk_Ecut_spin_
# layered: file_ii_components_Nk_Layer_Ecut_spin_Nc 
##
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
GREEN='\e[0;32m'
GRE='\e[1;32m'
YELLOW='\e[1;33m'
MAG='\e[0;35m'
NC='\e[0m' # No Color
##
## debug
function despulga {
Line
printf "\taqui\n"
exit 1
Line
}
## thanks
function gracias {
    printf "\tThanks for using ${cyan}TINIBA${NC}: ${RED}NO WARRANTIES WHATSOEVER\n${NC}"
}
##
function Line {
    printf "\t${cyan}--------------------${NC}\n"
}
##
exec="$TINIBA/utils/rconsistency-of-cfmn"
## input at will
case=`echo $PWD | awk -F / '{print$NF}'`
    latm="latm_new"
    grep nband setUpAbinit_$case.in > hoy
    grep -v \# hoy > hoy1
    grep -v kss hoy1 > hoy2
    Nband=`head -1 hoy2 | awk '{print $2}'`
    rm hoy*
if [ "$#" -eq 0 ]
    then
    Line
    printf "\tusage:\n"
    printf "\t${blue}first:${RED}only once${NC}\n"
    printf "\tconsistency-of-cfmn.sh ${RED}1 me_cfmn_*${NC}\n"
    printf "\tchoose:\n"
    ls me_cfmn_*
    printf "\t${blue}second:${RED}as many times as required${NC}\n"
    printf "\tconsistency-of-cfmn.sh ${RED}2 me_cfmn_*${NC}\n"
    Line
    exit 1
fi
if [ ! "$#" -eq 0 ]
    then
	file1=$2
	echo $file1 > hoy
	k=`awk -F_ '{print $3}' hoy`
	ecut=`awk -F_ '{print $5}' hoy`
	ori=`awk -F_ '{print $3"_"$4"_"$5}' hoy`
	file2=me_pmn_$k\_$ecut
	if [ "$1" == "1" ]
	then
	    Line
	    printf "\trunning for $file1 and $file2\n"
	    Line
	    ln -fs $file1 fort.2
	    ln -fs $file2 fort.1
	    echo $k $Nband | $exec
	    mv fort.3 me_pmn_consistency
	    rm -f fort*
	    Line
	    printf "\toutput:me_pmn_consistency\n"
	    Line
	fi
	if [ "$1" == "2" ]
	then
	    Line
	    printf "\t${blue}run all_responses.sh using me_cpmn_$ori${NC}\n"
	    printf "\t${blue}then${NC}:\n"
	    printf "\t>mv res/calChi1.sm* compa-ori.d\n"
	    printf "\t>mv me_cpmn_$ori me_cpmn_$ori.o\n"
	    printf "\t>cp me_pmn_consistency me_cpmn_$ori\n"
	    printf "\t${blue}run all_responses.sh as you just did${NC}\n"
	    printf "\t${blue}then${NC}:\n"
	    printf "\t>mv res/calChi1.sm* compa-new.d\n"
	    printf "\t>mv me_cpmn_$ori.o me_cpmn_$ori\n"
	    printf "\t${blue}plot ori.d and new.d\n"
	    printf "\t${blue}They ought to be identical\n"
	    printf "\t${red}If not: cry\n"
	    Line
	fi
fi
