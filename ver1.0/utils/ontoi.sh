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
##
function Line {
printf "\t${cyan}--------------------${NC}\n"
}
#
case=`echo $PWD | awk -F / '{print$NF}'`
name=`whoami`
parent=`echo $PWD | awk -F / '{a=NF; print $(a-1)}'`
#
if [ "$#" -eq 0 ]
    then   # Script needs at least one command-line argument.
    Line
    printf "\t${blue}gets data from logfile${NC}\n"
    printf "\t${blue}Usage:\n${NC}"
    printf "\tontoi.sh [${red}1${NC}]-estimated time [${red}2${NC}]-whole file [${red}3${NC}]-tail\n"
    Line
exit 1
fi
##
if [ ! "$#" -eq 0 ]
then
    cual=$1
#    less .machines_pmn
    cases=`less .machines_pmn`
    L=0
    for i in ${cases[@]}
    do
	L=`expr $L + 1`
	if [ "$cual" -eq 1 ]
	then
	    echo $i $case'_'$L
	    grep estimated /data.$i/$name/workspace/$parent/$case'_'$L/logfile
	fi
	if [ "$cual" -eq 2 ]
	then
	    Line
	    printf "\t$i $case"_"$L\n"
	    more /data.$i/$name/workspace/$parent/$case'_'$L/logfile
	fi
	if [ "$cual" -eq 3 ]
	then
	    Line
	    printf "\t$i $case"_"$L\n"
	    tail /data.$i/$name/workspace/$parent/$case'_'$L/logfile
	fi
    done
##
fi
