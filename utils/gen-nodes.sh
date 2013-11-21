#!/bin/bash 
# 
# runs abinit to check for symmetries
#
#
##
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
NC='\e[0m' # No Color
##
## Functions
clear
##
function Line {
    printf "\t${BLUE}=============================${NC}\n"
}
## Star
if [ "$#" -eq 0 ]
    then
    Line
    printf "\t${cyan}Generates the Nodes to be run at${NC}\n"
    printf "\t${blue}gen-nodes.sh${NC} -a [${red}architecture${NC}] -n [${red}\"nodes\"${NC}] -m [${red}max${NC}/type] -t [${red}total${NC}]\n"
    Line
    exit 1
fi
# reads
while getopts ":a:n:m:t:" OPTION
do
     case $OPTION in
         a)
             arch=$OPTARG
             ;;
         n)
	     cases=($OPTARG) #the parenthesis () are so it reads an array 
             ;;
         m)
             max=$OPTARG
             ;;
         t)
             tot=$OPTARG
             ;;
         ?)
	     Line
             printf "\t${RED}reading error${NC}\n"
	     Line
             exit
             ;;
     esac
done
num=${#cases[@]}
total=$(expr $num \* $max)
if [ "$total" -lt "$tot" ]
then
    Line
    printf "\tnodes*max=$total less than total=$tot\n"
    printf "\tincrese number of nodes or max per node\n"
    Line
    exit 1
fi
if [ "$max" -gt "12" ]
then
    Line
    printf "\t${red}chose no more than 12 nuclei per node!${NC}\n"
    Line
    exit 1
fi
if [ -e .machines_pmn.original ]
then
    rm .machines_pmn.original 
fi
nn=0
for i in ${cases[@]}
do
    n=0
    while [ $n -lt $max ]
    do
	n=`expr $n + 1`
	nn=`expr $nn + 1`
	if [ "$nn" -le "$tot" ]
	    then
	    printf "\t$nn $n $arch$i\n"
	    echo $arch$i >> .machines_pmn.original
	else
	    exit 1
	fi
    done
done
exit 1
if [ "$type" == "hexa" ]
then
    while [ $n -le $total ] 
fi
