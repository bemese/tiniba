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
printf "\t${blue}-----------------------------${NC}\n"
}
##
##
if [ "$#" -eq 0 ]
    then   # Script needs at least one command-line argument.
    Line
    printf "\t${blue}adds the columns of two files \n"
    printf "\t${blue}Usage:\n"
    printf "\t${BLUE}s-add-two-files.sh ${red}#-columns${NC} ${red}file-1${NC} ${red}file-2${NC}  ${red}out-file${NC}\n"
    Line
exit 1
fi
if [ ! "$#" -eq 0 ]
then
    col=$1
    f1=$2
    f2=$3
    of=$4
# checks that the input files exist
if [[ -f $f1 && -f $f2 ]] 
then
    Line
    printf "\tfiles exist\n"
    Line    
    else
    Line
    printf "\tcheck for the existence of the files\n"
    Line    
    exit 1
fi
# checks that the number of rows is the same
    row1=`wc -l $f1 | awk '{print $1}'`
    row2=`wc -l $f2 | awk '{print $1}'`
    if [  $row1 != $row2 ]
    then
	Line
	printf "\trow1=$row1 different from row2=$row2\n"
	printf "\t${RED}check and run again\n"
	Line
	exit 1
    fi
    ln -s $f1 fort.1
    ln -s $f2 fort.2
    echo $row1 $col | $TINIBAU/add/r-add-2-files
    mv fort.3 $of
    rm fort.1 fort.2
    Line
    printf "\tadded files in $of \n"
    Line
###
fi
