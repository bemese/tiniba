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
## checks if there are cart and red coordinates to use ONLY
## cart coordinates for the layers
case=`echo $PWD | awk -F / '{print$NF}'`
where=$TINIBA/utils/programs_layer
xredcart=`grep xred setUpAbinit_$case.in | tail -1 | awk '{print $1}'` 
if [ $xredcart == 'xred' ]
    then
    Line
    echo -e ${RED}xred: Reduced Coordinates at setUpAbinit_$case.in  ${NC}
##
    if [  -e $case.xyz_red ]
	then
	letrero=`diff -sq $case.xyz $case.xyz_red`
	sino=`diff -sq $case.xyz $case.xyz_red | awk '{print $NF}'`
	if [ $sino == 'differ' ]
	    then
	    echo -e ${blue} $letrero${NC}
	    echo -e ${RED} INCOMPATIBLE with setUpAbinit_$case.in${NC}
	fi
    else
	echo -e ${RED} there is only $case.xyz, is it reduced?
    fi
    Line
##
else
    Line
    echo -e ${RED}xcart: Cartesian Coordinates at setUpAbinit_$case.in ${NC}
##
    if [  -e $case.xyz_cart ]
	then
	letrero=`diff -sq $case.xyz $case.xyz_cart`
	sino=`diff -sq $case.xyz $case.xyz_cart | awk '{print $NF}'`
	if [ $sino == 'differ' ]
	    then
	    echo -e ${blue} $letrero${NC}
	    echo -e ${RED} INCOMPATIBLE with setUpAbinit_$case.in${NC}
	fi
    else
	echo -e ${RED} there is only $case.xyz, is it cartesian?
    fi
    Line
##
fi
