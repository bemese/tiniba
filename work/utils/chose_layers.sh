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
case=`echo $PWD | awk -F / '{print$NF}'`
where=$HOME/tiniba/ver1.0/utils/programs_layer
xredcart=`grep -x xred setUpAbinit_$case.in` 
xcartjl=`grep -x xcart setUpAbinit_$case.in` 
## copies to save the original coordinates
if [ ! -e  $case.xyz.original ]
then
    cp $case.xyz $case.xyz.original
fi
## selects executables according to architecture
ANFITRION=`hostname`
##
if [[ -z $xredcart && -z $xcartjl ]]
    then
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
    echo -e ${RED}ERROR: neither corrdinates choosen, check Cartesian Coordinates at setUpAbinit_$case.in ${NC}
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
    exit 1
fi
if [ -z $xredcart ]
    then
    Line
    printf "\t${RED}xcart: Cartesian Coordinates at setUpAbinit_$case.in ${NC}\n"
    Line
else
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
    echo -e ${RED}xred: Reduced Coordinates at setUpAbinit_$case.in  ${NC}
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
fi


## checks if there are cart and red coordinates to use ONLY
## cart coordinates for the layers
if [ -e $case.xyz ]
    then
    if [  -e $case.xyz_cart ]
	then
	letrero=`diff -sq $case.xyz $case.xyz_cart`
	sino=`diff -sq $case.xyz $case.xyz_cart | awk '{print $NF}'`
	if [ $sino == 'differ' ]
	    then
	    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
	    echo -e ${RED}$letrero${NC}
	    echo -e "${RED}Therefore Info for layers taken from ${blue}$case.xyz_cart${NC} file" 
	    cual=$case.xyz_cart
	    if [ $xredcart != 'xred' ]
		then
		echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
		echo -e ${blue}$case.xyz identical to $case.xyz_red: ${RED}xcart at setUpAbinit_$case.in INCOMPATIBLE  ${NC}
		echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
	    fi
	    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
	else
	    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
	    echo -e ${RED}$letrero${NC}
	    echo -e "${RED}Therefore Info for layers taken from ${blue}$case.xyz${NC} file"
	    cual=$case.xyz
	    if [ $xredcart == 'xred' ]
		then
		echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
		echo -e ${blue}$case.xyz identical to $case.xyz_cart: ${RED}xred at setUpAbinit_$case.in INCOMPATIBLE  ${NC}
		echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
	    fi
	    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
	fi
    else
	Line
	    printf "\tInfo from the one and only ${blue}$case.xyz${NC} file\n"
	    cual=$case.xyz
	    Line
    fi
fi
##
if [  -e $case.struct ]
    then
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
    echo -e "Info from ${blue}$case.struct${NC} file"
    cual=$case.struct
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
fi
grep acell setUpAbinit_$case.in > hoy
Lslab=`grep acell hoy | awk '{print $4}'`
a1=`grep acell hoy | awk '{print $2}'`
a2=`grep acell hoy | awk '{print $3}'`
echo $a1 $a2 $Lslab > .acell.d
zmax=`head -1 $cual | awk '{print $3}'`
zmin=`tail -1 $cual | awk '{print $3}'`
numL=`wc layers.d.original | awk '{print $1}'`
#printf "\tLslab=$Lslab and Tlayers=$ztot, zmin=$zmin and zmax=$zmax\n"
printf "\tNumber of Layers=$numL\n"
Line
if [ "$#" -eq 0 ]
    then
    Line
    printf "\t${blue}chose_layer.sh${NC} [${red}layer number${NC} and/or ${red}middle${NC} and/or ${red}half-slab${NC} or ${red}all${NC}] \n"
    Line
    exit 1
fi
rm layers.d
if [ -e .info_layers ]
then
    rm .info_layers
fi
echo "There are $numL layers" > .info_layers
lista=${@}
for i in ${lista[@]}
do 
echo $lista > .lista_layers
printf "\tyou have chosen layer: $i\n"
echo you have chosen layer: $i >> .info_layers
if [ "$i" == "all" ]
    then
    cp layers.d.original layers.d
    Line
    printf "\tYou have choosen ${red}all${NC} the layers\n"
    Line
fi
lay=$i
if [ "$i" == "middle" ]
    then
    lay=`expr $numL / 2 + 1`
    printf "\tthe ${RED}middle${NC} layer is: $lay\n"
    printf "\tthe ${RED}middle${NC} layer is: $lay\n" >> .info_layers
fi
if [ "$i" == "half-slab" ]
    then
    awk '{print $0}' half_slab.d >> layers.d
else
    echo "awk 'NR==$lay{print \$0}' layers.d.original >> layers.d" > dogy
    chmod +x dogy
    dogy
    rm dogy
fi
done
rm  hoy
Line
printf "\toutput: ${red}layers.d${NC} and ${blue}.info_layers${NC}\n"
Line
exit 1
