#!/bin/bash 
# 
# runs k-points from abinit file symmetry information
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
    function Line {
	printf "\t${BLUE}=============================${NC}\n"
    }
##
clear
host=`hostname`
case=`echo $PWD | awk -F / '{print$NF}'`
dir=$PWD
#exec=/home/bms/ras/utils/3D/ibz_new/trunk/ibz
    if [ "$host" == 'medusa' ]; then
	exec=$HOME/abinit_shells/ibz/ibz
    fi
    if [ "$host" == 'quad01' ]; then
	exec=$HOME/abinit_shells/ibz/ibz-quad
    fi
if [ "$#" -eq 0 ]
    then   # Script needs at least one command-line argument.
#    echo -e ${CYAN} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%${NC}
#    echo -e ${RED} %%%%% Valid only for B=0 %%%%%${NC}
#    echo -e ${CYAN} %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%${NC}
    Line
    printf "\t${CYAN}rklist.sh${NC} ${blue}Nx Ny Nz${NC}\n"
    printf "\tfor a surface Nz = 2  and Lx*Ly unit cell => Ny=Nx/(Lx*Ly)=odd\n"
    printf "\t${red}if the system is not centrosymmetric, ${blue}just say no=${RED}0${NC}\n"
    Line
    exit 1
fi
####
if [ $1 != '0' ] 
    then
    mkdir tmp
    cd tmp
    echo $1 $2 $3 > grid
    cp ../symmetries/pvectors .
    cp ../symmetries/sym.d .
# check if the system was rendered no-centrosymmetric
# via an odd_rank.sh call
    yn=`awk '{print $1}' ../ifcentrosymmetric`
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
    echo $exec -abinit -tetrahedra -cartesian -symmetries -reduced -mesh 
    $exec -abinit -tetrahedra -cartesian -symmetries -reduced -mesh 
#    rm fort.83
    Nk=`wc kpoints.reciprocal | awk '{print $1}'`
    mv kpoints.reciprocal ../$case.klist_$Nk
    mv kpoints.cartesian ../symmetries/$case.kcartesian_$Nk
    mv tetrahedra ../symmetries/tetrahedra_$Nk
    mv Symmetries.Cartesian ../symmetries/Symmetries.Cartesian_$Nk
    mv IBZsymmetries ../symmetries/IBZsymmetries
    cd ..
    echo -e ${CYAN}%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%${NC}
    echo -e you just generated ${RED}Nk=$Nk${NC} kpoints  $case.klist_$Nk and symmetries/$case.kcartesian_$Nk
    echo -e ${CYAN}%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%${NC}
    rm -rf tmp hoy
    rm -rf endpoint.txt startpoint.txt hoy klist_length.txt error*
fi
