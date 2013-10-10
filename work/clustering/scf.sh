#!/bin/bash
# runs the scf in node01
##
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
NC='\e[0m' # No Color
##
# reads TINIBA version from version-tiniba.txt
source version-tiniba.txt
#
##
function Line {
    printf "\t${BLUE}=============================${NC}\n"
}
##
trunc_dir=$HOME/tiniba/$ver/clustering/itaxeo
declare -a nodeArray_scf
nodeArray_scf=(`cat .machines_scf`)
nodo=`$trunc_dir/trunc.sh ${nodeArray_scf[0]}`
if [ $nodo = 'node' ]
    then
    nodo=xeon
fi
##
host=`hostname`
case=$1
diro=$2
serialp=$3
## executables
## old xeon
##
## mpich
mpich_dir_xeon=/usr/local/mpich_gm_intel9/bin
mpich_dir_itanium=/usr/local/mpich-itanium-intel9/bin
#
##
np=`wc .machines_scf | awk '{print $1}'`
# serial
if [ $serialp == '1' ]
    then
    printf "\tyou are in $PWD at $host running serial SCF: be more patient\n"
    $absexec < $case.files > log
#    echo -e ${RED} TERMINA PRUEBA ${NC}
fi
# paralel
if [ $serialp == '2' ]
    then
    Line
    printf "\tRunning SCF from ${BLUE}$host${NC} in ${RED}parallel at $nodo${NC}\n"
    Line
    if [ $nodo = 'xeon' ]
	then
	$mpich_dir_xeon/mpirun -np $np  -machinefile .machines_scf $abp_exec_xeon < $case.files > log
    fi
    if [ $nodo = 'itanium' ]
	then
	dir_scf=$PWD
	ssh itanium01 "cd $dir_scf;$mpich_dir_itanium/mpirun -np $np  -machinefile .machines_scf $abp_exec_itanium < $case.files > log"
    fi
    if [[ $nodo = 'quad' || $nodo = 'hexa' ]]
    then
	cd $diro
	rm -f finished_scf       
	/home/$USER/tiniba/$ver/clustering/itaxeo/runSCF_19_Octubre_2009.sh 2          
    fi
fi
#
touch -f finished_scf
