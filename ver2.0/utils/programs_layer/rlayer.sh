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
case=`echo $PWD | awk -F / '{print$NF}'`
where='/home/tonatiuh/abinit_shells/utils/rlayer'
##
if [  -e $case.xyz ]
    then
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
    echo -e "Info from ${blue}$case.xyz${NC} file"
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
fi
if [  -e $case.struct ]
    then
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
    echo -e "Info from ${blue}$case.struct${NC} file"
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
fi
grep acell setUpAbinit_$case.in > hoy
Lslab=`grep acell hoy | awk '{print $4}'`
a1=`grep acell hoy | awk '{print $2}'`
a2=`grep acell hoy | awk '{print $3}'`


echo $a1 $a2 $Lslab > acell.d
zmax=`head -1 $case.xyz | awk '{print $3}'`
zmin=`tail -1 $case.xyz | awk '{print $3}'`


#ztot=`echo "scale=4;( $zmax - $zmin) " | bc -l`
$where/operations $zmax $zmin '1' > hoy
ztot=`more hoy`
#maxvacuum=`echo "scale=1;( $Lslab - $ztot)/2 " | bc -l`
$where/operations $Lslab $ztot '2' > hoy
maxvacuum=`more hoy`
echo "Lslab=$Lslab and Tlayers=$ztot, zmin=$zmin and zmax=$zmax"
if [ "$#" -eq 0 ]
    then
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
    echo "run $0 'vacuum size,# front surface atoms,# bulk atomic planes and # of atoms per plane, # back surface atoms'"
    echo -e " vacuum smaller than ${RED}$maxvacuum${NC} "
    echo -e "${RED}you have the following choice:${NC}"
ls $case.xyz*
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
    exit 1
fi
rm acell.d
vacuum=$1
ps='xyz'
nfs=$2
nba=$3
nap=$4
nbs=$5
cp $case.$ps fort.1
echo $nfs $nba $nap $nbs | $where/prom_layer
echo $a1 $a2 $Lslab $vacuum > acell.d

#N=`wc $case.$ps | awk '{print $1}'`
N=`wc fort.2 | awk '{print $1}'`
#cp $case.$ps fort.1
mv fort.2 fort.1
echo $N $vacuum| $where/rslab
mv fort.2 front.layers
mv fort.3 back.layers
mv fort.7 front.layers.xy
mv fort.8 back.layers.xy
mv fort.99 layers.d
rm fort.1 hoy*
TotalLayers=`wc layers.d | awk '{print $1}'`
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
    echo -e ${RED}$TotalLayers${NC} layers created, vacuum extends to ${RED}$vacuum${NC}
    echo -e "${blue} output in:${NC}"
    echo " layer.d with z_layer Delta_front Delta_back: needed in layered calculation"
    echo " front.layers(.xy)  with front-surface layers for 3D gnuplot"
    echo " back.layers(.xy)   with back-surface  layers for 3D gnuplot"
    echo -e ${cyan}%%%%%%%%%%%%%%%%%%%${NC}
