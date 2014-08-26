#!/bin/bash
##
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
NC='\e[0m' # No Color
# reads TINIBA version from version-tiniba.txt
source version-tiniba.txt
#
##
    function Line {
	printf "\t${BLUE}=============================${NC}\n"
    }
##
clear
##
case=`echo $PWD | awk -F / '{print$NF}'`
where=$TINIBA/utils/programs_layer
xredcart=`grep -x xred setUpAbinit_$case.in` 
xcartjl=`grep -x xcart setUpAbinit_$case.in` 
## sort the coordinates along z
if [ ! -e  $case.xyz.original ]
then
#    mv $case.xyz $case.xyz.original
    cp $case.xyz $case.xyz.original
fi
#sort -n -r -k3  $case.xyz.original > $case.xyz
## selects executables according to architecture
ANFITRION=`hostname`
if [[ "$ANFITRION" == "quad"* ]] ;then 
exec=quad
operations=$where/roperations_$exec
prom_layer=$where/rprom_layer_$exec
rslab=$where/rrslab_$exec
fi
# needs to be checked
if [ "$ANFITRION" == "node01" ] ;then 
exec=xeon
operations=$where/roperations_$exec
prom_layer=$where/rprom_layer_$exec
rslab=$where/rrslab_$exec
fi
if [[ "$ANFITRION" == "medusa" || "$ANFITRION" == "hexa"* ]] ;then 
exec=hexa
operations=$where/roperations_$exec
prom_layer=$where/rprom_layer_$exec
rslab=$where/rrslab_$exec
fi
if [[ "$ANFITRION" == "itanium"* ]] ;then 
exec=itanium
operations=$where/roperations_$exec
prom_layer=$where/rprom_layer_$exec
rslab=$where/rrslab_$exec
fi
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
#ztot=`echo "scale=4;( $zmax - $zmin) " | bc -l`
$operations $zmax $zmin '1' > hoy
ztot=`more hoy`
#maxvacuum=`echo "scale=1;( $Lslab - $ztot)/2 " | bc -l`
$operations $Lslab $ztot '2' > hoy
#printf "\taqui toy\n"
maxvacuum=`more hoy`
printf "\tLslab=$Lslab and Tlayers=$ztot, zmin=$zmin and zmax=$zmax\n"
if [ "$#" -eq 0 ]
    then
    Line
    printf "\trlayer.sh ${blue}[vacuum size] [# front surface atoms]\n"
    printf "\t          [# bulk atomic planes and # of atoms per plane] [# back surface atoms]${NC}\n"
    printf "\tvacuum smaller than ${RED}$maxvacuum${NC}\n"
#    printf "\t${RED}you have the following choice:${NC}\n"
#ls $case.xyz*
    Line
    exit 1
fi
rm .acell.d
vacuum=$1
nfs=$2
nba=$3
nap=$4
nbs=$5
cp $cual fort.1
echo $nfs $nba $nap $nbs | $prom_layer
echo $a1 $a2 $Lslab $vacuum > .acell.d
N=`wc fort.2 | awk '{print $1}'`
mv fort.2 fort.1
#
echo $N $vacuum| $rslab
mv fort.2 .front.layers
mv fort.3 .back.layers
mv fort.7 front.layers.xy
mv fort.8 back.layers.xy
mv fort.99 layers.d
rm fort.1 hoy*
# Mod 28/ago/06 BMS down
# since top surface could have atoms at different z-positions
# we use the middle of z_min(top) and z_max(top)
# for the correct positioning of the half-slab
zfront=`head -1 layers.d | awk '{print $1}'`
zback=`tail -1 layers.d | awk '{print $1}'`
zhb=`$operations $zfront $zback '3'`
zhf=`head -1 layers.d | awk '{print $2}'`
echo $zfront $zhf $zhb > half_slab.d
awk '{print "rep",$1-$3}'    half_slab.d > half_slab.xy 
awk '{print "rep",$1+$2}'    half_slab.d >> half_slab.xy 
# Mod 28/ago/06 BMS up
TotalLayers=`wc layers.d | awk '{print $1}'`
Line
echo -e ${RED}$TotalLayers${NC} layers created, vacuum extends to ${RED}$vacuum${NC}
echo -e "${blue} output in:${NC}"
echo " layers.d or half_slab.d with z_layer Delta_front Delta_back: needed in layered calculation"
echo " half_slab.xy       with half-slab     layer  for 2D gnuplot"
echo " front.layers.xy  with front-surface layers for 3D gnuplot"
echo " back.layers.xy   with back-surface  layers for 3D gnuplot"
cp layers.d layers.d.original
echo all > .lista_layers
Line
