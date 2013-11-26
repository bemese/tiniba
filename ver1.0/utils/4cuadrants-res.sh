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
case=`basename $PWD`
espin=`grep nspinor $case'_check'/$case.out | awk -F= '{print $4}' | awk '{print $1}'`
ecut=`grep ecut setUpAbinit_$case.in  |  awk '{print $2}'`
printf "\t$espin $ecut\n"
if [ $espin == '1' ]
then
    ospin='nospin'
else
    ospin='spin'
fi
last_name='_'$ecut'-'$ospin
##
    function Line {
	printf "\t${BLUE}=============================${NC}\n"
    }
##
clear
##
if [ "$#" -eq 0 ]
    then
    Line
    printf "\truns any response for 4 cuadrants\n"
    printf "\tkz>0\n"
    printf "\tkx>0,ky>0=>c1,kx<0,ky>0=>c2,kx<0,ky<0=>c3,kx>0,ky<0=>c4\n"
    printf "\tusage:\n"
    printf "\t Edit 4cuadrants-res.sh with the options you want to use, then:\n"
    printf "\t${blue}4cuadrants-res.sh${NC} [${red}Nk${NC}]\n"
    Line
    exit 1
fi
if [ ! "$#" -eq 0 ]
    then
    Nk=$1
    cual=$case.klist_$Nk
# response component
    comp=yyx
    Nv=53
    Nc=53
# runs for each set of k-points
#    cases=(c1 c2 c3 c4)
    cases=(c7)
    for c in ${cases[@]}
    do
    printf "\t runing for $c $Nk$last_name\n"
    ln -fs  eigen_$Nk$last_name.$c  eigen_$Nk$last_name
    ln -fs me_pmn_$Nk$last_name.$c me_pmn_$Nk$last_name 
    ln -fs me_pnn_$Nk$last_name.$c me_pnn_$Nk$last_name 
# calculates the total response
    if [ 1 == 2 ]
    then
	res=eta2
	resn=3
	all_responses.sh total $Nk$last_name 0 $Nv $Nc $resn $comp
	mv res/$res.sm_$comp'_'$Nk$last_name'_Nc_'$Nc'_scissor_0' res/$res.sm_$comp'_'$Nk$last_name'_Nc_'$Nc'_scissor_0'.$c
    fi
# calculates the surface response
    if [ 1 == 1 ]
	then
	resn=25
	res=calEta2
#	layers=(1 2 3 4 5 6 7 8 9 10 11 12)
	layers=(1 12 middle)
	for L in ${layers[@]}
	do
	    ln -fs me_cpnn_$Nk'_'$L$last_name.$c me_cpnn_$Nk'_'$L$last_name 
	    all_responses.sh layer $Nk'_'$L$last_name 0 $Nv $Nc $resn $comp
	    echo res/$res.sm_$comp'_'$Nk'_'$L$last_name'_Nc_'$Nc'_scissor_0' res/$res.sm_$comp'_'$Nk'_'$L$last_name'_Nc_'$Nc'_scissor_0'.$c
	    mv res/$res.sm_$comp'_'$Nk'_'$L$last_name'_Nc_'$Nc'_scissor_0' res/$res.sm_$comp'_'$Nk'_'$L$last_name'_Nc_'$Nc'_scissor_0'.$c
	done
    fi
#
    done
# return to the original k-points
fi
