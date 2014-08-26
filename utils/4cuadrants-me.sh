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
    cual=symmetries/$case.kcartesian_$Nk
# generates the kpoints for the four cuadrants
# c1-direct lattice
    cp $case.klist_$Nk $case.klist_$Nk.c1 
# c2: change k-cartesian  and then transform to k-direct-lattice
    awk '{print -$1,$2,$3}' $cual > fort.9
    echo $Nk 1 | rkcart2dlatt > $case.klist_$Nk.c2 
# c3: change k-cartesian  and then transform to k-direct-lattice
    awk '{print -$1,-$2,$3}' $cual > fort.9 
    echo $Nk 1 | rkcart2dlatt > $case.klist_$Nk.c3 
# c4: change k-cartesian  and then transform to k-direct-lattice
    awk '{print $1,-$2,$3}' $cual > fort.9
    echo $Nk 1 | rkcart2dlatt > $case.klist_$Nk.c4
    rm fort.9
# runs for each set of k-points
#    exit 1
    cases=(c1 c2 c3 c4)
    for c in ${cases[@]}
    do
    printf "\t runing for $c $Nk\n"
    cp $case.klist_$Nk.$c $case.klist_$Nk 
#    run_tiniba.sh -r run -k $Nk -N 12 -x 2 -w -e -p -l
    run_tiniba.sh -r run -k $Nk -N 0 -x 2 -w -e -p 
    mv eigen_$Nk$last_name eigen_$Nk$last_name.$c
    mv me_pmn_$Nk$last_name me_pmn_$Nk$last_name.$c
    mv me_pnn_$Nk$last_name me_pnn_$Nk$last_name.$c
    if [ 1 == 2 ]
    then
	layers=(1 2 3 4 5 6 7 8 9 10 11 12)
	for L in ${layers[@]}
	do
	    mv me_cpmmd_$Nk'_'$L$last_name me_cpmmd_$Nk'_'$L$last_name.$c
	done
    fi
    done
# return to the original k-points
    cp $case.klist_$Nk.c1 $case.klist_$Nk  
fi
