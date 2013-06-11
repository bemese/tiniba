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
Nk=102
Nv=98
Nc=98
ecut=10
#layers=(1 2 3 4 5 6 7 8 9 10 middle half-slab)
layers=(half-slab)
#####
if [ 1 == 1 ]
    then
##### run wavefunction
    run_tiniba.sh -r run -k $Nk -N 0 -x 2 -w
##### run  energies and pmn and as a good paranoid still copy/check the WF
    run_tiniba.sh -r run -k $Nk -N 0 -x 2 -e -p
fi
#####
if [ 1 == 1 ]
    then
    for wl in ${layers[@]}
      do
      Line
      printf "\t${RED}Doing Layer $wl${NC} for ndot\n"
      Line
      chose_layers.sh $wl > hoym
### layered ndot me bypassing the copying of the WF
      run_tiniba.sh -r run -k $Nk -N 1 -x 2 -d -b
###
      Line
      printf "\t${RED}END Layer $wl${NC} for ndot\n"
      Line
      rm hoym
    done
fi
#####
#####
if [ 1 == 1 ]
    then
    for wl in ${layers[@]}
      do
      Line
      printf "\t${RED}Doing Layer $wl${NC} for zeta\n"
      Line
      chose_layers.sh $wl > hoym
### layered zeta me bypassing the copying of the WF
      run_tiniba.sh -r run -k $Nk -N 1 -x 2 -n -b
###
      Line
      printf "\t${RED}END Layer $wl${NC} for zeta\n"
      Line
      rm hoym
    done
fi
#####
# full responses
if [ 1 == 1 ]
then
    valence=(98)
    conduction=(98)
# chi
    all_responses.sh -w total -m $Nk'_'$ecut-spin -s 0 -o 1 -v $v -c $c -r 1 -t "xx yy zz"
# zeta
    all_responses.sh -w total -m $Nk'_'$ecut-spin -s 0 -o 1 -v $v -c $c -r 41 -t "zxy"
fi
#####
if [ 1 == 1 ]
then
# layer zeta for all v and all c => -o 1 , -r 29
    valence=(98)
    conduction=(98)
#    layers=()
    for L in ${layers[@]}
    do
	for v in ${valence[@]}
	do
	    for c in ${conduction[@]}
	    do
		all_responses.sh -w layer -m $Nk'_'$L'_'$ecut-spin -s 0 -o 1 -v $v -c $c -r 29 -t "zxy"
	    done    
	done
    done
# layer ndot for all v and all c => -o 1 , -r 26
    valence=(98)
    conduction=(98)
#    layers=()
    for L in ${layers[@]}
    do
	for v in ${valence[@]}
	do
	    for c in ${conduction[@]}
	    do
		all_responses.sh -w layer -m $Nk'_'$L'_'$ecut-spin -s 0 -o 1 -v $v -c $c -r 26 -t "xx yy zz"
	    done    
	done
    done
fi
# layer and pairs of v and c
if [ 1 == 2 ]
then
    valence=(178 177 176 175)
    conduction=(179 180)
    layers=(45 44 43 42 half-slab)
    for L in ${layers[@]}
    do
	for v in ${valence[@]}
	do
	    for c in ${conduction[@]}
	    do
#      echo all_responses.sh -w layer -m $Nk'_'$L'_'$ecut-spin -s 0 -o 2 -v $v -c $c -r 29 -t "zxy"
	      all_responses.sh -w layer -m $Nk'_'$L'_'$ecut-spin -s 0 -o 2 -v $v -c $c -r 26 -t "xx yy zz"
	    done    
	done
    done
fi
# layer and full v and c
if [ 1 == 2 ]
then
    valence=(178)
    conduction=(178)
    layers=(45 44 43 42 half-slab)
    for L in ${layers[@]}
    do
	for v in ${valence[@]}
	do
	    for c in ${conduction[@]}
	    do
	      all_responses.sh -w layer -m $Nk'_'$L'_'$ecut-spin -s 0 -o 1 -v $v -c $c -r 26 -t "xx yy zz"
	    done    
	done
    done
fi
# total
if [ 1 == 2 ]
then
    valence=(178 177 176 175 174 173 172 171)
    conduction=(179 180 181 182 183 184 185 186)
    for v in ${valence[@]}
    do
	for c in ${conduction[@]}
	do
#      echo all_responses.sh -w total -m $Nk'_'$ecut-spin -s 0 -o 2 -v $v -c $c -r 41 -t "zxy"
	    all_responses.sh -w total -m $Nk'_'$ecut-spin -s 0 -o 2 -v $v -c $c -r 1 -t "xx yy zz"
	done    
    done
#
fi
