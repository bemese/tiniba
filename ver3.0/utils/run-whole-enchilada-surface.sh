#!/bin/bash 
# 
# runs abinit to check for symmetries
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
## Functions
clear
ondi=`hostname`
cual=`pwd`
##
function Line {
    printf "\t${BLUE}=============================${NC}\n"
}
if [ "$#" -eq 0 ]
    then   # Script needs at least one command-line argument.
    Line
    printf "\t${blue}A Whole-Enchilada example${NC}\n"
    printf "\t${blue}Runs TINIBA for si_as_6 and calculates SHG${NC}\n"
    printf "\tBe sure to be at ${red}[hexa:/home?/user/... or quad01:/homeib/user...] ${blue}si_as_6 ${NC}directory${NC}\n"
    if [[ "$ondi" == "hexa"* ]]
    then
	printf "\tyou are at $ondi:$cual\n"
    fi
    if [[ "$ondi" == "quad"* ]]
    then
	printf "\tyou are at $ondi:$cual\n"
    fi
    Line
    printf "\t${red}cp $TINIBA/examples/surface/nospin/si_as_6/setUpAbinit_si_as_6.in .${NC}\n"
    printf "\t${red}cp $TINIBA/examples/surface/nospin/si_as_6/si_as_6.xyz .${NC}\n"
    if [[ "$ondi" == "hexa"* ]]
    then
	printf "\t${red}cp $TINIBA/examples/surface/nospin/si_as_6/hexa/.machines_* .${NC}\n"
    fi
    if [[ "$ondi" == "quad"* ]]
    then
	printf "\t${red}cp $TINIBA/examples/surface/nospin/si_as_6/quad/.machines_* .${NC}\n"
    fi
    Line
    printf "\t${red}make sure the nodes are for the chosen architecture${NC}\n"
    printf "\t${red}or change them ${blue}ad libitum!${NC}\n"
    Line
    printf "\trun: ${BLUE}run-whole-enchilada-surface.sh ${RED}1${NC}${NC}\n"
    printf "\t${cyan}     Human action may be required, so guacha!${NC}\n"
    Line
    exit 1
fi
####
if [ $1 == '1' ] 
then 
    Line
    printf "\t${cyan}Hold on to what is yours and hope for the best${NC}\n"
    Line
    if [[ "1" == "1" ]]
    then
	# abinit_check.sh 1
	echo abinit_check.sh 1 > natmat
	chmod +x natmat
	natmat
	# abinit_check.sh 2
	echo abinit_check.sh 2 > natmat
	chmod +x natmat
	natmat
    # It is a surface? (OJO: is this correct? if SHG comment the following line, otherwise don't)
	odd_rank.sh
    # k-points
	echo rklist.sh 19 19 2 abinit > natmat
	chmod +x natmat
	natmat
    # generates layers
	echo rlayer.sh 4.788090 1 4 1 1 > natmat
	natmat
    # choses half-slab layer
	echo chose_layers.sh half-slab > natmat
	natmat
    # gets the k-points distributed among the processors 
	echo run_tiniba.sh -r setkp -k 64 -g 2 -G 2 > natmat
	natmat
    # gets the wave function
	echo run_tiniba.sh -r run -k 64 -N 1 -x 2 -w > natmat
	natmat
    # gets the energies and the momentum matrix elements
    # bypassing the copying of the wavefunction
	echo run_tiniba.sh -r run -k 64 -N 1 -x 2 -b -e -p > natmat
	natmat
    # gets the layered momentum matrix elements
    # bypassing the copying of the wavefunction
	echo run_tiniba.sh -r run -k 64 -N 1 -x 2 -b -c > natmat
	natmat
    # calculates \chi_{xxx} for the surface
	echo all_responses.sh -w layer -m 64_half-slab_5-nospin -s 0 -o 1 -v 13 -c 26 -r 44 -t "xxx" > natmat
	natmat
	rm natmat
	Line
	Line
	printf "\t${red}Plot res/shgC.sm_xxx_64_half-slab_5-nospin_scissor_0_Nc_26${NC}\n"
	printf "\t${red}and $TINIBA/examples/surface/nospin/si_as_6/${NC}\n"
	printf "\t${red}    res/shgC.sm_xxx_64_half-slab_5-nospin_scissor_0_Nc_26${NC}\n"
	printf "\t${cyan}use the 1w and 2w imaginary parts: column 3 and 5${NC}\n"
	Line
	printf "\t${RED}         THEY OUGHT TO BE IDENTICAL!!!!!${NC}\n"
	printf "\t${blue}         IF NOT:${cyan} CRY!!!!!${NC}\n"
	printf "\t${blue}         IF YES:${cyan} DARE TO CALCULATE IN ANOTHER ARCHITECTURE!!!!!${NC}\n"
	Line
	printf "\tDO NOT FORGET TO ERASE\n"
	Line
    fi
fi
