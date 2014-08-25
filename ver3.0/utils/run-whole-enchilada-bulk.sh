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
    printf "\t${blue}Runs TINIBA for GaAs and calculates SHG${NC}\n"
    printf "\tBe sure to be at ${red}[hexa:/home?/user/... or quad01:/homeib/user/...] ${blue}gaas ${NC}directory${NC}\n"
    if [[ "$ondi" == "hexa"* ]]
    then
	printf "\tyou are at $ondi:$cual\n"
    fi
    if [[ "$ondi" == "quad"* ]]
    then
	printf "\tyou are at $ondi:$cual\n"
    fi
    Line
    printf "\t${red}cp $TINIBA/examples/bulk/nospin/gaas/setUpAbinit_gaas.in .\n"
    printf "\t${red}cp $TINIBA/examples/bulk/nospin/gaas/gaas.xyz .${NC}\n"
    if [[ "$ondi" == "hexa"* ]]
    then
	printf "\t${red}cp $TINIBA/examples/bulk/nospin/gaas/hexa/.machines_* .${NC}\n"
    fi
    if [[ "$ondi" == "quad"* ]]
    then
	printf "\t${red}cp $TINIBA/examples/bulk/nospin/gaas/quad/.machines_* .${NC}\n"
    fi
    Line
    printf "\t${red}make sure the nodes are for the chosen architecture${NC}\n"
    printf "\t${red}or change them ${blue}ad libitum!${NC}\n"
    Line
    printf "\trun: ${BLUE}run-whole-enchilada-bulk.sh ${RED}1${NC}${NC}\n"
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
	natmat
        # k-points
	kp=108
	echo rklist.sh $kp $kp $kp abinit > natmat
	natmat
	kt=27720
        # gets the k-points distributed among the processors 
	echo run_tiniba.sh -r setkp -k $kt -g 2 -G 2 > natmat
	natmat
        #  gets the wave function
 	echo run_tiniba.sh -r run -k $kt -N 0 -x 2 -w > natmat
	natmat
        # gets the energies and the momentum matrix elements
        # bypassing the copying of the wavefunction
	echo run_tiniba.sh -r run -k $kt -N 0 -x 2 -b -e -p > natmat
	natmat
#    fi 
       # calculates the scissors shift. The experimental gap for GaAs is 1.52
	gap_correction.sh 1.52
	source .scissors_gamma
       # calculates \chi_{xyz}-Length Gauge for the bulk
	echo all_responses.sh -w total -m $kt'_'20-nospin -s $tij -o 1 -v 4 -c 7 -r 21 -t "xyz" > natmat
	chmod +x natmat
	natmat
        # calculates \chi_{xyz}-Velocity Gauge for the bulk
	echo all_responses.sh -w total -m $kt'_'20-nospin -s $tij -o 1 -v 4 -c 7 -r 42 -t "xyz" > natmat
	natmat
	rm natmat
	Line
	Line
	printf "\t${red}Plot res/shgL.sm_xyz_27720_20-nospin_scissor_$tij_Nc_7${NC}\n"
	printf "\t${red}     res/shgVsm_xyz_27720_20-nospin_scissor_$tij_Nc_7${NC}\n"
	printf "\t${red}and $TINIBA/examples/bulk/nospin/gaas/${NC}\n"
	printf "\t${red}    res/shgL.sm_xyz_27720_20-nospin_scissor_$tij_Nc_7${NC}\n"
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
