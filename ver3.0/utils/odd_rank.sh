#!/bin/bash 
# 
# takes centrosymmetric coordinates
# and obtains non-centrosymmetric
#           symmeyries
##
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
NC='\e[0m' # No Color
##
where=$HOME/tiniba/ver1.0/utils
##
case=`echo $PWD | awk -F / '{print$NF}'`
dir=$PWD
## since we must run abinit_check.sh 1
rm -rf $case'_check'
## gets info from case.xyz
nc=`wc $case.xyz | awk '{print $1}'`
ni=`echo "( $nc - 1) " | bc -l`
#echo $nc $ni
# gets all but the very last coordinate
head -$ni $case.xyz > perro
# adds .1 to the very last z-coordinate
tail -1 $case.xyz | awk '{print $1,$2,$3+.1}' >> perro
# puts original coordinates into tmp file
mv $case.xyz $case.xyz_tmp
# runs abinint 1 and 2 with non-centrosymmetric coordinates
mv perro $case.xyz
# the second 1 in the input parameters of abinit_check.sh is
# for the shell to know that the original centrosymmetric slab
# is render not-centrosymmteric via odd_rank.sh, so
# the ibz is run with no (k) -> (-k) inversion
# thus file 'ifinversion' has the (one)line 'odd_rank no'
$where/abinit_check.sh 1 1
$where/abinit_check.sh 2 1
# returns original centrosymmetric coordinates to case.xyz
mv $case.xyz_tmp $case.xyz
# shows what's been done
echo -e ${cyan}%%%%%%%%%%%%%%%%%%${NC}
echo -e ${blue}The original centrosymmetric $case.xyz was rendered non-centrosymmetric ${NC}
echo -e "${blue}then, abinit_check.sh was run with both options, so the sym.d do not have inversion ${NC}"
echo -e "${blue}then, the coordinates are reset to the original centrosymmetric $case.xyz  ${NC}"
echo -e "${blue}so run_all.sh can be properly run for and odd-rank tensor surface response${NC}"
echo -e ${cyan}%%%%%%%%%%%%%%%%%%${NC}
echo -e "${RED}run rklist.sh AGAIN${NC}"
echo -e ${cyan}%%%%%%%%%%%%%%%%%%${NC}
