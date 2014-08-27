#!/bin/bash
## please keep this history.
##
## Built upon
## LAST MODIFICATION :  Septiembre 28 2010 by Cabellos  a 16:03

RED='\e[0;31m'
BLUE='\e[0;34m'
BLU='\e[1;34m'
CYAN='\e[0;36m'
GREEN='\e[0;32m'
GRE='\e[1;32m'
YELLOW='\e[1;33m'
NC='\e[0m' # No Color

WORKZPACE="workspace" 
BASEDIR=`dirname $PWD`
PARENT=`basename $BASEDIR`
CASO=`basename $PWD`
ANFIBIO=`hostname`


declare -a FALSEMACHINES
declare -a VIVOS
declare -a MUERTOS
### hha this all the cluster jl
MAQ501[1]="hexa1"
MAQ501[2]="hexa2"
MAQ501[3]="hexa3"
MAQ501[4]="hexa4"
MAQ501[5]="hexa5"
MAQ501[6]="hexa6"
MAQ501[7]="hexa7"
MAQ501[8]="hexa8"
MAQ501[9]="hexa9"
MAQ501[10]="hexa10"
MAQ501[11]="hexa11"
MAQ501[12]="hexa12"
MAQ501[13]="hexa13"
MAQ501[14]="hexa14"                                      
MAQ501[15]="hexa15"
MAQ501[16]="hexa16"
MAQ501[17]="hexa17"
MAQ501[18]="hexa18"                                      
MAQ501[19]="hexa19"                                      
MAQ501[20]="hexa20"
MAQ501[21]="hexa21"
MAQ501[22]="hexa22"
MAQ501[23]="hexa23"
MAQ501[24]="hexa24"
MAQ501[25]="hexa25"
MAQ501[26]="hexa26"
MAQ501[27]="hexa27"
MAQ501[28]="hexa28"
MAQ501[29]="hexa29"
MAQ501[30]="hexa30"
MAQ501[31]="hexa31"
MAQ501[32]="hexa32"
MAQ501[33]="hexa33"
MAQ501[34]="hexa34"
MAQ501[35]="hexa35"
MAQ501[36]="hexa36"
#
IPES[1]="172.17.1.1"
IPES[2]="172.17.1.2"
IPES[3]="172.17.1.3"
IPES[4]="172.17.1.4"
IPES[5]="172.17.1.5"
IPES[6]="172.17.1.6"
IPES[7]="172.17.1.7"
IPES[8]="172.17.1.8"
IPES[9]="172.17.1.9"
IPES[10]="172.17.1.10"
IPES[11]="172.17.1.11"
IPES[12]="172.17.1.12"
IPES[13]="172.17.1.13"
IPES[14]="172.17.1.14"
IPES[15]="172.17.1.15"
IPES[16]="172.17.1.16"
IPES[17]="172.17.1.17"
IPES[18]="172.17.1.18"
IPES[19]="172.17.1.19"
IPES[20]="172.17.1.20"
IPES[21]="172.17.1.21"
IPES[22]="172.17.1.22"
IPES[23]="172.17.1.23"
IPES[24]="172.17.1.24"
IPES[25]="172.17.1.25"
IPES[26]="172.17.1.26"
IPES[27]="172.17.1.27"
IPES[28]="172.17.1.28"
IPES[29]="172.17.1.29"
IPES[30]="172.17.1.30"
IPES[31]="172.17.1.31"
IPES[32]="172.17.1.32"
IPES[33]="172.17.1.33"
IPES[34]="172.17.1.34"
IPES[35]="172.17.1.35"
IPES[36]="172.17.1.36"
function findMaq {
    ALLOWED="0"
    SALIDA="1000"
    local kk=1
    local NOMAQ501a=`echo ${#MAQ501[@]}`
    for ((kk=1;kk<=($NOMAQ501a); kk++));do       
	if [ "${MAQ501[$kk]}" == "$1" ];then 
            SALIDA="$kk"
            ALLOWED=1
	fi 
    done 
}
function findIndex {
    INDES="1000"
    local kk=1
    local NOMAQ501a=`echo ${#MAQ501[@]}`
    for ((kk=1;kk<=($NOMAQ501a); kk++));do       
	if [ "${MAQ501[$kk]}" == "$1" ];then 
            INDES="$kk"
	fi 
    done 
}
function Line {
    printf "\t${BLUE}=============================${NC}\n"
}
echo $1 > dog
name=`awk -F. '{print $2}' dog`
rm dog
cp $1 .$name.original
IN=.$name.original
if [ $# -eq 0 ];then 
    printf "\t ${RED}Hold on !${NC}\n"
    printf "\t I need a input file with the machines HEXAxx\n"
    printf "\t ${RED}Stop right now ...${NC}\n"
    exit 0
fi 
if [ ! -e $IN ];then
    printf "\t ${RED}Hold on !, There is not FILE:${NC} $IN    ...create one.\n"
    printf "\t ${RED}Stop right now ...${NC}\n"
    exit 0
else
    FALSEMACHINES=(`cat $IN`)
    NOFALSEMACHINES=`echo ${#FALSEMACHINES[@]}`
    jj=0
    mm=0
    nn=0
    rm -f $IN
    touch $IN
    for ((hh=0;hh<=($NOFALSEMACHINES-1); hh++));do 
	findMaq ${FALSEMACHINES[$hh]}
	if [ "$ALLOWED" == "1" ]
	then 
	    IPT=`nmap --max_rtt_timeout 20  -oG - -p 514  ${FALSEMACHINES[$hh]} | grep open | cut -d" " -f2`
#	    printf "\t$IPT nmap --max_rtt_timeout 20  -oG - -p 514  ${FALSEMACHINES[$hh]}\n"
	    findIndex ${FALSEMACHINES[$hh]}
	    if [ "$IPT" == "${IPES[$INDES]}" ];then 
		let jj++
		let nn++ 
		echo ${FALSEMACHINES[$hh]} >> $IN
	    else
		let jj++
		let mm++
		MUERTOS[$mm]=${FALSEMACHINES[$hh]}
	    fi 
	fi 
    done     
    NOMUERTOS=`echo ${#MUERTOS[@]}`
    if [ $NOMUERTOS -gt 0 ];then 
	printf "\tYour original $IN has $NOMUERTOS nodes dead that have been eliminated\n"
	for ((hh=1;hh<=($NOMUERTOS); hh++));do
	    printf "\t%4d%12s${RED}%7s${NC}\n" "$hh" "${MUERTOS[$hh]}" "Dead" 
	done 
    fi 
fi 
if [ "$ANFIBIO" == "medusa" ];then 
    if [ ! -e $IN ];then
        printf "\t ${RED}There is not file${NC} $IN\n"
        exit 0
    else
        MACHINESinf=(`cat $IN`)
	NOMACHINESinf=`echo ${#MACHINESinf[@]}`
    fi
else 
#    printf "\t ${RED}Hold on !${NC}\n"
    printf "\tTo run with infiniband\n"
    printf "\tyou need to be in quad01\n"
#    printf "\t ${RED}Stop right now ...${NC}\n"
    exit 0 
fi 
######
SAL=0
while [ "$SAL" -lt "10" ];do
    MACHINESinf=(`cat $IN`)
    NOMACHINESinf=`echo ${#MACHINESinf[@]}`
#    Line
#    echo "mpdboot -v -r ssh -f $IN -n $NOMACHINESinf > INFI"
#    Line
    mpdboot -v -r ssh -f $IN -n $NOMACHINESinf > INFI
    QUEPEX=`grep "failed to connect to mpd" INFI`
    rm -f $IN
    touch $IN
    if [ -z "$QUEPEX" ];then 
	printf "\tInfiniband working in alive nodes. Your final list is:\n"
	for ((hh=0;hh<=($NOMACHINESinf-1); hh++));do
	    let "PP=hh+1"
	    printf "\t[$PP] ${MACHINESinf[$hh]}\n"
	    echo ${MACHINESinf[$hh]} >> $IN
	done 
	Line
	printf "\tin file ${RED}$IN${NC}\n"
	Line
	SAL=20
    else 
	NODE=`echo ${QUEPEX: -6}`
	for ((hh=0;hh<=($NOMACHINESinf-1); hh++));do
	    if [ ${MACHINESinf[$hh]} != $NODE ];then 
		echo ${MACHINESinf[$hh]} >> $IN 
	    else
		Line
		printf "\t${RED}node ${MACHINESinf[$hh]} does not have infiniband connection${NC}\n"
		Line
	    fi 
	done
    fi 
done      
rm -f INFI
