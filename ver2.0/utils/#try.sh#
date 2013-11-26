#!/bin/bash
##
## output
#  standard: file_Nk_Ecut_spin 
#  layered : cfile_Nk_Layer_Ecut_spin
# colors
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
NC='\e[0m' # No Color
## Functions
## rho
function lrho {
	    Line
	    printf "\t${RED}ERROR${NC}: file ${BLUE}$case""_klist_rho${NC} ${red}does not exists${NC}\n"
	    printf "\t${cyan}generate one with${NC}: ${BLUE}rklist.sh${NC} or creat one with few k-points\n"
	    printf "\tRename it as ${BLUE}$case""_klist_${RED}rho${NC}\n"
	    printf "\tand chose option ${RED}-k rho${NC}\n"
	    Line
}
## thanks
function gracias {
printf "\tThanks for using ${cyan}TINIBA${NC}: ${RED}NO WARRANTIES WHATSOEVER\n${NC}"
}
## options ala bms
function runoptions {
printf "\n   -w      Wave function(k)\n"
printf "   -m      rho(z) for a set of k-points in ${red}case${NC}.klist_rho\n"
printf "   -e      Energies E_{m}(k)\n"
printf "   -p      Momentum Matrix Elements p_{mn}(k), includes m=n\n"
#printf "   -v      Diagonal Momentum Matrix Elements p_{mm}(k)\n"
printf "   -c      Layered Momentum Matrix Elements calp_{mn}(k)\n"
printf "   -l      Layered Diagonal Momentum Matrix Elements calp_{mm}(k)\n"
printf "   -s      Spin Matrix Elements S_{cc'}(k)\n"
printf "   -n      Layered Spin Matrix Elements calS_{cc'}(k)\n"
printf "   -b      bypass WF checkup (${red}Never use on first run${NC})\n"
}
#
function options {
printf "${cyan}Usage${NC}:\n"
printf "\t***\n"
echo -e "${CYAN}run_tiniba.sh${NC} -r ${RED}run${NC} -k ${RED}Nk${NC} -N ${RED}N_Layer${NC} -x [serial-${red}1${NC} para-${red}2${NC}] ${BLUE}options${NC}:"

    printf "\n                           ${RED}N_Layer${NC}=number of layers or half-slab\n"

runoptions

printf "\t***\n"
echo -e "${CYAN}run_tiniba.sh${NC} -r ${RED}setkp${NC} -k ${RED}Nk${NC} -g ${RED}xeon/itanium${NC} -G ${RED}xeon/quad${NC} weigths"
echo -e "${CYAN}run_tiniba.sh${NC} -r ${RED}erase${NC}  To erase the calculation from the nodes" 
echo -e "${CYAN}run_tiniba.sh${NC} -r ${RED}erasescf${NC} To erase the SCF calculation" 
}
## Functions
 function nvalence {
# counts the number of occupied states from acs_check/case.out file
    grep -n 'occ ' $case'_check'/$case.out > hoyj
    iocc=`awk -F: '{print $1}' hoyj`

    grep -n 'prtvol' $case'_check'/$case.out > hoyj
    iprtvol=`awk -F: '{print $1}' hoyj`

    awk 'NR=='$iocc',NR=='$iprtvol'' $case'_check'/$case.out > hoyj2

# for spin-orbit each state has only one electron for a given spin
    grep -o 1.000 hoyj2 > hoyj3

    Nvf=`wc hoyj3 | awk '{print $2}'`
    if [ $Nvf == '0' ]
	then
	grep -o 2.000 hoyj2 > hoyj3
	Nvf=`wc hoyj3 | awk '{print $2}'`
    fi

    Nct=`expr $Nband - $Nvf`
    rm -f hoyj*
}
##
 function IsThereError {
     if [ -e killme ]; then
      printf "\t ${RED}Stoping right now ... ${NC}\n"
      rm -rf killme
      exit 1
     fi
    }
##
function Line {
    printf "\t${BLUE}=============================${NC}\n"
}
## Star
clear
rm -f error
rm -f finished*
rm -f finished*
#
where=$HOME/tiniba/ver1.0/clustering/itaxeo
cual=all_nodes.sh
host=$HOSTNAME
function depuranodos {
#
# checks that the nodes are working, otherwise it eliminates
# them from .machines_scf/pmn.original and creates
# .machines_scf/pmn for the actual run
#
# firts checks infiniband only if .machines_scf.original has quads
# 
nodin=`head -1 .machines_scf.original |awk -Fd '{print $1}'`
if [ "$nodin" == "qua" ]
then
    mv .machines_scf.original .machines_scf.quad
    Line
    printf "\tSCF  to be run on the Quads\n"
    Line
    $HOME/tiniba/ver1.0/utils/infiniband.sh .machines_scf.quad
    mv .machines_scf.quad .machines_scf.original
else
    Line
    printf "\tSCF not to be run on the Quads\n"
    Line
fi
$HOME/tiniba/ver1.0/utils/createRemoteDir.sh
}

if [ "$host" == "medusa" ]; then
    compita='compita_xeon'
fi
if [[ "$host" == "itanium"* ]]; then
    compita='compita_itanium'
fi
if [[ "$host" == "quad"* ]]; then
    compita='compita_quad'
fi
## arrays for cpu's
declare -a nodeArrayscf
declare -a nodeArraypmn
nodeArray_scf=(`cat .machines_scf.original`)
nodeArray_pmn=(`cat .machines_pmn.original`)
numberOfNodes_scf=`echo ${#nodeArray_scf[@]}`
numberOfNodes_pmn=`echo ${#nodeArray_pmn[@]}`
Ncpu_scf=`wc .machines_scf.original | awk '{print $1}'`
Ncpu_pmn=`wc .machines_pmn.original | awk '{print $1}'`
#
rm -f finished*
#
if [[ -e .peso1 && -e .peso2 ]]
then
    weigth1=`awk '{print $1}' .peso1`
    weigth2=`awk '{print $1}' .peso2`
    mensaje=$weigth1-$weigth2
else
    mensaje='choose one: -r setkp...'
fi
#
#
if [ $numberOfNodes_scf != $Ncpu_scf ]
then
    Line
    echo -e "${RED} Number of processes not equal to number of cpus${NC}"
    echo -e "${BLUE} Check .machines_scf, may be there are extra lines at the bottom${NC}"
    Line
exit 1
fi
#
if [ $numberOfNodes_pmn != $Ncpu_pmn ]
then
    Line
    echo -e "${RED} Number of processes not equal to number of cpus${NC}"
    echo -e "${BLUE} Check .machines_pmn, may be there are extra lines at the bottom${NC}"
    Line
exit 1
fi
##
case=`echo $PWD | awk -F / '{print$NF}'`
dir=$PWD
# 
grep nband2 setUpAbinit_$case.in > hoy
Nband=`head -1 hoy | awk '{print $2}'`
rm hoy
# get number of valence and conduction bands
nvalence
# puts info to be read by pmn.f90 at each working node
echo $Nvf > .fnval
# checks if the pseudopotentials exist
rm -f killme
$where/checkPSPabinit.pl setUpAbinit_$case.in 
IsThereError
# check for the existance of $case.out
if [ ! -e $case'_check'/$case.out ]
    then
    Line
    echo -e ${RED} $case'_check'/$case.out does not exist${NC}
    echo -e ${BLUE} run ${RED} abinit_check.sh ${NC} first
    Line
    exit 1
fi
# 
espin=`grep nspinor $case'_check'/$case.out | awk -F= '{print $4}' | awk '{print $1}'`
inspin=`grep nspinor setUpAbinit_$case.in | awk '{print $2}'`
if [ ! $espin == $inspin ]
then
    Line
    echo -e ${RED} Spin is different: $case'_check'/$case.out vs setUpAbinit_$case.in
    echo -e "${blue} fix it and run again from (abinit_check.sh) the very begining, that's a good place to start!"
    Line
if [[ $1 == 'run' || $# == 0 ]] 
then
exit 1
fi
fi
ecut=`grep ecut setUpAbinit_$case.in  |  awk '{print $2}'`
# checks if ecut has not changed
# from the $case_scf/$case.out
if [ -e $case'_scf'/$case.out ]
    then
    ecutout=`grep "ecut(hartree)" $case'_scf'/$case.out | awk '{print $2}'`
    ecutyn=`echo $ecut $ecutout | $where/$compita`
    if [ $ecutyn == 'no' ]
	then
	Line
	echo -e ${RED} ecut=$ecut at setUpAbinit_$case.in different from ecut=$ecutout at $case'_scf'/$case.out ${NC}
	echo -e "${blue}      if ecut must be changed, erase everything by hand" 
	echo -e "${blue}      and do the calculation from the begining, that's a good place to start!"
	Line
	exit 1
    fi
fi
#
rm -f spin_info
echo $espin > spin_info
if [ -e .acell.d ]
    then
## checking that Lslab is the same
    grep acell setUpAbinit_$case.in > hoy
    sLslab=`grep acell hoy | awk '{print $4}'`
    sa1=`grep acell hoy | awk '{print $2}'`
    sa2=`grep acell hoy | awk '{print $3}'`
    aLslab=`awk '{print $3}' .acell.d`
    aa1=`awk '{print $1}' .acell.d`
    aa2=`awk '{print $2}' .acell.d`
    rm -f hoy
#
    if [ $sLslab != $aLslab ]
	then
	Line
	echo -e " setUpAbinit_$case.in vacuum=${BLUE}$sLslab${NC} and .acell.d vacuum=${blue}$aLslab${NC} are different "
	echo -e "${red} check ${NC} setUpAbinit_$case.in"
	echo -e "${red} run ${NC} rlayer.sh with vacuum=${BLUE}$sLslab${NC}"
	echo " AND check if the SCF needs to be run"
	Line
	exit 1
    fi
fi
## creating directory for results
if [ ! -e res ]
    then
    mkdir res
fi
#
if [ "$#" -eq 0 ]
    then   # Script needs at least one command-line argument.
#
    if [ ! -e .acell.d ]
    then
	echo -e ${RED} "*********No .acell.d file exists, is this a bulk calculation??**********"${NC}
    fi
    $HOME/tiniba/ver1.0/utils/check_coord.sh
    echo -e "${BLUE} The following sets of k-points are available:${NC}"
    ls $case.klist*
    Line
    if [  -e layers.d ]
	then
	TotalLayers=`wc layers.d | awk '{print $1}'`
	mlay=`less .lista_layers`
    else
	TotalLayers=0
	mlay=0
    fi
# displays the chosen layers
    echo -e "${BLUE}$TotalLayers Layers: ${red}$mlay${BLUE}, $Nband bands ($Nvf-v $Nct-c)${NC}, spin=${RED}$espin${NC}, ecut=${RED}$ecut${NC} and xe/it/qu weigths=${RED}$mensaje${NC}"
    Line
# displays the options
    options
#
    Line
    exit 1
fi
########################## general RUN
### set up of the input data
wf="false"       #w
rho="false"       #m
em="false"    #e
pmn="false"   #p
#pmm="false"   #v
lpmn="false"  #c
lpmm="false"  #l
sccp="false"  #s
lsccp="false" #n
wfcheck="false" #b
weigth2=" "
### reads from the input line
### r: => reads 'data' from '-r data'
### v  => if set then is true otherwise is false, i.e. -v => v case is true
while getopts “:hr:k:N:x:wmepclsnbg:G:” OPTION
do
     case $OPTION in
         h)
             usage
             exit 1
             ;;
         r)
             action=$OPTARG
             ;;
         k)
             Nk=$OPTARG
             ;;
         N)
             layers=$OPTARG
             ;;
         x)
             serialp=$OPTARG
             ;;
         w)
	     wf="true"
             ;;
         m)
	     rho="true"
             ;;
         e)
             em="true"
             ;;
         p)
             pmn="true"
             ;;
#         v)
#             pmm="true"
#             ;;
         c)
             lpmn="true"
             ;;
         l)
             lpmm="true"
             ;;
         s)
             sccp="true"
             ;;
         n)
             lsccp="true"
             ;;
         g)
             weigth1="$OPTARG"
             ;;
         G)
             weigth2="$OPTARG"
             ;;
         b)
             wfcheck="true"
             ;;
         ?)
             printf "\t${RED}error${NC}\n"
             exit
             ;;
     esac
done
# string with the chosen options
moptions="$em $pmn $pmm $lpmn $lpmm $sccp $lsccp"
# give -G "strings"
for mi in ${weigth2[@]}
do
printf "\t $mi\n"
done
exit 1
# checks that the input parameters are correct
# first that -r has the correct value
#
if [[ $action != "run" ]] && [[ $action != "setkp" ]] && [[ $action != "erase" ]] && [[ $action != "erasescf" ]]
then
    Line
    printf "\tFor -r chose either ${RED}run, setkp, erase${NC} or ${RED}erasescf${NC}\n"
    Line
    exit 1
fi
########################## set k-points:begin #####################################
if [ $action == 'setkp' ] 
    then
## 
    if [[ -z $Nk ]] || [[ -z $weigth1 ]] || [[ -z $weigth2 ]]
    then
	Line
	printf "either -k ${RED}Nk${NC}, -g ${RED}xeon/itanium${NC} or -G ${RED}xeon/quad${NC} weigths are not defined\n"
	Line
	exit 1
    fi    
##
## this selects the nodes that are working
    depuranodos
##
    Nkl=$Nk # Nk is read from the input line
    Line
    echo -e "The optimization for weigth = ${BLUE} $weigth ${NC}is:"
    $where/arrange_machines_quad.pl $case.klist_$Nkl $numberOfNodes_pmn $weigth1 $weigth2
    echo -e "Please choose an option, or choose any other option to continue"
    echo -e "1. Choose another weigth       2. Exit to ${RED}run${NC} the full script"
    option=`$where/get_value.pl`
    echo $weigth1  > .peso1 #itanium/xeon
    echo $weigth2  > .peso2 #quad/xeon
    if [ $option == "2" ] 
	then 
#rm unecesary files
	rm -f startpoint.txt
	rm -f endpoint.txt
	rm -f klist_length.txt
	rm -f hoy
	exit 1
    fi
    while [ $option == "1" ]
    do
	Line
	echo "Please insert the new weigth:"
	weigth=`$where/get_value.pl`
	echo -e "The optimization for weigth = ${BLUE} $weigth ${NC}is:"
	$where/arrange_machines.pl $case.klist_$Nkl $numberOfNodes_pmn $weigth
	echo -e "Please choose an option, or choose any other option to continue"
	echo -e "1. Choose another weigth       2. Exit to ${RED}run${NC} the full"
	option=`$where/get_value.pl`
	echo $weigth1 > .peso1
	echo $weigth2 > .peso2
	if [ $option == "2" ] 
	then 
	    exit 1
	fi
    done
#rm unecesary files
    rm -f startpoint.txt
    rm -f endpoint.txt
    rm -f klist_length.txt
    rm -f hoy
    exit 1
fi
########################## set k-points: end #####################################
########################## erase: begin #####################################
if [ $action == 'erase' ] 
    then
    Line
    echo "Erasing"
    $where/$cual 2
    Line
exit 1
fi
########################## erase: end #####################################
########################## erase scf: begin #####################################
if [ $action == 'erasescf' ] 
    then
    Line
    echo "Erasing SCF"
    Line
    $where/$cual 3
    Line
exit 1
fi
########################## erase scf: end #####################################
########################## run: begin #####################################
# action=run
if [ $action = 'run' ]
then
## checks that the correct klist file is given
    if [ $rho = 'false' ]
	then
	if [[ $em = 'true' ]] || [[ $pmn = 'true' ]] || [[ $lpmn = 'true' ]] || [[ $lpmm = 'true' ]] || [[ $sccp = 'true' ]] || [[ $lsccp = 'true' ]]   
	then
	    if [ $Nk == "rho" ]
	    then
		Line
		printf "\t${red}-k rho${NC} is not valid for the chosen option\n"
		printf "\tchose -k ${red}number${NC} or ${red}bands${NC}\n"
		Line
		exit 1
	    fi
	fi
    fi
## checks that the input is given
    if [[ -z $Nk ]] || [[ -z $layers ]] || [[ -z $serialp ]]  
    then
	Line
	printf "either -k ${RED}Nk${NC}, -N ${RED}N_Layer${NC}, or -x [serial-${RED}1${NC}-paralel-${RED}2${NC}] are not defined\n"
	Line
	exit 1
    fi    
## checks that the options are given
    if [[ $wf = 'false' ]] && [[ $rho = 'false' ]] && [[ $em = 'false' ]] && [[ $pmn = 'false' ]] && [[ $lpmn = 'false' ]] && [[ $lpmm = 'false' ]] && [[ $sccp = 'false' ]] && [[ $lsccp = 'false' ]]   
    then
	Line
	printf "\t${red} To run, give at least one of the following option:${NC}\n"
	runoptions
	Line
	exit 1
    fi
##
    Nkl=$Nk #Nk is read from -k
    layers=$layers #layers is read from -N
    serialp=$serialp #serialp is read from -x
## checks that the weight were given
if [[ ! -e .peso1 &&  ! -e .peso2 ]]; then
    Line
    echo -e ${RED} NO itanium/xeon/quad weigth, choose one!
    echo -e ${RED} run with: ${BLUE}run_tiniba.sh -r ${blue}setkp...
    Line
    exit 1
fi
## saves info of the run parameters
    echo "#!/bin/bash"> run_tiniba_como_corri.sh
    echo "`date`      ">> run_tiniba_como_corri.sh
    echo "`dirname $0`/`basename $0` $@ " >> run_tiniba_como_corri.sh
    chmod 777 run_tiniba_como_corri.sh
## starts the overall time
     TIMESTARTALL=`date`
     Line
     printf "\tStarting at: $TIMESTARTALL \n" 
     Line
    fi
##
    if [[ $layers == 0 ]] && [[ $lpmn == 'true' || $lpmm == 'true' || $lsccp == 'true' ]]
	then
	Line
	printf "\tFor layered calculation ${RED}NLayer${NC} > 0 or ${RED}half-slab${NC}\n"
	Line
	exit 1
    fi
#
    if [[ $Nkl == "rho" ]] && [[ ! -e $case.klist_rho ]]
    then
	lrho
	exit 1
    else
	Nk=`wc $case.klist_$Nkl | awk '{print $1}'`
    fi
#
    if [ $lpmm == 'true' ]
    then
	if [ ! -e $dir/.ifcentrosymmetric ]
	then
	    Line
	    printf "\tfile: ${blue}ifcentrosymmetric ${RED}DOES NOT EXIST${NC}\n"
	    printf "\tgenerate one with${BLUE} abinit_check.sh${RED} 2${NC}\n"
	    printf "\tis this a Injection Current calculation?\n"
	    Line
	    exit 1
	fi
    fi
#
    if [ $layers == 'half-slab' ]
	then
	Nlayer='1'
	cp half_slab.d .ab_layers.d
    else
	Nlayer=$layers
        if [ -e layers.d ];then
	 cp layers.d .ab_layers.d
        fi
    fi
#
    if [ $espin == '1' ]
	then
	ospin='nospin'
    else
	ospin='spin'
    fi
    last_name='_'$ecut'-'$ospin
#
    weigth1=`awk '{print $1}' .peso1`
    weigth2=`awk '{print $1}' .peso2`
#
    if [[ $espin == 1 ]] && [[ $sccp == 'true' ||  $lsccp == 'true' ]] 
    then
	Line
	echo -e "${RED} Option -s or -n for spin matrix elements require SPIN=2"
	Line
	exit 1
    fi
# writes the variable values of the run
touch .info_run
fecha=`date`
echo $fecha >> .info_run
echo "$0 -r $action -k $Nk -N $layers -x $serialp wf(-w)=$wf rho(m)=$rho em(-e)=$em pmn(-p)=$pmn lpmn(-c)=$lpmn lpmm(-l)=$lpmm sccp(-s)=$sccp lsccp(-n)=$lsccp wfcheck(-b)=$wfcheck" >> .info_run
# this mimics a goto
#    goto='yes'
    goto='no'
if [ $goto == 'no' ]
then
    if [ $rho == 'true' ]
    then
# checks that -k rho is given
# it really doesn't matter since $case.klist_rho is used
# but is nicer to be consisten
	if [ $Nkl != "rho" ]
	then
	    Line
	    printf "\tChose -k ${RED}rho${NC}\n"
	    Line
	    exit 1
	fi
# checks that zmesh.d exists
	if [ ! -e zmesh.d ]
	then
	    Line
	    printf "\t${RED}ERROR${NC}: file ${BLUE}zmesh.d${NC} ${red}does not exists${NC}\n"
	    printf "\t${cyan}generate one with${NC}: ${BLUE}zmesh.pl${NC}\n"
	    Line
	    exit 1
	fi
# checks that $case.klist_rho exists
	if [ ! -e $case.klist_rho ]
	then
	    lrho
	    exit 1
	else
	    Nk=`wc $case.klist_rho | awk '{print $1}'`
	    Line
	    printf "\t${RED}rho(z)${NC} will be calculated for $Nk k-points\n"
	    printf "\tLook in $case.klist_rho or $case.klist_$Nk for the k-points\n"
	    Line
	    cp $case.klist_rho $case.klist_$Nk
	fi
    fi
# checking for layer info
    if [ ! $Nkl == 'bands' ] 
    then
	if [ ! -e layers.d ]
	then
	    if [ ! $Nlayer == '0' ]
	    then
		Line
		printf "\t${RED}layers.d${NC} does not exists\n"
		printf "\tget one with ${blue}rlayer.sh${NC}\n" 
		Line
		exit 1
	    fi
	fi
    fi
    if [ $Nkl == 'bands' ] 
    then
	Nk=`wc $case.klist_bands | awk '{print $1}'`
	Line
	printf "\tThe band strucure will be calculated for $Nk k-points\n"
	printf "\tlook in $case.klist_bands or $case.klist_$Nk for the k-points\n"
	printf "\tenergy band will be in ${RED}eigen_$Nk${NC}\n"
	echo
	Line
	cp $case.klist_bands $case.klist_$Nk
    fi
## this selects the nodes that are working
    depuranodos
#
    if [ -e $dir/JOBS ]
    then
	Line
	printf "\tJOBS exists!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
	printf "\tif you want to proceed, do nothing\n"
	printf "\tif do not want to proceed, kill within 1 sec\n"
	sleep 1
	Line
    fi
    if [ "$Nk" -lt "$numberOfNodes_pmn" ]
    then
	Line
	echo -e ${red}" WARNING: $Nk-kpoints less than $numberOfNodes_pmn CPUs:"${NC}
	echo -e ${blue}" change .machines_pmn.original "${NC}
	Line
	exit 1
    fi
    Line
    printf "\tRUN set.pl first\n"
    printf "\tusing $case.klist_$Nkl with $Nk k-points\n"
    Line
########################## set setUpAbinit #####################################
#
    $where/arrange_machines_quad.pl $case.klist_$Nkl $numberOfNodes_pmn $weigth1 $weigth2
    $where/set.pl $case.klist_$Nkl $numberOfNodes_pmn $Nlayer
#
    if [ -d JOBS ]
    then
	Line
	printf "\t${cyan} JOBS found, perhaps an old setUpAbinit_$case.sh${NC}\n"   
	printf "\t${cyan} Don't worry, is being erased${NC}\n"
	rm -rf JOBS
    fi
    Line
    printf "\t${BLUE}The file for concatenating the results is being created${NC}\n"
    $where/concatena.pl $case.klist_$Nkl $numberOfNodes_pmn $Nlayer
    Line
#############################################################################
#rm unecesary files
    rm -f medusa.jdf
    rm -fr $case'_bandstructure'
    rm $case.files
#   rm latm.params
    rm -f startpoint.txt
    rm -f endpoint.txt
    rm -f klist_length.txt
# run all-nodes.sh
    $where/$cual 1 $Nk $Nlayer $serialp $last_name $wf $rho $em $pmn $lpmn $lpmm $sccp $lsccp $wfcheck
#
# if -w is the only option, the shell is done and exits
#
    if [[ $wf == 'true' ]] && [[ $rho == 'false' ]] && [[ $em == 'false' ]] && [[ $pmn == 'false' ]] && [[ $lpmn == 'false' ]] && [[ $lpmm == 'false' ]] && [[ $sccp == 'false' ]] && [[ $lsccp == 'false' ]]
    then
	Line
	printf "\tall_nodes.sh: ${RED}Wave Function Calculated${NC}\n"
	printf "\tall_nodes.sh: ${blue}and copied to all working directories${NC}\n"
## generates total time below
	TIMEENDALL=`date`
	Line
	printf "\t--------------------------------------------------\n"
	printf "\t     Started at time: ${GREEN}$TIMESTARTALL ${NC}\n"
	printf "\t--------------------------------------------------\n"
	printf "\tFinished all at time: ${GREEN}$TIMEENDALL ${NC}\n"
	printf "\t--------------------------------------------------\n"
	TIME1=`date --date="$TIMESTARTALL" +%s`
	TIME2=`date --date="$TIMEENDALL" +%s`
	ELTIME=$[ $TIME2 - $TIME1 ]
	TMIN=$(echo "scale=9; $ELTIME/60" | bc)
	TMIN1=$(echo "scale=9; $ELTIME/3600" | bc)
	printf "\t total time:   $TMIN min. \n"
	printf "\t               $TMIN1 Hrs. \n"
	printf "\t--------------------------------------------------\n"
## generates total time above
###
	Line
	gracias
	Line
	exit 1
    fi
##
    printf "\trun_tiniba.sh: ${green}Waiting for all the nodes to finish${NC}\n"
    Line
    N=0
    for node in `cat .machines_pmn`
    do
	N=`expr $N + 1`
	while [ ! -e finished$N ]
	do
# to avoid system inconsitencies
	    sleep 1
#    echo "$dirnode NOT finished"
	    continue
	done
### for a given set of nodes uncomment next one
    done 
#
Line
printf "\t${blue} all nodes have finished${NC}\n"
Line
#
rm -f finished*
#
if [ -e killme ];then
    exit 1
fi
# check for error
if [ -e error ]
then 
    Line
    echo -e check file ${blue} error ${Nc}
    Line
    exit 1
fi
# checking if SCF really produced a wavefunction
if [ -e nojalo ]
then
    echo -e "${blue} exiting ${RED} run_tiniba.sh ${NC}"
    rm -f nojalo
    Line
    exit 1
fi
# for rho(z) no files need to be concatenated
if [ $rho == 'true' ]
then
## generates total time below
    TIMEENDALL=`date`
    printf "\t--------------------------------------------------\n"
    printf "\t     Started at time: ${GREEN}$TIMESTARTALL ${NC}\n"
    printf "\t--------------------------------------------------\n"
    printf "\tFinished all at time: ${GREEN}$TIMEENDALL ${NC}\n"
    printf "\t--------------------------------------------------\n"
    TIME1=`date --date="$TIMESTARTALL" +%s`
    TIME2=`date --date="$TIMEENDALL" +%s`
    ELTIME=$[ $TIME2 - $TIME1 ]
    TMIN=$(echo "scale=9; $ELTIME/60" | bc)
    TMIN1=$(echo "scale=9; $ELTIME/3600" | bc)
    printf "\t total time:   $TMIN min. \n"
    printf "\t               $TMIN1 Hrs. \n"
    printf "\t--------------------------------------------------\n"
    Line
    printf "\t${RED}you have created:${NC}\n"
    ls res/psiz_*
    ls res/rhoz_*
## generates total time above
    Line
    gracias
    Line
    exit 1
fi
# finishes the goto
fi
#
# concatenatign files
spinor=`awk '{print $1}' spin_info`
Line
printf "\t${BLUE}concatenating the following files:${NC}\n"
##
JOBS/concatenatefiles.pl $moptions
Line
printf "\tYou have created:\n"
## rename energy file
if [ $em == 'true' ]
then
    mv energy.d eigen_$Nkl$last_name
    ls  eigen_$Nkl$last_name
fi
## rename Pmn file
if [ $pmn == 'true' ]
then
    mv pmn.d me_pmn_$Nkl$last_name
    mv pnn.d me_pnn_$Nkl$last_name
    ls  me_pmn_$Nkl$last_name me_pnn_$Nkl$last_name
fi
## rename Pmn file
if [ $sccp == 'true' ]
then
    mv spinmn.d me_sccp_$Nkl$last_name
    ls me_sccp_$Nkl$last_name
fi
## rename CalPmn file
if [ $lpmn == 'true' ]
then
    if [ $layers == 'half-slab' ]
    then
	mv cpmnd_$Nkl'_1' me_cpmn_$Nkl'_half-slab'$last_name 
    else
	whichlayers=`less .lista_layers`
	if [ "$whichlayers" == "all" ]
	    then
	    nn=0
	    while [ $nn -lt $Nlayer ]
	      do
	      nn=`expr $nn + 1`
	      mv cpmnd_$Nkl'_'$nn  me_cpmn_$Nkl'_'$nn$last_name
	    done
	else
	    nn=0
	    for lname in ${whichlayers[@]}
	      do
	      nn=`expr $nn + 1`
	      mv cpmnd_$Nkl'_'$nn  me_cpmn_$Nkl'_'$lname$last_name
	    done
	fi
    fi
    ls me_cpmn_$Nkl*
fi
## rename CalPmm file
if [ $lpmm == 'true' ]
then
    if [ $layers == 'half-slab' ]
    then
	mv cpmmd_$Nkl'_1' me_cpnn_$Nkl'_half-slab'$last_name 
    else
	whichlayers=`less .lista_layers`
	if [ "$whichlayers" == "all" ]
	    then
	    nn=0
	    while [ $nn -lt $Nlayer ]
	      do
	      nn=`expr $nn + 1`
	      mv cpmmd_$Nkl'_'$nn  me_cpnn_$Nkl'_'$nn$last_name
	    done
	else
	    nn=0
	    for lname in ${whichlayers[@]}
	      do
	      nn=`expr $nn + 1`
	      mv cpmmd_$Nkl'_'$nn  me_cpnn_$Nkl'_'$lname$last_name
	    done
	fi
    fi
    ls me_cpnn_$Nkl*
fi
## rename CalScc' file
if [ $lsccp == 'true' ]
then
    if [ $layers == 'half-slab' ]
    then
	mv csmnd_$Nkl'_1' me_csccp_$Nkl'_half-slab'$last_name 
    else
	whichlayers=`less .lista_layers`
	if [ "$whichlayers" == "all" ]
	    then
	    nn=0
	    while [ $nn -lt $Nlayer ]
	      do
	      nn=`expr $nn + 1`
	      mv csmnd_$Nkl'_'$nn  me_csccp_$Nkl'_'$nn$last_name
	    done
	else
	    nn=0
	    for lname in ${whichlayers[@]}
	      do
	      nn=`expr $nn + 1`
	      mv csmnd_$Nkl'_'$nn  me_csccp_$Nkl'_'$lname$last_name
	    done
	fi
    fi
    ls me_csccp_$Nkl*
## finishes action=run
fi
########################## run: end #####################################
##
TIMEENDALL=`date`
Line
printf "\t--------------------------------------------------\n"
printf "\t     Started at time: ${GREEN}$TIMESTARTALL ${NC}\n"
printf "\t--------------------------------------------------\n"
printf "\tFinished all at time: ${GREEN}$TIMEENDALL ${NC}\n"
printf "\t--------------------------------------------------\n"
TIME1=`date --date="$TIMESTARTALL" +%s`
TIME2=`date --date="$TIMEENDALL" +%s`
ELTIME=$[ $TIME2 - $TIME1 ]
TMIN=$(echo "scale=9; $ELTIME/60" | bc)
TMIN1=$(echo "scale=9; $ELTIME/3600" | bc)
printf "\t total time:   $TMIN min. \n"
printf "\t               $TMIN1 Hrs. \n"
printf "\t--------------------------------------------------\n"
###
Line
gracias
Line
rm -f all_nodes* como_* concat* hoy  run_tiniba_como* siTie* spin_info*
rm -f error fnval
#	rm -f .peso* ifcentro*
