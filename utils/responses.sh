#!/bin/bash
##
## output
# standard: file_ii_components_Nk_Ecut_spin_
# layered: file_ii_components_Nk_Layer_Ecut_spin_Nc 
##
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
GREEN='\e[0;32m'
GRE='\e[1;32m'
YELLOW='\e[1;33m'
MAG='\e[0;35m'
NC='\e[0m' # No Color
##
function Line {
printf "\t${cyan}--------------------${NC}\n"
}
##
exec=`$TINIBA/utils/cual_node.sh`
## input at will
eminw=0
emaxw=20
stepsw=2001
#emaxw=3
#stepsw=601
#sicw=0

#toldef=0.015 ## zeta 
#toldef=0.045 ## zeta 
#toldef=0.15 ## zeta 

toldef=0.03  ## original

##
declare -a scases
where=$TINIBA/utils
where_latm=$TINIBA/latm
where_smear=$TINIBA/smear
where_trunc=$TINIBA/clustering/itaxeo
laheylib="env LD_LIBRARY_PATH=/usr/local/lf9562/lib"
dir=$PWD
case=`echo $PWD | awk -F / '{print$NF}'`
grep nband2 setUpAbinit_$case.in > hoy
Nband=`head -1 hoy | awk '{print $2}'`
Nmax=$Nband
##==========================
     ESPINsetUp=`grep nspinor setUpAbinit_$case.in  |  awk '{print $2}'`
       if [ -z $ESPINsetUp ];then
         printf "\tESPINsetUp= $ESPINsetUp"
         printf "you have to define your spin in: setUpAbinit_$case.in\n"
         exit 127
       fi
       if [ "$ESPINsetUp" -eq "1" ] || [ "$ESPINsetUp" -eq "2" ] ;then
	   nada=0
#         printf "\tESPINsetUp= $ESPINsetUp ok \n"
       else 
         printf "spin has to be 1 or 2 ...\n"
         exit 127 
       fi   
     ##==========================
  # checks if res directory exists ifnot creates it

if [ ! -d res ] 
then
    echo -e ${RED} creating ${BLUE}res${NC} directory  
    mkdir res
fi

# gets the nodes to be used and stores them into an array.

if [ ! -e '.machines_res' ]
then
    Line
    echo -e "The file $blue .machines_res $NC was not found. Please create one before running the script"
    Line
    exit 1
fi
if [ ! -e '.machines_latm' ]
then
    Line
    echo -e "The file $blue .machines_latm $NC was not found. Please create one before running the script"
    Line
    exit 1
fi

i=0;
for inode in `cat .machines_res`
  do
  i=$(( i + 1 )) 
  nodes[$i]=$inode
done
for inode in `cat .machines_latm`
  do
  latm_node=$inode
done
if [ "$inode" == "medusa" ]
then
    used_node=$inode
else
    used_node=`$where_trunc/trunc.sh $latm_node`
fi
# echo ${#nodes[@]}

####################################################################
# counts the number of occupied states from acs_check/case.out file
    grep -n 'occ ' $case'_check'/$case.out > hoyj
    iocc=`awk -F: '{print $1}' hoyj`

    grep -n 'prtvol ' $case'_check'/$case.out > hoyj
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

    Nct=`expr $Nmax - $Nvf`
    rm -f hoyj*
#
    if [ "$#" -eq 0 ]
	then   # Script needs at least one command-line argument.
	clear
	echo -e ${cyan}%%%%%%%%%%%%% ${BLUE} choose a response ${cyan}%%%%%%%%%%%%%%${NC}
	$where/print_responses.pl
	Line
	echo Check/Change Emin=$eminw, Emax=$emaxw and $stepsw intervals
	echo -e Check/Change ${blue}tolerance${NC} value of ${RED}$toldef${NC}
	Line
	echo -e "There are ${RED}$Nmax${NC} bands, so choose Nv=${RED}$Nvf${NC} and up to Nc=${RED}$Nct${NC} "
	Line
	printf "responses.sh -w [${red}layer${NC} or ${red}total${NC}] -m [_${red}case${NC}] -s [${red}scissors${NC}] -o [${red}1${NC}-full or ${red}2${NC}-vc] -v [${red}Nv${NC}] -c [${red}Nc${NC}] -r [response${red}#${NC}] -t [tensor ${red}\"ijk ...\" ${NC}]\n"
	Line
	echo you have the following options:
	echo
	ls pmn*
	echo
#	echo You can use the following k-points files:
#	ls *klist*
	Line
	exit 1
    fi
# gets options
while getopts ":w:m:s:o:v:c:r:t:" OPTION
do
     case $OPTION in
         w)
	     lt=$OPTARG
             ;;
         m)
             caso=$OPTARG
             pfix=$OPTARG
             ;;
         s)
             tijera=$OPTARG
             ;;
         o)
             opt=$OPTARG
             ;;
         v)
             Nv=$OPTARG
             ;;
         c)
             Nc=$OPTARG
             ;;
         r)
             response=$OPTARG
             ;;
         t)
             scases=($OPTARG) #the parenthesis () are so it reads an array 
             ;;
         ?)
             printf "\t${RED}error${NC}\n"
             exit
             ;;
     esac
done
## starts the overall time
TIMESTARTALLI=`date`
### shell starts:
# data for option=1 from all valence bands Nv to 1...Nc conduction bands
#          option=2 from a given valence band  to a given conduction band
	rm -f opt.dat
	echo $opt $Nv $Nc > opt.dat
###
    if [ $lt == 'layer' -o $lt == 'total' ]
	then
	echo $caso > chispas
	Nk=`awk -F _ '{print $1}' chispas`
	rm chispas

	n_responses=${#scases[@]}
	aux='nada' # nada is fine
	if [ $lt == 'layer' ]
	    then
# get correct last_name for eigenvalues
	    echo $caso > chispas
	    last_name=`awk -F _ '{print $3}' chispas`

	    pfixe=$Nk'_'$last_name
	    rm chispas
#
	    cspin='me_csccp_'
#	    rhomm='me_rhomm_'
	    rhomm='me_rhoccp_'
	    cal='me_cpmn_'
	    cur='me_cpnn_'
	    aux='nada' # nada is fine
	else
	    spin='me_sccp_'
	    pfixe=$pfix
	fi

### name of the response
	if [ -z $response ]
	    then
	    Line
	    echo -e ${red}variable response not set${NC}
	    Line
	else
	    echo  "awk -F : '{if(\$1==$response) print \$2}' $where/responses.txt" > cthoy
	chmod +x cthoy
	sname=`cthoy`
	sname=`echo $sname | sed 's/ //g'`
	rm cthoy
	fi

#
	rm -f chido
	rm -f tmp*
	rm -f hoy* halfe* spectra.params* fort* 
	rm -f bc*
	rm -f Symmetries.Cartesian* kpoints.reciprocal_$Nk kpoints.cartesian_$Nk tetrahedra_$Nk
	if [ -z $sname ] 
	then
	    Line
	    echo -e ${red}variable sname not set${NC}
	    Line
	else
	    rm -f $sname* 
	fi
#
# various checkups for consistency
	if [[ ${#scases[@]} == 0 ]]
	then
	    Line
	    echo -e "No tensor componentss were provided please provide at least one"
	    Line
	    exit 1
	fi
#
	if [ $lt == 'total' ]
	then
	    if [[ $response = 24 || $response = 25 || $response = 29 || $response = 44 || $response = 45 ]]
	    then
		Line
		printf "\t${BLUE}$sname${NC} is not total, chose a ${RED} total response${NC}\n"
		Line
		exit 1
	    fi
	fi
#
#
	if [ $lt == 'layer' ]
	    then
	    if [[ $response != 24 && $response != 25 && $response != 29 && $response != 44 && $response != 45 ]]
		then
		Line
		printf "\t${BLUE}$sname${NC} is not layered, chose a ${RED} layered response${NC}\n"
		touch fallo
		Line
		exit 1
	    fi
	fi
#
	if [[ ${#nodes[@]} -lt  $n_responses  ]]
	then
	    Line
	    echo -e "There are more responses than nodes. Correct it before running the script"
	    Line
	    exit 1
	fi
##
	cp symmetries/tetrahedra_$Nk .
	cp symmetries/Symmetries.Cartesian_$Nk Symmetries.Cartesian
##
	mme='me_pmn_'
	ene='eigen_'
	spe='spectrum_ab'
	rm -f tmp_$pfix
##
## chooses a scissors shift
## stored in "sicw"
## It renormalizes the momentum matrix elements according to 
## pmn->pmn*(Ec-Ev)/(Ec-Ev-Delta)
## Then the conduction bands energies are ridgily shifted by Delta
## at every k-point.
##        
	sicw=$tijera
	$where/scissors.sh eigen_$pfixe 0
#	Line
#	echo -e LDA shift of $sicw
#	Line
	Delta=$sicw
	echo $Delta > fort.69
##### spectra.params file
## number of tensor components
	num=${#scases[@]}
	echo $num                                  > spectra.params_$pfix
	j='0'
#	Line
#	echo -e pfix for eigenvalues and pmn: $pfixe
#	if [ $lt == 'layer' ]
#	then
#	    echo -e pfix for caligraphic matrix elements: $pfix
#	fi
	Line
	printf "\trunning respone num=${RED}$response${NC} for the following cases:\n"
	for i in ${scases[@]}
	  do
	  a=${scases[$j]}
	  echo $a > hoy
## changes x->1, y->2, z->3 and adds space for each tensor component
	  xyz123=`sed s/x/' 1'/g hoy | sed s/y/' 2'/g | sed s/z/' 3'/g `
	  j=`expr $j + 1`
	  file=`expr $j + 50`
## writes to screen
	  printf "\t$sname.$a.dat_$pfix\n"
#	  echo $xyz123 
## writes to file
	  echo $response "$sname.$a.dat_$pfix" $file T   >> spectra.params_$pfix
	  echo $xyz123 >> spectra.params_$pfix
	done
#####
	echo \&INDATA > tmp_$pfix
##### We use $Nvf for the total number of valence bands!!!
	echo nVal= $Nvf, >> tmp_$pfix
	echo nMax= $Nmax, >> tmp_$pfix
	echo nVal_tetra= $Nvf, >> tmp_$pfix
##############################################################
#### could be to any value of conduction bands               #   
#### if Nc=$Nmax - $Nv all conduction bands are being used   #
#### otherwise change to any value                           #
##############################################################
	echo nMax_tetra= $Nc, >> tmp_$pfix
#####################
	echo kMax= $Nk, >> tmp_$pfix
#####################
	echo scissor= $sicw, >> tmp_$pfix
	echo tol= $toldef, >> tmp_$pfix
##
         echo nSpinor= $ESPINsetUp, >> tmp_$pfix
        
### added 10 de diciembre de 2008 at 15:30
        if [  "$ESPINsetUp" -eq "1" ];then 
        echo withSO= .false., >> tmp_$pfix
        fi 
        if [  "$ESPINsetUp" -eq "2" ];then 
	echo withSO= .true., >> tmp_$pfix        
      fi
### added 10 de diciembre de 2008 at 15:30
#	Line
	printf "\twith ${RED}Spin-Orbit${NC}\n"
	if [ ! -r  $ene$pfixe ] 
	    then
	    Line
	    echo WARNING NO $ene$pfixe
	    Line
	    exit 1
	fi
	echo  energy_data_filename= \""$ene$pfixe"\", >> tmp_$pfix
#
	echo energys_data_filename= \""energys.d_$pfix"\", >> tmp_$pfix
	echo half_energys_data_filename= \""halfenergys.d_$pfix"\", >> tmp_$pfix
#
	    if [ ! -r $mme$pfixe ] 
		then
		Line
		echo WARNING NO $mme$pfixe
		Line
		exit 1
	    fi
	echo pmn_data_filename= \""$mme$pfixe"\", >> tmp_$pfix
	echo rmn_data_filename= \""rmn.d_$pfix"\", >> tmp_$pfix
# for spin-related calculations
#
		if [[ $response == '41' || $response == '29' ]]
		then
		    if [ ! -r $spin$pfix ] 
		    then
			Line
			echo WARNING NO $spin$pfix for bulk spin injection 
			Line
			exit 1
		    fi
# the smn_data_filename file is the same for bulk or layer
		    echo smn_data_filename= \""$spin$pfix"\", >> tmp_$pfix
		fi
# ndot calculation
		if [ -e $rhomm$pfix ]
		    then
		    echo rhomm_data_filename= \""$rhomm$pfix"\", >> tmp_$pfix
		fi
### layered calculation
	    if [ $lt == 'layer' ] 
	    then
#
		if [[ $response == '24' || $response == '44' || $response == '45' ]]
		then
		    if [ ! -r $cal$pfix ] 
		    then
			Line
			echo WARNING NO $cal$pfix for layer response
			Line
			exit 1
		    fi
		    echo cal_data_filename= \""$cal$pfix"\", >> tmp_$pfix
		fi
#
		if [ $response == '25' ]
		then
		    if [ ! -r $cur$pfix ] 
		    then
			Line
			echo WARNING NO $cur$pfix for layer injection current
			Line
			exit 1
		    fi
		    echo cur_data_filename= \""$cur$pfix"\", >> tmp_$pfix
		fi
#
		if [ $response == '29' ]
		then
		    if [ ! -r $cspin$pfix ] 
		    then
			Line
			echo WARNING NO $cspin$pfix for layer spin injection 
			Line
			exit 1
		    fi
# the smn_data_filename file is the same for bulk or layer
		    echo smn_data_filename= \""$cspin$pfix"\", >> tmp_$pfix
		fi
#
	    fi
### end: if [ $lt == 'layer' ] 
### continue
	    echo der_data_filename= \""der.d_$pfix"\", >> tmp_$pfix
	    echo tet_list_filename= \""tetrahedra_$Nk"\", >> tmp_$pfix
	    echo integrand_filename= \""Integrand_$pfix"\", >> tmp_$pfix
	    echo spectrum_filename= \""Spectrum_$pfix"\", >> tmp_$pfix
#####################
	    energy_min=$eminw
	    energy_max=$emaxw
	    energy_steps=$stepsw
	    echo energy_min= $energy_min,  >> tmp_$pfix
	    echo energy_max= $energy_max, >> tmp_$pfix
	    echo energy_steps= $energy_steps >> tmp_$pfix
#####################
	    echo  / >> tmp_$pfix
#####################
	    echo \&INDATA > tmp3_$pfix
	    echo energy_min= $energy_min,  >> tmp3_$pfix
	    echo energy_max= $energy_max, >> tmp3_$pfix
	    echo energy_steps= $energy_steps >> tmp3_$pfix
	    echo  / >> tmp3_$pfix
    fi
#exit 1
    if [ ! -e tmp_$pfix ]
    then
	Line
	echo Abnormal execution, tmp_$pfix  was not created. Please run again.
	Line
	exit 1
    fi

    energy_steps=`grep energy_steps tmp_$pfix | awk -F= '{print $2}'`

#    Line
    if [ $used_node  == 'node' ]
    then
	printf "\tRUNING: $where_latm/set_input_32b [$latm_node]\n"
	echo ssh $latm_node "cd $dir; $where_latm/set_input_32b tmp_$pfix spectra.params_$pfix "
	ssh $latm_node "cd $dir; $where_latm/set_input_32b tmp_$pfix spectra.params_$pfix "
    elif [[ $used_node  == 'medusa' || $used_node  == 'hexa' ]]
    then
	printf "\tRUNING: $where_latm/set_input_hexa [$latm_node]\n"
#	ssh $latm_node "cd $dir; $where_latm/set_input_64b tmp_$pfix spectra.params_$pfix "
	$where_latm/set_input_hexa tmp_$pfix spectra.params_$pfix 
   elif [ $used_node  == 'itanium' ]
    then
	printf "\tRUNING: $where_latm/set_input_64b [$latm_node]\n"
	ssh $latm_node "cd $dir; $where_latm/set_input_64b tmp_$pfix spectra.params_$pfix "
    elif [ $used_node  == 'quad' ];then
	CORRE="$where_latm/set_input_quad"
	if [ ! -e $CORRE ];then 
	    printf "\t $CORRE \n"
	    printf "\t Does not exits ... press any key to continue... \n"
	    read -p ""
	fi  
	Line
#	printf "\t $where_latm/set_input_quad tmp_$pfix  spectra.params_$pfix [$latm_node]\n"
	printf "\tRUNING: $where_latm/set_input_quad [$latm_node]\n"
	Line
        cp  tmp_$pfix input1set
        cp  spectra.params_$pfix input2set
	ssh $latm_node "cd $dir; $where_latm/set_input_quad tmp_$pfix spectra.params_$pfix "
    else
	echo -e "$RED  $used_node $NC is not a valid option "
	echo  Please run again
	exit 1
    fi
    Line
    TIMEENDALLI=`date`
    TIME1=`date --date="$TIMESTARTALLI" +%s`
    TIME2=`date --date="$TIMEENDALLI" +%s`
    ELTIME=$[ $TIME2 - $TIME1 ]
    TMIN=$(echo "scale=9; $ELTIME/60" | bc)
    TMIN1=$(echo "scale=9; $ELTIME/3600" | bc)
    printf "\tThe integrand has been calculated in $TMIN min\n"

#### to only calculate the integrand uncomment the next line
#    exit 1
####
    printf "\tThe integrand will be integrated\n"
    Line
######## run for each ij component
    diez='10'
    i=0
    label=''
    for ij in ${scases[@]}
    do
	i=`expr $i + 1`
	sed s/Integrand_$pfix/$sname.$ij.dat_$pfix/ tmp_$pfix > tmp1_$pfix
	sed s/Spectrum_$pfix/$sname.$ij.$spe'_'$pfix/ tmp1_$pfix > tmp2_$pfix
	grep -v $aux tmp2_$pfix > int_$ij'_'$pfix
#	rm -f $sname.$ij.$spe'_'$pfix $sname.$ij.kk.$spe'_'$pfix hoy_$ij'_'$pfix
	if [ "${nodes[$i]}" == "medusa" ]
	then
	    used_node=${nodes[$i]}
	else
	    used_node=`$where_trunc/trunc.sh ${nodes[$i]}`
	fi
	if [ $used_node == 'node' ]
	then
	    ssh ${nodes[$i]} "cd $dir;$laheylib  $where_latm/tetra_method_32b int_$ij'_'$pfix > hoy_$ij'_'$pfix" &
	elif [[ $used_node == 'medusa' ||  $used_node == 'hexa' ]]
	then
	    $where_latm/tetra_method_hexa int_$ij'_'$pfix > hoy_$ij'_'$pfix &
	elif [ $used_node == 'itanium' ]
	then
	    ssh ${nodes[$i]} "cd $dir;$laheylib  $where_latm/tetra_method_64b int_$ij'_'$pfix > hoy_$ij'_'$pfix" &
	elif [ $used_node == 'quad' ]
	then
#	    printf "\t$where_latm/tetra_method_quad int_$ij_$pfix > hoy_$ij_$pfix\n"
	    ssh ${nodes[$i]} "cd $dir;$laheylib  $where_latm/tetra_method_quad int_$ij'_'$pfix > hoy_$ij'_'$pfix" &
	else
	    echo -e "$RED ${nodes[$i]} $NC is not a valid option in .machines_res "
	    echo please run again
	    exit 1
	fi
	printf "\t${red}run${NC} $ij at ${RED}${nodes[$i]}${NC}\n"
# label for responses
	label=$label'_'$ij
    done
    printf "\t${RED}waiting for $case at each node to finish for $pfix${NC}\n"
    Line
#exit 1
########
    for ij in ${scases[@]}
    do
	if [ ! -e $sname.$ij.$spe'_'$pfix ]
	then
	    int='0'
	else
	    int=`wc -l $sname.$ij.$spe'_'$pfix | awk '{print $1}'`
	fi
#	    echo 1st $int $energy_steps
	until [ $int == $energy_steps ] 
	do
	    if [ ! -e $sname.$ij.$spe'_'$pfix ]
	    then
		int='0'
	    else
		int=`wc -l $sname.$ij.$spe'_'$pfix | awk '{print $1}'`
	    fi
#	    echo 2nd $int $energy_steps
	done
	printf "\t${blue}$ij ${red}done${NC}\n"
    done
    Line
    printf  "\t${blue}all $case nodes done for $pfix${NC}\n"
    Line
#exit 1
## kk
	for ij in ${scases[@]}
	do
            if [[ $response == '17' || $response == '25' || $response == '3' || $response == '41' || $response == '29' || $response == '26' || $response == '27' ]]
	    then
# ndot is  real => no need for KK
# zeta is purely imaginary => no need for KK
		printf "\t${red}no need for KK $ij${NC}\n"
		awk '{print $1}' $sname.$ij.$spe'_'$pfix > bc1_$pfix
		awk '{print $2,$3}' $sname.$ij.$spe'_'$pfix > bc$ij'_'$pfix
		echo bc$ij'_'$pfix >> chido 
	    else
		printf "\t${red}KK for $ij${NC}\n"
#                      printf "\t Doing kk \n" 
		$TINIBA/kk/rkramer_$exec 1 $sname.$ij.$spe'_'$pfix $sname.$ij.kk.$spe'_'$pfix >> hoy_$ij'_'$pfix
		awk '{print $1}' $sname.$ij.kk.$spe'_'$pfix > bc1_$pfix
		awk '{print $2,$3}' $sname.$ij.kk.$spe'_'$pfix > bc$ij'_'$pfix
		echo bc$ij'_'$pfix >> chido 
	    fi
	done
#exit 1
## pasting
	filename=chido
	declare -a array1
	array1=(`cat "$filename"`)
	file1=$sname.$label
	paste bc1_$pfix `echo ${array1[@]}` > $file1
################# Smearing a la Fred ##################		
###%%%###@@@@ I bet the daring young soul to get the files names to a modicum minimum 
	if [ 1 == 1 ];then
	    ap=$label'_'$pfix'_'
	    am='scissor_'$tijera'_'
	    label=$label'_'$pfix'_Nc_'$Nc
	    if [ $opt == '1' ]
	    then
		ultimo='Nc_'$Nc
	    fi
	    if [ $opt == '2' ]
	    then
		ultimo='vc_'$Nv'_'$Nc
	    fi
#
	    file2=$sname.sm$label
	    label2='_scissor_'$tijera
#		$where_smear/smear 1 $file1 $file2 > hoy                     #smearing
	    Line
            printf "\tSmearing\n"
#                 printf "$where_smear/rsmear2_$exec 1 $file1 $file2\n"
            if [ ! -e $where_smear ];then 
                printf "\t I could find this file: \n"
                printf "\t $where_smear/rsmear2_$exec \n"
                printf "\t Ctrl C to Stop\n"
                read -p ""
            fi  
	    $where_smear/rsmear2_$exec 1 $file1 $file2 > hoy     #smearing
#	    file3=$sname.kk$label$label2
#	    file4=$file2$label2
	    file3=$sname.kk$ap$am$ultimo
	    file4=$sname.sm$ap$am$ultimo
	    if [ "$response" -ne "25" ];then
		mv $file1  res/$file3
		mv $file2  res/$file4
		Line
		printf "\tOutput:\n"
		if [ -e  "res/$file3" ];then
		    printf "\t${BLUE}res/${GREEN}$file3${NC}\n"
		    if [[ $response -eq "21" || $response -eq "22" || $response -eq "42" || $response -eq "43" || $response -eq "44" || $response -eq "45" ]]
		    then
			echo $file3 > $response.kk.dat
		    fi
		fi
		if [ -e "res/$file4" ];then
		    printf "\t${BLUE}res/${GREEN}$file4${NC}\n"
		    if [[ $response -eq "21" || $response -eq "22" || $response -eq "42" || $response -eq "43" || $response -eq "44" || $response -eq "45" ]]
		    then
			echo $file4 > $response.sm.dat
		    fi
		fi 
		Line
	    fi
######
###### if the response is  do this any other case jump 
	    if [ "$response" == "25" ];then
		rm -f caleta1
		rm -f caleta2
		if [ -d res/25 ];then
		    printf "\t res/25 ok\n" 
		else 
		    mkdir -p res/25
		fi 
		cp $file1  res/25/$file3
		cp $file2  res/25/$file4
		mv $file1  caleta1
		mv $file2  caleta2
		
       ###### right now define the hostname 
       ###### 
		HOSTIA=`hostname`
		if [[ "$HOSTIA" == "medusa" ]]; then
		    HOWSTICKSLAB=$TINIBA/howtickslab/howtickslab.xeon
		fi
		if [[ "$HOSTIA" == "itanium"* ]]; then
		    HOWSTICKSLAB=$TINIBA/howtickslab/howtickslab.itan
		fi
		if [[ "$HOSTIA" == "quad"* ]]; then
		    HOWSTICKSLAB=$TINIBA/howtickslab/howtickslab.quad
		fi
       ###### 
       ###### 
       ###### 
		if [ -e "$HOWSTICKSLAB" ];then 
		    printf "$HOWSTICKSLAB ${GREEN}$ok${NC}\n"
		    $HOWSTICKSLAB caleta1 > hoy
         # read -p "please press any kye to continue ...."
		    $HOWSTICKSLAB caleta2 > hoy
		    mv caleta1  res/$file3
		    mv caleta2  res/$file4
		    Line
		    printf "\tThis response is multi by the wide of the slab... \n"
		    if [ -e  "res/$file3" ];then
			printf "\t${BLUE}res/${GREEN}$file3${NC}\n"
		    fi
		    if [ -e "res/$file4" ];then
			printf "\t${BLUE}res/${GREEN}$file4${NC}\n"
		    fi 
		    Line
		fi
	    fi 
	fi		
	rm -f chido
	if [ -z $sname ] 
	then
	    Line
	    echo -e ${red}variable sname not set${NC}
	    Line
	else
	    rm -f $sname* 
	fi
	rm -f bc*
	rm -f Symmetries.Cartesian* kpoints.reciprocal_$Nk kpoints.cartesian_$Nk tetrahedra_$Nk
        rm -rf tmp* endWELL*		
        rm -rf hoy*
	rm -f energys.d* fort* fromSmear halfene*
	rm -f input*set  tijeras spectra*
	rm -f  response_type 
	rm -f int_*
