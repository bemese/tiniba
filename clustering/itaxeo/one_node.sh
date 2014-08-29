#!/bin/bash
##
red='\e[0;31m'
RED='\e[1;31m'
blue='\e[0;34m'
BLUE='\e[1;34m'
cyan='\e[0;36m'
CYAN='\e[1;36m'
GREEN='\e[0;32m'
MAG='\e[0;35m'
NC='\e[0m' # No Color
#
# reads TINIBA version from version-tiniba.txt
source version-tiniba.txt
#
# displays the chosen options 
#
function option {
    caso=
    if [[ $rho == '1' ]]
    then
	caso="rho"
    fi
    if [[ $em == 'true' ]]
    then
	caso="$caso Em(k)"
    fi
    if [[ $pmn == 'true' ]]
    then
	caso="$caso Pmn(k)"
    fi
    if [[ $rhoccp == 'true' ]]
    then
	caso="$caso rhoccp(l;k)"
    fi
    if [[ $lpmn == 'true' ]]
    then
	caso="$caso CalPmn(k)"
    fi
    if [[ $lpmm == 'true' ]]
    then
	caso="$caso CalPmm(k)"
    fi
    if [[ $sccp == 'true' ]]
    then
	caso="$caso Scc'(k)"
    fi
    if [[ $lsccp == 'true' ]]
    then
	caso="$caso CalScc'(k)"
    fi
    if [[ $vnlkss == 'true' ]]
    then
	caso="$caso v^nl_nm(k)"
    fi
}
##
## Line
function Line {
    printf "\t${BLUE}=============================${NC}\n"
}
##
quien=`whoami`
# runs a given job in a given node
trunc_dir=$TINIBA/clustering/itaxeo
# input from all_nodes.sh
node=$1
input=$2
abiexec=$3
caseo=$4
diro=$5
No=$6
outdir=$7
Nlayers=$8
Nko=$9
last_name=${10}
rho=${11}
em=${12}
pmn=${13}
rhoccp=${14}
lpmn=${15}
lpmm=${16}
sccp=${17}
lsccp=${18}
vnlkss=${19}
void=${20}
dpexec=${21}
##
# rho=1 => rho(z)
# rho=2 no rho(z)
if [[ $rho == 'true' ]]
then
    rho=1
else
    rho=2
fi
##
options="$rho .$em. .$pmn. .$rhoccp. .$lpmn. .$lpmm. .$sccp. .$lsccp."
##
##
spinor=`awk '{print $1}' $diro/spin_info`
parent=`echo $diro | awk -F / '{a=NF; print $(a-1)}'`
# checks that spinor exists before the shell continues
while [[ $spinor != '1' &&   $spinor != '2' ]]
do
#echo spin_info does not exists for $node
    if [ -e spin_info ] 
    then
	spinor=`awk '{print $1}' spin_info`
    fi
done
## reads the node=node=>xeon or itanium
nodo=`$trunc_dir/trunc.sh $node`
##
## choses the correct executable for the matrix elements
##
if [ $nodo = 'node' ]
then
    rpmns_exec=$TINIBA/matrix_elements/rpmns.xeon
fi
if [ $nodo = 'itanium' ]
then
    rpmns_exec=$TINIBA/matrix_elements/rpmns.itanium
fi
if [ $nodo = 'quad' ]
then
    rpmns_exec=$TINIBA/matrix_elements/rpmns.quad
fi
if [ $nodo = 'hexa' ]
then
    rpmns_exec=$TINIBA/matrix_elements/rpmns.hexa
fi
##
if [ ! -d $input ]
then
    echo "$input on $node does not exist, creating."
    exit 1
fi
#
# changes to working directory
cd $input
# copy zmesh.d to working directory
if [[ $rho == '1' ]]
then
    cp $diro/zmesh.d .
fi
#
#
echo "#!/bin/bash"> oneNodeLocal.sh
echo "##`date`   ">> oneNodeLocal.sh 
echo "`dirname $0`/`basename $0` $@ " >> oneNodeLocal.sh
chmod 777 oneNodeLocal.sh
rm -f quemuevo.sh
touch quemuevo.sh
#
# gets Lslab
grep acell $caseo.in > hoy1
a=`awk '{print $2}' hoy1`
b=`awk '{print $3}' hoy1`
c=`awk '{print $4}' hoy1`
zmin=`tail -1 $diro/$caseo.xyz | awk '{print $3}'`
echo $c $Nlayers $zmin $rho > fort.98
rm -f hoy*
# runs abinit for wavefunctions
dime=$caseo'_'$No
# if case.in has changed it runs the wave function
diff_rows=1
#printf "\taqui voy $PWD\n"
if [ -e $caseo.diff ]
then
    diff_rows=`wc -l $caseo.diff | awk '{print $1}'`
fi
if [[ $diff_rows -ne '0' || ! -e $caseo'o_DS2_WFK' ]]
then
    if [ $diff_rows -ne '0' ]
    then
	Line
	printf "\t${RED}**** $caseo.in at node $node for case $dime has changed: **** ${NC}\n"
	printf "\t${BLUE}    copying:${NC}\n"
	printf "\t$diro/$caseo_$No/$caseo.in $node:/data/$quien/workspace/$parent/$caseo_$No/.\n"
	rcp $diro/$caseo'_'$No/$caseo.in $node:/data/$quien/workspace/$parent/$caseo'_'$No/.
	Line
    fi
    Line
    printf "\tcode is constructing the wavefunctions in node $node for case $dime\n"    Line
## starts the overall time
    TIMESTARTALL=`date`
#    printf "\t$abiexec < $caseo.files > log\n"
    $abiexec < $caseo.files > log
# 
    TIMEEND=`date`
    TIME1=`date --date="$TIMESTARTALL" +%s`
    TIME2=`date --date="$TIMEEND" +%s`
    ELTIME=$[ $TIME2 - $TIME1 ]
    TMIN=$(echo "scale=4; $ELTIME/60" | bc)
    printf "\t------------------------\n"
    printf "\tWavefunction has ${MAG}finished ${NC} for: ${MAG}$node${NC} in $TMIN min\n" 
    Line
# if case.in has not changed it doesn't run the wave function
else
    Line
    printf "\t$caseo.in at node $node for case $dime has NOT changed: same wf\n" 
    Line
fi
# gets the options to be run
option
# gets with or without spin
if [ $spinor == '1' ]
then
    wo='without'
else
    wo='with'
fi
#
# displays the chosen layers
if [ ! $Nlayers == '0' ]; then
    mlay=`less .lista_layers`
else
    mlay=0
fi
printf "\trunning me of: $caso for layer(s): $mlay at one_node.sh@$node $wo spin\n"
# continues with matrix elements
# executable
#
# start time
timei=`date`
# Matrix elements a la tiniba
#printf "\t$rpmns_exec $caseo'o_DS2_WFK' $options\n"
$rpmns_exec $caseo'o_DS2_WFK' $options > logfile
###
# Velocity Matrix elements of the non-local part of
# the pseudpotentials: v^\nl_{nm}(k)=(i/hbar)<nk|[\hat V^\nl,\har r]|mk>
# calculated with DP-code
if [[ $vnlkss == "true" ]]
then
printf "\tDP running me at one_node.sh@$node $wo spin\n"
$dpexec -i dp-vnl-$caseo.in -k $caseo'o_DS3_KSS' > dp-vnl-log
mv velocity.out $diro/$caseo'_'$No/vnl.d
fi
###
# moves output to files in $outdir (i.e. 'res' directory)
if [[ $rho == "1" ]]
then
    mv fort.39   $outdir/rhoz_$No
    mv fort.40   $outdir/psiz_$No
fi
# moves output to files to different directories from where they'll be concatenated
# energy
if [[ $em == "true" ]]
then
    mv eigen.d   $diro/$caseo'_'$No/energy.d
fi
# Pmn
if [[ $pmn == "true" ]]
then
    mv pmnhalf.d $diro/$caseo'_'$No/pmn.d
# pmn includes pnn
    mv pnn.d $diro/$caseo'_'$No/pnn.d
fi
# Scc'
if [[ $sccp == "true" ]]
then
    mv spinmn.d $diro/$caseo'_'$No/spinmn.d
fi
#
ondi=/data/$USER/workspace/$parent/$caseo'_'$No/logfile 
onde=$diro/logfiles/log'_'$caseo'_'$Nko'_'$Nlayers'_'$No
rcp $ondi $node:$onde
#
unitS1='39'
unitS2=`expr $unitS1 + 2 \* $Nlayers`
unitS3=`expr $unitS1 + 3 \* $Nlayers`
nn=0
while [ $nn -lt $Nlayers ]
do
    nn=`expr $nn + 1`
    cual1=`expr $nn + $unitS1`
    cual2=`expr $nn + $unitS2`
    cual3=`expr $nn + $unitS3`
    cual4=`expr $nn + $unitS1 + 10`
# CalPmn
    if [[ $lpmn == "true" ]]
    then 
	mv fort.$cual1 $diro/$caseo'_'$No/cpmnd'_'$nn
	mv fort.$cual4 $diro/$caseo'_'$No/cfmnd'_'$nn
    fi
# rhoccp
     if [[ $rhoccp == "true" ]]
     then 
 	mv fort.$cual1 $diro/$caseo'_'$No/rhoccpd'_'$nn
     fi
# CalPmm
    if [[ $lpmm == "true" ]]
    then 
	mv fort.$cual2 $diro/$caseo'_'$No/cpmmd'_'$nn
    fi
# CalScc'
    if [[ $lsccp == "true" ]]
    then
	mv fort.$cual2 $diro/$caseo'_'$No/csmnd'_'$nn
    fi
##
done
##
timef=`date`
TIME1m=`date --date="$timei" +%s`
TIME2m=`date --date="$timef" +%s`
ELTIMEm=$[ $TIME2m - $TIME1m ]
TMINm=$(echo "scale=4; $ELTIMEm/60" | bc)
##
parent=`echo $PWD | awk -F / '{a=NF; print $(a-1)}'`
Line
printf " [${GREEN}`hostname`${NC}] : /data/$USER/workspace/$parent/$caseo"_"$No ended in $TMINm min \n"
touch finished$No
cp finished$No $diro/.
rm finished$No  
#################
if [ -e hoy ]
then
    err="No Error"
    if [ ! -e pmnhalf.d ]
    then
	echo "***one_node.sh: ERROR: Cannot find pmnhalf.d in $node$input!"
	err="Error"
    fi
    if [ ! -e pnn.d ]
    then
	echo "***one_node.sh: ERROR: Cannot find pnnhalf.d in $node$input!"
	err="Error"
    fi
    
    if [ ! -e eigen.d ]
    then
	echo "***one_node.sh: ERROR: Cannot find eigen.d in $input!"
	err="Error"
    fi
    if [ $Nlayers = '0' ]
    then
	which="whole system"
    fi
    if [ $Nlayers != '0' ]
    then
	which="Layer"
    fi
    echo "***DONE: Calculation for $which on $node$input with $err."
fi
exit 0
#################
