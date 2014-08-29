#!/bin/bash
#CHILDREN:
##cabellos jl 19 Octubre 2009 
##
##
##
export I_MPI_DEBUG=5
RED='\e[0;31m'
BLUE='\e[0;34m'
BLU='\e[1;34m'
CYAN='\e[0;36m'
GREEN='\e[0;32m'
GRE='\e[1;32m'
YELLOW='\e[1;33m'
NC='\e[0m' # No Color
##
function Line {
    printf "\t${BLUE}=============================${NC}\n"
}
##

 declare -a MACHINESmn
# perhaps this ought to be change
 NUCLEOS=2
#
 DIR=$PWD
 USER=$USER
 BASEDIR=`dirname $PWD`
 CASO=`basename $PWD`
 PARENT=`basename $BASEDIR`
 DIRSCF=$CASO'_scf'
 WFSCF=$CASO'o_DS1_WFK'
## reads abinit executables from version-abinit.txt
source version-abinit.txt
# ab_exec_quad=/homeib/prog/QUAD/abinit-7.0.5/abinit/bin/abinit
# ab_exec_hexa=/home/prog/HEXA/abinit-7.0.5/abinit/bin/abinit
##
 INPUT1=$1
 MYOLDPWD=$PWD
 WFSCFLOCAL=$CASO'o_DS1_WFK'
##------SOURCE------------
##------SOURCE------------
##------SOURCE------------
##------SOURCE------------

 CORRE=0
if [ ! -d "$DIRSCF" ];then
         printf  "$DIRSCF doesnt exist ..."
         touch -f killme
         exit 1
fi
##
##
##
#      printf "\tLet me check ... begin to check WF SCF\n"  
 if [ -e "$DIRSCF/$WFSCFLOCAL" ];then
       printf "\t${BLUE}$DIRSCF${NC}/$WFSCFLOCAL  "
       printf "............... [${GREEN}ok${NC},exist] \n"        
     if [ -e "$DIRSCF/log" ];then
         WARRNINGS=`awk '/Delivered/ { print }' $PWD/$DIRSCF/log`
         CALCULATION=`awk '/Calculation/ { print }' $PWD/$DIRSCF/log`
           if [ -z "$CALCULATION" ];then
           CORRE=1
           else
           printf "\t${BLUE}$DIRSCF${NC}/log "
           printf ".................[${GREEN}ok${NC},exist] \n"
           printf "\t${BLUE}$DIRSCF${NC}/$WFSCFLOCAL "
           printf "....... [${GREEN}ok${NC},seems]\n"
           printf "\t$WARRNINGS \n"
           printf "\t${GREEN}$CALCULATION${NC}\n"
           printf "\t${CYAN}=================================${NC}\n"
           fi
     else 
      CORRE=1
     fi 
 else
  CORRE=1
 fi 
###parallel , cabellos esto es "nasty" .... 
 if [ $INPUT1 -eq "2" ];then 
      uniq .machines_scf > .trueMachines
      MACHINESscf=(`cat .trueMachines`)
      NOMACHINESscf=`echo ${#MACHINESscf[@]}`
      rm -f tmpQ
         if [ -e .trueMachines ];then  
          sed '/^ *$/d' .trueMachines  >  tmpQ
         else 
          printf "Something is wrong : there isnt file: .trueMachines \n"
          exit 1
         fi       
      mv tmpQ .trueMachines
      cp .trueMachines  $DIRSCF/machinesQUAD 
      cp .trueMachines  $DIRSCF/.trueMachines
      NMA=`wc .trueMachines | awk '{print$1}'`
      let "nPROC=$NMA * $NUCLEOS"
#      echo $NMA
 else 
     printf "\tUSAGE: 2=PARALELO \n"
      exit 1       
 fi 
######
######
######
if [ $CORRE == "1" ];then 
    if [[ "${MACHINESscf[00]}" != "quad"* && "${MACHINESscf[00]}" != "hexa"* ]]
    then
	printf "\t This FILE .machines_scf  is not a quad\n"
	exit 1 
    fi 
        TIME=`date`
        TIMESTARTSCF=`date`        
        printf "\tUsing following nodes:\n"
        for ((hh=0;hh<=($NOMACHINESscf-1); hh++)); do
            let "kk=hh+1"
            printf  "\t$kk ${GREEN}${MACHINESscf[$hh]}${NC} with $NUCLEOS nuclei\n"
        done
	if  [ `hostname` == "quad01" ]
	then 
	    rm -f $DIRSCF/$CASO.out*
	    rm -f $DIRSCF/log
	    Line
	    printf  "\tOpening the socket \n"
	    Line
	    cd $DIR/$DIRSCF
	     mpdboot -v -r ssh -f machinesQUAD -n $NMA
	     mpdtrace
#	     printf "\tmpiexec -ppn $NUCLEOS -n $nPROC -env I_MPI_DEVICE rdssm  $ab_exec_quad < $CASO.files >&log\n"
#	     mpiexec -ppn $NUCLEOS -n $nPROC -env I_MPI_DEVICE rdssm  $ab_exec_quad < $CASO.files >&log
	     mpiexec -ppn $NUCLEOS -n $nPROC $ab_exec_quad < $CASO.files >&log
	     cd $DIR
	     Line
	     printf  "\tClosing the ring\n"
	     mpdallexit
	elif [[ `hostname` == "hexa"* ]]
	then
# We use the file name machinesQUAD although the nodes are hexas 
	    rm -f $DIRSCF/$CASO.out*
	    rm -f $DIRSCF/log
# gets the first hexa to launch the ring of the Nibelungs
	    hexa0=`head -1 $DIRSCF/machinesQUAD`
	    Line
	    printf  "\tOpening the socket from $hexa0 \n"
	    ssh $hexa0 "cd $DIR/$DIRSCF;mpdboot -v -r ssh -f machinesQUAD -n $NMA;mpdtrace"
	    Line
	    printf  "\tAbinit for SCF is now runing\n"
	    ssh $hexa0 "cd $DIR/$DIRSCF;mpiexec -ppn $NUCLEOS -n $nPROC -env I_MPI_DEVICE rdssm  $ab_exec_hexa < $CASO.files >&log" 
	    Line
	    printf  "\tClosing the ring from $hexa0 \n"
	    ssh $hexa0 "cd $DIR/$DIRSCF;mpdallexit"
#;mpdtrace"
	else 
	    rm -f $DIRSCF/$CASO.out*
	    rm -f $DIRSCF/log
	    printf  "\t Opening the socket \n"
	    ssh quad01 "cd $DIR/$DIRSCF;mpdboot -v -r ssh -f machinesQUAD -n $NMA;mpdtrace"
	    ssh quad01 "cd $DIR/$DIRSCF;mpiexec -ppn $NUCLEOS -n $nPROC -env I_MPI_DEVICE rdssm  $ab_exec_quad < $CASO.files >&log" 
	    Line
	    printf  "\tClosing the ring\n"
	    mpdallexit
	fi             
       ####################
            TIMEENDSCF=`date`
            TIME1s=`date --date="$TIMESTARTSCF" +%s`
            TIME2s=`date --date="$TIMEENDSCF" +%s`
	    Line
            printf "\tSCF ended in parallel at time: "
            printf " ${GREEN}$TIMEENDSCF ${NC}\n"
            ELTIMEs=$[ $TIME2s - $TIME1s ]
            TMINs=$(echo "scale=9; $ELTIMEs/60" | bc)
            TMIN1s=$(echo "scale=9; $ELTIMEs/3600" | bc)
            printf "\tTotal time for SCF:   $TMINs min.\n"
            printf "\tTotal time for SCF:   $TMIN1s Hrs.\n"
	    Line
	    printf "\tLet me check ... begin to check WF SCF\n"  
	    if [ -e "$DIRSCF/$WFSCFLOCAL" ];then
		printf "\t${BLUE}$DIRSCF${NC}/$WFSCFLOCAL  "
		printf "............... [${GREEN}ok${NC},exist] \n"
		
		if [ -e "$DIRSCF/log" ];then
		    WARRNINGS=`awk '/Delivered/ { print }' $PWD/$DIRSCF/log`
		    CALCULATION=`awk '/Calculation/ { print }' $PWD/$DIRSCF/log`
		    if [ -z "$CALCULATION" ];then
			printf "\t ${RED}BUT IS WRONG ${NC}\n"
			printf "\tIt seems that your scf ABINIT did not run properly\n"
			printf "\tlook for error messages in:\n "
			printf "\t$DIRSCF/log \n"
			StopMe
		    else
			printf "\t${BLUE}$DIRSCF${NC}/log "
			printf ".................[${GREEN}ok${NC},exist] \n"
			printf "\t${BLUE}$DIRSCF${NC}/$WFSCFLOCAL "
			printf "....... [${GREEN}ok${NC},seems]\n"
			printf "\t$WARRNINGS \n"
			printf "\t${GREEN}$CALCULATION${NC}\n"
#			printf "\t${CYAN}=================================${NC}\n"
		    fi
		else
		    printf "\t${BLUE}$DIRSCF${NC}/log "
		    printf ".................[${RED}NO${NC},exist] \n"
		    printf "\tParanoia your abinit doesnt run parallel well\n"
		    StopMe
		    
		fi
	    fi
	    



   
fi ## CORRE

