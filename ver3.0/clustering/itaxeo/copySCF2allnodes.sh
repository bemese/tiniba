#!/bin/bash
##2 Julio at 23:54 hrs.
##FUNCTION:
##copy SCF to dir in .machines_pmn 
##CHILDREN:
##ineedsplitWFSCF.sh
RED='\e[0;31m'
BLUE='\e[0;34m'
BLU='\e[1;34m'
CYAN='\e[0;36m'
GREEN='\e[0;32m'
GRE='\e[1;32m'
YELLOW='\e[1;33m'
NC='\e[0m' # No Color
## global
ontoy=`hostname`
# reads TINIBA version from version-tiniba.txt
source version-tiniba.txt
#
##=========FUNCTIONS===============
function Line {
    printf "\t${BLUE}=============================${NC}\n"
}
function StopMe {
    printf "\t${RED}Stoping right now... ${NC}\n"
    exit 127
}
function CheckHost {
    ping -c 1 $1 >& /dev/null;
}
##================================= 
declare -a MACHINESpmn
declare -a WHEREWORKREMOTE
declare -a WHEREWORKLOCAL

DIR=$PWD
USER=$USER
BASEDIR=`dirname $PWD`
CASO=`basename $PWD`
PARENT=`basename $BASEDIR`
WORKZPACE="workspace" 
WFSCFLOCAL=$CASO'o_DS1_WFK'
WFSCFREMOTE=$CASO'i_DS1_WFK'
DIRSCF=$CASO'_scf'
INTENTOS=4
INEEDSPLIT=0
FILE2COPY=$1
ANFITRION=`hostname`
WHERE="$HOME/tiniba/$ver/clustering/itaxeo"

rm -f killme 
if [ ! -e "$WHERE/ineedsplitWFSCF.sh" ];then
    printf "\tWhere is your FILE: ineedsplitWFSCF.sh \n"
    printf "\tthe path actual is: "
    printf "$WHERE\n"
    touch -f killme
    StopMe
fi 

##===CHECK IF .machines file exist==================== !!
if [ ! -e .machines_pmn ]
then
    printf "\t ${RED}There is not .machines_pmn${NC}\n"
    touch -f killme
    StopMe
else
    MACHINESpmn=(`cat .machines_pmn`)
    NOMACHINESpmn=`echo ${#MACHINESpmn[@]}`
fi
##################################################
####========WHERE WORK ===========================
##################################################
for ((hh=0;hh<=($NOMACHINESpmn-1); hh++));do
    let "kk=hh+1"
    CAZO=$CASO"_"$kk
    if [ "$ontoy" == "medusa" ]
    then
	WHEREWORKREMOTE[$hh]="data/$USER/$WORKZPACE/$PARENT/$CAZO"
    else
	WHEREWORKREMOTE[$hh]="/data/$USER/$WORKZPACE/$PARENT/$CAZO"
    fi
    WHEREWORKLOCAL[$hh]="$PWD/$CAZO"
done
##################################################
####========REMOTE DIRECTORIOS====================
##################################################
printf "\t${BLUE}========================${NC}\n"
printf "\t${CYAN}Checking${NC} REMOTE directories \n"
for ((hh=0;hh<=($NOMACHINESpmn-1); hh++));do
#
    REMOTESERVER=${MACHINESpmn[$hh]}
    DIRQ=${WHEREWORKREMOTE[$hh]}
    BASE=`dirname $DIRQ`
    DIRE=`basename $DIRQ`
    printf "\t[$REMOTESERVER]:$BASE/${BLUE}$DIRE${NC}"          
    EXISTE=`ssh $REMOTESERVER 'test -d '$DIRQ'; echo $?'`
    if [ $EXISTE -eq 0 ] ;then
        printf " [${GREEN}exist${NC}]\n"
    else
        printf " [${GREEN}making${NC}]\n"
        ssh $REMOTESERVER "mkdir -p $DIRQ"
    fi
    sleep .1
done
printf "\t${BLUE}========================${NC}\n"
##################################################
####======== CHEKING SCF  ========================
##################################################
##===CHECK IF SCF DIR EXIST down======================
if [ -d "$DIRSCF" ];then
    printf "\t${BLUE}$DIRSCF${NC}  "
    printf "...................[${GREEN}ok${NC},exist] \n"
    if [ -e "$DIRSCF/$WFSCFLOCAL" ];then
	printf "\t${BLUE}$DIRSCF${NC}/$WFSCFLOCAL  "
	printf ".................[${GREEN}ok${NC},exist] \n"
	if [ -e "$DIRSCF/log" ];then
	    printf "\tBegin to check WF SCF...\n"
            WARRNINGS=`awk '/Delivered/ { print }' $PWD/$DIRSCF/log`
            CALCULATION=`awk '/Calculation/ { print }' $PWD/$DIRSCF/log`
            if [ -z "$CALCULATION" ];then
		printf "\t           ${RED}BUT IS WRONG ${NC}\n"
		printf "\tIt seems that your scf ABINIT did not run properly\n"
		printf "\tlook for error messages in:\n "
		printf "\t$PWD/${BLUE}$DIRSCF${NC}/log \n"
           #printf "\t ${RED}Stoping right now ... ${NC}\n"
		touch killme
		exit 1
            else
		printf "\t${BLUE}$DIRSCF${NC}/log "
		printf ".................[${GREEN}ok${NC},exist] \n"
		printf "\t$WARRNINGS \n"
		printf "\t$CALCULATION\n"
		printf "\t${BLUE}$DIRSCF${NC}/$WFSCFLOCAL "
		printf "....... [${GREEN}ok${NC}]\n"
            fi #end CALCULATION
        else #log
            printf "\t$DIRSCF/log doesnt exist \n"
            printf "\t${RED}Stoping right now ... ${NC}\n"
            touch -f killme
        #read -p "Ctrl C to Kill me ..."
            exit 1
        fi # end log
    else # "$DIRSCF/$WFSCFLOCAL"
        printf "\t${BLUE}$DIRSCF${NC}/$WFSCFLOCAL doesnt exist...\n"
        printf "\t CAUSE: ABINIT didn't run SCF...  "
        #printf "\t${RED}Stoping right now ... ${NC}\n"
        #read -p "Ctrl C to Kill me ..."
        touch -f killme
        exit 1
    fi # "$DIRSCF/$WFSCFLOCAL"
else
    printf "\t ${RED}There isn't DIRECTORY ${NC}${BLUE}$DIRSCF${NC} in :\n"
    printf "\t $DIR/ \n"
    printf "\t CAUSE: ABINIT didn't run SCF...  "
    printf "${RED}Stoping right now ... ${NC}\n"
    touch -f killme
   #read -p "Ctrl C to Kill me ..."
    exit 1
fi
##===CHECK IF SCF DIR EXIST up======================
##################################################
####========COPYING FILE ========================
##################################################
TIMER1=`date`
rm -f killme
rm -f tmp
Line
printf "\t${CYAN}Copying started at: $TIMER1 ${NC}\n"    
Line
TMP=`md5sum "$PWD/$DIRSCF/$WFSCFLOCAL"`
echo $TMP>tmp
MD5LOCAL=`awk '{print $1}' tmp`
rm -f tmp  
##===============================================
for ((hh=0;hh<=($NOMACHINESpmn-1); hh++));do
    let "HHSU=$hh+1"
    REMOTESERVER=${MACHINESpmn[$hh]}
    ADONDECOPY=${WHEREWORKREMOTE[$hh]}
# bms: if we are in the same node, copy from the node insted of copying from the original source
    let "ant=$hh-1"
    if [ "$ant" == "-1" ] 
    then
	anterior="void"
	backcopy="void"
    else
	anterior=${MACHINESpmn[$ant]}
	backcopy=${WHEREWORKREMOTE[$ant]}
    fi
##
###---------------------------
#  
    if [[ "$REMOTESERVER" == "itanium"* ]]; then
        MAQUINA501=$REMOTESERVER
        MAQUINA500=$anterior
        SWITCHNAME="Using ethernet"
    fi
    #
    if [[ "$REMOTESERVER" == "node"* ]]; then
        if [ $ANFITRION == "medusa" ];then # perhaps this line must be changed
            MAQUINA501=$REMOTESERVER"m"
            MAQUINA500=$anterior"m"
            SWITCHNAME="Using myrinet"
        else
            MAQUINA501=$REMOTESERVER
            MAQUINA500=$anterior
            SWITCHNAME="Not using myrinet"
        fi              
    fi
    #
    if [[ "$REMOTESERVER" == "quad"* ]]; then
        
        if [ $ANFITRION == "quad01" ];then
            MAQUINA501=$REMOTESERVER"ib"
            MAQUINA500=$anterior"ib"
            SWITCHNAME="Using infiniband"
		if [ -e $TINIBAC/.badQUADS ]; then
		aux=`grep quad $TINIBAC/.badQUADS`
		if [ -n "$aux" ]; then
		for damaged_quad in `cat $TINIBAC/.badQUADS`; do
			damaged_quad=$damaged_quad"ib"
	    		if [ $MAQUINA501 == "$damaged_quad" ];then
				#echo
				MAQUINA501=$REMOTESERVER
				#echo MAQUINA501=$MAQUINA501
				MAQUINA500=$anterior
				#echo MAQUINA500=$MAQUINA500
				SWITCHNAME="Using TCPIP. Infiniband connection damaged for $MAQUINA501."
			fi
		done
		fi
		fi

        else
            MAQUINA501=$REMOTESERVER
            MAQUINA500=$anterior
            SWITCHNAME="Not using infiniband"
        fi        
    fi 
    #
    if [[ "$REMOTESERVER" == "hexa"* ]]; then
        if [[ $ANFITRION == "hexa"* ]];then
            MAQUINA501=$REMOTESERVER"ib"
            MAQUINA500=$anterior"ib"
            SWITCHNAME="Using infiniband"
#	    exit 1
        else
            MAQUINA501=$REMOTESERVER
            MAQUINA500=$anterior
            SWITCHNAME="Not using infiniband"
        fi        
#	printf "\taqui: $MAQUINA501 $MAQUINA500\n"
    fi 
    ##---------------------------
    SALIDAeq=1
    SALIDAneq=1
    until [ "$SALIDAneq" -eq "$INTENTOS" ] || [ $SALIDAeq -eq 2 ];do
	if [ "$ontoy" == "medusa" ]
	then
	    EXISTE=`ssh $REMOTESERVER 'test -e '/$ADONDECOPY/$WFSCFREMOTE'; echo $?'`
	else
	    EXISTE=`ssh $REMOTESERVER 'test -e '$ADONDECOPY/$WFSCFREMOTE'; echo $?'`
	fi
	sleep .1
	if [ $EXISTE -eq 0 ] ;then ##existe
	    rm -f tmp1
	    if [ "$ontoy" == "medusa" ]
	    then
		TMP1=`ssh $REMOTESERVER 'md5sum '/$ADONDECOPY/$WFSCFREMOTE''`
	    else
		TMP1=`ssh $REMOTESERVER 'md5sum '$ADONDECOPY/$WFSCFREMOTE''`
	    fi
	    echo $TMP1>>tmp1
	    MD5REMOTE=`awk '{print $1}' tmp1`
	    rm -f tmp1  
	fi 
	if [ $EXISTE -ne 0 ] ;then ##no existe      
	    if [ "$INEEDSPLIT" -eq 0 ];then
		$WHERE/ineedsplitWFSCF.sh
		let "INEEDSPLIT+=1"
		printf "\tSplitting and copying $DIRSCF/$WFSCFLOCAL\n"
		printf "\tcould take a while...!!\n"
            fi 
# bms: if we are in the same node, copy from the node insted of copying from the original source
	    flaga=yes
	    if [ "$MAQUINA501" == "$MAQUINA500" ]
		then
		Line
		printf "\t$MAQUINA500 same as $MAQUINA501 copy from previous case\n"
		if [ "$ontoy" == "medusa" ]
		then
		    scp /$MAQUINA500.$backcopy/$WFSCFREMOTE /$MAQUINA501.$ADONDECOPY/
		else
		    scp $MAQUINA500:$backcopy/$WFSCFREMOTE $MAQUINA501:$ADONDECOPY/
		fi
		SWITCHNAME="copying from the same disk"
	    fi
	    if [ "$MAQUINA501" != "$MAQUINA500" ]
	    then
		echo $MAQUINA501 > hoy
		maq501base=`grep -oE "[[:lower:]]{1,}" hoy`
		echo $MAQUINA500 > hoy
		maq500base=`grep -oE "[[:lower:]]{1,}" hoy`
		rm hoy
		Line
		printf "\t$MAQUINA500 not equal to $MAQUINA501\n"
		if [ "$ontoy" == "medusa" ]
		then
		    if [ "$maq501base" == "$maq500base" ]
		    then
			flaga=no
			if [ "$maq500base" == "quad" ]
			then
			    maquinas=$MAQUINA501"ib"
			    csw=infiniband
			fi
			if [ "$maq500base" == "node" ]
			then
			    maquinas=$MAQUINA501"m"
			    csw=myrinet
			fi
			#
			printf "\tsame architecture though => using ${GREEN}$csw${NC}\n"
#			printf "\tssh $MAQUINA500;scp /$backcopy/$WFSCFREMOTE $maquinas:/$ADONDECOPY/\n"
			ssh $MAQUINA500 "scp /$backcopy/$WFSCFREMOTE $maquinas:/$ADONDECOPY/"
		    else
			scp $DIRSCF/$WFSCFLOCAL.block* /$MAQUINA501.$ADONDECOPY/
			ssh $MAQUINA501 "cd /$ADONDECOPY; cat $WFSCFLOCAL.block* > $WFSCFREMOTE; rm -f $WFSCFLOCAL.block*"

		    fi
		else
		    printf "\tcopying from source\n"
		    if [ "$ontoy" == "medusa" ]
		    then
			scp $DIRSCF/$WFSCFLOCAL.block* /$MAQUINA501.$ADONDECOPY/
			ssh $MAQUINA501 "cd /$ADONDECOPY; cat $WFSCFLOCAL.block* > $WFSCFREMOTE; rm -f $WFSCFLOCAL.block*"
		    else
			scp $DIRSCF/$WFSCFLOCAL.block* $MAQUINA501:$ADONDECOPY/
			ssh $MAQUINA501 "cd $ADONDECOPY; cat $WFSCFLOCAL.block* > $WFSCFREMOTE; rm -f $WFSCFLOCAL.block*"
		    fi
		fi
	    fi
	    printf "\t[${GREEN}copying${NC}] Attempt $SALIDAneq\n"
	    if [ "$flaga" == "yes" ]
	    then
		printf "\t${GREEN}$SWITCHNAME${NC}\n"
	    fi
      #-------------
	    rm -f tmp1
	    if [ "$ontoy" == "medusa" ]
	    then
#		printf "\tssh $REMOTESERVER 'md5sum '/$ADONDECOPY/$WFSCFREMOTE''\n"
		TMP1=`ssh $REMOTESERVER 'md5sum '/$ADONDECOPY/$WFSCFREMOTE''`
	    else
		TMP1=`ssh $REMOTESERVER 'md5sum '$ADONDECOPY/$WFSCFREMOTE''`
	    fi
	    echo $TMP1>>tmp1
	    MD5REMOTE=`awk '{print $1}' tmp1`
	    rm -f tmp1  
      #-------------
	fi  ##no existe 
##################
	if [ "$MD5REMOTE" == "$MD5LOCAL" ];then
	    SALIDAeq=2
	    if [ "$ontoy" == "medusa" ]
	    then
		printf "\t/$MAQUINA501.$ADONDECOPY/$WFSCFREMOTE\n"
	    else
		printf "\t$MAQUINA501:$ADONDECOPY/$WFSCFREMOTE\n"
	    fi
	    printf "\t[${GREEN}identical${NC}]\n"
	    
	else
      ## I need to copy again but first erase
	    if [ "$ontoy" == "medusa" ]
	    then
		printf "\t/$MAQUINA501.$ADONDECOPY/$WFSCFREMOTE\n"
	    else
		printf "\t$MAQUINA501:$ADONDECOPY/$WFSCFREMOTE\n"
	    fi
	    printf "\t[${RED}Not Identical${NC}] $SALIDAneq\n" 
	    let "INTEN=INTENTOS-2"
            if [ $SALIDAneq -eq $INTEN ];then
		MAQUINA501=$REMOTESERVER
		Line
		printf "\tAt attempt $INTEN we copy through ethernet $MAQUINA501\n"
		SWITCHNAME="Not using infiniband"
		Line
	    fi
	    let "INTE=INTENTOS-1"
            if [ $SALIDAneq -eq $INTE ];then
		printf "\t-----${RED}HOLD ON !!!!!${NC}-------\n"
		printf "after $SALIDAneq ATTEMPT "
		printf "Im not able to get the same copy, "
		printf "try to copy by hand:\n"
		printf "$PWD/$DIRSCF/$WFSCFLOCAL \n"
		printf "$MAQUINA502: $ADONDECOPY\n"
		printf "\t ${RED}Stoping right now ... ${NC}\n"
#              read -p "any key to continue or Ctrl C to Kill me"
              # touch -f killme
              # exit 1
            fi
	    let "SALIDAneq+=1"
	    ssh $MAQUINA501 "cd $ADONDECOPY;rm -f $WFSCFREMOTE;rm -f $WFSCFLOCAL.block00;rm -f $WFSCFLOCAL.block01;rm -f $WFSCFLOCAL.block02;rm -f $WFSCFLOCAL.block03"  
	fi 
    done ##until  
    sleep .1
done ## for
  ##--------------------------
TIMER2=`date`
Line
printf "\t${CYAN}Copying ended at: $TIMER2 ${NC}\n"
Line
TIME1=`date --date="$TIMER1" +%s`
TIME2=`date --date="$TIMER2" +%s`
ELTIME=$[ $TIME2 - $TIME1 ]
TMIN=$(echo "scale=9; $ELTIME/60" | bc)
TMIN1=$(echo "scale=9; $ELTIME/3600" | bc)
printf "\ttotal time:   $TMIN min. \n"
printf "\t              $TMIN1 Hrs. \n"
Line
rm -rf $PWD/$DIRSCF/$WFSCFLOCAL.block* #erase all children Local   
##StopMe
##nothing under here
