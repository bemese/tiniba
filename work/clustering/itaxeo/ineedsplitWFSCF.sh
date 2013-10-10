#!/bin/bash
##18 JANUARY 2007 at 14:05 hrs.
##FUNCTION:
##JUST SPLIT  the wafefunction file
##PARENT:
##checkWFSCFallnodes.sh
##CHILDREN:
##NONE 
##ADVICE:
##never use alone, this is a child of checkWFSCFallnodes.sh
##if you use alone it will fall !!!
##LAST MODIFICATION: 
##19 FRIDAY 2007 at 09:21
#----SOURCE-------------------------

 DIR=$PWD
 USER=$USER
 BASEDIR=`dirname $PWD`
 CASO=`basename $PWD`
 PARENT=`basename $BASEDIR`
#----------------------------
 DIRSCF=$CASO'_scf'
 WFSCFLOCAL=$CASO'o_DS1_WFK'
 WFSCFREMOTE=$CASO'i_DS1_WFK'
 MEMPWD=$PWD
 NOBLOCKS=4 ## value by default
        
        
         if [ $# -eq 1 ];then
           NOBLOCKS=$1
         else 
           NOBLOCKS=4
         fi

         cd $DIRSCF
         du -b $WFSCFLOCAL>tmp_du
         SIZEFILE=`awk '{print $1}' tmp_du` 
         rm -rf tmp_du
         let "NEWSIZEFILE = $SIZEFILE/($NOBLOCKS*1000)"
           #IS DEFINED 
              if [ -z "$NEWSIZEFILE" ];then
                 printf "\t NEWSIZE IS NOT DEFINED I CANT SPLIT \n"
                 printf "\t stoping right now ..\n"
                 exit 1
              fi
         split -b "$NEWSIZEFILE"k $WFSCFLOCAL -d $WFSCFLOCAL.block
         #ls *block*
         cd $MEMPWD
exit 1
