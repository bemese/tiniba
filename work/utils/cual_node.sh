#!/bin/bash
##04 julio 2007 at 16:10 hrs. 
##FUNCTION: 
##finds the name of the running plataform
##PARENTS:
##None
##CHILDREN:
##None
##
RED='\e[0;31m'
BLUE='\e[0;34m'
BLU='\e[1;34m'
CYAN='\e[0;36m'
GREEN='\e[0;32m'
GRE='\e[1;32m'
YELLOW='\e[1;33m'
NC='\e[0m' # No Color
##
host=`hostname`
if [ $host == medusa ]; then
    echo hexa
else
    exec=`$TINIBA/clustering/itaxeo/trunc.sh $host`
    echo $exec
fi
##
