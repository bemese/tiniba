# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
	. ~/.bashrc
fi

# User specific environment and startup programs


# PATH=$PATH:$HOME/bin
PATH=$PATH:$HOME/bin:/bin:/sbin:/etc:/usr/bin:/usr/sbin\
/usr/local/bin:/usr/local/sbin:/usr/local/share:\
/usr/local/lib:/usr/lib:/usr/share:\
/usr/X11R6/bin:/usr/X11R6/lib:\
/usr/local/mpich-gm-lahey/bin:/usr/local/mpich-gm-lahey/sbin:\
/opt/intel_cc_80/bin:.
#/opt/intel/fce/9.1.036/bin/\
#/opt/intel/cce/9.1.042/bin/:.
#/usr/local/lf9562/lib:/usr/local/lf9562/bin:.

export PATH

LD_LIBRARY_PATH=/usr/local/lf9562/lib:/usr/local/gm/lib:\
/usr/lib:/usr/local/lib:\
/usr/X11R6/lib:/usr/local/mpich-gm-lahey/lib\
/usr/local/SCALAPACK:/usr/local/BLACS/LIB:\
/usr/local:/usr/local/ATLAS/lib:$LD_LIBRARY_PATH

#/usr/local/lf9562/lib:/usr/local/imsl/lib/lf95:/usr/local/gm/lib:. 

export LD_LIBRARY_PATH

# IMSL-MP90
#. /usr/local/vni/CTT2.1/ctt/bin/cttsetup.sh

# for lf95  lahey 
#source /usr/local/lf9562/bash_setup

#source /opt/intel/fc/9.0/bin/ifortvars.sh

# For abinit DO NOT REMOVE
############################################
export I_MPI_FABRICS=shm:tcp
LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/lib64
############################################

BASH_ENV=$HOME/.bashrc
hel=bms@helios7.physics.utoronto.ca
kom=bms@komodo.physics.utoronto.ca
kom=bms@128.100.75.58             
afr=bms@u211-n75.physics.utoronto.ca
rap=bms@rapunsel.cio.mx           
gil=brenda@gibraltar.physics.utoronto.ca
USERNAME="bms"
go(){
cd $*
PS1="${PWD} yes Dr.> "
}
#alias cd="go"
PSTILL_PATH="/usr/local/pstill_dist"
export USERNAME BASH_ENV PATH PS1 PSTILL_PATH
MOZILLA_HOME=/usr/local/netscape
export MOZILLA_HOME
