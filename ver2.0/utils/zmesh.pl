#!/usr/bin/perl -w


########################################################################
####### Checking command line arguments ################################
########################################################################

$vac_div = $ARGV[0];   #vacuum divisions
$int_div = $ARGV[1];   #intra divisions
$cual = "zmesh.pl";
chop ( $case = qx /  echo \$PWD \| awk -F \/ '{print\$NF} '  /  );   #directory at which the user is working

if ( @ARGV != 2 )
{
  system (" clear ");
  print "\e[0;31m************************************************************************\e[00m \n";
  print " Usage: \n";
  print $cual,"[\e[0;34m vacuum divisions \e[00m] [\e[0;34m intratomic divisions between consecutive atoms\e[00m] \n";
  print "\e[0;31m************************************************************************\e[00m \n";
  exit();
}
if ( ! -e ".acell.d" )
{
    print "\e[0;31m************************************************************************\e[00m \n";
    print "\e[0;34m  .acell.d \e[00m does NOT exist, create one with \n";
    print "\e[0;31m  \e[0;34mrlayer.sh\e[00m \n";
    print "\e[0;31m************************************************************************\e[00m \n";
    exit {};
}



########################################################################
#### Checking that Lslab is the same ##################################
########################################################################


chop ( $sLslab = qx / grep acell setUpAbinit_$case.in | awk '{print \$4}' / );
chop ( $aLslab = qx / awk '{print \$3}' .acell.d  / );

if ( $sLslab != $aLslab )
{
    print "\e[0;31m************************************************************************\e[00m \n";
    print " setUpAbinit_",$case,".sh vacuum=","\e[0;34m",$sLslab;
    print "\e[00m and .acell.d vacuum= \e[0;34m",$aLslab,"\e[00m are different \n";
    print "\e[0;31m check \e[00m setUpAbinit_",$case,".sh \n";
    print "\e[0;31m run \e[00m  /home/bms/ras/layer/ab_input/rlayer.sh with vacuum=\e[0;34m",$sLslab,"\e[00m \n";
    print " and check if the SCF needs to be run \n";
    print "\e[0;31m************************************************************************\e[00m \n";
    exit();
}



########################################################################
#### Making mesh file ############### ##################################
########################################################################
##
#qx / sort -nur +2 $case.xyz | awk '{print \$3}' > mesh /;  #making mesh file
#Comment the rest in order to uncomment the last line:

qx/ awk '{print \$3}' $case.xyz> tmp  /; 
open( FH, "<tmp") or    
    die "Cannot open tmp: $!";
@tmp = <FH>;
close(FH);

system (" touch tmp2 ");
open( FH, ">tmp2") or 
      die "Cannot open tmp2: $!";
foreach $itmp ( @tmp )
{
    print FH $itmp + 0, "\n" ;
}
close(FH);

system(" sort -nur tmp2 > mesh ");
##

open( FH, "<mesh") or    
    die "Cannot open mesh: $!";
@z_array = <FH>;
close(FH);

$nz = @z_array;

print "\e[1;36m%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\e[00m \n";
print " running Ncoord=\e[0;34m",$nz,"\e[00m Lslab=\e[0;34m",$sLslab,"\e[00m vac-div=\e[0;34m";
print $vac_div,"\e[00m intra-div=\e[0;34m",$int_div,"\e[00m \n";
print "with \e[0;31m",$vac_div + ( $int_div * ($nz-1) ) + 1 , "\e[00m divisions \n";
print "\e[1;36m%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\e[00m \n";

########################################################################
#### Building zmesh.d ###################################################
########################################################################
$vac_size = $sLslab - ( $z_array[0]-$z_array[$nz-1] );   #vacuum size
$vac_spacing = $vac_size / $vac_div;

open(FILE, ">zmesh.d")or
    die "could not open file zmesh.d : $!";
print FILE $vac_div + ( $int_div * ($nz-1) ) + 1 . "\n";

foreach( 0 .. $vac_div-1 )
{
    print FILE ( $vac_size + $z_array[0] ) - ( $_ * $vac_spacing )."\n";
}
 
foreach $j ( 0 .. $nz-2 )

{

    $int_spacing = ( $z_array[$j] - $z_array[ $j + 1 ] ) / $int_div ;
#    print $int_spacing,"\n";
#    print 'int_div  ',$int_div, '$z_array[$j] ',$z_array[$j],' $z_array[ $j + 1 ] ', $z_array[ $j + 1 ] ,"\n" ;

    foreach $i ( 0 .. $int_div -1 )
    {
	print FILE ( $z_array[ $j ] ) - ( $i * $int_spacing )."\n";
    }
}
print FILE $z_array[$nz-1];

close(FILE);


system(" rm -f mesh ");
system(" rm -f tmp ");
system(" rm -f tmp2 ");
