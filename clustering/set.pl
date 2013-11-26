#!/usr/bin/perl -w
##########################################################################

##########################################################################
# Definition of variables ################################################
##########################################################################
use strict;
use Cwd;     # provides getcwd
use File::Basename;
use File::Copy;

my $debug = 1;

my $string = "";
my $state = 0; #Initial state
my $ppfiles = "";
my $common_vars = "";
my $scf_vars = "";
my $optic_vars = "";
my $xcart;
my ($start, $end);
my $i;
my $coor_system = "";
##################################################################
################ Get current path and directory ##################
##################################################################

my $directory = getcwd;              # gets current path
my ($basePath, $case) = breakup_dirname($directory);



##################################################################
###############  Test command line argument ######################
##################################################################
if (@ARGV!=3) {
    ARGVerror();
}
my $klist_file = $ARGV[0];
my $N = $ARGV[1];
my $Nlayer = $ARGV[2];
my @arrl = (1 .. $Nlayer);
my @arr = (1 .. $N);



##########################################################################
# Getting variables from files ###########################################
##########################################################################


open TEMP, "<setUpAbinit_$case.in" or die "Opening 'setUpAbinit_$case.in': $!\n";
while (<TEMP>)
{
    $string = "$_";

    if ( $string =~ /(\s*)\#/  )
    {
	#do nothing it just skips comments
    }
    else
    {
      switch:
	{
	    $state == 0 && do
	    {
		if ( $string =~ /(\s*)Section:(\s+)Coordinate(\s+)System/ )
		{
		    $state = 1;
		}
		last switch;
		
	    };
	    $state == 1 && do
	    {
		if ( $string =~ /(\s*)Section:(\s+)Pseudo/ )
		{
		    $state = 2;
		}
		else
		{
		    if ( $string =~ /\w/) #to avoid spaces
		    {
			$coor_system = $coor_system.$string;
		    }
		}
		last switch;
		
	    };

	    $state == 2 && do
	    {
		if ( $string =~ /(\s*)Section:(\s+)Common/ )
		{
		    $state = 3;
		}
		else
		{
		    if ( $string =~ /\w/) #to avoid spaces
		    {
			$ppfiles = $ppfiles.$string;
		    }
		}
		last switch;
	    };
	    $state == 3 && do
	    {
		if ( $string =~ /(\s*)Section:(\s+)Scf/ )
		{
		    $state = 4;
		}
		else
		{
		    $common_vars = $common_vars.$string;
		}
		last switch;
	    };

	    $state == 4 && do
	    {
		if ( $string =~ /(\s*)Section:(\s+)Optic/ )
		{
		    $state = 5;
		}
		else
		{
		    $scf_vars = $scf_vars.$string;
		}
		last switch;
	    };
	    $state == 5 && do
	    {

		$optic_vars = $optic_vars.$string;
		last switch;
	    };

		
	    print "Unknown input: ", $string,"in run.in";
	
	}
    }
}


close  TEMP;

open TEMP, "<$case.xyz" or die "Opening '$case.xyz': $!\n";
$xcart = join " ",<TEMP>;
close TEMP; 

#print $ppfiles,"Common vars: \n",$common_vars,"scf vars: \n",$scf_vars, "optic vars: \n",$optic_vars;


open(FH, "<$klist_file") or
    die "Cannot open $klist_file: $!";
my @lines = <FH>;
close(FH);

open( FH, "<.machines_pmn") or
    die "Cannot open .machines_pmn: $!";
my @machines = <FH>;
close(FH);

#################################################################
# Gets variables calculated in arrange_machines.pl ##############
#################################################################
my @startpoint2;
my @endpoint2;
my @klist_length;

if ( -e "startpoint.txt" )
{
    open( FH, "<startpoint.txt") or
    die "Cannot open startpoint.txt: $!";
    chop( @startpoint2 = <FH>);
    close(FH);
    chop( @startpoint2 );
}
if ( -e "endpoint.txt")
{
    open( FH, "<endpoint.txt") or
	die "Cannot open endpoint.txt: $!";
    chop( @endpoint2 = <FH>);
    close(FH);
    chop( @endpoint2 );
}
if ( -e "klist_length.txt")
{
    open( FH, "<klist_length.txt") or
	die "Cannot open klist_length.txt: $!";
    chop( @klist_length = <FH>);
    close(FH);
    chop( @klist_length );
}


##################################################################
#############  Make the .files file ##############################
##################################################################

my $files_file = $case.'.files';
open(FILESF,">$files_file");
print FILESF ($case.'.in',  "\n");
print FILESF ($case.'.out', "\n");
print FILESF ($case.'i',    "\n");
print FILESF ($case.'o',    "\n");
print FILESF ($case,        "\n");
print FILESF ($ppfiles,        "\n");
close(FILESF);


#  behaviour indicative of paranoia

if (!(-e $files_file)) {
   print "Cannot find $files_file.  EXITING\n";
   die;
}



##################################################################
############# Builds the variable named "arr_of_lists" ###########
##################################################################

my @arr_of_lists;

if ( ( -e "startpoint.txt") && ( -e "endpoint.txt") && ( -e "klist_length.txt") )
{
    foreach $i (@arr) {
	$start = $startpoint2[$i] - 1;
	$end = $endpoint2[$i] - 1;
	$arr_of_lists[$i] = [ @lines[$start .. $end] ];
    }
}



##################################################################
##################  make new directories #########################
##################################################################
my @dirs_to_make = ( "scf", @arr, "bandstructure" );
foreach (@dirs_to_make) {
   $_  = join("/", ( $directory, $case.'_'.$_) );
   ####################################
   ### note that the array element  ###
   ### being "foreached" is changed ###
   ####################################
   if (-e $_) {
      print "Directory $_ exists.\n";
   } else {
      print "making $_ \n" if $debug;
      mkdir("$_", 0777) || die "cannot mkdir $_: $!";
   }
}
my $scf_dir = $dirs_to_make[0];
##################################################################

##################################################################
################## build files for scf case ######################
##################################################################
# take the common file and the scf fle and concatentate them

my $out_file = $scf_dir."\/$case".'.in';
print "Making $out_file\n" if $debug;
open(OUTFILE, ">$out_file") ||
    die "Could not open $out_file: $!";

print OUTFILE "ndtset 1", "\n";
print OUTFILE $coor_system,"  ",$xcart,"\n";
print OUTFILE $common_vars, "\n";
print OUTFILE $scf_vars;

close(OUTFILE);


##################################################################
############# build files for response calculation ###############
##################################################################
foreach $i (@arr) {

  my $out_file = $dirs_to_make[$i]."\/$case".'.in';
  print "Making $out_file\n" if $debug;
  open(OUTFILE, ">$out_file") or 
      die "Could not open $out_file: $!";
  
  print OUTFILE "ndtset 2", "\n";
  print OUTFILE $coor_system,"  ",$xcart,"\n";
  print OUTFILE $common_vars,"\n";  
  print OUTFILE $optic_vars,"\n";

  if ( ( -e "startpoint.txt") && ( -e "endpoint.txt") && ( -e "klist_length.txt") )  
  {
      print OUTFILE "nkpt2     $klist_length[$i] \n";
      print OUTFILE "istwfk2   $klist_length[$i]\*1\n";
      print OUTFILE "kpt2      ";
  
      for (0 .. $klist_length[$i]-1) {
	  print OUTFILE $arr_of_lists[$i][$_];
      }
  }
  close(OUTFILE);
}
##################################################################


##################################################################
######################## get .files file #########################
##################################################################

# copy .files file to the directory

foreach (@dirs_to_make) {
  my $new_file = $_."\/$case\.files";
  copy ($files_file, $new_file) || 
      die "cannot copy $files_file to $new_file: $!";
}
##################################################################






########################## SUBROUTINES ###########################
#####################
sub breakup_dirname {
#####################
   # break-up input path into a base and present directory 
   $directory = $_[0];
   my @tmpArray = split(/\//,$directory);
   my $case = pop(@tmpArray);
   my $basePath = join("/", @tmpArray);
   return($basePath, $case);
}
#################################################################
