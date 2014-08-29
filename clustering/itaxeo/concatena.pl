#!/usr/bin/perl -w
use strict;
use Cwd;     # provides getcwd
use File::Basename;
use File::Copy;

my $debug = 0;
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
#print "Got command-line arguments \n" if $debug;
##################################################################
##################################################################
################ Get current path and directory ##################
##################################################################
my $directory = getcwd;              # gets current path
my ($basePath, $case) = breakup_dirname($directory);
print "\n" if $debug;
print "Determined directory to be: $directory \n" if $debug;
print "Determined case to be: $case \n" if $debug;
print "Determined basepath to be: $basePath \n" if $debug;
print "\n" if $debug;
##################################################################
########################################################################
######################## make JOBS directory ###########################
########################################################################
my $jobdir = $directory.'/JOBS';
if (-d $jobdir) {
   print "Found directory $jobdir \n";
} else {
   print "\tMaking directory $jobdir \.\.\. ";
   mkdir("$jobdir", 0777) || die "cannot mkdir $jobdir: $!";
   print "  MADE \n";
}
##################################################################
############# count number of lines in kpoint file ###############
##################################################################
my $numlines = linecount($klist_file);
print "Found $numlines lines in $klist_file.\n" if $debug;
##################################################################
##################################################################
############## make script to combine output files ###############
##################################################################
my $outfile = $jobdir.'/'."concatenatefiles.pl";
open( PL, ">$outfile") or
    die "Could not open file $outfile :$!";

print PL <<ENDOFFILE;
\#\!/usr/bin/perl
## gets arguments
\$em=\$ARGV[0];
\$pmn=\$ARGV[1];
\$rhoccp=\$ARGV[2];
\$lpmn=\$ARGV[3];
\$lpmm=\$ARGV[4];
\$sccp=\$ARGV[5];
\$lsccp=\$ARGV[6];
\$vnlkss=\$ARGV[7];
\$count = 0;
\@arr = (1 .. $N);
\@arrl = (1 .. $Nlayer);
\$Nk = $numlines;
##
########### Energy
if (\$em eq "true" )
{
   print "\\tEnergy Files\\n";
   \$outfile = "$directory\/energy.d";
   open (OUTF, ">\$outfile");
   foreach \$i (\@arr) {
   \$infile = "$directory\/$case\_\$i/energy.d";
   open( FL, "<\$infile");
   \@filearr = <FL>;
   close(FL);
   foreach (\@filearr) {
     \@words = split;
     \$count = \$count + 1;
     \$number = \$count/100;
     if (\$number == int(\$number)) {
        print "\$count", "\\n";
     }
     \$words[0] = \$count;
     \$_ = join(" ", \@words);
     print OUTF \$_, "\\n";
     }
   }
   close(OUTF);
}
####### pmn
if (\$pmn eq "true") 
{
      print "\tPmn Files\\n";
## Loop for pmn
   foreach \$mn ("pmn.d") 
   {
      \$outfile = "$directory/\$mn";
      open( OUTF, ">\$outfile") or die;
      foreach \$i (\@arr) 
      {
        \$infile = "$directory\/$case\_\$i/\$mn";
        open( FL, "<\$infile") or die;
        while (<FL>) 
        {
           print OUTF \$_;
        }
        close(FL);
      }
   }
   close( OUTF );
}
##### pnn (calculated along with pmn)
if (\$pmn eq "true") 
{
      print "\tPnn Files\\n";
## Loop for pnn
   foreach \$mn ("pnn.d") 
   {
      \$outfile = "$directory/\$mn";
      open( OUTF, ">\$outfile") or die;
      foreach \$i (\@arr) 
      {
        \$infile = "$directory\/$case\_\$i/\$mn";
        open( FL, "<\$infile") or die;
        while (<FL>) 
        {
           print OUTF \$_;
        }
        close(FL);
      }
   }
   close( OUTF );
}
####### vnl
if (\$vnlkss eq "true") 
{
      print "\tvnlmn Files\\n";
## Loop for vnl
   foreach \$mn ("vnl.d") 
   {
      \$outfile = "$directory/\$mn";
      open( OUTF, ">\$outfile") or die;
      foreach \$i (\@arr) 
      {
        \$infile = "$directory\/$case\_\$i/\$mn";
        open( FL, "<\$infile") or die;
        while (<FL>) 
        {
           print OUTF \$_;
        }
        close(FL);
      }
   }
   close( OUTF );
}
###########################spinccp
if (\$sccp eq "true") 
{
      print "\tSccp Files\\n";
## Loop for spinmn
   foreach \$mn ("spinmn.d") 
   {
      \$outfile = "$directory/\$mn";
      open( OUTF, ">\$outfile") or die;
      foreach \$i (\@arr) 
      {
         \$infile = "$directory\/$case\_\$i/\$mn";
         open( FL, "<\$infile") or die;
         while (<FL>) 
         {
             print OUTF \$_;
         }
         close(FL);
      }
   } 
   close( OUTF );
##
}
############################csmnd
if (\$lsccp eq "true") 
{
      print "\tCalSccp Files\\n";
## Loop for csmnd
   foreach \$L (\@arrl) 
   {
      foreach \$mn ("csmnd_") 
      {
	 \$outfile = "$directory/\$mn\$Nk\\_\$L";
	 open( OUTF, ">\$outfile") or die;
	 foreach \$i (\@arr) 
         {
	    \$infile = "$directory\/$case\_\$i/\$mn\$L";
	    open( FL, "<\$infile") or die;
	    while (<FL>) 
            {
	       print OUTF \$_;
	    }
	    close(FL);
	 }
      }
   }
   close( OUTF );   
}
############################rhoccp
if (\$rhoccp eq "true") 
{
      print "\trhoccp Files\\n";
## Loop for layer
   foreach \$L (\@arrl) 
   {
      foreach \$mn ("rhoccpd_") 
      {
	 \$outfile = "$directory/\$mn\$Nk\\_\$L";
	 open( OUTF, ">\$outfile") or die;
	 foreach \$i (\@arr) 
         {
	    \$infile = "$directory\/$case\_\$i/\$mn\$L";
	    open( FL, "<\$infile") or die;
	    while (<FL>) 
            {
	       print OUTF \$_;
	    }
	    close(FL);
	 }
      }
   }
   close( OUTF );
}
############################cpmn
if (\$lpmn eq "true") 
{
      print "\tCalPmn Files\\n";
## Loop for layer
   foreach \$L (\@arrl) 
   {
      foreach \$mn ("cpmnd_") 
      {
	 \$outfile = "$directory/\$mn\$Nk\\_\$L";
	 open( OUTF, ">\$outfile") or die;
	 foreach \$i (\@arr) 
         {
	    \$infile = "$directory\/$case\_\$i/\$mn\$L";
	    open( FL, "<\$infile") or die;
	    while (<FL>) 
            {
	       print OUTF \$_;
	    }
	    close(FL);
	 }
      }
   }
   close( OUTF );
}
############################cfmn
if (\$lpmn eq "true") 
{
      print "\tCalFmn Files\\n";
## Loop for layer
   foreach \$L (\@arrl) 
   {
      foreach \$mn ("cfmnd_") 
      {
   \$outfile = "$directory/\$mn\$Nk\\_\$L";
   open( OUTF, ">\$outfile") or die;
   foreach \$i (\@arr) 
         {
      \$infile = "$directory\/$case\_\$i/\$mn\$L";
      open( FL, "<\$infile") or die;
      while (<FL>) 
            {
         print OUTF \$_;
      }
      close(FL);
   }
      }
   }
   close( OUTF );
}
############################cpmm
if (\$lpmm eq "true") 
{
      print "\tCalPmm Files\\n";
## Loop for layer
   foreach \$L (\@arrl) 
   {
      foreach \$mn ("cpmmd_") 
      {
	 \$outfile = "$directory/\$mn\$Nk\\_\$L";
	 open( OUTF, ">\$outfile") or die;
	 foreach \$i (\@arr) 
         {
	    \$infile = "$directory\/$case\_\$i/\$mn\$L";
	    open( FL, "<\$infile") or die;
	    while (<FL>) 
            {
	       print OUTF \$_;
	    }
	    close(FL);
	 }
      }
   }
   close( OUTF );
}
############################jnmlk
if ( \$opt1 == 2 )
{
   if ( \$opt2 == 10 )
   {
      print "concatenating jnnlk for option: \$opt1","\\n";
   }
   else
   {
      print "concatenating jnnlk for option: \$opt1 \$opt2","\\n";
   }
   ## Loop for layer
   foreach \$L (\@arrl) 
   {
      foreach \$mn ("jnnlk_") 
      {
	 \$outfile = "$directory/\$mn\$Nk\\_\$L";
	 open( OUTF, ">\$outfile") or die;
	 foreach \$i (\@arr) 
         {
	    \$infile = "$directory\/$case\_\$i/\$mn\$L";
	    open( FL, "<\$infile") or die;
	    while (<FL>) 
            {
	       print OUTF \$_;
	    }
	    close(FL);
	 }
      }
   }
   close( OUTF );
}

############################cSmnd
if (
      (
         ( \$opt1 == 1 )|| ( \$opt1 == 4 )
      ) && ( \$spin == 2 )
   )
{
   if ( \$opt2 == 10 )
   {
      print "concatenating  cSmmd for option: \$opt1","\\n";
   }
   else
   {
      print "concatenating  cSmmd for option: \$opt1 \$opt2","\\n";
   }
   foreach \$L (\@arrl) 
   {
      foreach \$mn ("cSmmd_") 
      {
       \$outfile = "$directory/\$mn\$Nk\\_\$L";
	 open( OUTF, ">\$outfile") or die;
	 foreach \$i (\@arr) 
         {
	    \$infile = "$directory\/$case\_\$i/\$mn\$L";
	    open( FL, "<\$infile") or die;
	    while (<FL>) 
            {
	       print OUTF \$_;
	    }
	    close(FL);
	 }
      }
   }
   close( OUTF );
}
############################
############################


ENDOFFILE
close(PL);
chmod( 0754, $outfile) or
    die "Could not chmod $outfile :$!";
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
###############
sub ARGVerror {
###############
    print "Requires three arguments.\n";
    print "Argument 1: the klist file\n";
    print "Argument 2: the number of CPUs you want ".
          "klist split into\n";
    print "Argument 3: the number of Layers\n";
    die;
}
###############
###############
sub linecount {
###############
    my $filename = $_[0];
    my $cnt;
    open(FH, "<$filename") or 
        die "Couldn't open $filename: $!";
    $cnt++ while <FH>;
    close FH;
    return $cnt;
}
###############
