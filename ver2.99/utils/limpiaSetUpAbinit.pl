#!/usr/bin/perl
 ## Cabellos at 10 de octubre 2007 at 00:15
 ## LAST MODIFICATION :: Febrero 22 2010 a las 17:35 by cabellos jl
 ## function 
 use File::Basename;
 use Cwd;
 use File::Copy;
 use Term::ANSIColor;

  my @BUSCA;
  my $DIR = getcwd;
  my $BASENAME=dirname($DIR);
  my $CASO = basename($DIR);
  my $SETUPABINIT= "setUpAbinit_" . $CASO.".in"; 
  my $SETUPABINITOUT= "setUpAbinit_" . $CASO.".in_clean"; 


$BUSCA[0]="ecut";
$BUSCA[1]="nband2";
$BUSCA[2]="nspinor";
$BUSCA[3]="acell";

       system("rm -f killme");
       ($SCRIPTNAME) = fileparse($0);
     
 
    my @pseudopotentials;
    $FILEMACHINES ='.machines_pmn';
    $INFOABINIT = 'setUpAbinit_GaAs.in';
$INFOABINIT = $SETUPABINIT;
    my $char = '#';
    my $noquiero ='Section';

    if (-e "$SETUPABINIT")
       {
        print "\n"; 
        print "\t $SETUPABINIT      [";
        print color 'green';
        print "ok";
        print color 'reset';
        print "]";
    }
       else 
       {
	   print "\tUsage:  $SCRIPTNAME\n";
           print "\n";
           die "\tStop right now...\n";
          
       }

#######################################33
#######################################
#######################################
  open( SETUP, "> $SETUPABINITOUT");
  print SETUP" ## ********************************************\n";
  print SETUP" ## From: $SETUPABINITOUT\n";
  print SETUP" ## ********************************************\n";
 # print SETUP" ## * Warning:                                 \n";
 # print SETUP" ## * This $SETUPABINITOUT  was generated \n";
 # print SETUP" ##   automatically by: ";
 # print SETUP" $SCRIPTNAME \n";
 # print SETUP" ## * Don't edit this file, ";
 # print SETUP" edit $SCRIPTNAME instead. \n";
 # print SETUP" ##  *** ANY CHANGES MADE HERE WILL BE LOST! *** \n";
 # print SETUP" ##  *** Capelly *** \n";
 # print SETUP" ## ********************************************\n";







#######################################
#######################################
#######################################

    if (-e ".machines_pmn")
       {
           print " \n";
       }
       else {
           print color 'red';
           print "not FILE=.machines_pmn \n";
           #print "directory: $dir \n";
           print "Make or copy one... \n";
           print color 'reset';
          die ;
             }
     print "\n";
     print "\t=====================\n";


     open(INFOABINIT) or die("Could not open $INFOABINIT file.");
          {
     foreach $line (<INFOABINIT>) {
           # if (($line =~ m/.hgh/) && ($line !~ /[\#]/))  {
	    my $result = index($line, $char);
	    $vari=$line;
           if (($vari !~ /[\:]/) &&($vari !~ /[\#]/))  {
            #parse: if vari not contains  # , :
            if($vari !~  m/^\s*\n$/ ) { #skip blank lines
             chomp($vari);
             push @pseudopotentials, $vari;
             #print rtrim($vari)."\n";
	     }
                          
                                 } #end if
                                } #end foreach
                               } #end open

          $lenpp=@pseudopotentials;
$NOBUSCA=@BUSCA;
           for ($i=0; $i<$lenpp; $i++)
             {
             $origen=($pseudopotentials[$i]);


             if (($origen =~ /ecut/) || ($origen =~ /nspinor/) || ($origen =~ /nband2/) || ($origen =~ /acell/) ) {
             print color 'green';
              print "\t$origen \n";
              print color 'reset';
              print SETUP"$origen\n";
              
                                     }
               else
	         {
               print "\t$origen \n";
               print SETUP"$origen\n";
	         }

	 } ## end for 
print "\t=====================\n"; 
print "\n";

#print "$SETUPABINIT\n";
## cabellos 
