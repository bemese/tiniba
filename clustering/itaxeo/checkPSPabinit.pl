#!/usr/bin/perl
##FUNCTION : check if the file CASO.files contains all psp declared 
##CHILDREN : none
##OUTPUT   : file: killme if not exist some PSP
##                nothing if all PSP exists 
##PARENTS  : any 
##DATE     : May 1 2007 11:52 OSGRUP
##This is free software; .There is NO warranty. jl           
     use File::Basename;
     use File::Copy;
     use Term::ANSIColor;
     use strict;
     use POSIX;
     use Env;
     ##VAR=== =================== 
     my $LINEA;
     my $ABINITfiles;
     my $DISPLAY;
     ##SOURCE =================== 
     if ((@ARGV=~0)){
       print "\t Usage: checkPSPabinit.pl setUpAbinit_";
       print color 'red';
       print "case";
       print color "reset";
       print ".in 1 \n" ;
	die "\t I need a input FILE:  *.files\n";
     }
     else {
     $ABINITfiles=$ARGV[0];
      system("rm -f killme");
     $DISPLAY=$ARGV[1];
     }
    open( MANOfiles, "<$ABINITfiles") or die "Cannot open .machines_pmn";
        {
         foreach $LINEA (<MANOfiles>) {
          chomp($LINEA);
           if (($LINEA =~ m/.hgh/) && ($LINEA !~ /[\#]/))  {
             if (-e "$LINEA")
              { 
		 if (($DISPLAY =~ 1)){ 
                 print "\t $LINEA [";
                 print color 'green';
                 print "ok";
                 print color 'reset';
                 print "]\n";  
                  }           
              }
	     else {
	    system("touch killme");
            open (MYkillme, '>killme');
            print MYkillme "This was produced by: checkPSPabinit.pl\n";
            close (MYkillme);
	    printf "\t=========================\n";
            print "\t $LINEA [";
            print color 'red';
            print "does not exist";
            print color 'reset';
            print "]\n";
            printf "\t Hold on! ";
            printf "Your PSP doesnt exist...fix your $ABINITfiles \n";
            printf "\t=========================\n";

            } #end else            
           }#end if
	  }#close foreach
	 }#close open
            
### nada hay abajo de aqui  
