#!/usr/bin/perl
## gets arguments
$em=$ARGV[0];
$pmn=$ARGV[1];
$lpmn=$ARGV[2];
$lpmm=$ARGV[3];
$sccp=$ARGV[4];
$lsccp=$ARGV[5];
$count = 0;
@arr = (1 .. 6);
@arrl = (1 .. 7);
$Nk = 44;
##
########### Energy
if ($em eq "true" )
{
   print "\tEnergy Files\n";
   $outfile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/energy.d";
   open (OUTF, ">$outfile");
   foreach $i (@arr) {
   $infile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/si_as_h_7L_$i/energy.d";
   open( FL, "<$infile");
   @filearr = <FL>;
   close(FL);
   foreach (@filearr) {
     @words = split;
     $count = $count + 1;
     $number = $count/100;
     if ($number == int($number)) {
        print "$count", "\n";
     }
     $words[0] = $count;
     $_ = join(" ", @words);
     print OUTF $_, "\n";
     }
   }
   close(OUTF);
}
####### pmn
if ($pmn eq "true") 
{
      print "	Pmn Files\n";
## Loop for pmn
   foreach $mn ("pmn.d") 
   {
      $outfile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/$mn";
      open( OUTF, ">$outfile") or die;
      foreach $i (@arr) 
      {
        $infile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/si_as_h_7L_$i/$mn";
        open( FL, "<$infile") or die;
        while (<FL>) 
        {
           print OUTF $_;
        }
        close(FL);
      }
   }
   close( OUTF );
}
##### pnn (calculated along with pmn)
if ($pmn eq "true") 
{
      print "	Pnn Files\n";
## Loop for pnn
   foreach $mn ("pnn.d") 
   {
      $outfile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/$mn";
      open( OUTF, ">$outfile") or die;
      foreach $i (@arr) 
      {
        $infile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/si_as_h_7L_$i/$mn";
        open( FL, "<$infile") or die;
        while (<FL>) 
        {
           print OUTF $_;
        }
        close(FL);
      }
   }
   close( OUTF );
}
###########################spinccp
if ($sccp eq "true") 
{
      print "	Sccp Files\n";
## Loop for spinmn
   foreach $mn ("spinmn.d") 
   {
      $outfile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/$mn";
      open( OUTF, ">$outfile") or die;
      foreach $i (@arr) 
      {
         $infile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/si_as_h_7L_$i/$mn";
         open( FL, "<$infile") or die;
         while (<FL>) 
         {
             print OUTF $_;
         }
         close(FL);
      }
   } 
   close( OUTF );
##
}
############################csmnd
if ($lsccp eq "true") 
{
      print "	CalSccp Files\n";
## Loop for csmnd
   foreach $L (@arrl) 
   {
      foreach $mn ("csmnd_") 
      {
	 $outfile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/$mn$Nk\_$L";
	 open( OUTF, ">$outfile") or die;
	 foreach $i (@arr) 
         {
	    $infile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/si_as_h_7L_$i/$mn$L";
	    open( FL, "<$infile") or die;
	    while (<FL>) 
            {
	       print OUTF $_;
	    }
	    close(FL);
	 }
      }
   }
   close( OUTF );   
}
############################cpmn
if ($lpmn eq "true") 
{
      print "	CalPmn Files\n";
## Loop for layer
   foreach $L (@arrl) 
   {
      foreach $mn ("cpmnd_") 
      {
	 $outfile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/$mn$Nk\_$L";
	 open( OUTF, ">$outfile") or die;
	 foreach $i (@arr) 
         {
	    $infile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/si_as_h_7L_$i/$mn$L";
	    open( FL, "<$infile") or die;
	    while (<FL>) 
            {
	       print OUTF $_;
	    }
	    close(FL);
	 }
      }
   }
   close( OUTF );
}
############################cpmn
if ($lpmm eq "true") 
{
      print "	CalPmm Files\n";
## Loop for layer
   foreach $L (@arrl) 
   {
      foreach $mn ("cpmmd_") 
      {
	 $outfile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/$mn$Nk\_$L";
	 open( OUTF, ">$outfile") or die;
	 foreach $i (@arr) 
         {
	    $infile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/si_as_h_7L_$i/$mn$L";
	    open( FL, "<$infile") or die;
	    while (<FL>) 
            {
	       print OUTF $_;
	    }
	    close(FL);
	 }
      }
   }
   close( OUTF );
}
############################jnmlk
if ( $opt1 == 2 )
{
   if ( $opt2 == 10 )
   {
      print "concatenating jnnlk for option: $opt1","\n";
   }
   else
   {
      print "concatenating jnnlk for option: $opt1 $opt2","\n";
   }
   ## Loop for layer
   foreach $L (@arrl) 
   {
      foreach $mn ("jnnlk_") 
      {
	 $outfile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/$mn$Nk\_$L";
	 open( OUTF, ">$outfile") or die;
	 foreach $i (@arr) 
         {
	    $infile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/si_as_h_7L_$i/$mn$L";
	    open( FL, "<$infile") or die;
	    while (<FL>) 
            {
	       print OUTF $_;
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
         ( $opt1 == 1 )|| ( $opt1 == 4 )
      ) && ( $spin == 2 )
   )
{
   if ( $opt2 == 10 )
   {
      print "concatenating  cSmmd for option: $opt1","\n";
   }
   else
   {
      print "concatenating  cSmmd for option: $opt1 $opt2","\n";
   }
   foreach $L (@arrl) 
   {
      foreach $mn ("cSmmd_") 
      {
       $outfile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/$mn$Nk\_$L";
	 open( OUTF, ">$outfile") or die;
	 foreach $i (@arr) 
         {
	    $infile = "/home/bms/tiniba/ver1.0/examples/surface/si_as_h_7L/si_as_h_7L_$i/$mn$L";
	    open( FL, "<$infile") or die;
	    while (<FL>) 
            {
	       print OUTF $_;
	    }
	    close(FL);
	 }
      }
   }
   close( OUTF );
}
############################
############################


