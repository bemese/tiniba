#!/usr/bin/perl -w
## FUNCTION: Distribute k points in all arch. quad xeon and itanium 
## PARENTS : run_all.sh
## OUTPUT  : klist_length.txt, startpoint.txt endpoint.txt
## DATE    : Sunday April 20 2007, at 21:10hrs  grupo OSGROUP at C.I.O   
##===========================
     use File::Basename;
     use File::Copy;
     use Term::ANSIColor;
     use strict;
     use POSIX;
     use Env;
       ## LOCAL VAR 
       my $LINEA=0;
       my $NOMAQUINAS=0;
       my $NPXEON=0;
       my $NPITAN=0;
       my $NPQUAD=0;
       my $SEEMSNPXEON=0;
       my $SEEMSNPITAN=0;
       my $SEEMSNPQUAD=0;
       my $SEEMSNOMAQUINAS=0;
       my $NPKGIVENXEON=0;
       my $NPKGIVENITAN=0;
       my $NPKGIVENQUAD=0;
       my $MODULO;
       my $KLISTFILE;
       my $NCPUS=0;
       my $PESOI=0;
       my $PESOQ=0;
       my $NKPT=0;
       my $VARTMP=0;
       my $TMP=0;
       my $TMP1=0;
       my $NKPTCAL=0;
       my $HASTA=0;
       my $ii=0;
       my $QUETOCAPORCPUX=0;
       my $DIR;
       my $BASENAME;
       my $CASO;
       my $QUETOCACPUXEON=0;
       my $QUETOCACPUITAN=0;
       my $QUETOCACPUQUAD=0;
       my $MODULOXEON=0;
       my $MODULOITAN=0;
       my $MODULOQUAD=0;
       my $IXEON=1;
       my $IITAN=1;
       my $IQUAD=1;
       my $jj=0;
       my @MAQUINAS;
       my @SEEMSMAQUINAS;
       my @MAQUINASxeon;
       my @MAQUINASitan;
       my @MAQUINASquad;
       my @KPOINTS;
       my @STARTPOINTS;
       my @ENDPOINTS;

#####
#####========begin code======= 
       if (@ARGV!=4) {
        ARGVerror();
        }

        $DIR = getcwd;
        $BASENAME=dirname($DIR);
        $CASO = basename($DIR);
        $KLISTFILE=$ARGV[0];
        $NCPUS = $ARGV[1];
        $PESOI = $ARGV[2];
        $PESOQ = $ARGV[3];
          if ( $PESOI == 0 ){
            $PESOI=2;
	     printf "\tWARNING: Taking default peso Itanium 2 \n";
             } # end if
           if ( $PESOQ == 0 ){
	     $PESOQ=5;
	      printf "\tWARNING: Taking default peso Quad 5 \n";
              } # end if

       my @arr = (1 .. $NCPUS);
       $NKPT = linecount($KLISTFILE);
  
       open(FH, "<$KLISTFILE") or die "Cannot open $KLISTFILE: $!";
         my @lines = <FH>;
        close(FH);

     open( MACHINESpmn, "<.machines_pmn") or die "Cannot open .machines_pmn";
{
     foreach $LINEA (<MACHINESpmn>) {
      chomp($LINEA);
     if($LINEA =~ m/^(itanium|node|quad|hexa)/) {  
       push @MAQUINAS, $LINEA;
       }
    else{
     printf "\t=============================\n";
     printf "\tYou write something wrong in FILE: .machines_pmn\n ";
     printf "\tADVICE: Look for blank lines at the end of file\n ";
     print " $LINEA \n";
     system("touch killme");
     die "\tStoping right now ...\n";
             } #end if
             } #end foreach
             } #end open MACHINESpmn
     close(MACHINESpmn);
     $NOMAQUINAS= @MAQUINAS;  
     for ($ii=0; $ii<=$NOMAQUINAS-1; $ii++) {
        if ( substr ( $MAQUINAS[$ii], 0 , 4 ) eq "node" ){
         $NPXEON=$NPXEON+1;
        }
        if ( substr ( $MAQUINAS[$ii], 0 , 7 ) eq "itanium" ){
        $NPITAN=$NPITAN+1;  
        }
        if ( substr ( $MAQUINAS[$ii], 0 , 4 ) eq "quad" ){
        $NPQUAD=$NPQUAD+1;  
        }
        if ( substr ( $MAQUINAS[$ii], 0 , 4 ) eq "hexa" ){
        $NPQUAD=$NPQUAD+1;  
        }
     } #end for
#####################
$SEEMSNPXEON=($NPXEON+.5);
$SEEMSNPITAN=($NPITAN*$PESOI)+.5;
$SEEMSNPQUAD=($NPQUAD*$PESOQ)+.5;  
$SEEMSNPXEON=floor($SEEMSNPXEON);
$SEEMSNPITAN=floor($SEEMSNPITAN);
$SEEMSNPQUAD=floor($SEEMSNPQUAD);

$SEEMSNOMAQUINAS=$SEEMSNPXEON+$SEEMSNPITAN+$SEEMSNPQUAD;
$MODULO= $NKPT%$SEEMSNOMAQUINAS;
$QUETOCAPORCPUX=$NKPT/$SEEMSNOMAQUINAS;
$QUETOCAPORCPUX=floor($QUETOCAPORCPUX);
   for ($ii=1; $ii<=$SEEMSNOMAQUINAS; $ii++) {
 $SEEMSMAQUINAS[$ii] = "$QUETOCAPORCPUX";
      } ## end for
  if ($MODULO != 0){
 for ($ii=1; $ii<=$MODULO; $ii++)
{
    $SEEMSMAQUINAS[$ii] = $SEEMSMAQUINAS[$ii]+1;
     }## end for
    }## end modulo
##=====================
  $NPKGIVENQUAD=0;
  $NPKGIVENITAN=0;
  $NPKGIVENXEON=0;
  $TMP=0;
  $TMP1=0; 
##=====================QUAD
if ($NPQUAD > 0 ) {
for ($ii=1; $ii<=$SEEMSNPQUAD; $ii++){
    $TMP=$TMP+1;
    $NPKGIVENQUAD=$NPKGIVENQUAD+$SEEMSMAQUINAS[$ii];
    }#end for
} ##end if
##=====================ITAN
if ($NPITAN > 0 ) {
for ($ii=$TMP+1; $ii<=($SEEMSNPITAN+$TMP); $ii++){
    $TMP1=$TMP1+1;
    $NPKGIVENITAN=$NPKGIVENITAN+$SEEMSMAQUINAS[$ii];
}
}
##=====================XEON
if ($NPXEON > 0 ) {
$HASTA=$SEEMSNPITAN+$SEEMSNPXEON+$TMP;
 for ($ii=($SEEMSNPITAN+$TMP+1); $ii<=($HASTA); $ii++){
    $NPKGIVENXEON=$NPKGIVENXEON+$SEEMSMAQUINAS[$ii];
}
}
$NKPTCAL=$NPKGIVENXEON+$NPKGIVENITAN+$NPKGIVENQUAD;
##=====================SHOW INFO
printf "\t============================================\n";
printf "\tThis choice is equivalent to running in: ";
printf "$SEEMSNOMAQUINAS cpus XEON\n";
printf "\tList of Kpoints  = $KLISTFILE \n";
printf "\t============================================\n";
printf "\t           XEON    ITANIUM   QUAD/HEXA TOTAL\n";
printf "\tCPUS       $NPXEON\t   $NPITAN\t";
printf "     $NPQUAD\t     \t$NOMAQUINAS\n";
printf "\tKpoints    $NPKGIVENXEON\t   $NPKGIVENITAN\t";
printf "     $NPKGIVENQUAD     \t$NKPTCAL\n";
printf "\tWeight     1\t   $PESOI\t     $PESOQ   \t0\n";
printf "\t============================================\n";
##########################################
########ABOVE HERE YOU distribute only k points 

##=====================QUAD
if ($NPQUAD > 0 ) {
$MODULOQUAD=$NPKGIVENQUAD%$NPQUAD;
$QUETOCACPUQUAD=$NPKGIVENQUAD/$NPQUAD;
$QUETOCACPUQUAD=floor($QUETOCACPUQUAD);
for ($ii=1; $ii<=($NPQUAD); $ii++){
    $MAQUINASquad[$ii]="$QUETOCACPUQUAD";
}

    if ($MODULOQUAD != 0){
      for ($ii=1; $ii<=$MODULOQUAD; $ii++) {
    $MAQUINASquad[$ii] = $MAQUINASquad[$ii]+1;
     }## end for
    }## end modulo
      } ### end if QUQD

##=====================ITAN
if ($NPITAN > 0 ) {
$MODULOITAN=$NPKGIVENITAN%$NPITAN;
$QUETOCACPUITAN=$NPKGIVENITAN/$NPITAN;
$QUETOCACPUITAN=floor($QUETOCACPUITAN);
   for ($ii=1; $ii<=($NPITAN); $ii++){
    $MAQUINASitan[$ii]="$QUETOCACPUITAN";
}
      if ($MODULOITAN != 0){
      for ($ii=1; $ii<=$MODULOITAN; $ii++) {
    $MAQUINASitan[$ii] = $MAQUINASitan[$ii]+1;
     }## end for
    }## end modulo
} ##end if ITAN
##=====================XEON
if ($NPXEON > 0 ) {
$MODULOXEON=$NPKGIVENXEON%$NPXEON;
$QUETOCACPUXEON=$NPKGIVENXEON/$NPXEON;
$QUETOCACPUXEON=floor($QUETOCACPUXEON);
   for ($ii=1; $ii<=($NPXEON); $ii++){
    $MAQUINASxeon[$ii]="$QUETOCACPUXEON";
}
      if ($MODULOXEON != 0){
      for ($ii=1; $ii<=$MODULOXEON; $ii++) {
    $MAQUINASxeon[$ii] = $MAQUINASxeon[$ii]+1;
     }## end for
    }## end modulo
} #end if XEON
##############until here you have one array with por cada tipo de maquina
    $IXEON=1;
    $IITAN=1;
    $IQUAD=1;
     for ($ii=0; $ii<=$NOMAQUINAS-1; $ii++) {
        if ( substr ( $MAQUINAS[$ii], 0 , 4 ) eq "node" ){
	    $KPOINTS[$ii]=$MAQUINASxeon[$IXEON];
	    $IXEON=$IXEON+1;
        } 
        if ( substr ( $MAQUINAS[$ii], 0 , 7 ) eq "itanium" ){
           $KPOINTS[$ii]=$MAQUINASitan[$IITAN];
            $IITAN=$IITAN+1;
         }
        if ( substr ( $MAQUINAS[$ii], 0 , 4 ) eq "quad" ){
           $KPOINTS[$ii]=$MAQUINASquad[$IQUAD];
           $IQUAD=$IQUAD+1;
         }
        if ( substr ( $MAQUINAS[$ii], 0 , 4 ) eq "hexa" ){
           $KPOINTS[$ii]=$MAQUINASquad[$IQUAD];
           $IQUAD=$IQUAD+1;
         }
     } #end for
#####################################################
$STARTPOINTS[0]=1;
$TMP=0;
 for ($ii=0; $ii<=$NOMAQUINAS-1; $ii++) {
      $jj=$ii+1;
      $STARTPOINTS[$ii]=($TMP)+1;
      $TMP=$TMP+$KPOINTS[$ii];
      $ENDPOINTS[$ii]=$TMP;
   } 
######################################################
printf "\n";
printf "        Cpu              Kpoints   ";
printf "  Start Point   End Point\n";
print color 'blue';
printf "\t====================================================\n";
 print color 'reset';
        open (FILEstar, ">startpoint.txt");
        open (FILEend, ">endpoint.txt");
        open (FILEklist, ">klist_length.txt");
        print FILEstar "0\n";
        print FILEklist "0\n";
        print FILEend "0\n";
for ($ii=0; $ii<=$NOMAQUINAS-1; $ii++) {
    $jj=$ii+1;          
      print FILEstar "$STARTPOINTS[$ii] \n";
      print FILEklist "$KPOINTS[$ii] \n";
      print FILEend "$ENDPOINTS[$ii] \n";

  printf "%17.10s  %8.10s %12.10s %12.10s\n",
            $MAQUINAS[$ii], "$KPOINTS[$ii]", "$STARTPOINTS[$ii]", "$ENDPOINTS[$ii]"   
   } ## end for
        close (FILEstar);
        close (FILEend);
        close (FILEklist);
print color 'blue';
printf "\t====================================================\n";
print color 'reset';
if ( $TMP == $NKPT ){
printf "\t              SUM=";
print color 'green';
printf "$TMP\n";
print color 'reset';
printf "\tKpoints read file=";
print color 'green';
printf "$NKPT\n";
print color 'reset';

print color 'green';
printf "\tOK go go ...\n";
print color 'reset';   
}
else {
    system("touch killme");
    print color 'red';
    printf "\tHOLD!! on your suma of kpoints is not equal\n";
    print color 'reset';
    print "\t              SUM=$TMP\n";
    print "\tKpoints read file=$NKPT\n";
    
 die "\tStoping right now ...arrangeMachines.pl\n";
}
    print color 'blue';
    printf "\t==========================\n";
    print color 'reset';

############## END END END =============== CODE

###############
sub ARGVerror {
###############
    printf "\tRequires four arguments.\n";
    printf "\tArgument 1: the klist file\n";
    printf "\tArgument 2: the number of CPUs you want ".
           "klist split into\n";
    printf "\tArgument 3: Peso Itanium\n";
    printf "\tArgument 4: Peso Quad  \n";
    
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
### notihing under here jl  
