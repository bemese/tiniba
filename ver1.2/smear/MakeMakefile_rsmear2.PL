#!/usr/bin/perl
  use Cwd;    
  use Env;
  use Term::ANSIColor;
  use Sys::Hostname;
###===================================================
###Modificated by jl
###===================================================
   $host = hostname;
   
    print " $host \n ";

     if ($host eq "medusa") 
     { 
    print color 'blue';
    printf "\tMachine  :" ;
    print color 'reset';
    print color 'green';
    print "MEDUSA ok \n"; 
    print color 'reset';
}
  
   $micamino=getcwd;
  ($BasePath, $case) = breakup_dirname($micamino);
   
   
   open(MAKEFILE, "> Makefile");
  print MAKEFILE " ## ********************************************\n";
  print MAKEFILE " ## * Warning:                                 \n";
  print MAKEFILE " ## * This Makefile  was generated automatically by: ";
  print MAKEFILE " MakeMakefile.PL \n";
  print MAKEFILE " ## * Don't edit this file, ";
  print MAKEFILE " edit MakeMakefile.PL instead. \n";
  print MAKEFILE " ##  *** ANY CHANGES MADE HERE WILL BE LOST! *** \n";
  print MAKEFILE " ## ********************************************\n";
###======= time=======================================
   ($sec,$min,$hour,$mday,$mon,$year,$wday,
   $yday,$isdst)=localtime(time);
   print MAKEFILE " ## CREATED on:";
   printf  MAKEFILE " %4d-%02d-%02d %02d:%02d:%02d ",
   $year+1900,$mon+1,$mday,$hour,$min,$sec;
   print MAKEFILE " by USER:  $USER \n";
   print MAKEFILE " ## Centro de Investigaciones en Optica A.C. \n";
   print MAKEFILE " ## Leon, Guanajuato, Mexico \n";
   print MAKEFILE " ## This is free software;.There is NO warranty. JL\n";
   print MAKEFILE " ##  \n";

  
###======= Define common macros =======================
 print MAKEFILE "\tCOMPILER-XEON = /opt/intel/fc/10.1.008/bin/ifort\n"; 
 print MAKEFILE "\tCOMPILER-ITAN = /opt/intel/fc/9.1.036/bin/ifort\n";
 print MAKEFILE "\tCOMPILER-QUAD = /opt/intel/fce/10.1.008/bin/ifort\n";  
 print MAKEFILE "\t   FLAGS-XEON = -O3  -xW \n"; 
 print MAKEFILE "\t   FLAGS-ITAN = -O3 -tpp2\n"; 
 print MAKEFILE "\t   FLAGS-QUAD = -O3\n"; 
   print MAKEFILE "\n";
   print MAKEFILE "\tCWD=\$\(shell pwd\)\n";
   print MAKEFILE "\tPLATAFORM=\$\(shell hostname\)\n"; 
   print MAKEFILE "\tECHO=\@echo \n";
   print MAKEFILE "\tPRINTF=\@printf \n";
   print MAKEFILE "\tGREEN=\\e\[;32m\n"; 
   print MAKEFILE "\tMAG=\\e\[;35m\n"; 
   print MAKEFILE "\tNC=\\e\[0m\n"; 
     
###====================================================
       if  (@ARGV=~1) { 
          print color 'blue';
	  print "Taking Basename: ";
          print color 'reset';  
          print color 'green ';
          print "$ARGV[0] \n ";
          print color 'reset';   
                      }       
       if ((@ARGV=~0)||(@ARGV>=2)) {
	   my $src_x = 'SRC_';
           $string = $case;
           $first_4char= substr( $string, 0, 4 );  # SRC_
         #  print "$string\n";
         #  print "$first_4char\n";
         #  print "$src_x\n";
          
            if ( $first_4char eq $src_x ) {
                $salida=substr $case, 4;
                @ARGV[0]=$salida; 
             } else {
                @ARGV[0]=$case; 
             }
           


           
           print color 'blue';
	   print "\tBasename :";
           print color 'reset';  
           print color 'green ';
           print "@ARGV[0] \n ";
           print color 'reset'; 
           print color 'blue';
	   print "\tOutput   :";
           print color 'reset'; 
           print color 'green ';
           print "Makefile \n ";
           print color 'reset';
           print "   \n";  
                                   }#close

    
         
     if (($ARGV[0]=~"help")||($ARGV[0]=~"HELP")||($ARGV[0]=~"Help")){
        print " ";
        print color 'red'; 
        print "USAGE: Makefile.pl [namefile] \n\n";
        print "[namefile]=Any name file that you want\n";
        print "[namefile]=empty, take the name : $case \n";
        print "This produce a:  Makefile \n ";
        print color 'reset';
         die " stoping rigth now .." ;
                                        }
####=====================================================  

        #print MAKEFILE "\tBASENAME = $ARGV[0]\n\n";
        print MAKEFILE "\tBASENAME = rsmear2\n\n";
        print MAKEFILE "\n";
#### XEON
#### XEON
#### XEON
        print MAKEFILE "\tifeq\t\(\$\(PLATAFORM\),medusa\)\n";
        print MAKEFILE "\tNAME=\$\(BASENAME\)_xeon\n";
        print MAKEFILE "\tF90=\$\(COMPILER-XEON\)\n";
        print MAKEFILE "\tOPT = \$\(FLAGS-XEON\)\n";
        print MAKEFILE "\tWHATPL=XEON\n";
        print MAKEFILE "\tendif\n";
        print MAKEFILE "\n";
#### ITANIUM 
#### ITANIUM 
#### ITANIUM 
        print MAKEFILE "\tifeq\t\(\$\(PLATAFORM\),itanium01\)\n";
        print MAKEFILE "\tNAME=\$\(BASENAME\)_itanium\n";
        print MAKEFILE "\tF90=\$\(COMPILER-ITAN\)\n";
        print MAKEFILE "\tOPT = \$\(FLAGS-ITAN\)\n";
        print MAKEFILE "\tWHATPL=ITAN\n";
        print MAKEFILE "\tendif\n";
        print MAKEFILE "\n";
#### QUAD
#### QUAD
#### QUAD 
        print MAKEFILE "\tifeq\t\(\$\(PLATAFORM\),quad01\)\n";
        print MAKEFILE "\tNAME=\$\(BASENAME\)_quad\n";
        print MAKEFILE "\tF90=\$\(COMPILER-QUAD\)\n";
        print MAKEFILE "\tOPT = \$\(FLAGS-QUAD\)\n";
        print MAKEFILE "\tWHATPL=QUAD\n";
        print MAKEFILE "\tendif\n";
        print MAKEFILE "\n";

        
####============ Source listing==========================
        print MAKEFILE "SRCS =\t";
        @srcs = <*.f90 *.f *.F *.c >;
        &PrintWords(8, 0, @srcs);
        print MAKEFILE "\n\n";
#=============== Object listing==========================
        print MAKEFILE "OBJS =\t";
        @objs = @srcs;
        foreach (@objs) { s/\.[^.]+$/.o/ };
        &PrintWords(8, 0, @objs);
        print MAKEFILE "\n\n";
#"$SRC/$BASE.texi"
# Define common macros
# make
#
# make 
print MAKEFILE "\$(NAME): \$(OBJS)\n";
print MAKEFILE "\t\$(", &LanguageCompiler($ARGV[1], @srcs);
print MAKEFILE ") \$(OPT) -o \$@ \$(OBJS) \$(LIBS)\n";
print MAKEFILE "\t rm -f \$(OBJS) *.mod *~ *.o \n\n";
print MAKEFILE "\t cp \$(NAME) ..\/\n";            
print MAKEFILE "\n";
print MAKEFILE "\t \$(PRINTF) \" \\n \" \n";
print MAKEFILE "\t \$(PRINTF) \" \\t \$(MAG)***********************************\$(NC)\\n \" \n";
print MAKEFILE "\t \$(PRINTF) \" \\t  Compiled : \$\(WHATPL\)\\n \" \n";
print MAKEFILE "\t \$(PRINTF) \" \\t  Output   :..\/\$(GREEN)\$\(NAME\) \$(NC)   \\n \" \n";
print MAKEFILE "\t \$(PRINTF) \" \\t \$(MAG)***********************************\$(NC)\\n \" \n";



# make clean
print MAKEFILE "clean:\n";
print MAKEFILE "\t rm -f \$(OBJS) *.mod *~ *.o \n\n";

# make help 
 print MAKEFILE "help:\n";
 print MAKEFILE "\t \$(ECHO) \"you are compiling :\$\(WHATPL\)  \"   \n";
 print MAKEFILE "\t \$(ECHO) \"  make         -Compila and link \"    \n";
 print MAKEFILE "\t \$(ECHO) \"  make clean   -clean all *.o *.mod \" \n";



# Make .f90 a valid suffix

print MAKEFILE ".SUFFIXES: \$(SUFFIXES) .f90\n\n";
#
# .f90 -> .o
#
print MAKEFILE ".f90.o:\n";
print MAKEFILE "\t\$(F90) \$(F90FLAGS) -c \$<\n\n";
#
# Dependency listings
#
&MakeDependsf90($ARGV[1]);
&MakeDepends("*.f *.F", '^\s*include\s+["\']([^"\']+)["\']');
&MakeDepends("*.c",     '^\s*#\s*include\s+["\']([^"\']+)["\']');

sub PrintWords {
   local($columns) = 78 - shift(@_);
   local($extratab) = shift(@_);
   local($wordlength);
   #
   print MAKEFILE @_[0];
   $columns -= length(shift(@_));
   foreach $word (@_) {
      $wordlength = length($word);
      if ($wordlength + 1 < $columns) {
         print MAKEFILE " $word";
         $columns -= $wordlength + 1;
         }
      else {
         #
         # Continue onto a new line
         #
         if ($extratab) {
            print MAKEFILE " \\\n\t\t$word";
            $columns = 62 - $wordlength;
            }
         else {
            print MAKEFILE " \\\n\t$word";
            $columns = 70 - $wordlength;
            }
         }
      }
   }

#
# &LanguageCompiler(compiler, sources); --- determine the correct language
#    compiler
#
sub LanguageCompiler {
   local($compiler) = &toLower(shift(@_));
   local(@srcs) = @_;
   #
   if (length($compiler) > 0) {
      CASE: {
         grep(/^$compiler$/, ("fc", "f77")) &&
            do { $compiler = "FC"; last CASE; };
         grep(/^$compiler$/, ("cc", "c"))   &&
            do { $compiler = "CC"; last CASE; };
         $compiler = "F90";
         }
      }
   else {
      CASE: {
         grep(/\.f90$/, @srcs)   && do { $compiler = "F90"; last CASE; };
         grep(/\.(f|F)$/, @srcs) && do { $compiler = "FC";  last CASE; };
         grep(/\.c$/, @srcs)     && do { $compiler = "CC";  last CASE; };
         $compiler = "???";
         }
      }
   $compiler;
   }

#
# &toLower(string); --- convert string into lower case
#
sub toLower {
   local($string) = @_[0];
   $string =~ tr/A-Z/a-z/;
   $string;
   }

#
# &uniq(sorted word list); --- remove adjacent duplicate words
#
sub uniq {
   local(@words);
   foreach $word (@_) {
      if ($word ne $words[$#words]) {
         push(@words, $word);
         }
      }
   @words;
   }

#
# &MakeDepends(language pattern, include file sed pattern); --- dependency
#    maker
#
sub MakeDepends {
   local(@incs);
   local($lang) = @_[0];
   local($pattern) = @_[1];
   #
   foreach $file (<${lang}>) {
      open(FILE, $file) || warn "Cannot open $file: $!\n";
      while (<FILE>) {
         /$pattern/i && push(@incs, $1);
         }
      if (defined @incs) {
         $file =~ s/\.[^.]+$/.o/;
         print MAKEFILE "$file: ";
         &PrintWords(length($file) + 2, 0, @incs);
         print MAKEFILE "\n";
         undef @incs;
         }
      }
   }

#
# &MakeDependsf90(f90 compiler); --- FORTRAN 90 dependency maker
#
sub MakeDependsf90 {
   local($compiler) = &toLower(@_[0]);
   local(@dependencies);
   local(%filename);
   local(@incs);
   local(@modules);
   local($objfile);
   #
   # Associate each module with the name of the file that contains it
   #
   foreach $file (<*.f90>) {
      open(FILE, $file) || warn "Cannot open $file: $!\n";
      while (<FILE>) {
         /^\s*module\s+([^\s!]+)/i &&
            ($filename{&toLower($1)} = $file) =~ s/\.f90$/.o/;
         }
      }
   #
   # Print the dependencies of each file that has one or more include's or
   # references one or more modules
   foreach $file (<*.f90>) {
      open(FILE, $file);
      while (<FILE>) {
         /^\s*include\s+["\']([^"\']+)["\']/i && push(@incs, $1);
         /^\s*use\s+([^\s,!]+)/i && push(@modules, &toLower($1));
         }
      if (defined @incs || defined @modules) {
         ($objfile = $file) =~ s/\.f90$/.o/;
         print MAKEFILE "$objfile: ";
         undef @dependencies;
         foreach $module (@modules) {
            push(@dependencies, $filename{$module});
            }
         @dependencies = &uniq(sort(@dependencies));
         &PrintWords(length($objfile) + 2, 0,
                     @dependencies, &uniq(sort(@incs)));
         print MAKEFILE "\n";
         undef @incs;
         undef @modules;
         #
         # Cray F90 compiler
         #
         if ($compiler eq "cray") {
            print MAKEFILE "\t\$(F90) \$(F90FLAGS) -c ";
            foreach $depend (@dependencies) {
               push(@modules, "-p", $depend);
               }
            push(@modules, $file);
            &PrintWords(30, 1, @modules);
            print MAKEFILE "\n";
            undef @modules;
            }
         #
         # ParaSoft F90 compiler
         #
         if ($compiler eq "parasoft") {
            print MAKEFILE "\t\$(F90) \$(F90FLAGS) -c ";
            foreach $depend (@dependencies) {
               $depend =~ s/\.o$/.f90/;
               push(@modules, "-module", $depend);
               }
            push(@modules, $file);
            &PrintWords(30, 1, @modules);
            print MAKEFILE "\n";
            undef @modules;
            }
         }
      }
   }


    sub breakup_dirname {
#####################
   # break-up input path into a base and present directory
   $directory = $_[0];
   my @tmpArray = split(/\//,$directory);
   my $case = pop(@tmpArray);
   my $basePath = join("/", @tmpArray);
   return($basePath, $case);}
