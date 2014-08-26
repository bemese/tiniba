#!/usr/bin/perl 

use Env;

##########################################################################
# Opening responses.txt ###### ###########################################
##########################################################################

#open TEMP, "</home/bms/spin/abinit/utils/responses.txt" or die "Opening '/home/bms/spin/abinit/utils/responses.txt': $!\n";
open TEMP, "<$TINIBA/utils/responses.txt" or die "Opening '$TINIBA/utils/responses.txt': $!\n";
$string = join " ",<TEMP>;
close  TEMP;


# Saving each line of responses.txt in @tokens ############################

@tokens = split(/\n/, $string);

$i = 0;

foreach $token (@tokens)           #for each line
{
    if ( $token =~ /\w/ )          #if it is not a blank line
    {
	@words = split (/:/,$token);          #split the line into words separated by ":"
	$numbers[$i] = $words[0];             #filling the array @numbers
	$numbers[$i] = &trim2 ( $numbers[$i] ) ;
	$options[$i] = $words[1] ;    #filling the array @options
	$options[$i] = &trim2 ( $options[$i] ) ;
	$descriptions[$i]="";
	$nwords = @words;
	if ( $nwords >= 3 )
	{
	    $descriptions[$i] = $words[2];    #filling the array @descriptions
	    $descriptions[$i] = &trim2 ( $descriptions[$i]);
	}
	$i = $i + 1 ;
    }
}

$n = @numbers;

# Printing out the lines ################ ############################

foreach $i ( 0 .. $n-1 )
{
    print "\e[34m",$numbers[$i],"\e[0m";
    $length = length( $numbers[$i] );
    foreach $j ( 0 .. 2 - $length )
    {                                      # Format:
	print " ";                         # filling the lines with blanks
    }
    print $options[$i];

    $length = length( $options[$i] );
    foreach $j ( 0 .. 7-$length)
    {                                      # Format:
	if ( $descriptions[$i] eq "" )
	{
	    print " ";                         # filling the lines
	}
	else
	{
	    print "-";                         # filling the lines with blanks
	}
    }
    print $descriptions[$i];

    $length = length( $descriptions[$i] );
    foreach $j ( 0 .. 25-$length)
    {                                      # Format:
	print " ";                         # filling the lines with white spaces
    }

    if((( $i + 1 ) % 2 ) == 0 )            # To print out to columns
    {
	print "\n";
    }

}
print "\n";
##########################################################################
# Subroutines  ###########################################################
##########################################################################

########## Trim #########################################################
# sub trim {      
#     local ($s = $_[0]);
#     $s = &rtrim($s);
#     $s = &ltrim($s);
#     return $s;

# }
########## Left Trim #########################################################
# sub ltrim {
#     local ($str = $_[0]);
#     if($str =~ /\s+(.*)/) {
# 	return $1;
#     }else {
# 	return $str;
#     }

# }
########## Right Trim #########################################################
# sub rtrim {
#     local ($str = $_[0]);
#     local ($ret = $_[0]);
#     $str = reverse($str);
#     if($str =~ /\s+(.*)/) {
# 	$ret = reverse ($1);
# 	print $_[0],"=",$str,"=",$1,"\n";
#     }
#     return ($ret); 
# }
#########################################################################
########## Trim 2 (A more effective routine to trim) ####################
sub trim2
{
    local ( $str = $_[0]);
    $str = &ltrim2( $str );
    $str = reverse ( $str );
    $str = &ltrim2( $str );
    $str = reverse ( $str );
    return $str;
}
########## Left Trim #########################################################
 sub ltrim2
   {
     local ( $s = $_[0]);
     $s =~ s/^\s*(.*?)\s*/$1/; 
     return $s;
   }
