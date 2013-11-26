#!/bin/bash
##

#program that truncates a string 
#it removes the characters that appear after the first number


word=$1


position=0
position2=0
flag=0
for(( i=0;i<10;i++ ))
do
  position2=$(expr index "$word" $i )
  if (( flag == 0 ))
      then
      if (( position != position2 )) && (( position2 != 0 )) 
	  then
	  flag=1
	  position=$(($position2 - 1))
      fi
  else
      if (( position >= position2 )) && (( position2 != 0 )) 
	  then
	  position=$(($position2 - 1))
      fi
  fi
done


result=${word:0:$position}

echo $result

#echo ${word:0:4}
#if [ ${word:0:4} == 'node' ]
#    then
#    echo "yes"
#fi