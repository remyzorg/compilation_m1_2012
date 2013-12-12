#!/bin/bash

dir=part3

total=0
nbfail=0


FILES=`ls $dir/*.c`

for f in $FILES
do
         
  f=`basename $f .c`
  c="$dir/$f.c"
  s="$dir/$f.s"
  out="$dir/$f.out"

  echo -e "$c  \c"

  ../minic "$c"
  java -jar ../Mars_4_2.jar "$s" | sed '1,2d' | sed '$d' >tmp.out
  #../spim -noexception -quiet -file "$s" >out_tmp


  n=`diff tmp.out "$out" | wc -l`

  if [ $n -ne 0 ]; then
    echo -e "\033[31m[ FAIL ]\033[0m"
    let nbfail=nbfail+1
  else
    echo -e "\033[32m[ OK ]\033[0m"
  fi

  rm -f "$s"
  rm -f out_tmp

  let total=total+1

done

echo "TOTAL   : $total"
echo "NB FAIL : $nbfail"
