#!/bin/sh

FILES_SUCCESS=`ls success/*.c`
FILES_FAIL=`ls fail/*.c`

for FILE in $FILES_SUCCESS 
do
    ../minic "$FILE" 2>"$FILE.out" > "$FILE.out"
    if [ "$?" -eq "0" ]; then
        echo "$FILE OK"
    else
        echo "$FILE ERROR"
    fi
done

for FILE in $FILES_FAIL 
do
    ../minic "$FILE" 2>"$FILE.out" > "$FILE.out"
    if [ "$?" -eq "1" ]; then
        echo "$FILE OK"
    else
        echo "$FILE ERROR"
    fi
done



