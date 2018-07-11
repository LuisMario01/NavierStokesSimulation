#!/usr/bin/env bash

rm -f "$2/$1.boh"
rm -f "$2/$1.post.res"

# OutputFile: $2/$1.boh
# ErrorFile: $2/$1.err

# delete the line before and uncomment the following line 
# to execute the program

KERNEL=`uname -s`
if [ $KERNEL = "Darwin" ]
then
KERNEL_NAME="macosx"
else
KERNEL_NAME="linux"
fi

PLATFORM=`uname -m`
if [ $PLATFORM = "x86_64" ]
then
KERNEL_PLATFORM="64"
else
KERNEL_PLATFORM="32"
fi

output=$("$3/exec/R/alphaSolver.R" "$2/$1")

echo $output

if [ ! -e "$2/$1.post.res" -o ! -s "$2/$1.post.res" ]
 then
     #echo "Program '$3/exec/R/alphaSolver.R' failed" >> "$2/$1.err"
     echo $output >> "$2/$1.err"
fi
