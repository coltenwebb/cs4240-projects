#!/bin/sh

NAIVE=$(mktemp)
INTRA=$(mktemp)

sh run.sh naive $1 $NAIVE
sh run.sh intra $1 $INTRA

if [ -z $2 ]
then
  echo "######## NAIVE ########"
  spim -keepstats -f $NAIVE
  
  echo "######## INTRA ########"
  spim -keepstats -f $INTRA
else
  echo "######## NAIVE ########"
  spim -keepstats -f $NAIVE < $2
  
  echo "######## INTRA ########"
  spim -keepstats -f $INTRA < $2
fi

rm $NAIVE
rm $INTRA
