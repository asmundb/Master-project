#!/bin/bash

var=$1    #"WG"


for i in "$@"; do
  if [[ "$i" -ne "$var" ]]; then
    touch $var$i.analysis.dat $var$i.offline.dat $var$i.openloop.dat 
  fi
done
