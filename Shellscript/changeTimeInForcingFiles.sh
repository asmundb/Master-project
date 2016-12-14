#!/bin/bash

# make time:unit match time in filename

#path=/lustre/storeB/users/asmundb/surfex/FORCING_nc
path=/disk1/asmundb/FORCING/

for file in $path/* ; do
#  tmp=`echo $file | tail -c 22`
  tmp=`echo $file | tail -c 11`
  yyyy=`echo $tmp | cut -c1-4`
  mm=`echo $tmp | cut -c5-6`
  dd=`echo $tmp | cut -c7-8`
  HH=`echo $tmp | cut -c9-10`
  echo $tmp
 
  unit_str=`echo hours since $yyyy-$mm-$dd $HH:00:00`
  echo $unit_str
  ncatted -h -a units,time,m,c,"$unit_str" $file
done
