#!/bin/bash

inpath=/lustre/storeB/users/asmundb/surfex/2016
outpath=/lustre/storeB/users/asmundb/surfex/FORCING_nc
cd $inpath

#[! -d FORCING_nc ] && mkdir FORCING_nc
#rm FORCING_nc/*

for dir in $inpath/* ; do
  tmp=`echo $dir | tail -c 22`
  dtg1=`echo $tmp | cut -c1-10`
  dtg2=`echo $tmp | cut -c12-22`
  if [ -f $dir/FORCING.nc ]; then 
#    cp -v $dir/FORCING.nc $outpath/FORCING.nc_$dtg1"_"$dtg2
    echo -n
  else
    echo "FORCING MISSING FOR_" $dtg1"_"$dtg2
  fi
done
