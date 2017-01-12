#!/bin/bash

exp=$1 #openloop

inpath=/disk1/asmundb/RESULTS/$exp
outpath=/disk1/asmundb/results2/$exp

cd $inpath


ens=1
for dir in $inpath/ISBA/* ; do
  ens=1
  while [[ "$ens" -le "5" ]]; do
    tmp=`echo $dir | tail -c 9`
    ln -sf $dir/ISBA_PROGNOSTIC.OUT.nc_0$ens $outpath/offline/ISBA_PROGNOSTIC.OUT.nc_${tmp}_ens_$ens
    ens=$(( ens + 1 ))
  done
done


for dir in $inpath/ANALYSIS/* ; do
  ens=1
  tmp=`echo $dir | tail -c 11`
  while [[ "$ens" -le "5" ]]; do
    ln -sf $dir/SURFOUT$ens.nc $outpath/analysis/soda_analysis.nc_${tmp}_ens_$ens
    ens=$(( ens + 1 ))
  done
done

