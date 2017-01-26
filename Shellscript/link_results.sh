#!/bin/bash

exp=$1 #openloop

RESULTS=/disk1/asmundb/RESULTS/normSMOS
results2=/disk1/asmundb/results2


inpath=$RESULTS/enkf_5
outpath=$results2/enkf_5
#cd $inpath
rm $outpath/*/*

##if [[ "$2" == "-ens" ]];then
#ens=1
#for dir in $inpath/ISBA/* ; do
#  ens=1
#  while [[ "$ens" -le "5" ]]; do
#    tmp=`echo $dir | tail -c 9`
#    ln -sf $dir/ISBA_PROGNOSTIC.OUT.nc_0$ens $outpath/offline/ISBA_PROGNOSTIC.OUT.nc_${tmp}_ens_$ens
#    ln -sf $RESULTS/openloop_ens_5/ISBA/20$tmp/ISBA_PROGNOSTIC.OUT.nc_0$ens $outpath/openloop/ISBA_PROGNOSTIC.OUT.nc_${tmp}_ens_$ens
#    ens=$(( ens + 1 ))
#  done
#done
#for dir in $inpath/ANALYSIS/* ; do
#  ens=1
#  tmp=`echo $dir | tail -c 11`
#  while [[ "$ens" -le "5" ]]; do
#    ln -sf $dir/SURFOUT$ens.nc $outpath/analysis/soda_analysis.nc_${tmp}_ens_$ens
#    ens=$(( ens + 1 ))
#  done
#done

inpath=$RESULTS/ekf
outpath=$results2/ekf
rm $outpath/*/*
for dir in $inpath/ISBA/* ; do
  tmp=`echo $dir | tail -c 9`
  ln -sf $dir/ISBA_PROGNOSTIC.OUT.nc $outpath/offline/ISBA_PROGNOSTIC.OUT.nc_${tmp}
#  ln -sf $RESULTS/openloop/ISBA/20$tmp/ISBA_PROGNOSTIC.OUT.nc $outpath/openloop/ISBA_PROGNOSTIC.OUT.nc_${tmp}
done

for dir in $inpath/ANALYSIS/* ; do
  tmp=`echo $dir | tail -c 11`
  ln -sf $dir/SURFOUT.nc $outpath/analysis/soda_analysis.nc_${tmp}
done

