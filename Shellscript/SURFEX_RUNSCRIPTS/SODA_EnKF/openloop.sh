#!/bin/bash

# run offline without DA

RUNDIR=$(pwd)

AAAAMMJJRR=2016050100

aa=`echo $AAAAMMJJRR | cut -c3-4`
mm=`echo $AAAAMMJJRR | cut -c5-6`
jj=`echo $AAAAMMJJRR | cut -c7-8`
RR=`echo $AAAAMMJJRR | cut -c9-10`

#end of simulation
AAAAMMJJRR_end=2016101700

cp PREP.nc_2016050100 PREP.nc
cp OPTIONS.nam_openloop OPTIONS.nam

while [ $AAAAMMJJRR  -le $AAAAMMJJRR_end ]; do

  echo $AAAAMMJJRR
  cp ./FORCING/FORCING.nc_$AAAAMMJJRR ./FORCING.nc
  ./offline.exe || exit 1

  mv ISBA_PROGNOSTIC.OUT.nc time_series/openloop/ISBA_PROGNOSTIC.OUT.nc_${AAAAMMJJRR}
  mv SURFOUT.nc PREP.nc

  AAAAMMJJRR=`SMS_DATE/smsdate $AAAAMMJJRR 6`

done

cp PREP.nc_2016050100 PREP.nc
