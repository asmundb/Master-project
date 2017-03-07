#!/bin/bash

#set -x
#ln -sf /disk1/asmundb/SMOS/OBSERVATIONS/* INPUT/

source /home/asmundb/SURFEX2/open_SURFEX_V8_0/conf/profile_surfex-LXgfortran-SFX-V8-0-0-NOMPI-OMP-DEBUG-ASSIM_NILU

########################################################
############ USER INPUT ################################
start=2001010206
end=2001010306
#end=2016101700


# Usage:
#  $ ./soda_loop.sh EKF -o
#  $ ./soda_loop.sh ENKF 5


if [[ $# -lt 1 ]]; then
  echo "Usage: ./soda_loop.sh experiment nens [analysis]"
  echo " experiment:   EKF , ENKF"
  echo " nens      :   number of ensemble members, only ENKF"
  echo " -o        :   openloop "
  exit 1
fi

exp=$1
case $exp in
  "EKF" | "ENKF")
    echo "ok"
  ;;
  *)
    echo "first argument must be ENKF or EKF"
	exit 1
  ;;
esac
if [[ "$#" == "3" ]];then
  if [[ "$2" == "-o" ]]; then
    analysis=false
    NENS=$3
  elif [[ "$3" == "-o" ]];then
    analysis=false
    NENS=$2
  else
    analysis=true
    NENS=$2
  fi
elif [[ "$#" == "2" ]]; then 
  if [[ "$2" == "-o" ]]; then
    analysis=false
    NENS=5
  else
    analysis=true
    NENS=$2
  fi
else
  analysis=true
  NENS=5
fi

echo "Analysis: " $analysis
echo "NENS    : " $NENS

#########################################################
export expdir=$PWD
rm -rf RESULTS/
mkdir RESULTS
mkdir RESULTS/ANALYSIS
mkdir RESULTS/ISBA

dtg=$start
mode=1

cp NAMELISTS/OPTIONS.nam_ENKF_1  NAMELISTS/OPTIONS.nam_ENKF
while [ $dtg -lt $end ]; do


  dtgtmp=$dtg
  dtgtmp=`echo $dtgtmp | cut -c1-4`"-"`echo $dtgtmp | cut -c5-6`"-"`echo $dtgtmp | cut -c7-8`" "`echo $dtgtmp | cut -c9-10`":00"
  dtg2=`date -u --date "$dtgtmp 24 hours" +%Y%m%d%H`

  hh=`echo $dtg | cut -c9-10`

  echo $dtg

  ./run_soda.sh $exp $dtg $mode $NENS $analysis || exit 1

  sed -e "s/LENS_GEN = T/LENS_GEN = F/g" NAMELISTS/OPTIONS.nam_ENKF_1 > NAMELISTS/OPTIONS.nam_ENKF

#  if [[ "$analysis" = "yes" ]]; then
#    cp $expdir/RUN/RUN_OFFLINE_PERT_00/ISBA_PROGNOSTIC.OUT.nc $outpath/offline/ISBA_PROGNOSTIC.OUT.nc_$dtg || exit 1
#    cp $expdir/RUN/RUN_EKF/ISBA_PROGNOSTIC.OUT.nc $outpath/analysis/soda_analysis.nc_$dtg2 || exit 1
#    mv $expdir/OUTPUT/PREP_SODA_EKF.nc $expdir/OUTPUT/PREP_SODA.nc      || exit 1
#  else  
#    cp $expdir/RUN/RUN_OFFLINE/ISBA_PROGNOSTIC.OUT.nc $outpath/$offdir/ISBA_PROGNOSTIC.OUT.nc_$dtg || exit 1
#	mv $expdir/OUTPUT/PREP_OFFLINE.nc $expdir/OUTPUT/PREP_SODA.nc || exit 1
#  fi
  

  mode=0
  dtgtmp=$dtg
  dtgtmp=`echo $dtgtmp | cut -c1-4`"-"`echo $dtgtmp | cut -c5-6`"-"`echo $dtgtmp | cut -c7-8`" "`echo $dtgtmp | cut -c9-10`":00"
  dtg=`date -u --date "$dtgtmp 24 hours" +%Y%m%d%H`
done

