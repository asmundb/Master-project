#!/bin/bash


#ln -sf /disk1/asmundb/SMOS/OBSERVATIONS/* INPUT/

source /home/asmundb/SURFEX2/open_SURFEX_V8_0/conf/profile_surfex-LXgfortran-SFX-V8-0-0-NOMPI-OMP-DEBUG-ASSIM_NILU

########################################################
############ USER INPUT ################################
start=2016050100
end=2016050400
DA="yes" #Data assimilation: DA="yes"
        #open loop:         DA="no"


#########################################################
export expdir=$PWD
outpath=time_series/

if [[ "$DA" = "yes" ]]; then
  offdir="offline"
else
  offdir="openloop"
fi


dtg=$start
mode=1
analysis="yes"


while [ $dtg -lt $end ]; do


  dtgtmp=$dtg
  dtgtmp=`echo $dtgtmp | cut -c1-4`"-"`echo $dtgtmp | cut -c5-6`"-"`echo $dtgtmp | cut -c7-8`" "`echo $dtgtmp | cut -c9-10`":00"
  dtg2=`date -u --date "$dtgtmp 6 hours" +%Y%m%d%H`

  hh=`echo $dtg | cut -c9-10`

  case $DA in
    "yes")
#	  outpath=time_series/
      if [ $hh -eq 06 -o  $hh -eq 18 ]; then
      echo "skip this "
      analysis="no"
    else
      analysis="yes"
    fi
  ;;
    "no")
#	outpath=time_series/p
    analysis="no"
  
  esac

  echo "analysis: "$analysis   
  echo $dtg

  ./run_soda_smos.sh $dtg $mode $analysis || exit 1

  if [[ "$analysis" = "yes" ]]; then
    cp $expdir/RUN/RUN_OFFLINE_PERT_00/ISBA_PROGNOSTIC.OUT.nc $outpath/offline/ISBA_PROGNOSTIC.OUT.nc_$dtg || exit 1
    cp $expdir/RUN/RUN_EKF/ISBA_PROGNOSTIC.OUT.nc $outpath/analysis/soda_analysis.nc_$dtg2 || exit 1
    mv $expdir/OUTPUT/PREP_SODA_EKF.nc $expdir/OUTPUT/PREP_SODA.nc      || exit 1
  else  
    cp $expdir/RUN/RUN_OFFLINE/ISBA_PROGNOSTIC.OUT.nc $outpath/$offdir/ISBA_PROGNOSTIC.OUT.nc_$dtg || exit 1
	mv $expdir/OUTPUT/PREP_OFFLINE.nc $expdir/OUTPUT/PREP_SODA.nc || exit 1
  fi

  mode=0
  dtgtmp=$dtg
  dtgtmp=`echo $dtgtmp | cut -c1-4`"-"`echo $dtgtmp | cut -c5-6`"-"`echo $dtgtmp | cut -c7-8`" "`echo $dtgtmp | cut -c9-10`":00"
  dtg=`date -u --date "$dtgtmp 6 hours" +%Y%m%d%H`
done
