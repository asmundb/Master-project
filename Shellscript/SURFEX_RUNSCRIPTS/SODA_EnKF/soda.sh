#!/bin/bash
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --partition=medium
#SBATCH --mem 15000

set -x

RUNDIR=$(pwd)
#execdir=/home/jostein/SURFEX-LDAS/trunk/MY_RUN/rundir/
execdir=$RUNDIR/RUN
#
#run in tmpdir/ASSIM
if [ ! -d $execdir/EXP2 ]
then
	mkdir $execdir/EXP2
fi
cd $execdir/EXP2 || exit 1
rm -f *
#
#
#links all useful files for SODA
#
ln -s $RUNDIR/*.bin .
#
ln -f $RUNDIR/PGD.nc .
ln -f $RUNDIR/PREP* .    #### ???????????????????
#cp $RUNDIR/OUTPUT/SURFOUT_*.nc .
#rename s/SURFOUT/PREP/ SURFOUT*
#
ln -f $RUNDIR/OBSERVATION*.DAT .
#
ln -s $RUNDIR/OPTIONS.nam_S OPTIONS.nam

cp -f $RUNDIR/OUTPUT/RV_*.DAT .
#
#
#links the soda executable
ln -s $RUNDIR/soda.exe .
#
#MPI_TASKS=1
#
export OMP_NUM_THREADS=1
#
#export OMP_STACKSIZE=13m
#
#uncomment to use DRHOOK
#export DR_HOOK=1
#export DR_HOOK_OPT=prof
#
#ulimit -s unlimited
#
#run SODA on 8 MPI task and 1 OMP thread
#srun  -o soda -e soda -N1 -n${MPI_TASKS} -c${OMP_NUM_THREADS} ./soda.exe

echo "Before run?"

./soda.exe > soda || exit 1
#wait 

echo "After run?"
#
#ls -l
#
#mv -f drhook.prof.1 $RUNDIR
#get the output and listing
mv -f LISTING_SODA*.txt $RUNDIR/LISTING
mv -f soda $RUNDIR/LISTING 


rename s/SURFOUT/SURFOUT_/ SURFOUT*

#mv -f PREP_1.nc $RUNDIR/OUTPUT2/SURFOUT_1.nc
#mv -f PREP_2.nc $RUNDIR/OUTPUT2/SURFOUT_2.nc
#mv -f PREP_3.nc $RUNDIR/OUTPUT2/SURFOUT_3.nc
#mv -f PREP_4.nc $RUNDIR/OUTPUT2/SURFOUT_4.nc
#mv -f PREP_5.nc $RUNDIR/OUTPUT2/SURFOUT_5.nc

#mv -f SURFOUT*.* $RUNDIR
#
aa=$1
mm=$2
jj=$3
NT=$4

ls SURFOUT*
for file in SURFOUT_*.nc
do
  ens=`echo $file | cut -c9`
  cp $file $RUNDIR/time_series/analysis/soda_analysis.nc_20${aa}${mm}${jj}${NT}_ens_$ens || exit 1
#  mv ISBA_PROGNOSTIC.OUT.nc $RUNDIR/time_series/offline/ISBA_PROGNOSTIC_${aa}${mm}${jj}${NT}.nc_EnKF
done

#tar cvf assim_$aa$mm${jj}${NT}.tar ISBA_PROGNOSTIC.OUT_${aa}${mm}${jj}${NT}.nc
#gzip assim_$aa$mm${jj}${NT}.tar
#mv -f assim*.tar.gz $RUNDIR


#mv -f SURFEX_RESULTS SURFEX_RESULTS_${aa}${mm}${jj}${NT} 
#tar cvf increments_$aa$mm${jj}${NT}.tar SURFEX_RESULTS_${aa}${mm}${jj}${NT}
#gzip increments_$aa$mm${jj}${NT}.tar

#mv -f increments_*.tar.gz $RUNDIR
mv -f SURFOUT_* $RUNDIR/
mv -f RV_*.DAT $RUNDIR/OUTPUT
#
#
cd $RUNDIR
#
