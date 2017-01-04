#!/bin/bash
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --partition=medium
#SBATCH --mem 15000


RUNDIR=$(pwd)
#execdir=/home/jostein/SURFEX-LDAS/trunk/MY_RUN/rundir/
execdir=/home/asmundb/SURFEX2/EXPERIMENTS/homemade/RUN/
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
ln -f $RUNDIR/PREP* .
#
ln -f $RUNDIR/OBSERVATIONS.DAT .
#
ln -s $RUNDIR/OPTIONS.nam_S OPTIONS.nam

cp -f $RUNDIR/RV_*.DAT .
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
#ls -ltr
echo "Before run?"
#mpirun -np 1 ./soda.exe > soda
./soda.exe > soda || exit 1
#wait
#ls -ltr
echo "After run?"
#
#ls -l
#
#mv -f drhook.prof.1 $RUNDIR
#get the output and listing
mv -f assim.txt $RUNDIR/LISTING
mv -f soda $RUNDIR/LISTING 


#mv -f SURFOUT.nc $RUNDIR
mv -f PREP_1.nc $RUNDIR/SURFOUT_1.nc
mv -f PREP_2.nc $RUNDIR/SURFOUT_2.nc
mv -f PREP_3.nc $RUNDIR/SURFOUT_3.nc
mv -f PREP_4.nc $RUNDIR/SURFOUT_4.nc
mv -f PREP_5.nc $RUNDIR/SURFOUT_5.nc


#
aa=$1
mm=$2
jj=$3
NT=$4

ens=1
for file in $RUNDIR/SURFOUT_*
do 
  cp $file $RUNDIR/time_series/analysis/soda_analysis.nc_${aa}${mm}${jj}${NT}_ens_$ens
  ens=$((ens+1))
done

#tar cvf assim_$aa$mm${jj}${NT}.tar ISBA_PROGNOSTIC.OUT_${aa}${mm}${jj}${NT}.nc
#gzip assim_$aa$mm${jj}${NT}.tar
#mv -f assim*.tar.gz $RUNDIR


#mv -f SURFEX_RESULTS SURFEX_RESULTS_${aa}${mm}${jj}${NT} 
#tar cvf increments_$aa$mm${jj}${NT}.tar SURFEX_RESULTS_${aa}${mm}${jj}${NT}
#gzip increments_$aa$mm${jj}${NT}.tar

#mv -f increments_*.tar.gz $RUNDIR

mv -f RV_*.DAT $RUNDIR
#
#
cd $RUNDIR
#
