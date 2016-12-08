#!/bin/bash
#SBATCH -N 1
#SBATCH -n 4
#SBATCH -o offline_PP
#SBATCH -e offline_PP
#SBATCH --partition=medium
#SBATCH --mem 4000 
set -x
#in ASSIM part, OFFLINE is run batch
#
RUNDIR=$(pwd)
execdir=/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EnKF/RUN/
#
#rm *.txt
#tmpdir is specific to the job in batch mode

if [ ! -d $execdir/EXP2_PP ]
then
   mkdir $execdir/EXP2_PP
fi

cd $execdir/EXP2_PP || exit 1
rm -f *
#
#
#links all useful files for OFFLINE
#
ln -s $RUNDIR/*.bin .
#
ln -sf $RUNDIR/PGD.nc .
ln -sf $RUNDIR/PREP*.nc .  #### ?????????????????
#
cp -f $RUNDIR/FORCING.nc .
#cp -f $RUNDIR/Params_config*.txt .


if [ "1" -lt "10" ]
then
	cp -f $RUNDIR/OUTPUT/RV_001_00PP.DAT ./RV_001.DAT
	cp -f $RUNDIR/OUTPUT/RV_002_00PP.DAT ./RV_002.DAT
else
	cp -f $RUNDIR/OUTPUT/RV_001_01.DAT ./RV_001.DAT
	cp -f $RUNDIR/OUTPUT/RV_002_01.DAT ./RV_002.DAT
fi

#
#namelist is specific to the perturbation (NIVAR)
ln -s $RUNDIR/OPTIONS.nam_PP OPTIONS.nam

#
#
#links the prep executable
ln -s $RUNDIR/offline.exe .
#
#MPI_TASKS=4
#
export OMP_NUM_THREADS=1
#
export OMP_STACKSIZE=13m
#
#uncomment to use DRHOOK
#export DR_HOOK=1
#export DR_HOOK_OPT=prof
#
#ulimit -s unlimited
#

echo "Test before offline exec"
#run OFFLINE on 24 MPI task and 1 OMP thread
#srun -o offline_1 -e offline_1 -N1 -n${MPI_TASKS} -c${OMP_NUM_THREADS} ./offline.exe
./offline.exe > offline_PP  || exit 1
#
#
echo "Test after offline exec"
#mv -f drhook.prof.1 $RUNDIR/drhook.prof.1_1
#get the output and listing
mv -f LISTING_OFFLINE0.txt $RUNDIR/LISTING/LISTING_OFFLINE0_PP.txt
mv -f offline_PP $RUNDIR/LISTING 

#mv -f *.OUT.nc $RUNDIR
#mv -f *.TXT $RUNDIR

#if [ "1" -eq "0" ]
#then
aa=$1
mm=$2
jj=$3
NT=$4
#for file in *.OUT.nc
#  do 

mv ISBA_PROGNOSTIC.OUT.nc  $RUNDIR/time_series/offline/ISBA_PROGNOSTIC.OUT_${aa}${mm}${jj}${NT}_ens_PP.nc
#    mv $file ${file}_${aa}${mm}${jj}${NT}_en_1
#  done
#tar cvf output_$aa$mm${jj}${NT}_PP.tar ISBA_PROGNOSTIC.OUT_${aa}${mm}${jj}${NT}_ens_PP.nc
#gzip output_$aa$mm${jj}${NT}_PP.tar
#mv -f output*.tar.gz $RUNDIR
#fi
mv -f SURFOUT.nc $RUNDIR/SURFOUT_PP.nc

#
cd $RUNDIR
#
