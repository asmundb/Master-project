#!/bin/bash


# Some sanity checks
if [ "$SRC_SURFEX" == "" -o "$XYZ" == "" ]; then
  echo "You need to source the config file before calling this script!"
  exit 1
else
  dtg=$1
  suffix=$2
  nproc=$3
  extra=$4
  [ ! -d $expdir ] && echo "The directory $expdir was not found" && exit 1
  [ ! -f $SRC_SURFEX/exe/SODA$XYZ ] && echo "The needed binary $SRC_SURFEX/exe/SODA$XYZ was not found" && exit 1
fi

fcint=06
npert=4
forc_format="NETCDF"

YYYY=`echo $dtg | cut -c1-4`
YY=`echo $dtg | cut -c3-4`
MM=`echo $dtg | cut -c5-6`
DD=`echo $dtg | cut -c7-8`
HH=`echo $dtg | cut -c9-10`
dtgtmp=`echo $dtg | cut -c1-4`"-"`echo $dtg | cut -c5-6`"-"`echo $dtg | cut -c7-8`" "`echo $dtg | cut -c9-10`":00"
dtgtmp=`date -u --date "$dtgtmp $fcint hours" +%Y%m%d%H`
YYYY_RES=`echo $dtgtmp | cut -c1-4`
YY_RES=`echo $dtgtmp | cut -c3-4`
MM_RES=`echo $dtgtmp | cut -c5-6`
DD_RES=`echo $dtgtmp | cut -c7-8`
HH_RES=`echo $dtgtmp | cut -c9-10`

work=$expdir/RUN/RUN_EKF
[ -d $work ] && rm -r $work
mkdir $work
cd $work || exit 1

# Copy namelists
cat $expdir/NAMELISTS/OPTIONS.nam_$suffix $expdir/NAMELISTS/OPTIONS.nam_soda_EKF$extra > OPTIONS.nam

# Ecoclimap
ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .

# Copy PGD file
cp  $expdir/OUTPUT/PGD_SODA.$suffix .

# Copy input grib files
#cp $expdir/INPUT/FG_OI_MAIN .
#cp $expdir/INPUT/CANARI* .
cp $expdir/INPUT/OBSERVATIONS_* .
#cp $expdir/INPUT/SST_SIC* .
#cp $expdir/INPUT/clim_isba .
#cp $expdir/INPUT/FIRST_GUESS* .
#ln -sf $expdir/INPUT/CLIMATE.DAT_$dtg CLIMATE.DAT
#ln -sf $expdir/INPUT/LSM.DAT_$dtg LSM.DAT

# Run control + perturbed runs
pert=0
while [ $pert -le $npert ]; do
  mbr="_PERT_"$(printf "%.2d" "$pert")

  # Perturb initial state
  # Copy the initial PREP file that should be perturbed
  cp $expdir/OUTPUT/PREP_SODA.$suffix $expdir/OUTPUT/PREP_SODA$mbr.$suffix

  # Run (perturbed ) offline
  offline="$expdir/TESTS/OFFLINE.sh"
  if [ -f $offline ]; then
    $offline $dtg $fcint $pert $suffix $forc_format $nproc $extra || exit 1
  else
    echo "No script $offline found"
    exit 1
  fi

 echo "# Copy stored simulation results"
  cp $expdir/OUTPUT/PREP_OFFLINE$mbr.$suffix PREP_EKF_PERT$pert.$suffix
  ln -s PREP_EKF_PERT$pert.$suffix PREP_${YY_RES}${MM_RES}${DD_RES}H${HH_RES}_EKF_PERT$pert.$suffix

  pert=$(( $pert + 1 ))
done

# Copy first guess. In this case OFFLINE PREP file (control run)
cp $expdir/OUTPUT/PREP_OFFLINE_PERT_00.$suffix PREP_SODA.$suffix
ln -sf PREP_SODA.$suffix PREP_INIT.$suffix

###################
## Do EKF steps
###################

## Evolve B matrix - could be reset to BO periodically (test to be defined)
#
##
## Get BGROUND from the previous cycle
##
#
echo "Assuming we are evolving B matrix from initial state for first cycle with assimilation"

#
## Soil analyis
#

sed -e "s/LBEV=LBEV/LBEV=.FALSE./" \
    -e "s/LPRT=LPRT/LPRT=.FALSE./" \
    -e "s/LSIM=LSIM/LSIM=.FALSE./" \
    -e "s/NIVAR=NIVAR/NIVAR=1/" \
    $expdir/NAMELISTS/OPTIONS.nam_soda_EKF$extra > OPTIONS.nam.tmp1
cat $expdir/NAMELISTS/OPTIONS.nam_$suffix OPTIONS.nam.tmp1 > OPTIONS.nam.tmp2
sed -e "s/CFORCING_FILETYPE=CFORCING_FILETYPE/CFORCING_FILETYPE=\"$forc_format\"/" OPTIONS.nam.tmp2 > OPTIONS.nam

# Run SODA for EKF
[ ! -f $SRC_SURFEX/exe/SODA$XYZ ] && echo "$SRC_SURFEX/exe/SODA$XYZ not found" && exit 1
mpirun -np $nproc $SRC_SURFEX/exe/SODA$XYZ || exit 1

ls -1 BGROUNDout_LBEV.* > /dev/null 2>&1
if [ $? -eq 0 ]; then
  for f in `ls -1 BGROUNDout_LBEV.*`; do
    mv $f $expdir/OUTPUT/$f
  done
fi

ls -1 BGROUNDout_ASSIM.* > /dev/null 2>&1
if [ $? -eq 0 ]; then
  for f in `ls -1 BGROUNDout_ASSIM.*`; do
    mv $f $expdir/OUTPUT/$f
  done
fi

ls -1 LTM_del*_del* > /dev/null 2>&1
if [ $? -eq 0 ]; then
  for f in `ls -1 LTM_del*_del* 2>&1 > /dev/null`; do
    mv $f $expdir/OUTPUT/${f}_$YYYY$MM$DD$HH.DAT
  done
fi

# Save output
#mv SURFOUT.${YYYY_RES}${MM_RES}${DD_RES}_${HH_RES}h00.$suffix $expdir/OUTPUT/PREP_SODA_EKF.$suffix || exit 1
mv SURFOUT.$suffix $expdir/OUTPUT/PREP_SODA_EKF.$suffix || exit 1

