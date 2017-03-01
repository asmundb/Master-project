#!/bin/bash

#
# TEST of SODA with smos data
 
#set -x
################################
### USER DEPENDENT VARIABLE ####
################################
CLIMDATA=/disk1/asmundb/climdata/PGD/
#CLIMDATA=/lustre/storeB/project/nwp/surfex/PGD/

################################
################################
################################

export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
exp=$1  # EKF|ENKF|OPENLOOP 
dtg=$2  #  
extra="_point"
extra_pgd="_points"
extra_prep="_points"

mode=$3

CSURF_FILETYPE="NC"
nproc=1
suffix="nc"
#npert=5
fcint=06
forc_format="nc"
analysis=$5

eval `grep NVAR NAMELISTS/OPTIONS.nam_$exp | sed 's/,//' `

# number of perturbations/ensemle members
case $exp in
  "OPENLOOP")
#    analysis=false
    npert=$4
  ;;
  "EKF")
#    eval `grep NVAR NAMELISTS/OPTIONS.nam_EKF | sed 's/,//' `
	npert=$NVAR
#	analysis=true
  ;;
  "ENKF")
    npert=$4
#	analysis=true
  ;;
  *)
    echo "WHAT?"
	exit 1
  ;;
esac



# Start date
yyyymmddhh=$dtg
yy=`echo $yyyymmddhh | cut -c3-4`
mm=`echo $yyyymmddhh | cut -c5-6`
dd=`echo $yyyymmddhh | cut -c7-8`
hh=`echo $yyyymmddhh | cut -c9-10`

# Analysis date
dtgtmp=`echo $dtg | cut -c1-4`"-"`echo $dtg | cut -c5-6`"-"`echo $dtg | cut -c7-8`" "`echo $dtg | cut -c9-10`":00"
dtgtmp=`date -u --date "$dtgtmp $fcint hours" +%Y%m%d%H`
YYYY_RES=`echo $dtgtmp | cut -c1-4`
YY_RES=`echo $dtgtmp | cut -c3-4`
MM_RES=`echo $dtgtmp | cut -c5-6`
DD_RES=`echo $dtgtmp | cut -c7-8`
HH_RES=`echo $dtgtmp | cut -c9-10`






#
##########################  PGD  ##################################################################
#
case $mode in
  1|2)
    work=$expdir/RUN/RUN_PGD
    [ -d $work ] && rm -r $work
    mkdir $work
    cd $work || exit 1


    # Copy namelists
	cat $expdir/NAMELISTS/OPTIONS.nam_$suffix $expdir/NAMELISTS/OPTIONS.nam_pgd$extra_pgd > OPTIONS.nam


    #  Databases (SYSTEM DEPENDENT)
    if [ ! -d $CLIMDATA ]; then
      echo "You have to set your path for the climate databases correct!"
      exit 1
    else
      ln -sf $CLIMDATA/* .
	fi 
    # Ecoclimap
	ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
	ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .


    # Make PGD
#   [ ! -f $SRC_SURFEX/exe/PGD$XYZ ] && echo "$SRC_SURFEX/exe/PGD$XYZ not found" && exit 1
#   $SRC_SURFEX/exe/PGD$XYZ || exit 1
    $expdir/EXE/pgd.exe || exit 1

    if [ -f PGD_SODA.$suffix ]; then
	  [ -d $expdir/OUTPUT ] || mkdir /$expdir/OUTPUT
	  mv PGD_SODA.$suffix $expdir/OUTPUT/PGD_SODA.$suffix
	else
	  echo "File PGD_SODA.$suffix does not exist!"
	  exit 1
	fi
esac

#
############################  PREP  ###############################################################
#
case $mode in
  1|3)
    work=$expdir/RUN/RUN_PREP
    [ -d $work ] && rm -r $work
    mkdir $work
    cd $work || exit 1

    # Copy namelists
	cat $expdir/NAMELISTS/OPTIONS.nam_$suffix  \
	    $expdir/NAMELISTS/OPTIONS.nam_prep$extra_prep > OPTIONS.nam.tmp
    sed -e "s/YYYY0/20$yy/" OPTIONS.nam.tmp  > OPTIONS.nam.tmp2
    sed -e "s/MM0/$mm/"     OPTIONS.nam.tmp2 > OPTIONS.nam.tmp
    sed -e "s/DD0/$dd/"     OPTIONS.nam.tmp  > OPTIONS.nam.tmp2
    sed -e "s/HH0/$hh/"     OPTIONS.nam.tmp2 > OPTIONS.nam

    # Ecoclimap
	ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
	ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .

	# Copy PGD file
	[ -d $expdir/OUTPUT ] || mkdir $expdir/OUTPUT
	cp $expdir/OUTPUT/PGD_SODA.$suffix .
    
    # Make PREP
#   [ ! -f $SRC_SURFEX/exe/PREP$XYZ ] && echo "$SRC_SURFEX/exe/PREP$XYZ not found" && exit 1
#   $SRC_SURFEX/exe/PREP$XYZ || exit 1
    $expdir/EXE/prep.exe || exit 1


	if [ -f PREP_SODA.$suffix ]; then
      mv PREP_SODA.$suffix $expdir/OUTPUT/PREP_SODA.$suffix
    else
      echo "File PREP_SODA.$suffix does not exist!"
      exit 1
    fi
esac

###########################  Experiment: EKF/ENKF/OPENLOOP  #######################################

work=$expdir/RUN/RUN_$exp
[ -d $work ] && rm -r $work
mkdir $work
cd $work || exit 1

ln -sf $expdir/EXE/soda.exe .

# Copy namelist
cat $expdir/NAMELISTS/OPTIONS.nam_$suffix             \
    $expdir/NAMELISTS/OPTIONS.nam_io_varassim_ekf     \
    $expdir/NAMELISTS/OPTIONS.nam_$exp > OPTIONS.nam

# Ecoclimap
ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .

# Copy PGD file
cp  $expdir/OUTPUT/PGD_SODA.$suffix .

# Link observations
if [[ "$analysis" == "true" && -f $expdir/OBSERVATIONS/OBSERVATIONS_${YY_RES}${MM_RES}${DD_RES}H${HH_RES}.DAT ]]; then
  ln -sf $expdir/OBSERVATIONS/OBSERVATIONS_${YY_RES}${MM_RES}${DD_RES}H${HH_RES}.DAT .
  echo "SMOS copied"
else 
  ln -sf $expdir/OBSERVATIONS/SMOS_Default.DAT OBSERVATIONS_${YY_RES}${MM_RES}${DD_RES}H${HH_RES}.DAT
  echo "SMOS SKIPPED!!!"
fi

#if [ ! $analysis ]; then
#  ln -sf $expdir/OBSERVATIONS/SMOS_Default.DAT OBSERVATIONS_${YY_RES}${MM_RES}${DD_RES}H${HH_RES}.DAT
#fi

########################### Perturbed offline runs ################################################
# enkf do not need a unperturbed control run
pert=0
kind="PERT"
case $exp in
  "ENKF")
    pert=1
	kind="ENS"
esac

#if [[ (! $analysis) && ("$npert" -le "1") ]]; then
#  pert=0
#elif [[ (! $analysis) && ("$npert" -gt "1") ]]; then
#  pert=1
#  kind="ENS"
#fi


while [ $pert -le $npert ]; do
  mbr="_${kind}_"$(printf "%.2d" "$pert")

  # Perturb initial state
  # Copy the initial PREP file that should be perturbed
  if [[ $mode -gt 0 ]]; then
      cp $expdir/OUTPUT/PREP_SODA.$suffix $expdir/OUTPUT/PREP_SODA$mbr.$suffix
	  echo "initial cycle"
  fi
  # Run (perturbed ) offline
#  offline="$expdir/offline_$exp.sh"
  offline="$expdir/offline.sh"
  if [ -f $offline ]; then
    $offline $dtg $fcint $pert $suffix $forc_format $nproc $extra $exp || exit 1
  else
    echo "No script $offline found"
    exit 1
  fi

 echo "# Copy stored simulation results"
  cp $expdir/OUTPUT/PREP_OFFLINE$mbr.$suffix PREP_EKF_$kind$pert.$suffix
  ln -s PREP_EKF_$kind$pert.$suffix PREP_${YY_RES}${MM_RES}${DD_RES}H${HH_RES}_EKF_$kind$pert.$suffix

  pert=$(( $pert + 1 ))
done


#############################  Analysis  ##########################################################

case $exp in
  "EKF")
    cp $expdir/OUTPUT/PREP_OFFLINE_PERT_00.$suffix PREP_SODA.$suffix
    ln -sf PREP_SODA.$suffix PREP_INIT.$suffix
esac

case $exp in
  "EKF")
    cat $expdir/NAMELISTS/OPTIONS.nam_$suffix         \
        $expdir/NAMELISTS/OPTIONS.nam_obs             \
        $expdir/NAMELISTS/OPTIONS.nam_io_varassim_ekf \
        $expdir/NAMELISTS/OPTIONS.nam_EKF    > OPTIONS.nam.tmp
    sed -e "s/LBEV=LBEV/LBEV=.FALSE./" \
        -e "s/LPRT=LPRT/LPRT=.FALSE./" \
        -e "s/LSIM=LSIM/LSIM=.FALSE./" \
        -e "s/NIVAR=NIVAR/NIVAR=1/" \
        OPTIONS.nam.tmp > OPTIONS.nam.tmp1
    sed -e "s/CFORCING_FILETYPE=CFORCING_FILETYPE/CFORCING_FILETYPE=\"$forc_format\"/" \
	    OPTIONS.nam.tmp1 > OPTIONS.nam
  ;;
  "ENKF")
    cat $expdir/NAMELISTS/OPTIONS.nam_$suffix         \
        $expdir/NAMELISTS/OPTIONS.nam_obs             \
        $expdir/NAMELISTS/OPTIONS.nam_ENKF    > OPTIONS.nam.tmp
    sed -e "s/CPREPFILE=\"PREP_SODA\"/CPREPFILE=\"PREP_${YY_RES}${MM_RES}${DD_RES}H${HH_RES}_EKF_ENS1\"/g" \
	OPTIONS.nam.tmp > OPTIONS.nam
  ;;
  "OPENLOOP")
    echo "nothing?"
  ;;
esac

case $exp in
  "EKF" | "ENKF")
    ./soda.exe || exit 1
  ;;
  "OPENLOOP")
    echo "pease"
esac
#############################  Save results  ######################################################
rm -fr $expdir/RESULTS/ANALYSIS/$dtgtmp
mkdir $expdir/RESULTS/ANALYSIS/$dtgtmp
rm -fr $expdir/RESULTS/ISBA/$dtg
mkdir $expdir/RESULTS/ISBA/$dtg

case $exp in
  "EKF")
    cp $expdir/RUN/RUN_OFFLINE_PERT_00/ISBA_PROGNOSTIC.OUT.nc $expdir/RESULTS/ISBA/$dtg/
	cp $expdir/RUN/RUN_OFFLINE_PERT_00/ISBA_DIAGNOSTICS.OUT.nc $expdir/RESULTS/ISBA/$dtg/
    cp $expdir/RUN/RUN_EKF/*.0000001 $expdir/RESULTS/ANALYSIS/$dtgtmp/
	cp $expdir/RUN/RUN_EKF/HO_* $expdir/RESULTS/ANALYSIS/$dtgtmp/
    for prt in $(seq -f "%02g" 0 $npert); do
      cp $expdir/RUN/RUN_EKF/SURFOUT.nc $expdir/OUTPUT/PREP_SODA_PERT_$prt.nc
    done
  ;;
  "ENKF")
    cp $expdir/RUN/RUN_ENKF/SURFEX_RESULTS $expdir/RESULTS/ANALYSIS/$dtgtmp/
	cp $expdir/RUN/RUN_ENKF/*.0000001 $expdir/RESULTS/ANALYSIS/$dtgtmp/
	enss=1
    for ens in $(seq -f "%02g" 1 $npert); do
      cp $expdir/RUN/RUN_OFFLINE_ENS_$ens/ISBA_PROGNOSTIC.OUT.nc  \
	     $expdir/RESULTS/ISBA/$dtg/ISBA_PROGNOSTIC.OUT.nc_$ens
      cp $expdir/RUN/RUN_OFFLINE_ENS_$ens/ISBA_DIAGNOSTICS.OUT.nc \
	     $expdir/RESULTS/ISBA/$dtg/ISBA_DIAGNOSTICS.OUT.nc_$ens
	  cp $expdir/RUN/RUN_ENKF/SURFOUT$enss.nc $expdir/OUTPUT/PREP_SODA_ENS_$ens.$suffix
	  enss=$(( enss + 1 ))
	done
  ;;
  "OPENLOOP")
    if [[ $npert -le 1 ]]; then
      cp $expdir/RUN/RUN_OFFLINE_PERT_00/ISBA_PROGNOSTIC.OUT.nc $expdir/RESULTS/ISBA/$dtg/
      cp $expdir/RUN/RUN_OFFLINE_PERT_00/ISBA_DIAGNOSTICS.OUT.nc $expdir/RESULTS/ISBA/$dtg/
    else 
	  for ens in $(seq -f "%02g" 1 $npert); do
        cp $expdir/RUN/RUN_OFFLINE_ENS_$ens/ISBA_PROGNOSTIC.OUT.nc  \
           $expdir/RESULTS/ISBA/$dtg/ISBA_PROGNOSTIC.OUT.nc_$ens 
        cp $expdir/RUN/RUN_OFFLINE_ENS_$ens/ISBA_DIAGNOSTICS.OUT.nc \
           $expdir/RESULTS/ISBA/$dtg/ISBA_DIAGNOSTICS.OUT.nc_$ens
      done
    fi
   ;;
esac
case $exp in
  "EKF" | "ENKF")
    mv -f $expdir/RUN/RUN_ENKF/RV_*.DAT $expdir/RV/
    cp $expdir/RUN/RUN_$exp/SURFOUT* $expdir/RESULTS/ANALYSIS/$dtgtmp/
esac


