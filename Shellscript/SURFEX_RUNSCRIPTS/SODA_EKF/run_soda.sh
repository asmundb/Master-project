#!/bin/bash

#
# TEST of SODA with smos data
# 

################################
### USER DEPENDENT VARIABLE ####
################################
CLIMDATA=/disk1/climdata/PGD/
CLIMDATA=/lustre/storeB/project/nwp/surfex/PGD/
################################
################################
################################

if [ $# -ne 3 -a $# -ne 4 ]; then
  echo "Usage: $0 EXP CSURF_FILETYPE NPROC [mode]"
  echo "CSURF_FILETYPE = LFI/ASCII/NC"
  echo "mode=0 No PGD or PREP"
  echo "mode=1 PGD and PREP"
  echo "mode=2 Only PGD"
  echo "mode=3 Only PREP"
  exit 1
else
  export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
  exp=$1
  dtg=2008093018
  extra=
  extra_pgd=
  extra_prep=
  if [ "$exp" == "EKF_PATCH" ]; then
    exp="EKF"
    dtg=2008093018
    extra=
    extra_pgd="_patch"
    extra_prep=""
  elif [ "$exp" == "EKF_PATCH_POINT" ]; then
    exp="EKF"
    dtg=2015082500
    extra="_point"
    extra_pgd="_patch_point"
    extra_prep="_point"
  elif [ "$exp" == "EKF_PATCH_POINTS" ]; then # TEST WITH THIS
    exp="EKF"
    dtg=2015082800
    extra="_point"
    extra_pgd="_patch_points"
    extra_prep="_points"
 elif [ "$exp" == "EKF_POINT" ]; then
    exp="EKF"
    dtg=2015082500
    extra="_point"
    extra_pgd="_point"
    extra_prep="_point"
 elif [ "$exp" == "EKF_POINTS" ]; then
    exp="EKF"
    dtg=2015082800
    extra="_point"
    extra_pgd="_points"
    extra_prep="_points"
  elif [ "$exp" == "EKF_VARASSIM" ]; then
    exp="EKF"
    dtg=2007071006
    extra="_varassim"
    extra_pgd="_varassim"
    extra_prep="_varassim"
  elif [ "$exp" == "OI_POINT" ]; then
    exp="OI"
    dtg=2015082500
    extra="_point"
    extra_pgd="_point"
    extra_prep="_point"
  elif [ "$exp" == "OI_POINTS" ]; then
    exp="OI"
    dtg=2015082800
    extra="_point"
    extra_pgd="_points"
    extra_prep="_points"
  fi
  CSURF_FILETYPE=$2
  case $CSURF_FILETYPE in
    "LFI"|"ASCII"|"NC"|"FA")
    ;;
    *)
      echo "Not supported CSURF_FILETYPE: $CSURF_FILETYPE"
      exit 1
    ;; 
  esac
  nproc=$3
  # Some sanity checks
  if [ "$SRC_SURFEX" == "" -o "$XYZ" == "" ]; then
    echo "You need to source the config file before calling this script!"
    exit 1
  fi
  script="$SRC_SURFEX/MY_RUN/SODA/TESTS/$exp.sh"
  if [ ! -f $script ]; then
    echo "Invalid test experiment $exp"
    echo "$script not found"
    exit 1
  fi
  mode=1
  if [ $# -eq 4 ]; then
    mode=$4
    case $mode in 
      0|1|2|3)
      ;;
      *)
       echo "Invalid mode $mode"
       echo "mode=0 No PGD or PREP"
       echo "mode=1 PGD and PREP"
       echo "mode=2 Only PGD"
       echo "mode=3 Only PREP"
       exit 1
      ;;
    esac
  fi
  # Some sanity checks
  if [ "$SRC_SURFEX" == "" -o "$XYZ" == "" ]; then
    echo "You need to source the config file before calling this script!"
    exit 1
  fi
  suffix=""
  case $CSURF_FILETYPE in
    "ASCII")
       suffix="txt"
    ;;
    "LFI")
       suffix="lfi"
    ;;
    "NC")
       suffix="nc"
    ;;
    "FA")
       suffix="fa"
    ;;
    *)
      echo "Filetype is not defined: $CSURF_FILETYPE"
      exit 1
    ;; 
  esac
fi

#
# PGD
#
case $mode in 
  1|2)
    work=$SRC_SURFEX/MY_RUN/SODA/RUN/RUN_PGD
    [ -d $work ] && rm -r $work
    mkdir $work
    cd $work || exit 1

    # Copy namelists
    cat $SRC_SURFEX/MY_RUN/SODA/NAMELISTS/OPTIONS.nam_$suffix $SRC_SURFEX/MY_RUN/SODA/NAMELISTS/OPTIONS.nam_pgd$extra_pgd > OPTIONS.nam.tmp
    sed -e "s/CFORCING_FILETYPE=CFORCING_FILETYPE/CFORCING_FILETYPE=\"ASCII \"/" OPTIONS.nam.tmp > OPTIONS.nam

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
    [ ! -f $SRC_SURFEX/exe/PGD$XYZ ] && echo "$SRC_SURFEX/exe/PGD$XYZ not found" && exit 1
    $SRC_SURFEX/exe/PGD$XYZ || exit 1
 
    if [ -f PGD_SODA.$suffix ]; then
      [ -d $SRC_SURFEX/MY_RUN/SODA/OUTPUT ] || mkdir $SRC_SURFEX/MY_RUN/SODA/OUTPUT
      mv PGD_SODA.$suffix $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PGD_SODA.$suffix
    else
      echo "File PGD_SODA.$suffix does not exist!"
      exit 1
    fi
esac

#
# PREP
#
case $mode in
  1|3)
    work=$SRC_SURFEX/MY_RUN/SODA/RUN/RUN_PREP
    [ -d $work ] && rm -r $work
    mkdir $work
    cd $work || exit 1

    # Copy namelists
    cat $SRC_SURFEX/MY_RUN/SODA/NAMELISTS/OPTIONS.nam_$suffix $SRC_SURFEX/MY_RUN/SODA/NAMELISTS/OPTIONS.nam_prep$extra_prep > OPTIONS.nam.tmp
    sed -e "s/CFORCING_FILETYPE=CFORCING_FILETYPE/CFORCING_FILETYPE=\"ASCII \"/" OPTIONS.nam.tmp > OPTIONS.nam

    # Ecoclimap
    ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
    ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .

    # Copy PGD file
    [ -d $SRC_SURFEX/MY_RUN/SODA/OUTPUT ] || mkdir $SRC_SURFEX/MY_RUN/SODA/OUTPUT
    cp $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PGD_SODA.$suffix .

    # Copy input grib file
    cp $SRC_SURFEX/MY_RUN/SODA/INPUT/ECMWF_GRIB_FILE .

    # Make PREP
    [ ! -f $SRC_SURFEX/exe/PREP$XYZ ] && echo "$SRC_SURFEX/exe/PREP$XYZ not found" && exit 1
    $SRC_SURFEX/exe/PREP$XYZ || exit 1

    if [ -f PREP_SODA.$suffix ]; then
      mv PREP_SODA.$suffix $SRC_SURFEX/MY_RUN/SODA/OUTPUT/PREP_SODA.$suffix
    else
      echo "File PREP_SODA.$suffix does not exist!"
      exit 1
    fi
esac

# Run SODA test (With single or multiple offline runs)
if [ -f $script ]; then
  $script $dtg $suffix $nproc $extra || exit 1
else
  echo "Script $script for test $exp not found"
  exit 1
fi

