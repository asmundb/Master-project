#!/bin/bash

#
# TEST of SODA with smos data
# 

################################
### USER DEPENDENT VARIABLE ####
################################
CLIMDATA=/disk1/asmundb/climdata/PGD/
#CLIMDATA=/lustre/storeB/project/nwp/surfex/PGD/

################################
################################
################################

export OMP_NUM_THREADS=${OMP_NUM_THREADS:-1}
exp="EKF"
#dtg=2015082800
dtg=$1 #2016101600
extra="_point"
extra_pgd="_points"
extra_prep="_points"

analysis=$3

CSURF_FILETYPE="NC"
nproc=1


# Some sanity checks
if [ "$SRC_SURFEX" == "" -o "$XYZ" == "" ]; then
  echo "You need to source the config file before calling this script!"
  exit 1
fi

script="$expdir/TESTS/$exp.sh"

mode=$2

# Some sanity checks
if [ "$SRC_SURFEX" == "" -o "$XYZ" == "" ]; then
  echo "You need to source the config file before calling this script!"
  exit 1
fi

suffix="nc"

#
# PGD
#
case $mode in
  1|2)
    work=$expdir/RUN/RUN_PGD
    [ -d $work ] && rm -r $work
    mkdir $work
    cd $work || exit 1

    # Copy namelists
	cat $expdir/NAMELISTS/OPTIONS.nam_$suffix $expdir/NAMELISTS/OPTIONS.nam_pgd$extra_pgd > OPTIONS.nam.tmp
	sed -e "s/CFORCING_FILETYPE=CFORCING_FILETYPE/CFORCING_FILETYPE=\"NETCDF\"/" OPTIONS.nam.tmp > OPTIONS.nam


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
	  [ -d $expdir/OUTPUT ] || mkdir /$expdir/OUTPUT
	  mv PGD_SODA.$suffix $expdir/OUTPUT/PGD_SODA.$suffix
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
    work=$expdir/RUN/RUN_PREP
    [ -d $work ] && rm -r $work
    mkdir $work
    cd $work || exit 1

    # Copy namelists
	cat $expdir/NAMELISTS/OPTIONS.nam_$suffix $expdir/NAMELISTS/OPTIONS.nam_prep$extra_prep > OPTIONS.nam.tmp
	sed -e "s/CFORCING_FILETYPE=CFORCING_FILETYPE/CFORCING_FILETYPE=\"NETCDF\"/" OPTIONS.nam.tmp > OPTIONS.nam

    # Ecoclimap
	ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
	ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .

	# Copy PGD file
	[ -d $expdir/OUTPUT ] || mkdir $expdir/OUTPUT
	cp $expdir/OUTPUT/PGD_SODA.$suffix .
    
    # Make PREP
    [ ! -f $SRC_SURFEX/exe/PREP$XYZ ] && echo "$SRC_SURFEX/exe/PREP$XYZ not found" && exit 1
    $SRC_SURFEX/exe/PREP$XYZ || exit 1

	if [ -f PREP_SODA.$suffix ]; then
      mv PREP_SODA.$suffix $expdir/OUTPUT/PREP_SODA.$suffix
    else
      echo "File PREP_SODA.$suffix does not exist!"
      exit 1
    fi
esac

# Run SODA test (With single or multiple offline runs)
echo "analysis=" $analysis
if [[ "$analysis" = "yes" ]]; then
  echo "Do analysis"
  script="$expdir/TESTS/EKF.sh"
  if [ -f $script ]; then
    $script $dtg $suffix $nproc $extra  || exit 1
  else
    echo "Script $script for test $exp not found"
    exit 1
  fi
elif [[ "$analysis" = "no" ]]; then
  echo "Skip analysis"
  script="$expdir/TESTS/OFFLINE.sh"
  if [ -f $script ]; then
	$script $dtg 06 -1 $suffix "NETCDF" $nproc $extra || exit 1
  else
    echo "Script $script for test $exp not found"
    exit 1
  fi
fi
