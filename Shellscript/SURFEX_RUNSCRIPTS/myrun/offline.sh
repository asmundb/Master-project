#!/bin/bash



exp=$8

case $exp in
  "EKF")
    LPRT=".FALSE."
    IVAR="1"
    kind="PERT"
  ;;
  "ENKF")
    kind="ENS"
  ;;
  *)
    echo "what?"
  ;;
esac



#if [ $# -ne 6 -a $# -ne 7 ]; then
#  echo "Usage: $0 dtg pert suffix forc_format nproc [extra]"
#else
  dtg=$1
  fcint=$2
  mbr=""
  if [ $3 -ge 0 ]; then
    pert=$3
    mbr="_${kind}_"$(printf "%.2d" "$pert")
    echo "Running offline for perturbation $pert"
    if [ "$pert" -gt 0 ]; then
      LPRT=".TRUE."
      IVAR="$pert"
    fi
  else
    echo "Running offline"
  fi
  suffix=$4
  forc_format=$5
  nproc=$6
  extra=$7
#fi

yyyy=`echo $dtg | cut -c1-4`
yy=`echo $dtg | cut -c3-4`
mm=`echo $dtg | cut -c5-6`
dd=`echo $dtg | cut -c7-8`
hh=`echo $dtg | cut -c9-10`

dtgtmp=$dtg
dtgtmp=`echo $dtgtmp | cut -c1-4`"-"`echo $dtgtmp | cut -c5-6`"-"`echo $dtgtmp | cut -c7-8`" "`echo $dtgtmp | cut -c9-10`":00"
dtg2=`date -u --date "$dtgtmp $fcint hours" +%Y%m%d%H`
yyyy2=`echo $dtg2 | cut -c1-4`
yy2=`echo $dtg2 | cut -c3-4`
mm2=`echo $dtg2 | cut -c5-6`
dd2=`echo $dtg2 | cut -c7-8`
hh2=`echo $dtg2 | cut -c9-10`

work=$expdir/RUN/RUN_OFFLINE$mbr
[ -d $work ] || mkdir $work
cd $work || exit 1

ln -sf $expdir/EXE/offline.exe .


# Copy namelists
cat $expdir/NAMELISTS/OPTIONS.nam_offline                \
    $expdir/NAMELISTS/OPTIONS.nam_io_varassim_offline    \
    $expdir/NAMELISTS/OPTIONS.nam_$exp  > OPTIONS.nam.tmp

case $exp in
  "ENKF")
    sed -e "s/NIE = 0/NIE = $pert/g" \
	    -e "s/LPRT=LPRT/LPRT=.FALSE./g"   OPTIONS.nam.tmp > OPTIONS.nam.tmp1
  ;;
  "EKF")
    sed -e "s/NIVAR=NIVAR/NIVAR=$IVAR/" \
	    -e "s/LPRT=LPRT/LPRT=$LPRT/g"     OPTIONS.nam.tmp > OPTIONS.nam.tmp1
  ;;
esac

sed -e "s/LASSIM=.TRUE./LASSIM=.FALSE./" \
    OPTIONS.nam.tmp1 > OPTIONS.nam.tmp
cat $expdir/NAMELISTS/OPTIONS.nam_$suffix OPTIONS.nam.tmp > OPTIONS.nam
eval `grep NVAR OPTIONS.nam | sed 's/,//' `

case $exp in
  "ENKF")
    ivar=1
#	echo $NVAR
    while [[ $ivar -le $NVAR ]]; do
#	  echo "RV_00${ivar}_00$pert.DAT"
      cp -f $expdir/RV/RV_00${ivar}_00$pert.DAT ./RV_00$ivar.DAT
      ivar=$(( ivar + 1 ))
    done
esac

# Ecoclimap
ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.bin .
ln -sf $SRC_SURFEX/MY_RUN/ECOCLIMAP/*.dat .

# Copy Forcing

echo "copy FORCING_$dtg"
if [ -f $expdir/FORCING/FORCING.nc_$dtg* ]; then
  cp $expdir/FORCING/FORCING.nc_$dtg* FORCING.nc
else
  echo "No forcing found: $expdir/FORCING/FORCING.nc_$dtg"
  exit 1
fi

# Copy PGD file
cp  $expdir/OUTPUT/PGD_SODA.$suffix .

# Copy OFFLINE PREP file
cp  $expdir/OUTPUT/PREP_SODA$mbr.$suffix PREP_SODA.$suffix

# Run OFFLINE
#[ ! -f $SRC_SURFEX/exe/OFFLINE$XYZ ] && echo "$SRC_SURFEX/exe/OFFLINE$XYZ not found" && exit 1
#mpirun -np $nproc $SRC_SURFEX/exe/OFFLINE$XYZ || exit 1
./offline.exe  || exit 1


#output=SURFOUT.${yyyy2}${mm2}${dd2}_${hh2}h00.$suffix
output=SURFOUT.$suffix

if [ -f $output ]; then
  mv $output $expdir/OUTPUT/PREP_OFFLINE$mbr.$suffix
else
  echo "Output file $output from SURFEX not found"
  exit 1
fi
