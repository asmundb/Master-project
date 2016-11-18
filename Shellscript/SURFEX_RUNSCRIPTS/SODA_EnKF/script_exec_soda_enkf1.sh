#!/bin/bash
#set -x

nens=5

#repres=/scratch/work/jostein/LDAS/FRANCE/RES/EXP2

#cp -f OPTIONS.nam_INIT OPTIONS.nam

#cp -f SCRIPTS/* .

ln -sf /home/asmundb/SURFEX2/open_SURFEX_V8_0/src/dir_obj-LXgfortran-SFX-V8-0-0-NOMPI-OMP-DEBUG/ASSIM_NILU/PREP ./prep.exe
ln -sf /home/asmundb/SURFEX2/open_SURFEX_V8_0/src/dir_obj-LXgfortran-SFX-V8-0-0-NOMPI-OMP-DEBUG/ASSIM_NILU/PGD ./pgd.exe
ln -sf /home/asmundb/SURFEX2/open_SURFEX_V8_0/src/dir_obj-LXgfortran-SFX-V8-0-0-NOMPI-OMP-DEBUG/ASSIM_NILU/OFFLINE ./offline.exe
ln -sf /home/asmundb/SURFEX2/open_SURFEX_V8_0/src/dir_obj-LXgfortran-SFX-V8-0-0-NOMPI-OMP-DEBUG/ASSIM_NILU/SODA ./soda.exe
ln -sf /home/asmundb/SURFEX2/open_SURFEX_V8_0/MY_RUN/ECOCLIMAP/*.bin .

EXP=TEST

RUNDIR=$(pwd)

#repres=/scratch/work/fairbairnd/TEST


#run PGD
#./PGD.EXE
#run PREP
#./PREP.EXE
#beginning of simulation

AAAAMMJJRR_deb=2016101600
AAAAMMJJRR=$AAAAMMJJRR_deb


aa=`echo $AAAAMMJJRR | cut -c3-4`
mm=`echo $AAAAMMJJRR | cut -c5-6`
jj=`echo $AAAAMMJJRR | cut -c7-8`
RR=`echo $AAAAMMJJRR | cut -c9-10`

#end of simulation
AAAAMMJJRR_end=2016101618

#AAAMMJJRR_end=2007010109

#./GET_CANARI.sh $RUNDIR "july_2012" $aa $mm $jj $RR # What does EXP2 do here?!!!!!!

#cp /nilu_wrk/toves/surfex/obs_Norway5km_2012/SMOS_${aa}${mm}${jj}H${RR}.DAT CANARI_NATURE_${aa}${mm}${jj}H${RR}.DAT

#loop on days
while [ $AAAAMMJJRR  -le $AAAAMMJJRR_end ]; do
  echo 'Start of window'
  #remove LISTING_SODA0 and PREP_0 from previous day
  rm -f LISTING/LISTING_SODA0.txt
#  rm -f SURFOUT_1.nc SURFOUT_2.nc SURFOUT.nc

  echo $AAAAMMJJRR 
  aa0=`echo $AAAAMMJJRR | cut -c3-4`
  mm0=`echo $AAAAMMJJRR | cut -c5-6`
  jj0=`echo $AAAAMMJJRR | cut -c7-8`
  RR0=`echo $AAAAMMJJRR | cut -c9-10`

 # Forcings

  case $RR0 in
   00) NT=00;;
   06) NT=06;;
   12) NT=12;;
   18) NT=18;;
    *) echo "Error in date $1 !!"; exit 1;;
  esac

  # Get .txt forcing files for every 6'th hour

#  for var in TA QA WIND LW DIR_SW SCA_SW RAIN SNOW PS DIR CO2
#  do
#     cp /nilu_wrk/toves/surfex/forcing_EuropeBig_WRF_2012-2013/Forc_${var}_20${aa0}${mm0}${jj0}_r${NT}.txt Forc_${var}.txt
#  done
#  cp /nilu_wrk/toves/surfex/forcing_EuropeBig_WRF_2012-2013/Params_config_20${aa0}${mm0}${jj0}_r${NT}.txt Params_config.txt

  cp ./FORCING/FORCING.nc_20${aa0}${mm0}${jj0}$RR0 ./FORCING.nc

  ens=1

#  while [ $ens -le $nens ]
#  do
#	rm -f SURFOUT_${ens}.nc
#        ens=$(( $ens + 1 ))
#  done

  echo $AAAAMMJJRR

  #increment the date
  AAAAMMJJRRobs=$AAAAMMJJRR
  AAAAMMJJRR=`SMS_DATE/smsdate $AAAAMMJJRR 6`

  aa=`echo $AAAAMMJJRR | cut -c3-4`
  mm=`echo $AAAAMMJJRR | cut -c5-6`
  jj=`echo $AAAAMMJJRR | cut -c7-8`
  RR=`echo $AAAAMMJJRR | cut -c9-10`


  echo 'Run control + perturbed runs'


# Run control + perturbed runs
  ens=1
  ens2=0

  while [ $ens -le $nens ]; do
    echo 'ens', $ens

    echo "####### SODA_EKF_ENS"$ens

    cp -f OPTIONS.nam_OFF OPTIONS.nam_save

    sed -e "s/NIE = 0/NIE = $ens/g" OPTIONS.nam_save > OPTIONS.nam_$ens

    rm -f PREP_${ens}.nc

    if [ $AAAAMMJJRRobs  -gt $AAAAMMJJRR_deb ]
    then
      echo 'hellllo_next_cycle', ${ens}
      cp -f OPTIONS.nam_$ens OPTIONS.nam_save
      sed -e "s/LENS_GEN = T/LENS_GEN = F/g" OPTIONS.nam_save > OPTIONS.nam_temp   #jb: changed to T to check if we spread the ensembles for each time
	  cp -f $RUNDIR/OUTPUT/SURFOUT_${ens}.nc PREP_${ens}.nc
      sed -e "s/CPREPFILE = \"PREP\"/CPREPFILE = \"PREP_${ens}\"/g" OPTIONS.nam_temp > OPTIONS.nam_$ens
    else
      echo 'hellllo', ${ens}
      ln -sf PREP.nc PREP_${ens}.nc
    fi
    sed -e "s/PP/$ens/g" offline.sh > OFFLINE_$ens.EXE
	chmod +x OFFLINE_$ens.EXE
    ./OFFLINE_$ens.EXE $aa0 $mm0 $jj0 ${NT} ${nens}  # Need time here as well because of 6h cyclem, removed exclusive

    ens2=$ens
    ens=$(( $ens + 1 ))
  done
  while [ ! -f LISTING/LISTING_SODA0.txt ]
  do
  	var=0
    id=1
  	while [ $id -le $nens ]
	do 
      if [ -f SURFOUT_$id.nc ] 
	  then
	    var=$(( $var + 1 ))
	  fi
	  id=$(( $id + 1 ))
	done 

	if [ "$var" = "$nens" ] 
	then 

      ens=1
      while [ $ens -le $nens ]
      do
  	    mv -f SURFOUT_${ens}.nc PREP_${aa}${mm}${jj}H${RR}_EKF_ENS${ens}.nc
    	ens=$(( $ens + 1 ))
      done

      if [ $RR -eq 06 ]
      then
          cp -f INPUT/OBSERVATIONS_$aa$mm${jj}H${RR}.DAT CANARI_NATURE_$aa$mm${jj}H${RR}.DAT
      elif [ $RR -eq 18 ] 
      then
          cp -f INPUT/OBSERVATIONS_$aa$mm${jj}H${RR}.DAT CANARI_NATURE_$aa$mm${jj}H${RR}.DAT 
      else
          cp -f INPUT/SMOS_Default.DAT CANARI_NATURE_$aa$mm${jj}H${RR}.DAT 
      fi

      cp -f CANARI_NATURE_$aa$mm${jj}H${RR}.DAT OBSERVATIONS_$aa$mm${jj}H${RR}.DAT

#      cp -f OPTIONS.nam_INIT OPTIONS.nam_save
      sed -e "s/LASSIM = F/LASSIM = T/g" OPTIONS.nam_save > OPTIONS.nam_S
      cp -f OPTIONS.nam_S OPTIONS.nam_save
      sed -e "s/CPREPFILE = \"PREP\"/CPREPFILE = \"PREP_${aa}${mm}${jj}H${RR}_EKF_ENS1\"/g" OPTIONS.nam_save > OPTIONS.nam_S
      echo "####### SODA_ASSIM " 
      ./soda.sh $aa $mm $jj $RR || exit 1


#      mv -f output*.tar.gz /home/jostein/scratch_data/ASSIM_EUROPE/$EXP/
#      mv -f increments_*.tar.gz /home/jostein/scratch_data/ASSIM_EUROPE/$EXP/
#      mv -f SURFEX_RESULTS /home/jostein/scratch_data/ASSIM_EUROPE/$EXP/SURFEX_Results_$aa0$mm0${jj0}H${RR0}
      # mv -f output_*.tar.gz $repres pdh
#      rm -f PREP_$aa0$mm0${jj0}*.nc

      echo 'finished soda'
      rm -f CANARI_NATURE_$aa0$mm0${jj0}H${RR0}.DAT
      rm -f OBSERVATIONS_$aa0$mm0${jj0}H${RR0}.DAT  

      #save output analysed files
      echo 'debug 3'
#      mv -f assim_$aa0$mm0${jj0}${NT}.tar.gz /home/jostein/scratch_data/ASSIM_EUROPE/$EXP/
      echo 'debug 4'
      #rm -f assim_$aa0$mm0${jj0}.tar.gz 
      rm -f *.OUT*

      #save surfout analysed file only once a day
#      if [ $RR -eq 06 ]
 #     then
#         tar cvf prep_$aa0$mm0${jj0}${NT}.tar PREP_*_EKF_ENS*.nc 
#         gzip prep_$aa0$mm0${jj0}${NT}.tar
#         mv -f prep_$aa0$mm0${jj0}${NT}.tar.gz /home/jostein/scratch_data/ASSIM_EUROPE/$EXP/
#      fi
      echo 'debug 5'
#      rm -f PREP_*_EKF_ENS*.nc
	  ens=1
	  while [ $ens -le $nens ]
      do
        mv PREP_${aa}${mm}${jj}H${RR}_EKF_ENS${ens}.nc $RUNDIR/OUTPUT/SURFOUT_$ens.nc
        ens=$(( $ens + 1 ))
      done

      #./PUT_RES.sh $RUNDIR "EXP2" $aa0 $mm0 $jj0 $NT
      echo 'debug 6'
      
      ens=1
      #while [ $ens -le $nens ]
      #do
      #    cp -f SURFOUT${ens}.nc $repres/SURFOUT${ens}_$aa$mm${jj}.nc
      #    ens=$(( $ens + 1 ))
      #done
      fi
#  echo 'debug 7'
  done
# echo 'debug 8'
done

echo 'debug 9'
echo $PWD
ls OPTIONS.nam
[ -f OPTIONS.nam ] &&  cp -f OPTIONS.nam OPTIONS.nam_save
sed -e "s/LENS_GEN = F/LENS_GEN = T/g" OPTIONS.nam_save > OPTIONS.nam

rm -f *.EXE
rm -f PREP_*.nc
rm -f OBSERVATIONS_*
rm -f CANARI_NATURE*
rm -f OPTIONS.nam_1 OPTIONS.nam_2 OPTIONS.nam_3 OPTIONS.nam_4 OPTIONS.nam_5 OPTIONS.nam_temp OPTIONS.nam_save OPTIONS.nam_S
