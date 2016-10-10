#!/bin/bash

# download files using
# wget -r ftp://c1e96d:xov8ng28@eftp.ifremer.fr/SM/GRIDDED/L3SM/RE04/MIR_CLF31A/2014/@DAY@

# extract ncfiles from tgz files

work="/disk1/asmundb/SMOS/"
files=( `ls -d -1 $work/eftp.ifremer.fr/SM/GRIDDED/L3SM/RE04/MIR_CLF31D/**/**/*` )

if [ ! -d $work/nc ]; then
  mkdir nc
fi

for f in ${files[@]};
do 
  echo $f
  tar -xf $f --wildcards --no-anchored '*.nc' -C $work/nc/ || exit 1
done

