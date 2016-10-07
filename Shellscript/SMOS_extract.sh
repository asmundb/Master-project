#!/bin/bash

# download files using
# wget -r ftp://c1e96d:xov8ng28@eftp.ifremer.fr/SM/GRIDDED/L3SM/RE04/MIR_CLF31A/2014/@DAY@

# extract ncfiles from tgz files

files=( `ls -d -1 $PWD/**/**/**/**/**/**/**/**/*` )

if [ ! -d $PWD/nc ]; then
  mkdir nc
fi

for f in ${files[@]};
do 
  tar -xf $f --wildcards --no-anchored '*.nc' -C $PWD/nc/ || exit 1
done

