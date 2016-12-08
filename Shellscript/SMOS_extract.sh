#!/bin/bash

# extract ncfiles from tgz files

work="/disk1/asmundb/SMOS"
cd $work
files=( `ls -d -1 eftp.ifremer.fr/SM/GRIDDED/L3SM/OPER/**/**/**/*` )

if [ ! -d ./nc ]; then
  mkdir nc
fi

echo "$work/nc/"
for f in ${files[@]};
do 
  ff=${f%.tgz}
  ff=${ff##*/}
#  echo $ff.DBL.nc
  echo $f
  if [ ! -f nc/$ff.DBL.nc ]; then 
    echo "hei"
    tar -x -k -f $f --wildcards --no-anchored '*.nc' -C ./nc || exit 1
  fi
done


#SM_OPER_MIR_CLF31A_20161016T000000_20161016T235959_300_001_7.DBL.nc  SM_OPER_MIR_CLF31A_20161016T000000_20161016T235959_300_001_7.tgz
