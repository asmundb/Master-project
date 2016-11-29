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
  echo $f
  tar -xf $f --wildcards --no-anchored '*.nc' -C ./nc/ || exit 1
done

