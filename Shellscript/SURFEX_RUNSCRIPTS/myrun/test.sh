#!/bin/bash

dir=normSMOS

#./soda_loop.sh EKF -o
#mv RESULTS/ /disk1/asmundb/RESULTS/$dir/openloop

#./soda_loop.sh EKF
#mv RESULTS/ /disk1/asmundb/RESULTS/$dir/ekf

./soda_loop.sh ENKF 5 -o
mv RESULTS/ /disk1/asmundb/RESULTS/$dir/openloop_ens_5

./soda_loop.sh ENKF 5
mv RESULTS/ /disk1/asmundb/RESULTS/$dir/enkf_5
