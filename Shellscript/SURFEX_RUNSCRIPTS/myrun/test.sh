#!/bin/bash

./soda_loop.sh EKF -o
mv RESULTS/ /disk1/asmundb/RESULTS/openloop

./soda_loop.sh EKF
mv RESULTS/ /disk1/asmundb/RESULTS/ekf

./soda_loop.sh ENKF 5 -o
mv RESULTS/ /disk1/asmundb/RESULTS/openloop_ens_5

./soda_loop.sh ENKF 5
mv RESULTS/ /disk1/asmundb/RESULTS/enkf_5
