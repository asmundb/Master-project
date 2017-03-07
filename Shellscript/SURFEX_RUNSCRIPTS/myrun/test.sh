#!/bin/bash

dir=20170228_run/

#CASE1
#arr="0.053098,  0.057232,  0.060209,  0.063886,  0.068012,  0.074807,  0.088081"

#CASE2 
#arr="0.035399,  0.038154,  0.040139,  0.042590,  0.045341,  0.049871,  0.058721"

#CASE3
#arr="0.017699,  0.019077,  0.020070,  0.021295,  0.022671,  0.024936,  0.029360"



#sed -e "s/BMAT/$arr/g" NAMELISTS/OPTIONS.nam_test_ekf > NAMELISTS/OPTIONS.nam_EKF
#sed -e "s/BMAT/$arr/g" NAMELISTS/OPTIONS.nam_test_enkf > NAMELISTS/OPTIONS.nam_ENKF_1

#./soda_loop.sh EKF -o
#mv RESULTS/ /disk1/asmundb/RESULTS/$dir/openloop

#./soda_loop.sh EKF
#mv RESULTS/ /disk1/asmundb/RESULTS/$dir/ekf

#./soda_loop.sh ENKF 5 -o
#mv RESULTS/ /disk1/asmundb/RESULTS/$dir/openloop_ens_5

#./soda_loop.sh ENKF 5
#mv RESULTS/ /disk1/asmundb/RESULTS/$dir/enkf_5
