PATH_enkf=/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EnKF/time_series/
PATH_ekf=/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EKF/time_series/
PATH_homemade=/home/asmundb/SURFEX2/EXPERIMENTS/homemade/time_series/
PATH_obs=/disk1/asmundb/SMOS/OBSERVATIONS/

VAR=WG2

.PHONY : all
all: enkfdats ekfdats homemadedats obsdats


.PHONY : enkfdats
enkfdats: dat/EnKF/*.dat 

dat/EnKF/$(VAR).%.dat: $(PATH_enkf)/%/*
	Rscript read_timeseries_output.R $(PATH_enkf) $@ $(VAR) $*

.PHONY : ekfdats
ekfdats: dat/EKF/*.dat

dat/EKF/$(VAR).%.dat: $(PATH_ekf)/%/*
	Rscript read_timeseries_output.R $(PATH_ekf) $@ $(VAR) $*

.PHONY : homemadedats
homemadedats: dat/homemade/*.dat

dat/homemade/$(VAR).%.dat: $(PATH_homemade)/%/*
	Rscript read_timeseries_output.R $(PATH_homemade) $@ $(VAR) $*

.PHONY : obsdats
obsdats: dat/obs/*.dat

dat/obs/$(VAR).%.dat: $(PATH_obs)/*
	Rscript read_timeseries_output.R $(PATH_ekf) $@ $(VAR) $*


# Automated variables
# $< is first dependency
# $^ is all dependencies
# $@ is the target
# $* in action corresponds to % wildcard in target/dependecy

#.PHONY : clean
#clean:
	

## self documenting makefile
#.PHONY : help
#help: Makefile
#	@sed -n 's/^##//p' $<

