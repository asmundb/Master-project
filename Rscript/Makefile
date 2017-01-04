PATH_enkf=/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EnKF/time_series/
PATH_ekf=/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EKF/time_series/
PATH_homemade=/home/asmundb/SURFEX2/EXPERIMENTS/homemade/time_series/
PATH_DIF=/home/asmundb/SURFEX2/EXPERIMENTS/SODA_DIF/time_series/
PATH_obs=/disk1/asmundb/SMOS/OBSERVATIONS/

VAR=WG1


#prog=R CMD BATCH
prog=Rscript
runopt= >/dev/null


.PHONY : all
all: enkfdats ekfdats homemadedats obsdats difdats

.PHONY : difdats
difdats: dat/DIF/*.dat

dat/DIF/$(VAR).%.dat: $(PATH_DIF)/%/*
	$(prog) read_timeseries_output.R $(PATH_DIF) $@ $(VAR) $* $(runopt)


.PHONY : enkfdats
enkfdats: dat/EnKF/*.dat 

dat/EnKF/$(VAR).%.dat: $(PATH_enkf)/%/*
	$(prog) read_timeseries_output.R $(PATH_enkf) $@ $(VAR) $* $(runopt)

.PHONY : ekfdats
ekfdats: dat/EKF/*.dat

dat/EKF/$(VAR).%.dat: $(PATH_ekf)/%/*
	$(prog) read_timeseries_output.R $(PATH_ekf) $@ $(VAR) $* $(runopt)

.PHONY : homemadedats
homemadedats: dat/homemade/*.dat

dat/homemade/$(VAR).%.dat: $(PATH_homemade)/%/*
	$(prog) read_timeseries_output.R $(PATH_homemade) $@ $(VAR) $* $(runopt)

.PHONY : obsdats
obsdats: dat/obs/*.dat

dat/obs/$(VAR).%.dat: $(PATH_obs)/*
	$(prog) read_timeseries_output.R $(PATH_ekf) $@ $(VAR) $* $(runopt)


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

