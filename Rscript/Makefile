SHELL := /bin/bash # Use bash syntax

PATH1=/disk1/asmundb/results2
PATH_enkf=$(PATH1)/enkf_5/
PATH_ekf=$(PATH1)/ekf/
PATH_obs=/disk1/asmundb/SMOS/OBSERVATIONS/
DATDIR=dat/postMod

#VAR=WG3


#prog=R CMD BATCH
prog= #Rscript
runopt=  #>/dev/null


.PHONY : all
all: ekfdats enkfdats obsdats 

### EnKF dat files
.PHONY : enkfdats
enkfdats: $(DATDIR)/EnKF/*.dat read_timeseries_output.R

$(DATDIR)/EnKF/%.dat: $(PATH_enkf)/**/* read_timeseries_output.R
	$(prog) ./read_timeseries_output.R $(PATH_enkf) $@ $(runopt)

### EKF dat files
.PHONY : ekfdats
ekfdats: $(DATDIR)/EKF/*.dat read_timeseries_output.R

$(DATDIR)/EKF/%.dat: $(PATH_ekf)/**/* read_timeseries_output.R
	$(prog) ./read_timeseries_output.R $(PATH_ekf) $@ $(runopt)


### obs dat files
.PHONY : obsdats 
obsdats: dat/obs/*.dat read_timeseries_output.R

dat/obs/%.dat: $(PATH_obs)/* read_timeseries_output.R
	$(prog) ./read_timeseries_output.R $(PATH_ekf) $@ $(runopt)


# Automated variables
# $< is first dependency
# $^ is all dependencies
# $@ is the target
# $* in action corresponds to % wildcard in target/dependecy

#.PHONY : clean
#clean:
	
.PHONY: variable
variable:
	@echo ${PATH}


## self documenting makefile
#.PHONY : help
#help: Makefile
#	@sed -n 's/^##//p' $<

