NC_PATH_enkf=/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EnKF/time_series/
NC_PATH_ekf=/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EKF/time_series/

.PHONY: enkf_dat
enkf_dat: 
	Rscript Rscript/read_timeseries_output.R $(NC_PATH_enkf) dat/EnKF/ WG2 T T T T





# Automated variables
# $< is first dependency
# $^ is all dependencies
# $@ is the target

## hell
#.PHONY: all
#all: timeserie figures/diffr.pdf

#.PHONY: timeserie
#timeserie: Makefile.soda Makefile.openloop 
#	make -f Makefile.soda -j 4
#	make -f Makefile.openloop -j 4

#.PHONY: dats
#dats: $(SODA_DIR)/WG1.dir $(OPENLOOP_DIR)/WG1.dir

#figures/diffr.pdf: $(DAT_FILES) Rscript/compare.R
#	Rscript Rscript/compare.R $(DAT_FILES) $@ $(POINTS)


.PHONY : clean
clean:
	

## self documenting makefile
.PHONY : help
help: Makefile
	@sed -n 's/^##//p' $<

