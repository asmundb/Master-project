SODA_DIR=datfiles/soda
OPENLOOP_DIR=datfiles/openloop

DAT_FILES= $(SODA_DIR)/WG1.dat $(OPENLOOP_DIR)/WG1.dat

POINTS= 1 2 3 4 5 6 7 8

# Automated variables
# $< is first dependency
# $^ is all dependencies
# $@ is the target

## hell
## helloo
.PHONY: all
all: timeserie figures/diffr.pdf

.PHONY: timeserie
timeserie: Makefile.soda Makefile.openloop 
	make -f Makefile.soda -j 4
	make -f Makefile.openloop -j 4

#.PHONY: dats
#dats: $(SODA_DIR)/WG1.dir $(OPENLOOP_DIR)/WG1.dir

figures/diffr.pdf: $(DAT_FILES) Rscript/compare.R
	Rscript Rscript/compare.R $(DAT_FILES) $@ $(POINTS)


.PHONY : clean
clean:
	make -f Makefile.soda clean
	make -f Makefile.openloop clean
			

## self documenting makefile
.PHONY : help
help: Makefile
	@sed -n 's/^##//p' $<

