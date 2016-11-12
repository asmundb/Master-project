DAT_DIR=datfiles
PDF_DIR=figures
SRC=Rscript

NC_FILES=$(wildcard /home/asmundb/SURFEX2/EXPERIMENTS/SODA_EKF/time_series/soda_*)
DAT_FILES=$(wildcard $(DAT_DIR)/*.dat)
VARS=$(patsubst $(DAT_DIR)/%.dat, %, $(DAT_FILES))
PDF_FILES=$(patsubst $(DAT_DIR)/%.dat, $(PDF_DIR)/%.pdf, $(DAT_FILES))
OUT=$(PDF_DIR)
prog=Rscript                  # could be R, python or whatever
plotScript=$(SRC)/plot_timeserie.R
createDatScript=$(SRC)/read_nc.R

# Automated variables
# $< is first dependency
# $^ is all dependencies
# $@ is the target

.PHONY: all
all: pdfs

## dat files
.PHONY : dats
dats: $(DAT_FILES)

# declare dependencies and run script 
$(DAT_DIR)/%.dat:  $(NC_FILES) $(createDatScript) 
	$(prog) $(createDatScript) $* $@ 

## pdf files
.PHONY : pdfs
pdfs: $(PDF_FILES)

## make pdfs with plotScript
$(PDF_DIR)/%.pdf: $(DAT_DIR)/%.dat $(plotScript)
	$(prog) $(plotScript) $< $@


.PHONY : clean
clean:
	rm -f datfiles/*.dat
	rm -f figures/*.pdf

.PHONY : variables
variables:
	@echo DAT_FILES is now: $(DAT_FILES)
	@echo PNG_FILES is now: $(PDF_FILES)
#	@echo NC_FILES  is now: $(NC_FILES)
	@echo VARS      is now: $(VARS) 

## self documenting makefile
.PHONY : help
help: Makefile
	@sed -n 's/^##//p' $<

