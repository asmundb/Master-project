source("read_ISBA.R")


### Variables to be read ###
prog_vars <- c("WG1","WG2","WG3","WG4","WG5","WG6","WG7", "TG1","TG2")
diag_vars <- c("LE_ISBA","H_ISBA","RN_ISBA","T2M_ISBA","HU2M_ISBA")

#########################################################################################
### SMAP ###
path <- "/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs038_2/ISBA/"
files1 <- list.files(path,
                    pattern="ISBA_PROGNOSTIC.OUT.nc",
                    recursive=T,
                    full.names=T)
prog_smap <- load_isba(files1, prog_vars)

files2 <- list.files(path,
                    pattern="ISBA_DIAGNOSTICS.OUT.nc",
                    recursive=T,
                    full.names=T)

diag_smap <- load_isba(files2, diag_vars)

### SMOS ###
path <- "/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMOS/SEKF/obs05_2/ISBA/"
files1 <- list.files(path,
                    pattern="ISBA_PROGNOSTIC.OUT.nc",
                    recursive=T,
                    full.names=T)
prog_smos <- load_isba(files1, prog_vars)

files2 <- list.files(path,
                    pattern="ISBA_DIAGNOSTICS.OUT.nc",
                    recursive=T,
                    full.names=T)

diag_smos <- load_isba(files2, diag_vars)

### Open loop ###
path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/SPINUP/ISBA/"
files1 <- list.files(path,
                    pattern="ISBA_PROGNOSTIC.OUT.nc",
                    recursive=T,
                    full.names=T)[125:492]
prog_ol <- load_isba(files1, prog_vars)

files2 <- list.files(path,
                    pattern="ISBA_DIAGNOSTICS.OUT.nc",
                    recursive=T,
                    full.names=T)[125:492]

diag_ol <- load_isba(files2, diag_vars)
#########################################################################################







