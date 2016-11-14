# Read NetCDF files created by SURFEX ( ISBA_PROGNOSTIC.OUT.nc )
# data output: (ntimes, nvars, npoints)
# if file is soda_analysis, ntimes=1 
#
#
#
#
library("ncdf4")
source("/home/asmundb/Master-project/Rscript/functions.R")

args <- commandArgs(trailingOnly=T)
if (length(args) == 0){
  stop("please specify variable name as argument")
} else if (length(args) == 1) {
#  var <- args[1]
#  filename <- sprintf("%s.dat",var)
  stop("3 argumens required")
} else {
  var <- args[1]
  filename <- args[2]
  path.ts <- args[3]
}

#### get files ####
# SURFEX output
#path.ts  <- "/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EKF/openloop_timeseries"
files.analysis  <- list.files(path=path.ts, pattern="soda_analysis*",full.name=T) # test if nc mabye
nfiles.analysis <- length(files.analysis)
files.offline  <- list.files(path=path.ts, pattern="soda_offline*",full.name=T)
nfiles.offline  <- length(files.offline)

# stations/points
stations <- read_stlist(stationlist="/home/asmundb/Master-project/Rscript/stationlist.cfg")

# SMOS observation
obspath <- "/disk1/asmundb/SMOS/OBSERVATIONS"
obsfiles <- list.files(path=obspath,pattern="OBSERVATIONS*", full.name=T)
nobsfiles <- length(obsfiles)

#### read files ####
cat("\r","reading observations...                ")
obs <- array(dim=c(0,8))
for (i in 1:nobsfiles){
  obs <- rbind(obs, read_OBSERVATION(obsfiles[i])[[1]])
}
flush.console()

cat("\r","reading analyses...                   ")
# analysis
if (nfiles.analysis > 0 ){
  var.analysis <- get_var(files.analysis[1],var)
  for (i in 2:nfiles.analysis){
    var.analysis <- rbind(var.analysis, get_var(files.analysis[i],var))
  }
} else {
  var.analysis <- NA
}

flush.console()
cat("\r","reading offline...                     ")
# offline
var.offline <- get_var(files.offline[1],var)
for (i in 2:nfiles.offline){
    var.offline <- rbind(var.offline, get_var(files.offline[i],var))
}
flush.console()
#### save to file ####
cat("\r","saving variables...                      ")
save(var.analysis, var.offline, obs, file=filename)

cat("\r", sprintf("done: variables saved to %s        ",filename))
cat("\n")

#######################################################################
