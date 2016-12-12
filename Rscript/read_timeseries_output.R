# Load variables from nc files and saves result to .dat
#
source("functions.R")
library("ncdf4")


############### USER VARIABLES ########################################
var      <- "WG1"  # surfex variable name
nprt     <- 5  # number of perturbations/members of experiment

# path to time_series/ dir with model output
#path.nc <- "/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EKF/time_series/"
path.nc  <- "/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EnKF/time_series/"

path.out <- "dat/EnKF"
########################################################################




########################################################################
# List nc files
files.analysis  <- list.files(path=paste(path.nc,"analyses",sep=""),full.name=T) # test if nc mabye
nfiles.analysis <- length(files.analysis)
files.offline   <- list.files(path=paste(path.nc,"offline",sep=""),full.name=T)
nfiles.offline  <- length(files.offline)


# get station info
stations        <- read_stlist(stationlist="/home/asmundb/Master-project/Rscript/stationlist.cfg")

# SMOS observation
obspath         <- "/disk1/asmundb/SMOS/OBSERVATIONS"
files.obs       <- list.files(path=obspath,pattern="OBSERVATIONS*", full.name=T)
nfiles.obs     <- length(files.obs)


#### read files ####
# observations
cat("\r","reading observations...                ")
obs <- array(dim=c(0,8))
for (i in 1:nfiles.obs){
		  obs <- rbind(obs, read_OBSERVATION(files.obs[i])[[1]])
}
flush.console()


cat("\r","reading analyses...                   ")
# analysis
var.analysis <- list()
pb <- txtProgressBar(min = 0, max = nfiles.analysis, style = 3)
if (nfiles.analysis > 0 ){
    for (prt in 1:nprt){
    var.analysis[[prt]] <- get_analysis(files.analysis[prt],var)
	setTxtProgressBar(pb, prt)
  }
  if (nfiles.analysis > nprt){
    prt <- 1
    for (i in (nprt+1):nfiles.analysis){
      var.analysis[[prt]] <- rbind(var.analysis[[prt]], get_analysis(files.analysis[i],var))
	  setTxtProgressBar(pb, i)
      prt <- prt+1
	  if (prt > nprt) { prt <- 1}
    }
  }
  close(pb)
  } else {
    var.analysis <- NA
}

var.analysis <- prt2pntReshape(var.analysis)


flush.console()
cat("\r","reading offline...                     ")
# offline
var.offline <- list()
pb <- txtProgressBar(min = 0, max = nfiles.offline, style = 3)
for (prt in 1:nprt){
  var.offline[[prt]] <- get_var(files.offline[prt],var)
  setTxtProgressBar(pb, prt)
}
if (nfiles.offline > nprt){
  for (i in (nprt+1):nfiles.offline){
    var.offline[[prt]] <- rbind(var.offline[[prt]], get_var(files.offline[i],var))
    prt <- prt+1
    if (prt > nprt) { prt <- 1}
	setTxtProgressBar(pb, i)
  }
}
close(pb)

var.offline  <- prt2pntReshape(var.offline)

flush.console()
cat("\n")


###################### SAVE VARIABLES ##################################

saveRDS(var.analysis, file=paste(path.out,"var.analysis.dat",sep="/"))
saveRDS(var.offline, file=paste(path.out,"var.offline.dat",sep="/"))
saveRDS(obs, file=paste(path.out,"obs.dat",sep="/"))


########################################################################
