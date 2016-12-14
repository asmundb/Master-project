# Load variables from nc files and saves result to .dat
#
source("functions.R")
library("ncdf4")


############### USER VARIABLES ########################################
var      <- "WG2"  # surfex variable name
nprt     <- 1  # number of perturbations/members of experiment
saveObs  <- F  # if true save observations to file
saveAna  <- F
saveOff  <- F
saveOpe  <- T
expr     <- "SODA_EKF"        # name of experiment

# path to time_series/ dir with model output
#path.nc <- "/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EKF/time_series/"
#path.nc  <- "/home/asmundb/SURFEX2/EXPERIMENTS/homemade/time_series/"

path.out <- sprintf("dat/EKF/", expr)
path.nc <- sprintf("/home/asmundb/SURFEX2/EXPERIMENTS/%s/time_series/",expr)

########################################################################




########################################################################
# List nc files
files.analysis  <- list.files(path=paste(path.nc,"analyses",sep=""),full.name=T) # test if nc mabye
nfiles.analysis <- length(files.analysis)
files.offline   <- list.files(path=paste(path.nc,"offline",sep=""),full.name=T)
nfiles.offline  <- length(files.offline)
files.openloop   <- list.files(path=paste(path.nc,"openloop",sep=""),full.name=T)
nfiles.openloop  <- length(files.openloop)


# get station info
stations        <- read_stlist(stationlist="/home/asmundb/Master-project/Rscript/stationlist.cfg")

# SMOS observation
obspath         <- "/disk1/asmundb/SMOS/OBSERVATIONS"
files.obs       <- list.files(path=obspath,pattern="OBSERVATIONS*", full.name=T)
nfiles.obs     <- length(files.obs)


#### read files ####
# observations
if (saveObs){
  cat("\r","reading observations...                ")
  obs <- array(dim=c(0,8))
  for (i in 1:nfiles.obs){
    obs <- rbind(obs, read_OBSERVATION(files.obs[i]))
  }
  flush.console()
}

if (saveAna){
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
}

if (saveOff){
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
}
flush.console()
cat("\n")

if (saveOpe){
  flush.console()
  cat("\r","reading openloop...                     ")
  # openloop
  var.openloop <- list()
  pb <- txtProgressBar(min = 0, max = nfiles.openloop, style = 3)
  var.openloop[[1]] <- get_var(files.openloop[1],var)
  for (i in 22:nfiles.openloop){
	  var.openloop[[1]] <- rbind(var.openloop[[1]], get_var(files.openloop[i],var))
	  setTxtProgressBar(pb, i)
  }
  close(pb)
  var.openloop  <- prt2pntReshape(var.openloop)
}
flush.console()
cat("\n")


###################### SAVE VARIABLES ##################################
if (saveAna){
  saveRDS(var.analysis, file=paste(path.out,var,".analysis.dat",sep=""))
}
if (saveOff){
  saveRDS(var.offline, file=paste(path.out,var,".offline.dat",sep=""))
}

if (saveOpe){
		  saveRDS(var.openloop, file=paste(path.out,var,".openloop.dat",sep=""))
}

if (saveObs){
  saveRDS(obs, file=paste("dat/obs","obs.dat",sep="/"))
}



########################################################################
