#!/usr/bin/Rscript

# Load variables from nc files and saves result to .dat
#
source("functions.R")
library("ncdf4")


############### USER VARIABLES ########################################

args <- commandArgs(trailingOnly=T)
nargs <- length(args)


path.nc  <- args[1]                   # path to nc model output
file.out <- args[2]                   # path to save to
#var      <- args[3]                   # surfex variable name
#data     <- args[3]
var  <- regmatches(file.out,regexec("\\/([[:alnum:]]*)\\.",file.out))[[1]][2]
data <- regmatches(file.out,regexec("\\.([[:alnum:]]*)\\.",file.out))[[1]][2]

saveOpe  <- F     # if true save openloop to file
saveObs  <- F     # if true save observations to file
saveOff  <- F     # if true save prognosis to file
saveAna  <- F     # if true save analysis to file


if ( data == "analysis" ){
  saveAna  <- T
} else if (data == "offline" ){
  saveOff  <- T
} else if (data == "openloop"){
  saveOpe  <- T
} else if (data == "obs"){
  saveObs  <- T
} else {
  print("unknown data")
  stop()
}
print(path.nc)
print(file.out)
print(var)
print(data)


#saveOpe  <- as.logical(args[4])       # if true save openloop to file
#saveObs  <- as.logical(args[5])       # if true save observations to file
#saveOff  <- as.logical(args[6])       # if true save prognosis to file
#saveAna  <- as.logical(args[7])       # if true save analysis to file

########################################################################

########################################################################
# List nc files
files.analysis  <- list.files(path=paste(path.nc,"analysis",sep="/"),full.name=T) # test if nc mabye
nfiles.analysis <- length(files.analysis)
files.offline   <- list.files(path=paste(path.nc,"offline",sep="/"),full.name=T)
nfiles.offline  <- length(files.offline)
files.openloop   <- list.files(path=paste(path.nc,"openloop",sep="/"),full.name=T)
nfiles.openloop  <- length(files.openloop)

# check if ensemble
ensemble <- length(grep("ens_*", files.offline)) > 0
if ( ensemble ){
  nprt     <- max(as.numeric(substr(files.offline, nchar(files.offline), nchar(files.offline))))
} else {
  nprt <- 1
}
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
  for (prt in 1:nprt){
    var.openloop[[prt]] <- get_var(files.openloop[prt],var)
    setTxtProgressBar(pb, prt)
  }
  if (nfiles.openloop > nprt){
    for (i in (nprt+1):nfiles.openloop){
      var.openloop[[prt]] <- rbind(var.openloop[[prt]], get_var(files.openloop[i],var))
      prt <- prt+1
      if (prt > nprt) { prt <- 1}
      setTxtProgressBar(pb, i)
    }
  }
  close(pb)
  var.openloop  <- prt2pntReshape(var.openloop)
}

#  flush.console()
#  cat("\r","reading openloop...                     ")
#  # openloop
#  var.openloop <- list()
#  pb <- txtProgressBar(min = 0, max = nfiles.openloop, style = 3)
#  var.openloop[[1]] <- get_var(files.openloop[1],var)
#  for (i in 22:nfiles.openloop){
#	  var.openloop[[1]] <- rbind(var.openloop[[1]], get_var(files.openloop[i],var))
#	  setTxtProgressBar(pb, i)
#  }
#  close(pb)
#  var.openloop  <- prt2pntReshape(var.openloop)
#}
flush.console()
cat("\n")


###################### SAVE VARIABLES ##################################
if (saveAna){
  saveRDS(var.analysis, file=file.out)
}
if (saveOff){
  saveRDS(var.offline, file=file.out)
}

if (saveOpe){
  saveRDS(var.openloop, file=file.out)
}

if (saveObs){
  saveRDS(obs, file=file.out)
}



########################################################################


###################### SAVE VARIABLES ##################################
#if (saveAna){
#		  saveRDS(var.analysis, file=paste(path.out,var,".analysis.dat",sep=""))
#}
#if (saveOff){
#		  saveRDS(var.offline, file=paste(path.out,var,".offline.dat",sep=""))
#}
#
#if (saveOpe){
#		          saveRDS(var.openloop, file=paste(path.out,var,".openloop.dat",sep=""))
#}
#
#if (saveObs){
#		  saveRDS(obs, file=paste("dat/obs","obs.dat",sep="/"))
#}



########################################################################
