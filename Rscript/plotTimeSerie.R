# Load variables from .dat file
# and plot a timeserie
#

source("functions.R")
library("ncdf4")


var <- "WG1"
nprt<- 5

#args <- commandArgs(trailingOnly=T)
#nargs <- length(args) 
#if (nargs == 0){
#  stop("")
#} else if (nargs == 1) {
#  var.dat <- args[1]
#  var <- gsub(".dat","",var.dat)
#  filename <- sprintf("%s.pdf",var)
#} else {
#  var.dat <- args[1]
#  filename<- args[2]
#  points2plot <- as.numeric(c(args[3:nargs]))
#}


# List nc files
path.nc         <- "/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EnKF/time_series/"
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
cat("\r","reading observations...                ")
obs <- array(dim=c(0,8))
for (i in 1:nfiles.obs){
		  obs <- rbind(obs, read_OBSERVATION(files.obs[i])[[1]])
}
flush.console()


cat("\r","reading analyses...                   ")
# analysis
var.analysis <- list()
if (nfiles.analysis > 0 ){
    for (prt in 1:nprt){
    var.analysis[[prt]] <- get_analysis(files.analysis[prt],var)
  }
  if (nfiles.analysis > nprt){
    prt <- 1
    for (i in (nprt+1):nfiles.analysis){
      var.analysis[[prt]] <- rbind(var.analysis[[prt]], get_analysis(files.analysis[i],var))
      prt <- prt+1
	  if (prt > nprt) { prt <- 1}
    }
  }
  } else {
    var.analysis <- NA
}

flush.console()
cat("\r","reading offline...                     ")
# offline
var.offline <- list()
for (prt in 1:nprt){
  var.offline[[prt]] <- get_var(files.offline[prt],var)
}
if (nfiles.offline > nprt){
  prt <- 1
  for (i in (nprt+1):nfiles.offline){
    var.offline[[prt]] <- rbind(var.offline[[prt]], get_var(files.offline[i],var))
    prt <- prt+1
    if (prt > nprt) { prt <- 1}
  }
}
flush.console()
cat("\n")

############################################################################
######## plot ##############################################################

npoints <- dim(stations)[1]
nanalysis <- dim(var.analysis[[1]])[1]
ntimes <- dim(var.offline[[1]])[1]
nobs <- dim(obs)[1]


points2plot <- 1:8

col = rainbow(npoints)

#pdf(filename)

#ylim = c( min(c(min(var.analysis), min(var.offline)), na.rm=T),
#          max(c(max(var.analysis), max(var.offline)), na.rm=T))
ylim = c(0,1)

plot(NA,xlim=c(0,ntimes),ylim=ylim)

for (i in points2plot){
  for (j in 1:(ntimes/6)){
    for (prt in 1:nprt){
      lines(1:6+(j-1)*6, var.offline[[prt]][1:6+(j-1)*6,i],col=col[i])
    }
  }
# lines(c(6,7), c(var.analysis[1,i], var.offline[7,i]),col=col[i])
# lines(c(12,13), var.offline[12:13,i],col=col[i])
# lines(c(18,19), c(var.analysis[2,i], var.offline[19,i]),col=col[i])
  if (nanalysis > 0){
    for (prt in 1:nprt){
      points(seq(6,nanalysis*12,12),var.analysis[[prt]][,i],col=col[i])
	}
  }
#
}
#
if (var == "WG1"){		
  for (i in 1:nobs){
    points(rep(12*(i-1)+6,8), obs[i,], bg=col, pch=24)
  }
}
#points(rep(18,8), obs2, bg=col, pch=24)

#abline(v=c(6,18),lwd=0.1)

#legend("topleft", legend=dimnames(stations)[[1]], fill=col)
#legend("bottomleft", legend=c("SMOS", "analysis","offline"),

#dev.off()
