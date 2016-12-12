# Load variables from .dat file
# and plot a timeserie
#
source("functions.R")

####################### USER VARIABLES #################################

Tstart  <- 2016050100    # from
Tend    <- 2016050300    # to
station <- 3             # point(s) to plot in same figure. if 0 plot all
pltObs  <- T             # if TRUE, plot observations
pltAna  <- T             # if TRUE, plot analysis
pltOff  <- T             # if TRUE, plot prognosis
pltPrt  <- 1:5  #c(1,2,4)  # ensemble members to plot. if 0 plot all

# specify .dat files to plot
anaFile <- "dat/EnKF/var.analysis.dat"
offFile <- "dat/EnKF/var.offline.dat"
obsFile <- "dat/obs/obs.dat"

########################################################################

####################### LOAD FILES #####################################
analysis <- readRDS(anaFile)
offline  <- readRDS(offFile)
obs      <- readRDS(obsFile)

####################### SET PARAMETERS #################################
# make sure data are consistent
if (dim(analysis)[3] != dim(offline)[3] | dim(analysis)[3] != dim(obs)[2]) {
  print("data inconsistency: number of stations differ!")
  stop()
}else{
  npoints  <- dim(analysis)[3]
}
if (station == 0){
  station <- 1:npoints
}


if (dim(analysis)[2] != dim(offline)[2]) {
  print("data inconsistency: number of perturbations differ!")
  stop()
}else{
  nprt  <- dim(analysis)[2]
}
if (length(pltPrt) == 1 || pltPrt == 0){
  pltPrt <- 1:nprt
}


#npoints    <- length(station)
nanalysis  <- dim(analysis)[1]
noffline   <- dim(offline)[1]
nobs       <- dim(obs)[1]

assimWindow<- noffline/nanalysis  # hours between analyses 6 for enkf, 12 for ekf
obsfreq    <- 12 #noffline/nobs

times      <- as.numeric(dimnames(offline)[[1]]) # time vector
ntimes     <- length(times)


if (any(times == Tstart) & any(times == Tend)){
  tstart <- which(times == Tstart)
  tend   <- which(times == t)
} else if (any(times == Tstart) & ! any(times == Tend)) {
  tstart <- which(times == Tstart)
  tend   <- ntimes 
} else if (! any(times == Tstart) & any(times == Tend)) {
  tstart <- 1
  tend   <- which(times == Tend)
} else {
  tstart <- 1
  tend   <- ntimes
}
pltTimes <- tstart:tend



####################### REMOVE EXTREME VALUES ##########################
obs[which(obs == 999)] <- NA


####################### PLOT ###########################################

col = rainbow(npoints)

#pdf(filename)

ylim = c( min(c(min(unlist(analysis)), min(unlist(offline)), min(obs,na.rm=T)), na.rm=T),
          max(c(max(unlist(analysis)), max(unlist(offline)), max(obs,na.rm=T)), na.rm=T))
ylim = c(0,1)

plot(NA,xlim = c(tstart,tend), ylim=ylim)


for (point in station){
  for (j in 1:(length(pltTimes)/assimWindow)){
    for (prt in pltPrt){
	  if (pltOff){
        lines(tstart:assimWindow + (j-1)*assimWindow,
		      offline[tstart:assimWindow + (j-1)*assimWindow,prt,point],
        #lines(1:assimWindow+(j-1)*assimWindow,
#        offline[1:assimWindow+(j-1)*assimWindow,prt,point],
              col=col[point])
	  }
    }
  }
  if (pltAna){
    for (prt in pltPrt){
       points(seq(tstart+assimWindow-1,tend,assimWindow), analysis[tstart:(tend/assimWindow),prt,point],col=col[point])
#      points(seq(assimWindow,nanalysis*2*assimWindow,2*assimWindow),analysis[,prt,point],col=col[point])
	}
  }
}
#
if (pltObs) {
  for (i in station){
#    points(seq(tstart + ,tend,obsfreq), obs[tstart:tend,point],bg=col[point], pch=24)
    points(seq(obsfreq,nobs*2*obsfreq,2*obsfreq),obs[,point],bg=col[point], pch=24)
  }
}
#legend("topleft", legend=dimnames(stations)[[1]], fill=col)
#legend("bottomleft", legend=c("SMOS", "analysis","offline"),

#dev.off()
