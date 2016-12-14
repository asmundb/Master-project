# Load variables from .dat file
# and plot a timeserie
#
source("functions.R")

####################### USER VARIABLES #################################

Tstart  <- 2015070100    # from
Tend    <- 2016050400    # to
station <- 0           # point(s) to plot in same figure. if 0 plot all
pltObs  <- F             # if TRUE, plot observations
pltAna  <- F             # if TRUE, plot analysis
pltOff  <- F             # if TRUE, plot prognosis
pltOpe  <- T
pltPrt  <- 1  #c(1,2,4)  # ensemble members to plot. if 0 plot all
expr    <- "EnKF"        # name of experiment


# specify .dat files to plot
anaFile <- sprintf("dat/%s/WG2.analysis.dat",expr)
offFile <- sprintf("dat/%s/WG2.offline.dat",expr)
opeFile <- sprintf("dat/%s/WG2.openloop.dat",expr)
obsFile <- "dat/obs/obs.dat"

########################################################################

# bother?

if (! any(pltObs, pltAna, pltOpe, pltOff) ) {
  print("No data is selected, aborting script")
  stop()
}




####################### LOAD FILES #####################################

openloop <- readRDS(opeFile)
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

####################### REMOVE EXTREME VALUES ##########################
obs[which(obs == 999)] <- NA


####################### TIME MERGE #####################################
# align data in time

ope_time <- as.numeric(dimnames(openloop)[[1]])
off_time <- as.numeric(dimnames(offline)[[1]])
ana_time <- as.numeric(dimnames(analysis)[[1]])
obs_time <- as.numeric(dimnames(obs)[[1]])

ope.df <- data.frame(time=ope_time, openloop=1)
off.df <- data.frame(time=off_time, offline=1)
ana.df <- data.frame(time=ana_time, analysis=1)
obs.df <- data.frame(time=obs_time, obs=1)

x <- merge(ana.df,obs.df,by="time", all=T)
y <- merge(x,off.df,by="time", all=T)
pltmsk <- merge(y,ope.df,by="time", all=T)

# indecies corresponding to each data set
ope_ind <- which(pltmsk$openloop == 1)  - min(which(pltmsk$openloop == 1))  + 1
off_ind <- which(pltmsk$offline == 1)  - min(which(pltmsk$offline == 1))  + 1
ana_ind <- which(pltmsk$analysis == 1) - min(which(pltmsk$analysis == 1)) + 1
obs_ind <- which(pltmsk$obs == 1)      - min(which(pltmsk$obs == 1))      + 1

times      <- pltmsk$time    # time vector
ntimes     <- length(times)


if (any(times == Tstart) & any(times == Tend)){
  tstart <- which(times == Tstart)
  tend   <- which(times == Tend)
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



####################### PLOT ###########################################

col = rainbow(npoints)

#pdf(filename)

ylim = c( min(c(min(unlist(analysis)), min(unlist(offline)), min(obs,na.rm=T)), na.rm=T),
          max(c(max(unlist(analysis)), max(unlist(offline)), max(obs,na.rm=T)), na.rm=T))
ylim = c(0,1)

plot(NA,xlim = c(tstart,tend), ylim=ylim, main=expr)


for (point in station){
  if (pltOpe){
    lines(ope_ind, openloop[,1,point],col=col[point])
  }

# for (j in 1:(length(pltTimes)/assimWindow)){
    for (prt in pltPrt){
	  if (pltOff){
        lines(off_ind, offline[,prt,point],
#        lines(tstart:assimWindow + (j-1)*assimWindow,
#		      offline[tstart:assimWindow + (j-1)*assimWindow,prt,point],
        #lines(1:assimWindow+(j-1)*assimWindow,
#        offline[1:assimWindow+(j-1)*assimWindow,prt,point],
              col=col[point])
	  }
    }
  }
  if (pltAna){
    for (prt in pltPrt){
      points(ana_ind, analysis[,prt,point], col=col[point])
#       points(seq(tstart+assimWindow-1,tend,assimWindow), analysis[tstart:(tend/assimWindow),prt,point],col=col[point])
#      points(seq(assimWindow,nanalysis*2*assimWindow,2*assimWindow),analysis[,prt,point],col=col[point])
	}
 #}
}
#

tmp     <- as.character(obs_time)
ascend  <- grep(".*06$",tmp, perl=T)
descend <- grep(".*18$",tmp, perl=T)

if (pltObs) {
  for (point in station){
   points(obs_ind[ascend], obs[ascend,point], bg=col[point], pch=24)
   points(obs_ind[descend], obs[descend,point], bg=col[point], pch=25)
  }
}

# Legend

stations<- read_stlist(stationlist="/home/asmundb/Master-project/Rscript/stationlist.cfg")

labl    <-   which(c(   pltObs, pltObs ,    pltAna ,  pltOff ,    pltOpe ))

labs    <-  c("smosA", "smosD", "analysis","offline", "openloop")[labl]
labpch  <-  c(    24 ,     25 ,        21 ,      NA ,        NA )[labl]
labfill <-  c(    NA ,     NA ,        NA ,  "black",    "black")[labl]
lablty  <-  c(     0 ,      0 ,         0 ,       2 ,         1 )[labl]       

legend("topright", legend=dimnames(stations)[[1]][station], fill=col)
legend("topleft", legend=labs, pch=labpch, lty=lablty)

#dev.off()




