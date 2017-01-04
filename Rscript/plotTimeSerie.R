# Load variables from .dat file
# and plot a timeserie
#
source("functions.R")

####################### USER VARIABLES #################################

Tstart  <- 2016050100    # from
Tend    <- 2016050700    # to
station <- 1          # point(s) to plot in same figure. if 0 plot all
pltObs  <- T             # if TRUE, plot observations
pltAna  <- T             # if TRUE, plot analysis
pltOff  <- T             # if TRUE, plot prognosis
pltOpe  <- T
pltPrt  <- 0  #c(1,2,4)  # ensemble members to plot. if 0 plot all
expr    <- "DIF"        # name of experiment
var     <- "WG2"         # surfex variable name


# specify .dat files to plot
anaFile <- sprintf("dat/%s/%s.analysis.dat", expr, var)
offFile <- sprintf("dat/%s/%s.offline.dat" , expr, var)
opeFile <- sprintf("dat/%s/%s.openloop.dat", expr, var)
obsFile <- sprintf("dat/obs/%s.obs.dat", var)

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

####################### NO DATA VALUES ##########################
obs[which(obs == 999)] <- NA


####################### TIME MERGE #####################################
# align data in time

Tstart_tmp <- as.POSIXlt(strptime(Tstart,format="%Y%m%d%H"))
Tend_tmp   <- as.POSIXlt(strptime(Tend,format="%Y%m%d%H"))

tim_time <- as.numeric(format(seq(Tstart_tmp,Tend_tmp,by="hours"), "%Y%m%d%H"))
ope_time <- as.numeric(dimnames(openloop)[[1]])
off_time <- as.numeric(dimnames(offline)[[1]])
ana_time <- as.numeric(dimnames(analysis)[[1]])
obs_time <- as.numeric(dimnames(obs)[[1]])

tim.df <- data.frame(time=tim_time, tim=1)
ope.df <- data.frame(time=ope_time, openloop=1)
off.df <- data.frame(time=off_time, offline=1)
ana.df <- data.frame(time=ana_time, analysis=1)
obs.df <- data.frame(time=obs_time, obs=1)

t1 <- merge(obs.df, off.df, by="time", all=T)
t2 <- merge(t1    , ope.df, by="time", all=T)
t3 <- merge(t2    , ana.df, by="time", all=T)
pltmsk <- merge(t3, tim.df, by="time", all=T)


#x <- merge(ana.df,obs.df,by="time", all=T)
#y <- merge(x,off.df,by="time", all=T)
#pltmsk <- merge(y,ope.df,by="time", all=T)


tim_ind0 <- min(which(pltmsk$tim  == 1))

ana_ind0 <- which(ana_time == pltmsk$time[min(which(pltmsk$analysis == 1 & pltmsk$tim == 1))])
obs_ind0 <- which(obs_time == pltmsk$time[min(which(pltmsk$obs      == 1 & pltmsk$tim == 1))])
off_ind0 <- which(off_time == pltmsk$time[min(which(pltmsk$offline  == 1 & pltmsk$tim == 1))])
ope_ind0 <- which(ope_time == pltmsk$time[min(which(pltmsk$openloop == 1 & pltmsk$tim == 1))])
# indecies corresponding to each data set


ope_ind <- seq(ope_ind0, by=1, length.out=length(which(pltmsk$openloop == 1 & pltmsk$tim == 1)))
off_ind <- seq(off_ind0, by=1, length.out=length(which(pltmsk$offline  == 1 & pltmsk$tim == 1)))
ana_ind <- seq(ana_ind0, by=1, length.out=length(which(pltmsk$analysis == 1 & pltmsk$tim == 1))) 
obs_ind <- seq(obs_ind0, by=1, length.out=length(which(pltmsk$obs      == 1 & pltmsk$tim == 1)))


# xaxis values 
obs_put <- which(pltmsk$obs      == 1 & pltmsk$tim == 1) - tim_ind0 + 1
ope_put <- which(pltmsk$openloop == 1 & pltmsk$tim == 1) - tim_ind0 + 1
off_put <- which(pltmsk$offline  == 1 & pltmsk$tim == 1) - tim_ind0 + 1
ana_put <- which(pltmsk$analysis == 1 & pltmsk$tim == 1) - tim_ind0 + 1

tstart <- 1
tend <- length(tim_time)
pltTimes <- 1:tend

####################### PLOT ###########################################


col = rainbow(npoints)

#pdf(filename)

ylim = c( min(c(min(unlist(analysis)), min(unlist(offline)), min(obs,na.rm=T)), na.rm=T),
          max(c(max(unlist(analysis)), max(unlist(offline)), max(obs,na.rm=T)), na.rm=T))
ylim = c(0,0.5)

plot(NA,xlim = c(tstart,tend), ylim=ylim, xaxt='n')

#axis(1, pltTimes, tim_time)

for (point in station){
  if (pltOpe){
    lines(ope_put, openloop[ope_ind,1,point],col=col[point])
  }

# for (j in 1:(length(pltTimes)/assimWindow)){
  for (prt in pltPrt){
    if (pltOff){
      lines(off_put, offline[off_ind,prt,point],
              lty=2, col=col[point])
	}
# }
  
    if (pltAna){
      points(ana_put, analysis[ana_ind,prt,point], col=col[point])
    }
   }
}
#
#

tmp     <- as.character(obs_time)
ascend  <- grep(".*06$",tmp, perl=T)
descend <- grep(".*18$",tmp, perl=T)

if (pltObs) {
  for (point in station){
   points(obs_put[ascend], obs[obs_ind[ascend],point], bg=col[point], pch=24)
   points(obs_put[descend], obs[obs_ind[descend],point], bg=col[point], pch=25)
  }
}

# time axis
maint = paste(var, expr, sep=" ")
axfmt = ""

tmp     <- as.character(tim_time)
ntimes  <- length(tmp)
if (ntimes < 24*7 ){
  axTime_ind <- grep(".*(00|06|12|18)$",tmp, perl=T)
  axTime <- as.POSIXlt(strptime(tim_time[axTime_ind],format="%Y%m%d%H"))
  axfmt <- "%d h%H"
  maint <- paste(maint, format(axTime[1], "%b %Y"), sep =" ")
} else if (ntimes > 24*7 & ntimes < 24*30){
  axTime_ind <- grep(".*00$",tmp, perl=T)
  axTime <- as.POSIXlt(strptime(tim_time[axTime_ind],format="%Y%m%d%H"))
  axfmt <-"%b-%d"
  maint <- paste(maint, format(axTime[1], "%Y"), sep =" ")
} else {
  axTime_ind <- grep(".*(0100|1500)$",tmp, perl=T)
  axTime <- as.POSIXlt(strptime(tim_time[axTime_ind],format="%Y%m%d%H"))
  axfmt <- "%b-%d"
  maint <- paste(maint, format(axTime[1], "%Y"), sep =" ")
}

axis(1, axTime_ind, format(axTime,axfmt))
title(main=maint)

# Legend

stations<- read_stlist(stationlist="/home/asmundb/Master-project/Rscript/stationlist.cfg")

labl    <-   which(c(   pltObs, pltObs ,    pltAna ,  pltOff ,    pltOpe ))

labs    <-  c("smosA", "smosD", "analysis","offline", "openloop")[labl]
labpch  <-  c(    24 ,     25 ,        21 ,      NA ,        NA )[labl]
labfill <-  c(    NA ,     NA ,        NA ,  "black",    "black")[labl]
lablty  <-  c(     0 ,      0 ,         0 ,       2 ,         1 )[labl]       

legend("topright", legend=dimnames(stations)[[1]][station], fill=col[station])
legend("topleft", legend=labs, pch=labpch, lty=lablty)

#dev.off()




