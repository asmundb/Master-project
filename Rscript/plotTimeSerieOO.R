# Load variables from .dat file
# and plot a timeserie
#
source("functions.R")


####################### USER VARIABLES #################################

#Tstart  <- 2016050100    # from
#Tend    <- 2016060700    # to
#station <- 1             # point(s) to plot in same figure. if 0 plot all
#pltObs  <- T             # if TRUE, plot observations
#pltAna  <- F             # if TRUE, plot analysis
#pltOff  <- T             # if TRUE, plot prognosis
#pltOpe  <- T
#pltPrt  <- -1 # c(1,2,4)  # ensemble members to plot. if 0 plot all, -1 to plot ensemble mean
#datDir  <- "dat/postMod"
#expr    <- "EKF"        # name of experiment
#var     <- "WG1"         # surfex variable name
#ylim    <- c(0,0.41) # auto if ylim == NA


########################################################################


sodaType <- function(file, Tstart=2016050100, Tend=2016101700){
   # make a list of all necessary variables and parameter 
   # for plotting

  ####################### LOAD FILES #####################################

  case      <- readRDS(file)
  ncase  <-  dim(case)[1] 
 
  ####################### NO DATA VALUES ##########################
  case[which(case == 999)] <- NA
  
  ####################### TIME MERGE #####################################
  # align data in time
  
  Tstart_tmp <- as.POSIXlt(strptime(Tstart,format="%Y%m%d%H"))
  Tend_tmp   <- as.POSIXlt(strptime(Tend,format="%Y%m%d%H"))
  
  tim_time <- as.numeric(format(seq(Tstart_tmp,Tend_tmp,by="hours"), "%Y%m%d%H"))
  case_time <- as.numeric(dimnames(case)[[1]])
  
  tim_df <- data.frame(time=tim_time, tim=1)
  case_df <- data.frame(time=case_time, case=1)
  
  pltmsk <- merge(case_df, tim_df, by="time", all=T)
  
  tim_ind0 <- min(which(pltmsk$tim  == 1))
  
  case_ind0 <- which(case_time == pltmsk$time[min(which(pltmsk$case      == 1 & pltmsk$tim == 1))])
  # indecies corresponding to each data set
  
  case_ind <- seq(case_ind0, by=1, length.out=length(which(pltmsk$case == 1 & pltmsk$tim == 1)))
  
  # xaxis values 
  case_put <- which(pltmsk$case == 1 & pltmsk$tim == 1) - tim_ind0 + 1
  if (length(dim(case)) == 2){
    vals <- array(dim=c(dim(case[case_ind,])[1], 1, dim(case)[2]))
    vals[,1,] <- case[case_ind,]
  } else {
    vals <- array(dim=c(dim(case[case_ind,,])[1], dim(case)[2], dim(case)[3]))
    vals[,,] <- case[case_ind,,]
  }
  sodaType <- list(  vals=vals,
                     time=case_time[case_ind],
  	                 put=case_put,
					 dims=dim(vals),
					 ndim=length(dim(case)) )

  class(sodaType) <- append(class(sodaType), "sodaType")
  
  return(sodaType)
}



####################### PLOT ###########################################


plotTest <- function(analysis,offline,openloop,obs,
                     station=0,
                     pltObs=T,
                     pltAna=T,
                     pltOff=T,
                     pltOpe=T,
                     pltPrt=-1,
                     ylim=NA,
					 outfile=NA){

# make sure data are consistent
if (dim(analysis$vals)[3] != dim(offline$vals)[3] | dim(analysis$vals)[3] != dim(obs$vals)[3]) {
  print("data inconsistency: number of stations differ!")
  stop()
}else{
  npoints  <- analysis$dims[3]
}
if (station == 0){
  station <- 1:npoints
}


if (dim(analysis$vals)[2] != dim(offline$vals)[2]) {
  print("data inconsistency: number of perturbations differ!")
  stop()
}else{
  nprt  <- dim(analysis$vals)[2]
}
if (expr == "EnKF"){
if (length(pltPrt) == 1 || pltPrt == 0){
  ens_mean <- F
  if (pltPrt < 0) {
    pltPrt <- 1
    ens_mean <- T
    openloop$vals[,1,] <- apply(openloop$vals,c(1,3),mean)
    offline$vals[,1,]  <- apply(offline$vals,c(1,3),mean)
  }else if(pltPrt == 0) {
    pltPrt <- 1:nprt
}
}
} else {
  pltPrt <- 1
}

if (is.na(ylim[1])){
  ylim = c( min(c(min(unlist(analysis$vals)), min(unlist(offline$vals)), min(obs$vals,na.rm=T)), na.rm=T),
            max(c(max(unlist(analysis)), max(unlist(offline)), max(obs,na.rm=T)), na.rm=T))
}


if (! is.na(outfile)){
  pdf(file=outfile)
}

tstart <- 1
tend   <- offline$dims[1]

col <- rainbow(npoints)

plot(NA,xlim = c(tstart,tend), ylim=ylim, xaxt='n')

#axis(1, pltTimes, tim_time)
tmp <- as.character(offline$time)
if (expr == "EKF"){
  off_ana_ind <- grep(".*06$|.*18$",tmp, perl=T)
  daw <- 12
} else {
  off_ana_ind <- grep(".*00$|.*06$|.*12$|.*18$",tmp, perl=T)
  daw <- 6
}


for (point in station){
  col_ind <- point
  for (prt in pltPrt){
    if (length(station) == 1 & length(pltPrt) > 1){
      col_ind <- prt
    }
    if (pltOpe){
      lines(openloop$put, openloop$vals[,prt,point],col=col[col_ind])
    }
    if (pltOff){
  #   lines(offline$put, offline$vals[,prt,point],
  #           lty=2, col=col[col_ind])
	  for (t0 in off_ana_ind[1:(length(off_ana_ind)-1)]){
	    ana_curr <-  which(analysis$time == offline$time[t0])
        lines(t0:(t0+daw), c(analysis$vals[ana_curr,prt,point], offline$vals[(t0+1):(t0+daw),prt,point]),
              lty=2, col=col[col_ind])
	  }
	}
    if (pltAna){
      points(analysis$put, analysis$vals[,prt,point], col=col[col_ind])
    }
  }

#

tmp     <- as.character(obs$time)
ascend  <- grep(".*06$",tmp, perl=T)
descend <- grep(".*18$",tmp, perl=T)
if (pltObs) {
  for (point in station){
	col_ind <- point
    points(obs$put[ascend], obs$vals[ascend,,point], bg=col[col_ind], pch=24)
    points(obs$put[descend], obs$vals[descend,,point], bg=col[col_ind], pch=25)
  }
}
}
# time axis
maint = paste(var, expr, sep=" ")
axfmt = ""

tmp     <- as.character(offline$time)
ntimes  <- length(tmp)
if (ntimes < 24*7 ){
  axTime_ind <- grep(".*(00|06|12|18)$",tmp, perl=T)
  axTime <- as.POSIXlt(strptime(offline$time[axTime_ind],format="%Y%m%d%H"))
  axfmt <- "%d. %H:00"
  maint <- paste(maint, format(axTime[1], "%b %Y"), sep =" ")
} else if (ntimes > 24*7 & ntimes < 24*30){
  axTime_ind <- grep(".*00$",tmp, perl=T)
  axTime <- as.POSIXlt(strptime(offline$time[axTime_ind],format="%Y%m%d%H"))
  axfmt <-"%b-%d"
  maint <- paste(maint, format(axTime[1], "%Y"), sep =" ")
} else {
  axTime_ind <- grep(".*(0100|1500)$",tmp, perl=T)
  axTime <- as.POSIXlt(strptime(offline$time[axTime_ind],format="%Y%m%d%H"))
  axfmt <- "%b-%d"
  maint <- paste(maint, format(axTime[1], "%Y"), sep =" ")
}

axis(1, axTime_ind, format(axTime,axfmt))

# Legend

stations<- read_stlist(stationlist="/home/asmundb/Master-project/Rscript/stationlist.cfg")

labl    <-   which(c(   pltObs, pltObs ,    pltAna ,  pltOff ,    pltOpe ))

labs    <-  c("smosA", "smosD", "analysis","offline", "openloop")[labl]
labpch  <-  c(    24 ,     25 ,        21 ,      NA ,        NA )[labl]
labfill <-  c(    NA ,     NA ,        NA ,  "black",    "black")[labl]
lablty  <-  c(     0 ,      0 ,         0 ,       2 ,         1 )[labl]       

if (expr == "EnKF") {
 if (length(station) > 1) {
   legend("topright", legend=dimnames(stations)[[1]][station], fill=col[station])
 } else if(length(station) == 1 & length(pltPrt) == 1){
   legend("topright", legend=dimnames(stations)[[1]][station], fill=col[station])
   if (ens_mean){
     maint <- paste(maint, "ens_mean", sep =" ")
   } else {
     maint <- paste(maint, sprintf("ens_%d",pltPrt), sep =" ")
   }
 } else{
   legend("topright", legend=sprintf("ens_%d",pltPrt), fill=col[pltPrt])
   maint <- paste(maint, dimnames(stations)[[1]][station], sep =" ")
 }
} else {
  if (length(station) > 1) {
    legend("topright", legend=dimnames(stations)[[1]][station], fill=col[station])
  } else {
    maint <- paste(maint, dimnames(stations)[[1]][station], sep =" ")
  }
}
legend("topleft", legend=labs, pch=labpch, lty=lablty)

title(main=maint)


if (! is.na(outfile)){
  dev.off()
}

}
### Function end ###
