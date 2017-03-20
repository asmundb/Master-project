# Load variables from .dat file
# and plot a timeserie
#
source("functions.R")
#source("plotTimeSerieFunc.R")
source("plotTimeSerieOO.R")
####################### USER VARIABLES #################################

Tstart  <- 2016050100    # from
Tend    <- 2016101700    # to
station <- 3            # point(s) to plot in same figure. if 0 plot all
pltObs  <- F             # if TRUE, plot observations
pltAna  <- F             # if TRUE, plot analysis
pltOff  <- T             # if TRUE, plot prognosis
pltOpe  <- T
pltPrt  <- -1#c(1,2,3,4,5)  # ensemble members to plot. if 0 plot all, -1 to plot ensemble mean
datDir  <- "dat/normSMOS"
expr    <- "EnKF"        # name of experiment
VAR     <- "WG2"    # surfex variable name
ylim    <- c(0,0.41) # auto if ylim == NA
outfile <- NA #"test.pdf"


#anaFile <- sprintf("%s/%s/%s.analysis.dat",datDir, expr, VAR)
#offFile <- sprintf("%s/%s/%s.offline.dat" ,datDir, expr, VAR)
#opeFile <- sprintf("%s/%s/%s.openloop.dat",datDir, expr, VAR)
#obsFile <- sprintf("dat/normSMOS/EKF/WG2.obs.dat", VAR)
#
#analysis <- sodaType(anaFile, Tstart, Tend)
#offline  <- sodaType(offFile, Tstart, Tend)
#openloop <- sodaType(opeFile, Tstart, Tend)
#obs      <- sodaType(obsFile, Tstart, Tend)


#period <- 24*7
#dates <- offline$time[seq(1,offline$dims[1]-period,by=period)]


### SCATTERPLOT ###

#for (iVAR in 1:9){
#VAR <- paste("WG",iVAR,sep="")
#anaFile <- sprintf("%s/%s/%s.analysis.dat",datDir, expr, VAR)
#offFile <- sprintf("%s/%s/%s.offline.dat" ,datDir, expr, VAR)
#opeFile <- sprintf("%s/%s/%s.openloop.dat",datDir, expr, VAR)
##opeFile <- sprintf("dat/postMod/%s/%s.openloop.dat", expr, VAR)
#obsFile <- sprintf("dat/normSMOS/EKF/WG2.obs.dat", VAR)
#
#analysis <- sodaType(anaFile, Tstart, Tend)
#offline  <- sodaType(offFile, Tstart, Tend)
#openloop <- sodaType(opeFile, Tstart, Tend)
#obs      <- sodaType(obsFile, Tstart, Tend)

#
#for (pnt in 1:8){
#
#tit <- sprintf("%s_%s_%s_normalized", expr, VAR, obs$stname[pnt])
#outfile <- paste(sprintf("figures/scatter/%s_%s_normalized",expr,VAR),"_", pnt,".pdf",sep="")
#pdf(file=outfile)
#plot(openloop$vals[,,pnt], offline$vals[,,pnt],pch=20, cex=0.2,
#     xlab="openloop ", ylab="ekf", main=tit)
#if (iVAR==2){
#  points(obs$vals[,,pnt],offline$vals[obs$put,,pnt], col="red",pch=3)
#  legend("bottomright", legend=c("openloop-ekf", "obs-ekf"), fill=c("black","red"))
#}  
#abline(c(0,0),c(1,1),lty=2,lwd=0.5)
#dev.off()
#}
#runcom <- sprintf("pdfunite figures/scatter/%s_%s_normalized_[1-8]* figures/scatter/%s_%s_normalized_scatter.pdf",expr,VAR, expr, VAR)
#system(runcom)
#}
#
####
#if (iVAR==2){
#  pltObs <- T
#}else{
#  pltObs <- F
#}


#for (station in 1:8){
 # outfile <- sprintf("figures/normSMOS_%s_long_%s_%s.pdf",VAR, station,expr)
#outfile <- NA
#plotTest(analysis, offline, openloop, obs,
#         station=station,
#         pltObs=pltObs,
#         pltAna=pltAna,
#         pltOff=pltOff,
#         pltOpe=pltOpe,
#         pltPrt=pltPrt,
#         ylim=ylim,
#         outfile=outfile)
#}

#}
### TIMESERIES ###
#for (wk in 1:length(dates)){
#  Tstart <- dates[wk]
#  Tend   <- dates[wk+1]
##  print(Tstart)
##  print(Tend)
#
#
#  analysis <- sodaType(anaFile, Tstart, Tend)
#  offline  <- sodaType(offFile, Tstart, Tend)
#  openloop <- sodaType(opeFile, Tstart, Tend)
#  obs      <- sodaType(obsFile, Tstart, Tend)
#  
#  
#  for (station in 1:8){
#  
#    
#    outfile <- sprintf("figures/normSMOS_week_%02d_%s_%d_%s.pdf", wk,VAR, station,expr)
#    
#    plotTest(analysis, offline, openloop, obs,
#                         station=station,
#                         pltObs=pltObs,
#                         pltAna=pltAna,
#                         pltOff=pltOff,
#                         pltOpe=pltOpe,
#                         pltPrt=pltPrt,
#                         ylim=ylim,
#                         outfile=outfile)
#    
#    
#  }
#}

open_ekf <- sodaType("dat/postMod/EKF/WG2.openloop.dat",Tstart,Tend)
open_enkf <- sodaType("dat/normSMOS/EnKF/WG2.openloop.dat",Tstart,Tend)



ens_mean <- apply(open_enkf$vals[,,], c(1,3), mean)
ens_var  <- apply(open_enkf$vals[,,], c(1,3), var)

pnt <- 1

pdf("figures/openloop.pdf")
plot(open_ekf$vals[,,pnt], type='l',col="blue", main="all obs=999.0")
lines(ens_mean[,1],col="red")
legend("topleft", legend=c("ekf", "enkf_mean"), fill=c("blue","red"))
dev.off()
#for (i in 1:5){
#  lines(open_enkf$vals[,i,pnt])
#}

