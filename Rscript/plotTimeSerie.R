# Load variables from .dat file
# and plot a timeserie
#
source("functions.R")
#source("plotTimeSerieFunc.R")
source("plotTimeSerieOO.R")
####################### USER VARIABLES #################################

Tstart  <- 2016050100    # from
Tend    <- 2016101700    # to
station <- 0            # point(s) to plot in same figure. if 0 plot all
pltObs  <- T             # if TRUE, plot observations
pltAna  <- F             # if TRUE, plot analysis
pltOff  <- T             # if TRUE, plot prognosis
pltOpe  <- T
pltPrt  <- -1#c(1,2,3,4,5)  # ensemble members to plot. if 0 plot all, -1 to plot ensemble mean
datDir  <- "dat/normSMOS"
expr    <- "EKF"        # name of experiment
var     <- "WG1"    # surfex variable name
ylim    <- c(0,0.41) # auto if ylim == NA
outfile <- NA #"test.pdf"


anaFile <- sprintf("%s/%s/%s.analysis.dat",datDir, expr, var)
offFile <- sprintf("%s/%s/%s.offline.dat" ,datDir, expr, var)
opeFile <- sprintf("dat/postMod/%s/%s.openloop.dat", expr, var)
obsFile <- sprintf("dat/normSMOS/EKF/WG2.obs.dat", var)

analysis <- sodaType(anaFile, Tstart, Tend)
offline  <- sodaType(offFile, Tstart, Tend)
openloop <- sodaType(opeFile, Tstart, Tend)
obs      <- sodaType(obsFile, Tstart, Tend)

period <- 24*7
dates <- offline$time[seq(1,offline$dims[1]-period,by=period)]


### SCATTERPLOT ###

for (ivar in 1:9){
var <- paste("WG",ivar,sep="")
anaFile <- sprintf("%s/%s/%s.analysis.dat",datDir, expr, var)
offFile <- sprintf("%s/%s/%s.offline.dat" ,datDir, expr, var)
opeFile <- sprintf("dat/postMod/%s/%s.openloop.dat", expr, var)
obsFile <- sprintf("dat/normSMOS/EKF/WG2.obs.dat", var)

analysis <- sodaType(anaFile, Tstart, Tend)
offline  <- sodaType(offFile, Tstart, Tend)
openloop <- sodaType(opeFile, Tstart, Tend)
obs      <- sodaType(obsFile, Tstart, Tend)

#
#for (pnt in 1:8){
#
#tit <- sprintf("%s_%s_%s_normalized", expr, var, obs$stname[pnt])
#outfile <- paste(sprintf("figures/scatter/%s_%s_normalized",expr,var),"_", pnt,".pdf",sep="")
#pdf(file=outfile)
#plot(openloop$vals[,,pnt], offline$vals[,,pnt],pch=20, cex=0.2,
#     xlab="openloop ", ylab="ekf", main=tit)
#if (ivar==2){
#  points(obs$vals[,,pnt],offline$vals[obs$put,,pnt], col="red",pch=3)
#  legend("bottomright", legend=c("openloop-ekf", "obs-ekf"), fill=c("black","red"))
#}  
#abline(c(0,0),c(1,1),lty=2,lwd=0.5)
#dev.off()
#}
#runcom <- sprintf("pdfunite figures/scatter/%s_%s_normalized_[1-8]* figures/scatter/%s_%s_normalized_scatter.pdf",expr,var, expr, var)
#system(runcom)
#}
#
####
if (ivar==2){
  pltObs <- T
}else{
  pltObs <- F
}


for (station in 1:8){
  outfile <- sprintf("figures/normSMOS_%s_long_%s_%s.pdf",var, station,expr)
  plotTest(analysis, offline, openloop, obs,
         station=station,
         pltObs=pltObs,
         pltAna=pltAna,
         pltOff=pltOff,
         pltOpe=pltOpe,
         pltPrt=pltPrt,
         ylim=ylim,
         outfile=outfile)
}

}
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
#    outfile <- sprintf("figures/normSMOS_week_%02d_%s_%d_%s.pdf", wk,var, station,expr)
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
