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
datDir  <- "dat/postMod"
expr    <- "EKF"        # name of experiment
var     <- "WG2"    # surfex variable name
ylim    <- c(0,0.41) # auto if ylim == NA
outfile <- NA #"test.pdf"


anaFile <- sprintf("%s/%s/%s.analysis.dat",datDir, expr, var)
offFile <- sprintf("%s/%s/%s.offline.dat" ,datDir, expr, var)
opeFile <- sprintf("%s/%s/%s.openloop.dat",datDir, expr, var)
obsFile <- sprintf("dat/obs/WG2.obs.dat", var)

analysis <- sodaType(anaFile, Tstart, Tend)
offline  <- sodaType(offFile, Tstart, Tend)
openloop <- sodaType(opeFile, Tstart, Tend)
obs      <- sodaType(obsFile, Tstart, Tend)

period <- 24*7
dates <- offline$time[seq(1,offline$dims[1]-period,by=period)]


for (wk in 1:length(dates)){
  Tstart <- dates[wk]
  Tend   <- dates[wk+1]
  print(Tstart)
  print(Tend)


  analysis <- sodaType(anaFile, Tstart, Tend)
  offline  <- sodaType(offFile, Tstart, Tend)
  openloop <- sodaType(opeFile, Tstart, Tend)
  obs      <- sodaType(obsFile, Tstart, Tend)
  
  
  for (station in 1:8){
  
    
    outfile <- sprintf("figures/week_%02d_%s_%d_%s.pdf", wk,var, station,expr)
    
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
