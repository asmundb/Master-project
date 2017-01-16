#  Calculate statistical measures  #
#                                  #
#                                  #

source("functions.R")
source("plotTimeSerieOO.R")

MM <- function(x,n){
# Clalculate moving mean for x around i-n:i+n
  N  = length(x) + 2*n-1
  mm = numeric(length=N)
  x  = c(rep(NA,n), x, rep(NA,n))
  for (i in n:(N-n)){
    wn = sum(!is.na(x[(i-n):(i+n)]))
    mm[i] = sum(x[(i-n):(i+n)], na.rm=T)/(wn)
  } 
  return(mm[n:(N-n)])
}



Tstart <- 2016050100
Tend   <- 2016101700



########  Check observations  ########

# Load data
obs <- sodaType("dat/obs/WG2.obs.dat", Tstart, Tend)
tmp     <- as.character(obs$time)
ascend  <- grep(".*06$",tmp, perl=T)
descend <- grep(".*18$",tmp, perl=T)
stations<- read_stlist(stationlist="/home/asmundb/Master-project/Rscript/stationlist.cfg")


nobs <- colSums( !apply(obs$vals, c(1,3), is.na))

#cat(sprintf("Total number of passes: %d \n", obs$dims[1]))
#cat(sprintf("%16s &  %3d & %3.1f %% \\\\ \n",
#            dimnames(stations)[[1]], nobs, nobs/obs$dims[1]*100))

#### PLOT timeseries of SMOS data
#
#plot(NA, xlim=c(0, obs$dims[1]), ylim=c(0,0.6) )
col = rainbow(8)
for (pnt in 1:obs$dim[3]){
  pdf(sprintf("figures/SMOS_obs/SMOS_total_%d.pdf", pnt))
  plot(NA, xlim=c(0, obs$dims[1]), ylim=c(0,0.6),
           main=sprintf("%s",dimnames(stations)[[1]][pnt]),
           xlab="time index",
           ylab="Soil moisture")
  points(ascend, obs$vals[ascend,1,pnt], col="blue", pch=20,cex=0.5)
  points(descend, obs$vals[descend,1,pnt], col="green",  pch=20,cex=0.5)
  lines(MM(obs$vals[,,pnt], 5))
  lines(ascend, MM(obs$vals[ascend,,pnt],5), col="blue")
  lines(descend,MM(obs$vals[descend,,pnt],5), col="green")
  legend("topright", legend=c("total","ascending","descending"), fill=c("black","blue","green"))
#  Sys.sleep(4)
  dev.off()
}


#### NORMALISERING ####

openloop <- sodaType("dat/postMod/EKF/WG2.openloop.dat", Tstart, Tend)


linReScale <- function(x,y){
  # Return rescaled x to y's mean and spread
  sigma_x = sd(x,na.rm=T)
  sigma_y = sd(y,na.rm=T)
  x_mean  = mean(x,na.rm=T)
  y_mean  = mean(y,na.rm=T)
  x_new = (x - x_mean)*(sigma_y/sigma_x) + y_mean
  print(sprintf("sd_x=%.3f, sd_y=%.3f, mean_x=%.3f, mean_y=%.3f",
                 sigma_x, sigma_y, x_mean, y_mean))
  return(x_new)
}  



for (pnt in 1:8){

obs_new <- linReScale(obs$vals[,,pnt], openloop$vals[,,pnt])

plot(NA, xlim=c(0, oopenloop$dims[1]), ylim=c(0,0.6),
          main=sprintf("%s",dimnames(stations)[[1]][pnt]),
          xlab="time index",
          ylab="Soil moisture")
points( obs$vals[,1,pnt], col="blue", pch=20,cex=0.5)
lines(openloop$vals[,,pnt])
points( obs_new, col="red", pch=20,cex=0.5)
lines(MM(obs$vals[,,pnt],5), col="blue")
lines(MM(obs_new,5),col="red")

legend("topright", legend=c("openloop","SMOS","SMOS-norm"), fill=c("black","blue","red"))

Sys.sleep(4)
}
