#  Calculate statistical measures  #
#                                  #
#                                  #

source("functions.R")
source("plotTimeSerieOO.R")

MM <- function(x,n){
# Clalculate moving mean for x around i-n:i+n
  N  = length(x) + 2*n
  mm = numeric(length=N)
  x <- c(rep(NA,n), x, rep(NA,n))
  for (i in n:(N-n)){
    mm[i] = sum(x[(i-n):(i+n)], na.rm=T)/(2*n)
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

# PLOT LOOKS LIKE SHIT! SEPARATE BY STATION!!!! 

plot(NA, xlim=c(0, obs$dims[1]), ylim=c(0,0.6) )
col = rainbow(8)
for (pnt in 1:obs$dim[3]){
  points(1:obs$dims[1], obs$vals[,1,pnt], col=col[pnt])
  lines(MM(obs$vals[,,pnt], 5), col=col[pnt])
}
legend("topright", legend=dimnames(stations)[[1]], fill=col)

