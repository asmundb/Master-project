#  Calculate statistical measures  #
#                                  #
#                                  #

source("functions.R")
source("plotTimeSerieOO.R")

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
#col = rainbow(8)
#for (pnt in 1:obs$dim[3]){
#  pdf(sprintf("figures/SMOS_obs/SMOS_total_%d.pdf", pnt))
#  plot(NA, xlim=c(0, obs$dims[1]), ylim=c(0,0.6),
#           main=sprintf("%s",dimnames(stations)[[1]][pnt]),
#           xlab="time index",
#           ylab="Soil moisture")
#  points(ascend, obs$vals[ascend,1,pnt], col="blue", pch=20,cex=0.5)
#  points(descend, obs$vals[descend,1,pnt], col="green",  pch=20,cex=0.5)
#  lines(MM(obs$vals[,,pnt], 5))
#  lines(ascend, MM(obs$vals[ascend,,pnt],5), col="blue")
#  lines(descend,MM(obs$vals[descend,,pnt],5), col="green")
#  legend("topright", legend=c("total","ascending","descending"), fill=c("black","blue","green"))
#  Sys.sleep(4)
#  dev.off()
#}


######################## NORMALISERING ########################################

openloop <- sodaType("dat/postMod/EKF/WG2.openloop.dat", Tstart, Tend)


#for (pnt in 1:8){

#obs_new_A <- linReScale(obs$vals[ascend,,pnt], openloop$vals[,,pnt])
#obs_new_D <- linReScale(obs$vals[descend,,pnt], openloop$vals[,,pnt])
#obs_new <- linReScale(obs$vals[,,pnt], openloop$vals)

#pdf(sprintf("figures/SMOS_obs/SMOS_1-99_linReScale_%d.pdf", pnt))
#
#plot(NA, xlim=c(0, openloop$dims[1]), ylim=c(0,0.6),
#
#          main=sprintf("%s",dimnames(stations)[[1]][pnt]),
#          xlab="time hours",
#          ylab="Soil moisture")
##points(obs$put, obs$vals[,1,pnt], col="blue", pch=20,cex=0.5)
##points(obs$put, obs_new, col="red", pch=20,cex=0.5)
#lines(openloop$put, openloop$vals[,,pnt])
#
##points(obs$put[ascend], obs_new_A, col="red", pch=20,cex=0.5)
#
#lines(obs$put, obs_new, col="red")
#lines(obs$put, obs$vals[,,pnt], col="blue")
##lines(obs$put[ascend], MM(obs$vals[ascend,,pnt],5), col="darkblue")
##lines(obs$put[descend], MM(obs$vals[descend,,pnt],5), col="darkgreen")
##lines(obs$put[ascend], MM(obs_new_A,5),col="blue")
##lines(obs$put[descend], MM(obs_new_D,5),col="green")
#
##legend("topright", legend=c("openloop","SMOS_norm","SMOS","SMOS_A","SMOS_D","SMOS_A_norm","SMOS_D_norm"), 
##                     fill=c("black","red","darkred","darkblue","darkgreen","blue","green"))
##dev.off()
##Sys.sleep(4)
#
#
#}
#

##################### RESCALE AND SAVE AS OBSERVATIONS.DAT #####################

obs_new <- array(dim=c(obs$dims[1],8))

for (pnt in 1:8){
  obs_new[,pnt] <- linReScale(obs$vals[,,pnt], openloop$vals[,,pnt])
}
dimnames(obs_new)[[1]] <- obs$time

obs_new[which(is.na(obs_new))] <- 999


#saveRDS(obs_new, file="dat/normSMOS/EKF/WG2.obs.dat")


for (i in 1:length(obs_new)){
  yyyymmddhh <- as.character(obs$time[i])
#  print(yyyymmddhh)
  yy <- substr(yyyymmddhh, 3,4)
  mm <- substr(yyyymmddhh, 5,6)
  dd <- substr(yyyymmddhh, 7,8)
  hh <- substr(yyyymmddhh, 9,10)
  outfile <- sprintf("/disk1/asmundb/SMOS/OBSERVATIONS_LINRESCALE2/OBSERVATIONS_%s%s%sH%s.DAT", yy,mm,dd,hh)
  cat(outfile, obs_new[i,],"\n")
  write(obs_new[i,], outfile, ncolumns=1)
  Sys.sleep(0.1)
}


################### Density plot ####################

den <- function(v){
  x <- density(v, na.rm=T,from=0)
  return(x)
}


for (pnt in c(1,3,5,6,7,8)){
obs_minMax <- minMaxReScale(obs$vals[,,pnt], openloop$vals[,,pnt])
obs_linRescale <- linReScale(obs$vals[,,pnt], openloop$vals[,,pnt])

x1 <- den(openloop$vals[,,pnt])
x2 <- den(obs$vals[,,pnt])
x3 <- den(obs_linRescale)
x4 <- den(obs_minMax)


pdf(sprintf("figures/dist/Distribution_%d.pdf",pnt))

plot(x1, main=sprintf("Distribution %s",openloop$stname[pnt]),
     xlim=c(0,0.6), ylim=c(0,max(c(max(x1$y), max(x2$y), max(x3$y), max(x4$y)))))

lines(x2,col="blue")
lines(x3, col="red")
lines(x4, col="orange")
legend("topright", legend=c("openloop","SMOS","SMOS-norm","SMOS-minMax"), fill=c("black","blue","red","orange"))

dev.off()

}



