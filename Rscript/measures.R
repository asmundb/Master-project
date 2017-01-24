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


######################## NORMALISERING ########################################

openloop <- sodaType("dat/postMod/EKF/WG2.openloop.dat", Tstart, Tend)



for (pnt in 1:8){

obs_new_A <- linReScale(obs$vals[ascend,,pnt], openloop$vals[,,pnt])
obs_new_D <- linReScale(obs$vals[descend,,pnt], openloop$vals[,,pnt])
obs_new <- linReScale(obs$vals[,,pnt], openloop$vals[,,pnt])

pdf(sprintf("figures/SMOS_obs/SMOS_linRe_sep_%d.pdf", pnt))

plot(NA, xlim=c(0, openloop$dims[1]), ylim=c(0,0.6),

          main=sprintf("%s",dimnames(stations)[[1]][pnt]),
          xlab="time index",
          ylab="Soil moisture")
#points(obs$put, obs$vals[,1,pnt], col="blue", pch=20,cex=0.5)
lines(openloop$put, openloop$vals[,,pnt])

#points(obs$put[ascend], obs_new_A, col="red", pch=20,cex=0.5)

lines(obs$put, MM(obs_new,5), col="red")
lines(obs$put, MM(obs$vals[,,pnt],5), col="darkred")
lines(obs$put[ascend], MM(obs$vals[ascend,,pnt],5), col="darkblue")
lines(obs$put[descend], MM(obs$vals[descend,,pnt],5), col="darkgreen")
lines(obs$put[ascend], MM(obs_new_A,5),col="blue")
lines(obs$put[descend], MM(obs_new_D,5),col="green")

legend("topright", legend=c("openloop","SMOS_norm","SMOS","SMOS_A","SMOS_D","SMOS_A_norm","SMOS_D_norm"), 
                     fill=c("black","red","darkred","darkblue","darkgreen","blue","green"))
dev.off()
#Sys.sleep(4)
}


##################### RESCALE AND SAVE AS OBSERVATIONS.DAT #####################

obs_new <- array(dim=c(obs$dims[1],8))

for (pnt in 1:8){
  obs_new[,pnt] <- minReScale(obs$vals[,,pnt], openloop$vals[,,pnt])
}

obs_new[which(is.na(obs_new))] <- 999




#for (i in 1:length(obs_new)){
#  yyyymmddhh <- as.character(obs$time[i])
#  yy <- substr(yyyymmddhh, 3,4)
#  mm <- substr(yyyymmddhh, 5,6)
#  dd <- substr(yyyymmddhh, 7,8)
#  hh <- substr(yyyymmddhh, 9,10)
#  outfile <- sprintf("OBSERVATIONS_LINRESCALE/OBSERVATIONS_%s%s%sH%s.DAT", yy,mm,dd,hh)
#  write(obs_new[i,], outfile, ncolumns=1)
#}






