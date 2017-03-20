# Read NetCDF files created by SURFEX ( ISBA_PROGNOSTIC.OUT.nc )
# data output: (ntimes, nvars, npoints)
# if file is soda_analysis, ntimes=1 
#
#
#
#
library("ncdf4")
source("functions.R")

path  <- "/home/asmundb/SURFEX2/EXPERIMENTS/homemade/time_series"


files.analysis  <- list.files(path=path, pattern="EnKF_analysis*",full.name=T) # test if nc mabye
nfiles.analysis <- length(files.analysis)
files.offline  <- list.files(path=path, pattern="ISBA*",full.name=T)
nfiles.offline  <- length(files.offline)


# stations/points
stations <- read_stlist(stationlist="stationlist.cfg")

#vars <- c("TG1","TG2","WG1","WG2")
vars <- "WG2"

nvars <- length(vars)

# analysis
var.analysis <- get_analysis(files.analysis[1],vars)
for (i in 2:nfiles.analysis){
  var.analysis <- rbind(var.analysis, get_analysis(files.analysis[i],vars))
}
#var.offline <- get_var(files.offline[1],vars)
var.offline <- array(dim=c(24,8,5))
fi <-  1
for (i in 1:(nfiles.offline/5)){
  for (ens in 1:5){
#    print( files.offline[fi] )
    var.offline[,,ens] <- get_var(files.offline[fi],vars)
	fi <- fi + 1
  }
}

col <- rainbow(8)

s = 1
#pdf("first_run_ever.pdf")
plot(NA,xlim=c(0,24),ylim=c(0,max(var.offline)))
for (i in 1:5){
 # for (j in 1:4){
 #   lines(1:6+(j-1)*6, var.offline[1:6+(j-1)*6,s,i],col=col[i])
 # }
  lines(var.offline[,s,i],col=col[i])
#  lines(c(6,7), c(var.analysis[1,i], var.offline[7,1,i]),col=col[i])
#  lines(c(12,13), var.offline[12:13,1,i],col=col[i])
#  lines(c(18,19), c(var.analysis[2,i], var.offline[19,1,i]),col=col[i])

}
points(c(6,12,18,24),var.analysis[,s],col="black")

obs1 <- read_OBSERVATION("~/SURFEX2/EXPERIMENTS/SODA_EKF/INPUT/OBSERVATIONS_161016H06.DAT")[[1]]
obs2 <- read_OBSERVATION("~/SURFEX2/EXPERIMENTS/SODA_EKF/INPUT/OBSERVATIONS_161016H18.DAT")[[1]]

#obs1[which(obs1==999)] <- -0.03
#obs2[which(obs1==999)] <- -0.03
points(rep(6,8), obs1, bg=col, pch=24)
points(rep(18,8), obs2, bg=col, pch=24)

abline(v=c(6,18),lwd=0.1)

#legend("topleft", legend=dimnames(stations)[[1]], fill=col)
#legend("bottomleft", legend=c("SMOS", "analysis","offline"),

#dev.off()
