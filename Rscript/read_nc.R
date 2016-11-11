# Read NetCDF files created by SURFEX ( ISBA_PROGNOSTIC.OUT.nc )
# data output: (ntimes, nvars, npoints)
# if file is soda_analysis, ntimes=1 
#
#
#
#
library("ncdf4")
source("functions.R")

path  <- "/home/asmundb/SURFEX2/EXPERIMENTS/SODA_EKF/time_series"

files.analysis  <- list.files(path=path, pattern="soda_analysis*",full.name=T) # test if nc mabye

nfiles.analysis <- length(files.analysis)
files.offline  <- list.files(path=path, pattern="soda_offline*",full.name=T)

nfiles.offline  <- length(files.offline)


# stations/points
stations <- read_stlist(stationlist="stationlist.cfg")

#vars <- c("TG1","TG2","WG1","WG2")
vars <- "WG1"

nvars <- length(vars)

# analysis
var.analysis <- get_var(files.analysis[1],vars)
for (i in 2:nfiles.analysis){
  var.analysis <- rbind(var.analysis, get_var(files.analysis[i],vars))
}

var.offline <- get_var(files.offline[1],vars)
for (i in 2:nfiles.offline){
    var.offline <- rbind(var.offline, get_var(files.offline[i],vars))
}

col = rainbow(8)

#pdf("first_run_ever.pdf")
plot(NA,xlim=c(0,6*nfiles.offline),ylim=c(0,1))
for (i in 1:8){
  for (j in 1:nfiles.offline){
    lines(1:6+(j-1)*6, var.offline[1:6+(j-1)*6,i],col=col[i])
  }
# lines(c(6,7), c(var.analysis[1,i], var.offline[7,i]),col=col[i])
# lines(c(12,13), var.offline[12:13,i],col=col[i])
# lines(c(18,19), c(var.analysis[2,i], var.offline[19,i]),col=col[i])
  points(seq(6,nfiles.analysis*12,12),var.analysis[,i],col=col[i])

}

#obs1 <- read_OBSERVATION("~/SURFEX2/EXPERIMENTS/SODA_EKF/INPUT/OBSERVATIONS_161016H06.DAT")[[1]]
#obs2 <- read_OBSERVATION("~/SURFEX2/EXPERIMENTS/SODA_EKF/INPUT/OBSERVATIONS_161016H18.DAT")[[1]]

obspath <- "/disk1/asmundb/SMOS/OBSERVATIONS"
obsfiles <- list.files(path=obspath,pattern="OBSERVATIONS*", full.name=T)


obs <- array(dim=c(0,8))
for (i in 1:nfiles.analysis){
  obs <- rbind(obs, read_OBSERVATION(obsfiles[i])[[1]])
}


#obs1[which(obs1==999)] <- -0.03
#obs2[which(obs1==999)] <- -0.03
for (i in 1:nfiles.analysis){
  points(rep(12*(i-1)+6,8), obs[i,], bg=col, pch=24)
}
#points(rep(18,8), obs2, bg=col, pch=24)

#abline(v=c(6,18),lwd=0.1)

#legend("topleft", legend=dimnames(stations)[[1]], fill=col)
#legend("bottomleft", legend=c("SMOS", "analysis","offline"),

#dev.off()
