require(ncdf4)
require(fields)

radFile <- "surfex_files/radar_20140626.nc"
forcFiles <-c("/lustre/storeB/users/asmundb/surfex/FORCING/domain/FORCING.nc_2014062600",
              "/lustre/storeB/users/asmundb/surfex/FORCING/domain/FORCING.nc_2014062606",
              "/lustre/storeB/users/asmundb/surfex/FORCING/domain/FORCING.nc_2014062612",
              "/lustre/storeB/users/asmundb/surfex/FORCING/domain/FORCING.nc_2014062618")

# Load radar file
ncid <- nc_open(radFile)
radlon <- ncvar_get(ncid,ncid$var$lon)[200:900,1200:1869]
radlat <- ncvar_get(ncid,ncid$var$lat)[200:900,1200:1869]
radPrec <- ncvar_get(ncid,ncid$var$lwe_precipitation_rate)[200:900,1200:1869,]
nc_close(ncid)

# load forcing files

ncid <- nc_open(forcFiles[1])
forclon <- ncvar_get(ncid,ncid$var$LON)
forclat <- ncvar_get(ncid,ncid$var$LAT)
forcPrec <- ncvar_get(ncid,ncid$var$Rainf)
nc_close(ncid)


forcPrec <- array(NA, dim=c(111,111,24))
k<- 1
for (i in 1:4){
  ncid <- nc_open(forcFiles[i])
  tmp <- ncvar_get(ncid,ncid$var$Rainf)
  nc_close(ncid)
  for (j in 1:6){
    forcPrec[,,k] <- matrix(tmp[,j],111,111)
    k <- k+1
  }
}

# extract my grid from radar file
source("ffunctions.R")
Nlons <- length(forclon)

#X0 <- list()
#for (i in 1:Nlons){
#  X0[[i]] <- c(forclon[i],forclat[i])
#}
#
#test <- function(x0){
#  dyn.load("FLIB/nn_lambert.so")
#  x <- fnn_lamb(array(radlon), array(radlat), x0[1], x0[1])
#  return(x$ij_out)
#}
#
#require(prallel)
#cl <- makeCluster(16)
#clusterExport(cl, list("fnn_lamb", "radlon","radlat"))
#
#IJ <- parLapply(cl, X0, test)
#ij <- as.numeric(IJ)


ij2 <- numeric(length(forclon))
dist2 <- numeric(length(forclon))

for ( k in 1:length(forclon)){
  cat(k, "of 12321\r")
  x <- fnn_lamb(array(radlon), array(radlat), forclon[k], forclat[k])
  ij2[k]   <- x$ij_out
  dist2[k] <- x$dist[x$ij_out]
  flush.console()
}

mygrid <- array(NA, dim=c(nx,ny,dim(radPrec)[3]))
for (i in 1:dim(radPrec)[3]){
  radPrec_mygrid[,,i] <- matrix(array(radPrec[,,i])[ij2], nx, ny)
}


for (i in 1:24){
  png(sprintf("tmp/%02d.png",i))
  par(mfrow=c(2,1))
  image.plot(radPrec_mygrid[,,i],col=two.colors(100,"white","yellow","red"), zlim=c(0,78),main="Radar: lwe_precipitation_rate")
  topo()
  image.plot(forcPrec[,,i],col=two.colors(100,"white","yellow","red"),zlim=c(0,0.01864783), main=sprintf("AROME: Rainfall %02d",i-1))
  topo()
  dev.off()
}
