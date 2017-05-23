
require(ncdf4)
require(fields)


# OPEN SURFEX RDS FILES
print("open offline files")
wg1 <- readRDS("RDS_files/wg1_long_spinup_open_loop.rds")
wgi1 <- readRDS("RDS_files/wgi1_long_spinup_open_loop.rds")


# MAKE TIME ARRAY
time <- seq(as.POSIXlt("2016-05-01 01:00"), as.POSIXlt("2016-10-17 00:00"), by=3600)

start <- which(as.character(time)=="2016-10-06 01:00:00")
end <- length(time)
ntimes <- end-start

### READ AROME ###

# get coordinates from forcingfile
print("read forcing file")
filename <- "surfex_files/FORCING.nc"
ncid <- nc_open(filename)
lon  <- ncvar_get(ncid, ncid$var$LON)
lat  <- ncvar_get(ncid, ncid$var$LAT)
zs   <- ncvar_get(ncid, ncid$var$ZS)
nc_close(ncid)


# get coordinates from arome file
print("read arome file")
filename <- "/lustre/storeB/immutable/short-term-archive/DNMI_AROME_METCOOP/2016/10/07/AROME_MetCoOp_00_sfx.nc_20161007"
ncid <- nc_open(filename)
lon_arome <- ncvar_get(ncid, ncid$var$longitude)
lat_arome <- ncvar_get(ncid, ncid$var$latitude)
nc_close(ncid)


nx=111
ny=111

#source("ffunctions.R")
#ij <- numeric(length(lon))
#dist <- numeric(length(lon))

#for ( k in 1:length(lon)){
#  cat(k, "of 12321\r")
#  x <- fnn_lamb(array(lon_arome), array(lat_arome), lon[k], lat[k])
#  ij[k]   <- x$ij_out
#  dist[k] <- x$dist[x$ij_out]
#  flush.console()
#}
#print("load grid mask")
#ij <- readRDS("RDS_files/ij_mask.rds")
#
#
#
#path <- "/lustre/storeB/immutable/short-term-archive/DNMI_AROME_METCOOP/2016/10"
#files1 <- list.files(path=path,pattern="AROME_MetCoOp_.*_sfx.nc_.*", recursive=T,full.names=T)
#files1 <- files1[40:122]
#files1 <- files1[grep("_00_|_06_|_12_|_18_",files1)]
#
######################################################################
#
#
#files <- array(NA, dim=10*4)
#dd <- sprintf("%02d",5:16)
#hh <- c("00","06","12","18")
#
#k=1
#for (i in 1:length(dd)){
#  jj <- 1
#  for (j in 1:4){
#    files[k] <- sprintf("%s/%s/AROME_MetCoOp_%s_sfx.nc_201610%s",path,dd[i],hh[jj],dd[i])
#    jj <- jj + 1
#    k <- k+1
#  }
#}
#
#
#
#
#
######################################################################
#
#
#
#print("read arome files")
#tmp1 <- array(NA,dim=c(739, 949, ntimes+1))
#tmp2 <- array(NA,dim=c(739, 949, ntimes+1))
#
#
#k=1
##loop
#for (i in 5:length(files)){
#  cat(i, "of",length(files) , "\r")
#  if ( ! files[i] %in% files1 ){
#    if ( ! files[i-1] %in% files1 ){
#      ncid <- nc_open(files[i-2])
#      get <- 13:18
#    }else{
#      ncid <- nc_open(files[i-1])
#      get <- 7:12
#    }
#  } else { 
#    ncid <- nc_open(files[i])
#    get <- 1:6
#  }
#  tmp2[,,k:(k+5)] <- ncvar_get(ncid, ncid$var$WGI1)[,,get]
#  tmp1[,,k:(k+5)] <- ncvar_get(ncid, ncid$var$WG1)[,,get]
#  k=k+6
#  nc_close(ncid)
#  flush.console()
#}
#
#wgi1_arome <- array(NA,dim=c(nx,ny,ntimes+1))
#wg1_arome <- array(NA,dim=c(nx,ny,ntimes+1))
#
#k=1
#for (i in 1:ntimes){
#  wgi1_arome[,,i] <- matrix(array(tmp2[,,i])[ij], nx, ny)
#  wg1_arome[,,i] <- matrix(array(tmp1[,,i])[ij], nx, ny)
#}

wgi1_arome <- readRDS("RDS_files/wgi1_arome.rds")
wg1_arome <- readRDS("RDS_files/wg1_arome.rds")



topo <- function(levels=c(0,2,10,50,100,200,500,1000)){
  contour(matrix(zs,nx,ny), add=T, levels=levels)
}






#

print("plot")


colo <- rev(tim.colors())

wgi_max <- max(c(max(wgi1,na.rm=T), max(wgi1_arome,na.rm=T)))
wgi_min <- 0
wgi_zlim <- c(wgi_min, wgi_max)

wg_max <- max(c(max(wg1,na.rm=T), max(wg1_arome,na.rm=T)))
wg_min <- 0
wg_zlim <- c(wg_min, wg_max)

for (i in 1:(end-start)){
  png(sprintf("tmp/%03d.png", i))

  par(mfrow=c(2,2))
  image.plot(wgi1[,,start+i-1],col=colo, zlim=wgi_zlim, main=sprintf("wgi1_offline: %s", time[start+i]))
  topo()

  image.plot(wgi1_arome[,,i+1],col=colo, zlim=wgi_zlim, main="wgi1_arome")
  topo()

  image.plot(wg1[,,start+i-1],col=colo, zlim=wg_zlim, main="wg1_offline")
  topo()

  image.plot(wg1_arome[,,i+1],col=colo, zlim=wg_zlim, main="wg1_arome")
  topo()

  dev.off()
}

rm_null <- function(x){
  y <- x
  y[which(y == 0)] <- NA
  return(y)
}

WGI1 <- rm_null(wgi1)
WG1 <- rm_null(wg1)
WGI1_arome <- rm_null(wgi1_arome)
WG1_arome <- rm_null(wg1_arome)

for (i in 1:(end-start)){
  png(sprintf("tmp/%03d.png", i))

  par(mfrow=c(2,2))
  image.plot(WGI1[,,start+i-1],col=colo, zlim=wgi_zlim, main=sprintf("wgi1_offline: %s", time[start+i]))
  topo()

  image.plot(WGI1_arome[,,i+1],col=colo, zlim=wgi_zlim, main="wgi1_arome")
  topo()

  image.plot(WG1[,,start+i-1],col=colo, zlim=wg_zlim, main="wg1_offline")
  topo()

  image.plot(WG1_arome[,,i+1],col=colo, zlim=wg_zlim, main="wg1_arome")
  topo()

  dev.off()
}

