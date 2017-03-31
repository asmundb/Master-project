library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
gpclibPermit()
library(mapproj)
library(ncdf4)
library(fields)

######################### CONSTANTS ############################
### Forcing.nc ###
#
#  contains lon, lat, and zs (topography)
#

filename <- "surfex_files/FORCING.nc"
ncid <- nc_open(filename)
lon  <- ncvar_get(ncid, ncid$var$LON)
lat  <- ncvar_get(ncid, ncid$var$LAT)
zs   <- ncvar_get(ncid, ncid$var$ZS)
nc_close(ncid)

### PGD.nc ###
# 
#  contains frac_nature (points which is assimilated
#59.950516, 10.803464
#

filename <- "surfex_files/PGD_2D.nc"
ncid <- nc_open(filename)
frac_nature <- ncvar_get(ncid, ncid$var$FRAC_NATURE)
nx <- ncid$dim$xx$len
ny <- ncid$dim$yy$len
nc_close(ncid)

### PREP.nc ###

filename <- "surfex_files/PREP_SODA.nc"
ncid <- nc_open(filename)
wwilt1 <- ncvar_get(ncid, ncid$var$WWILT1)
wfc1   <- ncvar_get(ncid, ncid$var$WFC1)
nc_close(ncid)
################################################################

### AROME-METCOOP ###

filename <- "surfex_files/AROME_MetCoOp_00_sfx.nc_20161010"
ncid <- nc_open(filename)
lon_arome <- ncvar_get(ncid, ncid$var$longitude)
lat_arome <- ncvar_get(ncid, ncid$var$latitude)
wg1_arome <- ncvar_get(ncid, ncid$var$WG1)
wgi1_arome <- ncvar_get(ncid, ncid$var$WGI1)
nc_close(ncid)

lonlat0 <- arrayInd(which.min( (lat_arome - min(lat))^2 + (lon_arome - min(lon))^2 ), dim(lat_arome))
lons <- seq(lonlat0[1], by=1, length.out=111)
lats <- seq(lonlat0[2], by=1, length.out=111)
wg1_arome_mygrid <- wg1_arome[lons,lats,]
#rm(wg1_arome)

source("ffunctions.R")
ij <- numeric(length(lon))
dist <- numeric(length(lon))

for ( k in 1:length(lon)){
  x <- fnn_lamb(array(lon_arome), array(lat_arome), lon[k], lat[k])
  ij[k]   <- x$ij_out
  dist[k] <- x$dist[x$ij_out]
}
wg1_arome_mygrid2 <- array(dim=c(111,111,dim(wg1_arome)[3]))
for (i in 1:dim(wg1_arome)[3]){
  wg1_arome_mygrid2[,,i] <- matrix(array(wg1_arome[,,i])[ij], nx, ny)
}

### SURFEX-OFFLINE ###

path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/openloop/ISBA/"

loadISBA <- function(path, filename, varname){
  require(ncdf4)
  files <- list.files(path=path,
                    pattern=filename,
                    recursive=T,
                    full.name=T)
  nfiles <- length(files)

  var    <- array(dim=c(nx,ny,nfiles*6))
  tmp    <- array(dim=c(nx,ny))
  kk     <- 1
  for (i in 1:nfiles){
    ncid <- nc_open(files[i])
    tmp <- ncvar_get(ncid, ncid$var[[varname]])
    var[,,kk:(kk+5)] <- tmp
    kk <- kk+6 
    nc_close(ncid)
  }
  return(var)
}

### plots ###
# static
image.plot(frac_nature, col=two.colors(n=256,start="blue", end="darkgreen", middle="orange"))
contour(matrix(zs, 111,111), add=T, levels=c(0,2,10,50,100,200,500,1000))

# time 
time <- seq(as.POSIXlt("2016-10-07 06:00"),length=16, by=3600*12)

