require(h5)

rot <- function(x) t(apply(x, 2, rev))




files <- list.files("/lustre/storeB/users/asmundb/SMAP/n5eil01u.ecs.nsidc.org/SMAP/SPL3SMP.004/",
                    pattern="SMAP_L3_SM_P", full.names=T, recursive=T)
files <- files[1:(length(files)-1)]
nfiles <- length(files)


lonInt=503:516
latInt=377:381

sm_e <- array(NA, dim=c(964,406,2,nfiles))

for (i in 1:nfiles){
  file <- files[i]
  f <- h5file(file,"r")
  sm_e[,,1,i] <- f["/Soil_Moisture_Retrieval_Data_AM/soil_moisture_error"][,]
  sm_e[,,2,i] <- f["/Soil_Moisture_Retrieval_Data_PM/soil_moisture_error_pm"][,]
  
  h5close(f)
}

plot_var <- function(dataset){
  sm <- f[dataset]
  lon <- rot(f["/Soil_Moisture_Retrieval_Data_AM/longitude"][,])[lonInt,latInt]
  lat <- rot(f["/Soil_Moisture_Retrieval_Data_AM/latitude"][,])[lonInt,latInt]
  y <- rot(sm[,])[lonInt,latInt]
  print(dim(y))
  if (min(y,na.rm=T) == max(y,na.rm=T)){
    print("nada")
  }else{
#    par(mfrow=c(2,1))
    taby <- table(y)
    n <- min(15,length(taby),na.rm=T)
    print(taby[1:n])
    y[which(y == -9999)] <- NA
    image.plot(lon,lat,y,main=dataset)
  }
  return(y)
}



datasets <- list.datasets(f)
for (i in 3:length(datasets)){
  plot_var(datasets[i])
  readline(prompt="Press [enter] to continue")
}


library(ncdf4)
library(fields)

### Forcing.nc ###
#
#  contains lon, lat, and zs (topography)
#
print("read coordinates from forcing file...")
filename <- "surfex_files/FORCING.nc"
ncid <- nc_open(filename)
lon  <- ncvar_get(ncid, ncid$var$LON)
lat  <- ncvar_get(ncid, ncid$var$LAT)
zs   <- ncvar_get(ncid, ncid$var$ZS)
nc_close(ncid)
mylon <- matrix(lon,111,111)
mylat <- matrix(lat,111,111)
mytopo <- matrix(zs,111,111)
myland <- (mytopo > 2)*1

seamask <- which(myland == 0)

y <-  plot_var("/Soil_Moisture_Retrieval_Data_AM/surface_flag")
image.plot(mylon,mylat,myland,add=T,col=two.colors(2,"blue","white",alpha=0.2),zlim=c(0,1))


