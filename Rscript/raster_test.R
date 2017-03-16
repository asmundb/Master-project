library(ncdf4)
library(raster)
library(rasterVis)
library(maptools)
library(maps)

forcingfile <- "/lustre/storeB/users/asmundb/surfex/FORCING/domain/FORCING.nc_2014062300"

ncid <- nc_open(forcingfile)
ncol <- 111
nrow <- 111


lon <- ncvar_get(ncid, ncid$var$LON)
lat <- ncvar_get(ncid, ncid$var$LAT)
zs <-  ncvar_get(ncid, ncid$var$ZS)


var <- list( lon=matrix(lon,nrow,ncol),
             lat=matrix(lat,nrow,ncol),
             zs=matrix(zs,nrow,ncol))

#pdf("domain_forcing.pdf")
map(regions=c("Norway(?!:Svalbard)","Sweden","Denmark"), xlim=c(6,16), ylim=c(57,62))
#points(lon,lat,pch='.')
#points(10.814179,60.77770) # Kise
#points(8.5,58.5)
#text(8.5,58.5,"(8.5,58.5)",pos=1)
#points(10.1 ,59.75)
#text(10.1 ,59.75, "(10.1 ,59.75)",pos=1)
#dev.off()

#pdf("domain_forcing.pdf")
#map(regions=c("Norway(?!:Svalbard)","Sweden","Denmark"), xlim=c(6,16), ylim=c(57,62))
#image(var$zs,add=T)
#lines(var$lon[1,],var$lat[1,])
#lines(var$lon[,1],var$lat[,1])
#lines(var$lon[,111],var$lat[,111])
#lines(var$lon[111,],var$lat[111,])
#dev.off()
