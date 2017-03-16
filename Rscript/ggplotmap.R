library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
gpclibPermit()
library(mapproj)
library(ncdf4)


forcingfile <- "/lustre/storeB/users/asmundb/surfex/FORCING/domain/FORCING.nc_2016100700"

ncid <- nc_open(forcingfile)
ncol <- 111
nrow <- 111


lon2d <- ncvar_get(ncid, ncid$var$LON)
lat2d <- ncvar_get(ncid, ncid$var$LAT)
zs <-  ncvar_get(ncid, ncid$var$ZS)
tair <- ncvar_get(ncid, ncid$var$Tair)[,1]
dat <- data.frame(lon2d,lat2d,tair)


var <- list( lon=matrix(lon2d,nrow,ncol),
             lat=matrix(lat2d,nrow,ncol),
             zs=matrix(zs,nrow,ncol))


plotMap <- function(lon2d, lat2d, var, ...){
  dat <- data.frame(lon2d,lat2d,var)
  
  argnames <- names(list(...))
  if (!("xlim" %in% argnames)){
    xlim <- c(min(lon2d),max(lon2d))
  }else{
    xlim <- list(...)[["xlim"]]
  }
  if (!("ylim" %in% argnames)){
    ylim <- c(min(lat2d),max(lat2d))
  }else{
    ylim <- list(...)[["ylim"]]
  }

  # Get the map outlines
  outlines <- as.data.frame(map("world", plot = FALSE, 
                              xlim = c(min(lon2d), max(lon2d)), 
                              ylim = c(min(lat2d), max(lat2d)))[c("x","y")])
  worldmap <-geom_path(aes(x, y), inherit.aes = FALSE, 
                       data = outlines, alpha = 0.8, show.legend = T)

  # The layer for the observed variable
  zsmap <- geom_point(aes(x=lon2d, y=lat2d, colour=var), data=dat) 

  # Prjoection
  projection <- coord_map(projection="lambert", lat0=57.5, lat1=57.5, orientation=c(90,0,10.1),
                          xlim=xlim, ylim=ylim)
  
  # Colour
  colors <- scale_colour_continuous(breaks=c(250,300,350))

  # Plot map
  ggplot() + zsmap + worldmap + projection + colors
}

