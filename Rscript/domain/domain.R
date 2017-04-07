library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
gpclibPermit()
library(mapproj)
library(ncdf4)
library(fields)

### Forcing.nc ###
#
#  contains lon, lat, and zs (topography)
#

filename <- "../surfex_files//FORCING.nc"
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

filename <- "../surfex_files//PGD_2D.nc"
ncid <- nc_open(filename)
frac_nature <- ncvar_get(ncid, ncid$var$FRAC_NATURE)
nx <- ncid$dim$xx$len
ny <- ncid$dim$yy$len
nc_close(ncid)

### PREP.nc ###

filename <- "../surfex_files//PREP_SODA.nc"
ncid <- nc_open(filename)
wwilt1 <- ncvar_get(ncid, ncid$var$WWILT1)
wfc1   <- ncvar_get(ncid, ncid$var$WFC1)
nc_close(ncid)

### AROME-METCOOP ###

filename <- "../surfex_files//AROME_MetCoOp_00_sfx.nc_20161010"
ncid <- nc_open(filename)
lon_arome <- ncvar_get(ncid, ncid$var$longitude)
lat_arome <- ncvar_get(ncid, ncid$var$latitude)
wg1_arome <- ncvar_get(ncid, ncid$var$WG1)
wgi1_arome <- ncvar_get(ncid, ncid$var$WGI1)
nc_close(ncid)


m######

#plotMap <- function(lon2d, lat2d, var, ...){
# Plot using ggplot2
#

#### OFFLINE GRID ####
  

  domBord <- c(1:nx, seq(nx, by=nx, length=ny), (nx*ny):(nx*ny-nx-1), rev(seq(1, by=nx,length=ny)))
#  group <- sort(rep(1:4,111))

  mydom <- data.frame(lon=lon[domBord],lat=lat[domBord])

######################

#### AROME GRID ######

  mx <- dim(lon_arome)[1]
  my <- dim(lat_arome)[2]
  lon_edge_arome <- c(lon_arome[1:mx,1], lon_arome[mx,1:my], lon_arome[mx:1,my], lon_arome[1,my:1])
  lat_edge_arome <- c(lat_arome[1:mx,1], lat_arome[mx,1:my], lat_arome[mx:1,my], lat_arome[1,my:1])

  aromedom <- data.frame(lon=lon_edge_arome, lat=lat_edge_arome)


######################

  # Get the map outlines
#  outlines <- as.data.frame(map("world", plot = FALSE, xlim = c(min(lon), max(lon)), ylim = c(min(lat), max(lat)))[c("x","y")])

  xlim <- c(min(lon_arome)+5,max(lon_arome)-5)
  ylim <- c(min(lat_arome)-5,max(lat_arome))

  outline  <- as.data.frame(map("world", plot=F)[c("x","y")])
  worldmap <-geom_path(aes(x, y), inherit.aes = FALSE,   data = outline, alpha = 0.8, show.legend = T)

  mydomain <- geom_polygon(data=mydom, aes(x=lon, y=lat), fill=NA, color="blue")
  
  aromeDom <- geom_polygon(data=aromedom, aes(x=lon,y=lat), fill=NA, color="red")

  projection <- coord_map(projection="lambert", lat0=57.5, lat1=57.5, orientation=c(90,0,15),
                          xlim=xlim, ylim=ylim)

  ggplot() + worldmap + mydomain + aromeDom + projection


  # The layer for the observed variable
  zsmap <- geom_point(aes(x=lon2d, y=lat2d, colour=var), data=dat, shape=15, size=10)

  # Prjoection
  projection <- coord_map(projection="lambert", lat0=57.5, lat1=57.5, orientation=c(90,0,10.1),
                          xlim=xlim, ylim=ylim)

  # Colour
  colors <- scale_colour_continuous(breaks=c(250,300,350))

  # Plot map
  ggplot() + zsmap + worldmap + projection #+ colors


outlines <- as.data.frame(map("world", plot = FALSE, xlim = c(min(lon), max(lon)), ylim = c(min(lat), max(lat)))[c("x","y")])


