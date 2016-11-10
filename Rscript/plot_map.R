library("maps")
library("mapproj")
library("fields")

source("functions.R")
#source("read_SMOS_data.R")


#map(plot=T, xlim=c(min(lon),max(lon)), ylim=c(min(lat),max(lat)))

# Norway
xl <- c(5,17)
yl <- c(58,65)

# World
#xl <- c(min(lon),max(lon))
#yl <- c(min(lat),max(lat))

#ev.off()
#ev.off()

#pdf("SMOS_SWOTS.pdf")
map(plot=T, xlim=xl, ylim=yl)


map_axis = par("usr")

lon_range = which(map_axis[1] < lon & map_axis[2] > lon)
lat_range = which(map_axis[3] < lat & map_axis[4] > lat)

par(new=T)
#image.plot(lon,lat,x)
image(     x=lon[lon_range],
           y=lat[lat_range],
           z=Soil_moisture_A[lon_range, lat_range],
           xlim=c(min(lon[lon_range]),max(lon[lon_range])),
           ylim=c(min(lat[lat_range]),max(lat[lat_range])))
par(new=T)

image(     x=lon[lon_range],
           y=lat[lat_range],
		   z=Soil_moisture_D[lon_range, lat_range],
		   xlim=c(min(lon[lon_range]),max(lon[lon_range])),
		   ylim=c(min(lat[lat_range]),max(lat[lat_range])))
dev.off()

#dev.new()
pdf("stat_map.pdf")

map(plot=T, xlim=xl, ylim=yl)

map_axis = par("usr")

lon_range = which(map_axis[1] < lon & map_axis[2] > lon)
lat_range = which(map_axis[3] < lat & map_axis[4] > lat)

par(new=T)
image(x=lon[lon_range],
      y=lat[lat_range],
	  z=x[lon_range, lat_range],
      xlim=c(min(lon[lon_range]),max(lon[lon_range])),
      ylim=c(min(lat[lat_range]),max(lat[lat_range])),
	  col=rev(gray.colors(10)))
#dev.off()
stas <- read_stlist("stationlist.cfg")

points(x=stas[,2], y=stas[,3])
text(stas[,2],stas[,3],labels=dimnames(stas)[[1]],pos=4)
dev.off()
