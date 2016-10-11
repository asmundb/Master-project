library("maps")
library("mapproj")
library("fields")


#map(plot=T, xlim=c(min(lon),max(lon)), ylim=c(min(lat),max(lat)))

# Norway
xl <- c(4,18)
yl <- c(58,68)

# World
#l <- c(min(lon),max(lon))
#l <- c(min(lat),max(lat))

#ev.off()
#ev.off()


map(plot=T, xlim=xl, ylim=yl)


map_axis = par("usr")

lon_range = which(map_axis[1] < lon & map_axis[2] > lon)
lat_range = which(map_axis[3] < lat & map_axis[4] > lat)

par(new=T)
#image.plot(lon,lat,Soil_moisture)
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

dev.new()

map(plot=T, xlim=xl, ylim=yl)
par(new=T)
image(x=lon[lon_range],
      y=lat[lat_range],
	  z=x[lon_range, lat_range],
      xlim=c(min(lon[lon_range]),max(lon[lon_range])),
      ylim=c(min(lat[lat_range]),max(lat[lat_range])),
	  col=rev(gray.colors(10)))

source("stations.R")
points(x=stas$LON, y=stas$LAT)



