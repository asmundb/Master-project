library("maps")
library("mapproj")
library("fields")


#map(plot=T, xlim=c(min(lon),max(lon)), ylim=c(min(lat),max(lat)))
map(plot=T, xlim=c(-10,20), ylim=c(40,70))


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
