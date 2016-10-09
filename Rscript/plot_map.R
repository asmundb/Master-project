library("maps")
library("mapproj")
library("fields")


#map(plot=T, xlim=c(min(lon),max(lon)), ylim=c(min(lat),max(lat)))
map(plot=T, xlim=c(-10,35), ylim=c(50,72))

map_axis = par("usr")

lon_range = which(map_axis[1] < lon & map_axis[2] > lon)
lat_range = which(map_axis[3] < lat & map_axis[4] > lat)

par(new=T)
#image.plot(lon,lat,Soil_moisture)
image(lon[lon_range],lat[lat_range],Soil_moisture[lon_range, lat_range])
