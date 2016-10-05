library("maps")
library("mapproj")
library("fields")

map(regions=c("norway","sweden","denmark","finland"),projection="cylindrical")

map_axis = par("usr")

lon_range = which(map_axis[1] < lon & map_axis[2] > lon)
lat_range = which(map_axis[3] < lat & map_axis[4] > lat)

par(new=T)
#image.plot(lon,lat,Soil_moisture)
image.plot(lon_range,lat_range,Soil_moisture[lon_range, lat_range])
