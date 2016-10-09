library("maps")
library("mapproj")
library("fields")


#map(plot=T, xlim=c(min(lon),max(lon)), ylim=c(min(lat),max(lat)))
map(plot=T, xlim=c(90,180), ylim=c(-50,0),projection="cylindrical")


map_axis = par("usr")

lon_range = which(map_axis[1] < lon & map_axis[2] > lon)
lat_range = which(map_axis[3] < lat & map_axis[4] > lat)

par(new=T)
#image.plot(lon,lat,Soil_moisture)
image(     x=lon[lon_range],
           y=lat[lat_range],
           z=Soil_moisture[lon_range, lat_range])#,
           xlim=c(min(lon[lon_range]),max(lon[lon_range]),
           ylim=c(min(lat[lat_range]),max(lat[lat_range])))
