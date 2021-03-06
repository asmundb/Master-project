### Functions calling fortran subroutines ###


#### fnn ####
# nearest neighbour 
dyn.load("FLIB/nn_lon_lat.so")
fnn <- function(lon,lat,lon0,lat0){
  ij_out <- .Fortran("nn_lon_lat",
                     lon0=as.numeric(lon0),
                     lat0=as.numeric(lat0),
                     lon=as.numeric(lon),
                     lat=as.numeric(lat),
                     nlon=as.integer(length(lon)),
                     nlat=as.integer(length(lat)),
                     ij_out=as.integer(c(0,0)))[["ij_out"]]
  return(t(as.matrix(ij_out)))
}

#### fnn_lamb ####
dyn.load("FLIB/nn_lambert.so")
fnn_lamb <- function(lon, lat, lon0, lat0){
  if (length(lon) != length(lat)){
    stop()
  }
  npnt <- length(lon)
  x <- .Fortran("nn_lambert",
                     lon0=as.numeric(lon0),
                     lat0=as.numeric(lat0),
                     lon=as.numeric(lon),
                     lat=as.numeric(lat),
                     npnt=as.integer(npnt),
                     dist=numeric(length=npnt),
                     ij_out=as.integer(0))
  out <- list( ij_out=x$ij_out,
               dist=x$dist )
  return(out)
}
