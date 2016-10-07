# Read NetCDF files created by SURFEX

library("ncdf4")


# path to SMOS files
path  <- "/disk1/asmundb/SMOS/nc/"

#files <- list.files(path=path, full.name=T) # test if nc mabye
#files <- c(files, paste(path,"/PREP_SODA_EKF.nc",sep=""))
files <-  paste(path, "SM_OPER_MIR_CLF31A_20150820T000000_20150820T235959_300_001_7.DBL.nc",sep="")

nfiles<- length(files)

vars <- c("Soil_Moisture")
nvars<- length(vars)

# read ncfile and extract variables

ncid    <- nc_open(files)

nlat    <- ncid$dim$lat$len
nlon    <- ncid$dim$lon$len

lat     <- ncid$dim$lat$vals
lon     <- ncid$dim$lon$vals

Soil_moisture <- ncvar_get(ncid, ncid$var[[vars]])

nc_close(ncid)


nn <- function(i,j,I,J){
  # returns nearest neighbour
  d <- outer((I-i)^2 , (J-j)^2)
  x <- which(min(d) == d, arr.ind=T)
  return(x)
}



write(SM_points, filename)
