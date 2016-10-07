# Read NetCDF files with SMOS data

library("ncdf4")

# parameters 

# coordinate pairs, model grid / station points

I      <- c(11,10)  # lontitude
J      <- c(60,59)  # latitude
# check for pairity
if (length(I) != length(J)) {
  stop()
}

npoints <- length(I)

# infile for SODA
outfile <- "OBSERVATION_"


# path to SMOS files
#path  <- "/disk1/asmundb/SMOS/nc/"
path <- "../Shellscript/nc/"

# files to open
files <- list.files(path=path, full.name=T) # test if nc mabye
#files <- c(files, paste(path,"/PREP_SODA_EKF.nc",sep=""))
#files <-  paste(path, "SM_OPER_MIR_CLF31A_20150820T000000_20150820T235959_300_001_7.DBL.nc",sep="")


nfiles <- length(files)

# specify variable name
vars   <- c("Soil_Moisture")
nvars  <- length(vars)

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

SM_points <- array(0,dim=c(npoints))
for (k in 1:npoints){
  print(k)
  SM_points[k] <- Soil_moisture[nn(lon,lat,I[k],J[k])]
}

outfile <- paste(outfile,regmatches(files,regexpr("[0-9]{8}", files)),"00.dat",sep="")

write(SM_points, outfile)
