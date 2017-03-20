# Read NetCDF files with SMOS data

library("ncdf4")
source("functions.R")

#####################################################################
#################### USER PARAMETERS ################################

# path to SMOS nc files
# see Shellscripts/SMOS_extract.sh
path    <- "/disk1/asmundb/SMOS/nc/"
path <- "/lustre/storeB/users/asmundb/SMOS/nc/"

# path to save OBSERVATIONS_XXXXXXXX.DAT files (input in soda)
#outpath <- "/disk1/asmundb/SMOS/OBSERVATIONS/2016/"

#####################################################################
#####################################################################

# coordinate pairs, model grid / station points
# read from stationlist or something?
npoints <- 2
I      <- seq(from=11.04, to=11.4, length=npoints)   # longtitude
J      <- seq(from=61.649, to=61.19, length=npoints)  # latitude

# check for missmatch
if (length(I) != length(J)) {
  stop("lon and lat lengths differ")
}
# continue
npoints <- length(I)


# files to open
# should probably find a better way to run program
# and list files ??                    "SM_RE04.*"
files <- list.files(path=path, full.name=T) #test if nc mabye
A_files <- files[grep(x=files, pattern="CLF31A")]
D_files <- files[grep(x=files, pattern="CLF31D")]

# number of days
ndates <- length(A_files)

# specify variable name
vars   <- c("Soil_Moisture")
nvars  <- length(vars)

#start loop
l=1

ncid    <- nc_open(A_files[l])
nlat    <- ncid$dim$lat$len
nlon    <- ncid$dim$lon$len

x <- array(0, dim=c(nlon,nlat))
ones <- array(1, dim=c(nlon,nlat))


pb <- txtProgressBar(min = 0, max = ndates, style = 3)
for (l in 1:ndates) {

  ###################################
  # read ncfile and extract variables
  ncid_A    <- nc_open(A_files[l])
  ncid_D    <- nc_open(D_files[l])
  nlat    <- ncid_A$dim$lat$len
  nlon    <- ncid_A$dim$lon$len
  lat     <- ncid_A$dim$lat$vals
  lon     <- ncid_A$dim$lon$vals
  Soil_moisture_A <- ncvar_get(ncid_A, ncid_A$var[[vars]])
  Soil_moisture_D <- ncvar_get(ncid_D, ncid_D$var[[vars]])
  nc_close(ncid_A)
  nc_close(ncid_D)
  ###################################

#  x <- x + ones[which(!is.na(Soil_moisture_A), arr.ind=)] + ones[which(!is.na(Soil_moisture_D), arr.ind=T)]
  
  x[which(!is.na(Soil_moisture_A))] <- x[which(!is.na(Soil_moisture_A))] + 1

  setTxtProgressBar(pb, l)
}
close(pb)

x[which(x==0)] <- NA

# done 
