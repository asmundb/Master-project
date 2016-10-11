# Read NetCDF files with SMOS data

library("ncdf4")
source("functions.R")

#####################################################################
#################### USER PARAMETERS ################################

# path to SMOS nc files
# see Shellscripts/SMOS_extract.sh
path    <- "/disk1/asmundb/SMOS/nc/"

# path to save OBSERVATIONS_XXXXXXXX.DAT files (input in soda)
outpath <- "/disk1/asmundb/SMOS/OBSERVATIONS/2016/"

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
files <- list.files(path=path, pattern="SM_OPER.*", full.name=T) # test if nc mabye
A_files <- files[grep(x=files, pattern="CLF31A")]
D_files <- files[grep(x=files, pattern="CLF31D")]

# number of days
ndates <- length(A_files)

# specify variable name
vars   <- c("Soil_Moisture")
nvars  <- length(vars)

#start loop
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

  ###################################
  # find nearest neighbour and save value
  SM_points_A <- array(0,dim=c(npoints))  # up pass
  SM_points_D <- array(0,dim=c(npoints))  # down pass
  SM_points   <- array(0,dim=c(npoints))  # combined
  # loop through points
  for (k in 1:npoints){
    SM_points_A[k] <- Soil_moisture_A[nn(lon,lat,I[k],J[k])]
    SM_points_D[k] <- Soil_moisture_D[nn(lon,lat,I[k],J[k])]
  }
  ##################################
  # average over up and down pass + minimize NAs
  SM_points <- colMeans(rbind(SM_points_A,SM_points_D),na.rm=T)

  # make input file for SODA
  outfile <- "OBSERVATIONS_"
  outfile_ending <- paste(regmatches(files[l], 
                    regexpr("([0-9]{6})T([0-9]{2})", files[l])), sep="")
  outfile_ending <- gsub(x=outfile_ending, pattern="T", replacement="H")
  outfile <- paste(outpath, outfile, outfile_ending, ".DAT", sep="")
  write(SM_points, outfile, ncolumns=1) #write to file
}
# done 
