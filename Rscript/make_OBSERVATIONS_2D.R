# Read NetCDF files with SMOS data

library("ncdf4")
source("functions.R")
source("ffunctions.R")

#####################################################################
#################### USER PARAMETERS ################################

# path to SMOS nc files
# see Shellscripts/SMOS_extract.sh
path    <- "/lustre/storeB/users/asmundb/SMOS/nc"

# path to save OBSERVATIONS_XXXXXXXX.DAT files (input in soda)
outpath <- "/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS/"

#####################################################################
#####################################################################

# coordinate pairs, model grid / station points
# read from forcingfile ?

print("reading coordinates from forcing...")
ncid <- nc_open("/lustre/storeB/users/asmundb/surfex/FORCING/domain/FORCING.nc_2016100700")
lon  <- ncvar_get(ncid, ncid$var$LON)
lat  <- ncvar_get(ncid, ncid$var$LAT)
#zs   <- get_ncvar(ncid, ncid$var$ZS)
nc_close(ncid)
print("done")
#npoints <- length(lon)#dim(coords)[2]

I      <- lon #coords[1,]  # longtitude
J      <- lat #coords[2,]  # latitude

# check for missmatch
if (length(I) != length(J)) {
  stop("lon and lat lengths differ")
}
# continue
npoints <- length(I)


# files to open
# should probably find a better way to run program
# and list files ??                    "SM_RE04.*"
print("listing files...")
files <- list.files(path=path, pattern="SM_RE04.*", full.name=T) # test if nc mabye
A_files <- files[grep(x=files, pattern="CLF31A")]
D_files <- files[grep(x=files, pattern="CLF31D")]

# number of days
ndates <- length(A_files)

ncid <- nc_open(A_files[1])
lat1     <- ncid$dim$lat$vals
lon1     <- ncid$dim$lon$vals
nc_close(ncid)

print("finding nearest neighbour")
ij <- matrix(NA, npoints,2)
k=1
for (k in 1:npoints){
  cat(k, "of 12321\r")
  ij[k,] <- fnn(lon1,lat1,I[k],J[k])
  flush.console()
}




# specify variable name
vars   <- c("Soil_Moisture")
nvars  <- length(vars)
print("done")
#start loop
#pb <- txtProgressBar(min = 1, max = ndates, style = 3)
for (l in 1:ndates) {

  ###################################
  # read ncfile and extract variables
  print("reading ncfiles...")
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
  print("done")
  ###################################

  ###################################
  # find nearest neighbour and save value
  print("Reading variables...")
  SM_points_A <- array(0,dim=c(npoints))  # up pass
  SM_points_D <- array(0,dim=c(npoints))  # down pass
  SM_points   <- array(0,dim=c(npoints))  # combined

  # make selection
  lons <- which(lon >= min(I)-2 & lon <= max(I) +2)
  lats <- which(lat >= min(J)-2 & lat <= max(J) +2)

  # loop through points
  for (k in 1:npoints){
#    cat(k, "of 12321\r")
    SM_points_A[k] <- Soil_moisture_A[ij[k,1], ij[k,2]]
    SM_points_D[k] <- Soil_moisture_D[ij[k,1], ij[k,2]]
#    flush.console()
  }
  ##################################
  print("done")

  # missing value is 999
  SM_points_A[which(is.na(SM_points_A))] <- 999
  SM_points_D[which(is.na(SM_points_D))] <- 999


  # make input file for SODA
  # Ascending satellite (A) at H06
  # Descending ---||--- (D) at H18
  print("writing OBSERVATIONS files...")
  outfile <- "OBSERVATIONS_"
  yymmdd <- substr(strsplit(strsplit(A_files[l],split="/")[[1]][length(strsplit(A_files[l],split="/")[[1]])], split="_")[[1]][5],3,8)
  out_A <- paste(outpath, outfile, yymmdd,"H06", ".DAT", sep="")
  out_D <- paste(outpath, outfile, yymmdd,"H18", ".DAT", sep="")
  write(SM_points_A, out_A, ncolumns=1) #write to file
  write(SM_points_D, out_D, ncolumns=1) 
  print(yymmdd)
  print("done")
 # setTxtProgressBar(pb, l)
}
# done 
