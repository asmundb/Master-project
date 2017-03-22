# Read NetCDF files with SMOS data

library("ncdf4")
source("functions.R")
source("ffunctions.R")

#####################################################################
#################### USER PARAMETERS ################################

# path to SMOS nc files
# see Shellscripts/SMOS_extract.sh
#path    <- "/disk1/asmundb/SMOS/nc"
path <- "/lustre/storeB/users/asmundb/SMOS/nc"

# path to save OBSERVATIONS_XXXXXXXX.DAT files (input in soda)
#outpath <- "/disk1/asmundb/SMOS/OBSERVATIONS/"

#####################################################################
#####################################################################

# coordinate pairs, model grid / station points
# read from stationlist or something?
coords <- coords_from_stlist("stationlist.cfg")
npoints <- dim(coords)[2]

I      <- coords[1,]  # longtitude
J      <- coords[2,]  # latitude

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

dataA <- array(dim=c(ndates, npoints))
dataD <- array(dim=c(ndates, npoints))
#start loop
for (l in 1:ndates) {
  cat("loop iteration: ",l, '\n')
  ###################################
  # read ncfile and extract variables
  cat("open files..", '\n')
  ncid_A    <- nc_open(A_files[l])
  ncid_D    <- nc_open(D_files[l])
  cat("read files..",'\n')
  nlat    <- ncid_A$dim$lat$len
  nlon    <- ncid_A$dim$lon$len
  lat     <- ncid_A$dim$lat$vals
  lon     <- ncid_A$dim$lon$vals
  Soil_moisture_A <- ncvar_get(ncid_A, ncid_A$var[[vars]])
  Soil_moisture_D <- ncvar_get(ncid_D, ncid_D$var[[vars]])
  nc_close(ncid_A)
  nc_close(ncid_D)
  cat("files closed.",'\n')
  ###################################

  ###################################
  # find nearest neighbour and save value
  SM_points_A <- array(0,dim=c(npoints))  # up pass
  SM_points_D <- array(0,dim=c(npoints))  # down pass
  SM_points   <- array(0,dim=c(npoints))  # combined
  # loop through points
  cat("1",'\r')
  for (k in 1:npoints){
    SM_points_A[k] <- Soil_moisture_A[fnn(lon,lat,I[k],J[k])]
    SM_points_D[k] <- Soil_moisture_D[fnn(lon,lat,I[k],J[k])]
  }
  cat("2", "\n")
  ##################################

  ### SAVE TO dataA/D
  dataA[l,] <- SM_points_A
  dataD[l,] <- SM_points_D

  # missing values is 999
#  SM_points_A[which(is.na(SM_points_A))] <- 999
#  SM_points_D[which(is.na(SM_points_D))] <- 999

  # make input file for SODA
  # Ascending satellite (A) at H06
  # Descending ---||--- (D) at H18

  outfile <- "OBSERVATIONS_"
  yymmdd <- substr(strsplit(strsplit(A_files[l],split="/")[[1]][6], split="_")[[1]][5],3,8)
#  out_A <- paste(outpath, outfile, yymmdd,"H06", ".DAT", sep="")
#  out_D <- paste(outpath, outfile, yymmdd,"H18", ".DAT", sep="")
#  write(SM_points_A, out_A, ncolumns=1) #write to file
#  write(SM_points_D, out_D, ncolumns=1) 
  print(yymmdd)
}
# done 
