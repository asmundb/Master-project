# Read NetCDF files with SMOS data

library("ncdf4")
source("ffunctions.R")

#####################################################################
#################### USER PARAMETERS ################################

# path to SMOS nc files
# see Shellscripts/SMOS_extract.sh
path    <- "/lustre/storeB/users/asmundb/SMOS/nc"

# path to save OBSERVATIONS_XXXXXXXX.DAT files (input in soda)
#outpath <- "/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS/"

#####################################################################
#####################################################################

# coordinate pairs, model grid / station points
# read from forcingfile ?

print("reading coordinates from forcing...")
ncid <- nc_open("/lustre/storeB/users/asmundb/surfex/FORCING/domain/FORCING.nc_2016100700")
I  <- ncvar_get(ncid, ncid$var$LON)
J  <- ncvar_get(ncid, ncid$var$LAT)
#zs   <- get_ncvar(ncid, ncid$var$ZS)
nc_close(ncid)
print("done")
#npoints <- length(lon)#dim(coords)[2]


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
files <- list.files(path=path, pattern="SM_OPER.*", full.name=T) # test if nc mabye
A_files <- files[grep(x=files, pattern="CLF31A")]
D_files <- files[grep(x=files, pattern="CLF31D")]

# number of days
ndates <- length(A_files)

# specify variable name
vars   <- c("Soil_Moisture")
nvars  <- length(vars)
print("done")

## Array to save in ##

sm <- array(dim=c(111,111,ndates*2))

#start loop
#pb <- txtProgressBar(min = 1, max = ndates, style = 3)

# Mask

# coordinates in SMOS file
ncid_A    <- nc_open(A_files[1])
lat       <- ncid_A$dim$lat$vals
lon       <- ncid_A$dim$lon$vals
nc_close(ncid_A)

print("getting grid mask...")
mask <- matrix(nrow=2,ncol=npoints)
for (k in 1:npoints){
    cat(k, "of 12321\r")
    mask[,k] <- fnn(lon,lat,I[k],J[k])
    flush.console()
}
print("done")

itime <- 1
for (idate in 1:ndates) {
#  cat(idate, "of ", ndates,"\r")
  print(idate)
  ###################################
  # read ncfile and extract variables
  print("reading ncfiles...")
  ncid_A    <- nc_open(A_files[idate])
  ncid_D    <- nc_open(D_files[idate])
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
#  print("finding nearest neighbour...")
  SM_points_A <- array(0,dim=c(npoints))  # up pass
  SM_points_D <- array(0,dim=c(npoints))  # down pass
#  SM_points   <- array(0,dim=c(npoints))  # combined

  # make selection
#  lons <- which(lon >= min(I)-2 & lon <= max(I) +2)
#  lats <- which(lat >= min(J)-2 & lat <= max(J) +2)



  tmp1 <- array(dim=c(111,111))
  tmp2 <- array(dim=c(111,111))
  # loop through points
  print("extract grid...")
  k <- 1
  for (i in 1:111){
    for (j in 1:111){
      tmp1[j,i] <- Soil_moisture_A[mask[1,k],mask[2,k]]
      tmp2[j,i] <- Soil_moisture_D[mask[1,k],mask[2,k]]
      k <- k+1
    }
  }
#    cat(k, "of 12321\r")
  #print("extract grid...")
#    SM_points_A <- Soil_moisture_A[mask]
#    SM_points_D <- Soil_moisture_D[mask]
#    flush.console()
#  }
  
  ##################################
  print("done")

  # missing value is 999
#  SM_points_A[which(is.na(SM_points_A))] <- 999
#  SM_points_D[which(is.na(SM_points_D))] <- 999


  # make input file for SODA
  # Ascending satellite (A) at H06
  # Descending ---||--- (D) at H18
  print("writing OBSERVATIONS files...")
#  outfile <- "OBSERVATIONS_"
#  yymmdd <- substr(strsplit(strsplit(A_files[l],split="/")[[1]][length(strsplit(A_files[l],split="/")[[1]])], split="_")[[1]][5],3,8)
  sm[,,itime]   <- tmp1 #matrix(SM_points_A,111,111)
  sm[,,itime+1] <- tmp2 #matrix(SM_points_D,111,111)
  itime <- itime+2
#  out_A <- paste(outpath, outfile, yymmdd,"H06", ".DAT", sep="")
#  out_D <- paste(outpath, outfile, yymmdd,"H18", ".DAT", sep="")
#  write(SM_points_A, out_A, ncolumns=1) #write to file
#  write(SM_points_D, out_D, ncolumns=1) 
#  print(yymmdd)
 # print("done")
 # setTxtProgressBar(pb, l)
  print("done")
}
# done 
