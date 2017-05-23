# Read NetCDF files with SMOS data

library("ncdf4")
source("ffunctions.R")


loadSMOSTimeserie <- function(path){

  print("reading coordinates from forcing...")
  ncid <- nc_open("surfex_files/FORCING.nc")
  I  <- ncvar_get(ncid, ncid$var$LON)
  J  <- ncvar_get(ncid, ncid$var$LAT)
  nc_close(ncid)
  print("done")
  
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
  
  time <- array(dim=ndates*2)
  sm <- array(dim=c(111,111,ndates*2))
  sf <- array(dim=c(111,111,ndates*2))
  dqx <- array(dim=c(111,111,ndates*2))
  ef <- array(dim=c(111,111,ndates*2))
  
  # Mask
  
  # coordinates in SMOS file
  ncid_A    <- nc_open(A_files[1])
  lat       <- ncid_A$dim$lat$vals
  lon       <- ncid_A$dim$lon$vals
  nc_close(ncid_A)
  
  print("getting grid mask...")
  mask <- matrix(nrow=2,ncol=npoints)
  
  for (kk in 1:npoints){
      cat(kk, "of 12321\r")
      mask[,kk] <- fnn(lon,lat,I[kk],J[kk])
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
    SM_dqx_A        <- ncvar_get(ncid_A, ncid_A$var$Soil_Moisture_Dqx)
    SM_dqx_D        <- ncvar_get(ncid_D, ncid_D$var$Soil_Moisture_Dqx)
    event_flag_A    <- ncvar_get(ncid_A, ncid_A$var$Event_Flags)
    event_flag_D    <- ncvar_get(ncid_D, ncid_D$var$Event_Flags)
    sci_flag_A      <- ncvar_get(ncid_A, ncid_A$var$Science_Flags)
    sci_flag_D      <- ncvar_get(ncid_D, ncid_D$var$Science_Flags)
    nc_close(ncid_A)
    nc_close(ncid_D)
    print("done")
    ###################################
  
  
    tmp1 <- array(dim=c(111,111))
    tmp2 <- array(dim=c(111,111))
    tmp3 <- array(dim=c(111,111))
    tmp4 <- array(dim=c(111,111))
    tmp5 <- array(dim=c(111,111))
    tmp6 <- array(dim=c(111,111))
    tmp7 <- array(dim=c(111,111))
    tmp8 <- array(dim=c(111,111))

  
    # loop through points
    print("extract grid...")
    k <- 1
    for (i in 1:111){
      for (j in 1:111){
        tmp1[j,i] <- Soil_moisture_A[mask[1,k],mask[2,k]]
        tmp2[j,i] <- Soil_moisture_D[mask[1,k],mask[2,k]]
        tmp3[j,i] <- SM_dqx_A[mask[1,k],mask[2,k]]
        tmp4[j,i] <- SM_dqx_D[mask[1,k],mask[2,k]]
        tmp5[j,i] <- sci_flag_A[mask[1,k],mask[2,k]]
        tmp6[j,i] <- sci_flag_D[mask[1,k],mask[2,k]]
        tmp7[j,i] <- event_flag_A[mask[1,k],mask[2,k]]
        tmp8[j,i] <- event_flag_D[mask[1,k],mask[2,k]]
        k <- k+1
      }
    }
    
    ##################################
    print("done")
  
    print("saving...")
    yymmdd <- substr(strsplit(strsplit(A_files[idate],split="/")[[1]][length(strsplit(A_files[idate],split="/")[[1]])], split="_")[[1]][5],3,8)
  
  
    date   <- sprintf("20%s-%s-%s", substr(yymmdd,1,2), substr(yymmdd,3,4), substr(yymmdd,5,6))
    tstr6  = paste(date,"06:00", sep=" ")
    tstr18 = paste(date,"18:00", sep=" ")
    
    print(date)
  
    time[itime]   <- as.character(tstr6)  
    time[itime+1] <- as.character(tstr18)   
    sm[,,itime]   <- tmp1 
    sm[,,itime+1] <- tmp2
    sf[,,itime]   <- tmp5
    sf[,,itime+1] <- tmp6 
    dqx[,,itime]  <- tmp3
    dqx[,,itime+1]<- tmp4
    ef[,,itime]   <- tmp7
    ef[,,itime+1] <- tmp8
  
    itime <- itime+2
    print("done")
  }
  
  SMOS <- list( time=time,
                sm=sm, 
                sm_dqx=dqx,
                sci_flag=sf,
                evn_flag=ef ) 
  return(SMOS)
  
}
