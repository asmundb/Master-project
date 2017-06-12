library(ncdf4)


### Load openloop ###
print("Load openloop")
#path  <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/OPEN_LOOP_SPINUP/ISBA"
path  <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2014/SPINUP/ISBA"
files <- list.files(path=path,pattern="ISBA_PROGNOSTIC",recursive=T, full.names=T) 
nfiles <- length(files)

files <- files[125:nfiles]
nfiles <- length(files)

time <- seq(as.POSIXlt("2014-06-01 01:00"), as.POSIXlt("2014-09-01 00:00"), by=3600)
Atime <- grep("06:00",as.character(time))
Dtime <- grep("18:00",as.character(time))


var <- array(dim=c(111,111,6*nfiles))
k=1
for (i in 1:nfiles){
  ncid <- nc_open(files[i])
  tmp <- ncvar_get(ncid, ncid$var$WG2)
  nc_close(ncid)
  var[,,k:(k+5)] <- tmp
  k <- k+6
}

### Calculate mean and sd ###
Avar_mean <- as.numeric(apply(var[,,Atime], c(1,2), mean))
Avar_sd   <- as.numeric(apply(var[,,Atime], c(1,2), sd))

Dvar_mean <- as.numeric(apply(var[,,Dtime], c(1,2), mean))
Dvar_sd   <- as.numeric(apply(var[,,Dtime], c(1,2), sd))

## Load SMOS ###
print("Load SMOS")
path  <- "/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS/"
files <- list.files(path=path,,pattern="OBSERVATIONS_14", full.names=T)
nfiles<- length(files)

ASMOSfiles <- files[grep("H06",files)]
ASMOSfiles <- ASMOSfiles[53:(53+91)]
DSMOSfiles <- files[grep("H18",files)]
DSMOSfiles <- DSMOSfiles[53:(53+91)]

nfiles <- length(ASMOSfiles)

Aobs <- array(dim=c(12321, nfiles))
Dobs <- array(dim=c(12321, nfiles))
for (i in 1:nfiles){
  Aobs[,i] <- read.table(file=ASMOSfiles[i])[,1]
  Dobs[,i] <- read.table(file=DSMOSfiles[i])[,1]
}
Aobs[which(Aobs==999)] <- NA
Dobs[which(Dobs==999)] <- NA

Aobs_mean <- as.numeric(apply(Aobs,1,mean,na.rm=T))
Aobs_sd   <- as.numeric(apply(Aobs,1,sd,na.rm=T))

Dobs_mean <- as.numeric(apply(Dobs,1,mean,na.rm=T))
Dobs_sd   <- as.numeric(apply(Dobs,1,sd,na.rm=T))

### Rescale data #nd write to file##
print("Rescale and write")
for (i in 1:nfiles){
  Aobs_new = (Aobs[,i] - Aobs_mean)*(Avar_sd/Aobs_sd) + Avar_mean
  Dobs_new = (Dobs[,i] - Dobs_mean)*(Dvar_sd/Dobs_sd) + Dvar_mean
  Aobs_new[is.na(Aobs_new)] <- 999
  Dobs_new[is.na(Dobs_new)] <- 999
  Aoutfile <- gsub("OBSERVATIONS//", "OBSERVATIONS_rescaled/", ASMOSfiles[i])
  Doutfile <- gsub("OBSERVATIONS//", "OBSERVATIONS_rescaled/", DSMOSfiles[i])
  write(Aobs_new, file=Aoutfile,ncolumns=1)
  write(Dobs_new, file=Doutfile,ncolumns=1)
}


