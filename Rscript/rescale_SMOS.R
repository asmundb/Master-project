library(ncdf4)


### Load openloop ###
print("Load openloop")
path  <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/OPEN_LOOP_SPINUP/ISBA"
files <- list.files(path=path,pattern="ISBA_PROGNOSTIC",recursive=T, full.names=T) 
nfiles <- length(files)


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
var_mean <- as.numeric(apply(var, c(1,2), mean))
var_sd   <- as.numeric(apply(var, c(1,2), sd))

### Load SMOS ###
print("Load SMOS")
path  <- "/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS/"
files <- list.files(path=path, full.names=T)
nfiles<- length(files)
obs <- array(dim=c(12321, nfiles))
for (i in 1:nfiles){
  obs[,i] <- read.table(file=files[i])[,1]
}
obs[which(obs==999)] <- NA

obs_mean <- as.numeric(apply(obs,1,mean,na.rm=T))
obs_sd   <- as.numeric(apply(obs,1,sd,na.rm=T))

### Rescale data #nd write to file##
print("Rescale and write")
for (i in 1:nfiles){
  obs_new = (obs[,i] - obs_mean)*(var_sd/obs_sd) + var_mean
  obs_new[is.na(obs_new)] <- 999
  outfile <- gsub("OBSERVATIONS//", "OBSERVATIONS_rescaled/", files[i])
  write(obs_new, file=outfile,ncolumns=1)
}


