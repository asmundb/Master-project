require(ncdf4)
require(fields)

get_smos <- function(path){
  files <- list.files(path=path, full.names=T)
  nfiles<- length(files)
  obs <- array(dim=c(12321, nfiles))
  for (i in 1:nfiles){
    obs[,i] <- read.table(file=files[i])[,1]
  }
  obs[which(obs==999)] <- NA 
  obs_m <- array(NA, dim=c(111,111,nfiles))
  for (i in 1:nfiles){
    obs_m[,,i] <- matrix(obs[,i],111,111)
  }
  return(obs_m)
}
 

SMOS <- get_smos("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS/")
SMOS_lin <- get_smos("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS_rescaled/")
SMOS_sw <- get_smos("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS_sat-wilt/")


### Load openloop ###
print("Load openloop")
path  <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/OPEN_LOOP_SPINUP/ISBA"
files <- list.files(path=path,pattern="ISBA_PROGNOSTIC",recursive=T, full.names=T)
nfiles <- length(files)
var <- array(dim=c(111,111,6*nfiles))
k=1
for (i in 1:nfiles){
  cat(i,"/",nfiles,"\r")
  ncid <- nc_open(files[i])
  tmp <- ncvar_get(ncid, ncid$var$WG2)
  nc_close(ncid)
  var[,,k:(k+5)] <- tmp
  k <- k+6
  flush.console()
}


pdf("figures/rescale_evaluation/histogram_obs.pdf")
par(mfrow=c(2,2))
hist(SMOS)
hist(SMOS_lin)
hist(SMOS_sw)
hist(var)
dev.off()
