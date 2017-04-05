library(ncdf4)
library(fields)

### Load SMOS ###
obs <- readRDS("SMOS_31_3.rds")


obs_mean <- apply(obs,c(1,2),mean,na.rm=T)
obs_sd   <- apply(obs,c(1,2),sd,na.rm=T)
obs_max  <- apply(obs,c(1,2),max,na.rm=T)
obs_min  <- apply(obs,c(1,2),min,na.rm=T)

obs_max[is.infinite(obs_max)] <- NA
obs_min[is.infinite(obs_min)] <- NA

obs_range <- obs_max-obs_min

### model soil moisture range ###

ncid <- nc_open("surfex_files/PREP_SODA.nc")
wwilt <- ncvar_get(ncid, ncid$var$WWILT1)
wfcp  <- ncvar_get(ncid, ncid$var$WFC1)
wsat  <- ncvar_get(ncid, ncid$var$WSAT1)
nc_close(ncid)

mod_range <- wsat-wwilt

### Rescale data #nd write to file##

obs_new <- obs
for (i in 1:dim(obs)[3]){
  obs_new[,,i] = (obs[,,i] - obs_min)*(mod_range/obs_range) + wwilt
}

obs_new_mean <- apply(obs_new,c(1,2),mean,na.rm=T)
obs_new_sd   <- apply(obs_new,c(1,2),sd,na.rm=T)
obs_new_max  <- apply(obs_new,c(1,2),max,na.rm=T)
obs_new_min  <- apply(obs_new,c(1,2),min,na.rm=T)
obs_new_max[is.infinite(obs_new_max)] <- NA
obs_new_min[is.infinite(obs_new_min)] <- NA

obs_new_range <- obs_new_max-obs_new_min

#image.plot(obs_new_max- obs_max, col=rev(tim.colors()))

obs_new[is.na(obs_new)] <- 999

times <- seq(as.POSIXlt("2016-04-09 06:00"), length=400, by=12*3600)
times_fmt <- format(times, format="%y%m%dH%H")
for (i in 1:400){
  filename <- sprintf("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS_sat-wilt/OBSERVATIONS_%s.DAT",times_fmt[i] )
  print(filename)
  write(as.numeric(obs_new[,,i]), file=filename, ncolumns=1)
}


