require(ncdf4)

nx <- 111
ny <- 111

# MAKE TIME ARRAY
time <- seq(as.POSIXlt("2016-05-01 01:00"), as.POSIXlt("2016-10-17 00:00"), by=3600)

start <- which(as.character(time)=="2016-10-06 01:00:00")
end <- length(time)
ntimes <- end-start

ij <- readRDS("RDS_files/ij_mask.rds")


path <- "/lustre/storeB/immutable/short-term-archive/DNMI_AROME_METCOOP/2016/10"
files1 <- list.files(path=path,pattern="AROME_MetCoOp_.*_fp.nc_.*", recursive=T,full.names=T)
files1 <- files1[40:122]
files1 <- files1[grep("_00_|_06_|_12_|_18_",files1)]


#####################################################################


files <- array(NA, dim=10*4)
dd <- sprintf("%02d",5:16)
hh <- c("00","06","12","18")

k=1
for (i in 1:length(dd)){
  jj <- 1
  for (j in 1:4){
    files[k] <- sprintf("%s/%s/AROME_MetCoOp_%s_fp.nc_201610%s",path,dd[i],hh[jj],dd[i])
    jj <- jj + 1
    k <- k+1
  }
}



tmp1 <- array(NA,dim=c(739, 949, ntimes+1))
tmp2 <- array(NA,dim=c(739, 949, ntimes+1))
tmp3 <- array(NA,dim=c(739, 949, ntimes+1))

print("read nc files")
k=1
#loop
for (i in 5:length(files)){
  cat(i, "of",length(files) , "\r")
  if ( ! files[i] %in% files1 ){
    if ( ! files[i-1] %in% files1 ){
      ncid <- nc_open(files[i-2])
      get <- 13:18
    }else{
      ncid <- nc_open(files[i-1])
      get <- 7:12
    }
  } else { 
    ncid <- nc_open(files[i])
    get <- 1:6
  }
  tmp2[,,k:(k+5)] <- ncvar_get(ncid, ncid$var$water_evaporation_amount)[,,get]
  tmp1[,,k:(k+5)] <- ncvar_get(ncid, ncid$var$cloud_area_fraction)[,,get]
  tmp3[,,k:(k+5)] <- ncvar_get(ncid, ncid$var$atmosphere_boundary_layer_thickness)[,,get]
  k=k+6
  nc_close(ncid)
  flush.console()
}
print("done")

evap_arome <- array(NA,dim=c(nx,ny,ntimes+1))
cf_arome <- array(NA,dim=c(nx,ny,ntimes+1))
ablt <- array(NA,dim=c(nx,ny,ntimes+1))

k=1
for (i in 1:ntimes){
  ablt <- matrix(array(tmp3[,,i])[ij], nx, ny)
  evap_arome[,,i] <- matrix(array(tmp2[,,i])[ij], nx, ny)
  cf_arome[,,i] <- matrix(array(tmp1[,,i])[ij], nx, ny)
}

#stop()

#kkk <- apply(cf_arome, c(1,2), sum, na.rm=T)

#pdf("figures/iceEvap/cloudCoverSum.pdf")
#image.plot(kkk/dim(cf_arome)[3], main="Average cloud cover AROME")
#dev.off()



