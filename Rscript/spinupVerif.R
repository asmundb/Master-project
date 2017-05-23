# load wg & tg for complete period

require(ncdf4)
require(fields)

path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/OPEN_LOOP_SPINUP/ISBA/"

files <- list.files(path=path, pattern="ISBA_PROGNOSTIC.OUT.nc",recursive=T,full.names=T)
nfiles <- length(files)

ntimes <- nfiles*6
nvar   <- 7

cvar <- paste("WG",1:nvar,sep="")
dvar <- paste("TG",1:nvar,sep="")

WG <- array(0, dim=c(nvar,ntimes))
TG <- array(0, dim=c(nvar,ntimes))

k <- 1
# loop
for (i in 1:nfiles){
  cat(i,"/",nfiles,"\r") 
  ncid <- nc_open(files[i])
  for (ivar in 1:nvar){
#    WG[ivar,k:(k+5)] <- apply(ncvar_get(ncid, ncid$var[[cvar[ivar]]]), 3, mean,na.rm=T)
    TG[ivar,k:(k+5)] <- apply(ncvar_get(ncid, ncid$var[[dvar[ivar]]]), 3, mean,na.rm=T)
  }
  k <- k+6
  flush.console()
}


# PLOT

col=tim.colors(nvar)

pdf("figures/spinup_domain/wg_spinup.pdf")
plot(WG[1,], type='l',main="Domain average, WG, SPINUP",ylab="Volumetric water content [m3m-3]",xlab="hours since 2016-05-01 00",col=col[1])
for (i in 2:nvar){
  lines(WG[i,], col=col[i])
}
legend("topright",legend=cvar, lty=1, col=col[1:nvar])
dev.off()

pdf("figures/spinup_domain/tg_spinup.pdf")
plot(TG[1,], type='l',main="Domain average, TG, SPINUP",ylab="Soil temperature [K]",xlab="hours since 2016-05-01 00",col=col[1])
for (i in 2:nvar){
  lines(TG[i,], col=col[i])
}
legend("topright",legend=dvar, lty=1, col=col[1:nvar])
dev.off()
~   
