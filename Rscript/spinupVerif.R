# load wg & tg for complete period

require(ncdf4)
require(fields)

path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/OPEN_LOOP_SPINUP/ISBA/"

files <- list.files(path=path, pattern="ISBA_PROGNOSTIC.OUT.nc",recursive=T,full.names=T)
nfiles <- length(files)

ntimes <- nfiles*6
nvar   <- 14

cvar <- paste("WG",1:nvar,sep="")
dvar <- paste("TG",1:nvar,sep="")

WG <- array(0, dim=c(111,111,ntimes,nvar))
TG <- array(0, dim=c(111,111,ntimes,nvar))

k <- 1
# loop
for (i in 1:nfiles){
  cat(i,"/",nfiles,"\r") 
  ncid <- nc_open(files[i])
  for (ivar in 1:nvar){
    WG[,,k:(k+5),ivar] <- ncvar_get(ncid, ncid$var[[cvar[ivar]]])
    TG[,,k:(k+5),ivar] <- ncvar_get(ncid, ncid$var[[dvar[ivar]]])
  }
  k <- k+6
  flush.console()
}


domain_mean_WG <- apply(WG,3:4,mean,na.rm=T)
domain_sd_WG   <- apply(WG,3:4,sd,na.rm=T)

domain_mean_TG <- apply(TG,3:4,mean,na.rm=T)
domain_sd_TG   <- apply(TG,3:4,sd,na.rm=T)

time <- seq(as.POSIXlt("2016-05-01 01:00"), as.POSIXlt("2016-09-01 00:00"), by=3600)


# PLOT

col=tim.colors(nvar)

pdf("figures/other/wg_spinup.pdf",height=3.5,width=5.7)
par(mar=c(4.1,4.1,3.1,7.1))
plot(domain_mean_WG[,1], type='l',main="Domain average, WG, SPINUP",ylab="Volumetric water content [m3m-3]",xlab="hours since 2016-05-01 00",col=col[1])
for (i in 2:nvar){
  lines(domain_mean_WG[,i], col=col[i])
}
#abline(h=mean(wwilt1,na.rm=T),lty=2)
#abline(h=mean(wfc1,na.rm=T),lty=2)
#abline(h=mean(wsat,na.rm=T),lty=2)
par(xpd=T)
ymid <- par()$usr[3] + (par()$usr[4]-par()$usr[3])/2
xpos <- par()$usr[2] + par()$mar[4]*182 
legend(xpos,ymid,legend=cvar, lty=1, col=col[1:nvar],xjust=1,yjust=0.5)
dev.off()

pdf("figures/other/tg_spinup.pdf",height=3.5,width=5.7)
par(mar=c(4.1,4.1,3.1,7.1))
plot(domain_mean_TG[,1], type='l',main="Domain average, TG, SPINUP",ylab="Soil temperature [K]",xlab="hours since 2016-05-01 00",col=col[1])
for (i in 2:nvar){
  lines(domain_mean_TG[,i], col=col[i])
}
par(xpd=T)
ymid <- par()$usr[3] + (par()$usr[4]-par()$usr[3])/2
xpos <- par()$usr[2] + par()$mar[4]*182
legend(xpos,ymid,legend=cvar, lty=1, col=col[1:nvar],xjust=1,yjust=0.5)
dev.off()


# spread
pdf("figures/other/wg_spinup_spread.pdf",height=3.5,width=5.7)
par(mar=c(4.1,4.1,3.1,7.1))
plot(domain_sd_WG[,1], type='l',ylim=c(0,0.07),main="Domain sd, WG, SPINUP",ylab="Volumetric water content [m3m-3]",xlab="hours since 2016-05-01 00",col=col[1])
for (i in 2:nvar){
  lines(domain_sd_WG[,i], col=col[i])
}
par(xpd=T)
ymid <- par()$usr[3] + (par()$usr[4]-par()$usr[3])/2
xpos <- par()$usr[2] + par()$mar[4]*182
legend(xpos,ymid,legend=cvar, lty=1, col=col[1:nvar],xjust=1,yjust=0.5)
dev.off()

pdf("figures/other/tg_spinup_spread.pdf",height=3.5,width=5.7)
par(mar=c(4.1,4.1,3.1,7.1))
plot(domain_sd_TG[,1], type='l', ylim=c(0,6),main="Domain sd, TG, SPINUP",ylab="Soil temperature [K]",xlab="hours since 2016-05-01 00",col=col[1])
for (i in 2:nvar){
  lines(domain_sd_TG[,i], col=col[i])
}
par(xpd=T)
ymid <- par()$usr[3] + (par()$usr[4]-par()$usr[3])/2
xpos <- par()$usr[2] + par()$mar[4]*182
legend(xpos,ymid,legend=cvar, lty=1, col=col[1:nvar],xjust=1,yjust=0.5)
dev.off()


