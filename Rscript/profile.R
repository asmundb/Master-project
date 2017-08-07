require(ncdf4)

ncid <- nc_open("/lustre/storeA/users/asmundb/surfex/RESULTS/2014/SEKF/obs06_b005/ISBA/2014080812/ISBA_PROGNOSTIC.OUT.nc")
TG <- array(NA, dim=c(14,6))
WG <- array(NA, dim=c(14,6))

for (i in 1:14){
  var <- paste("TG",i,sep="")
  tmp <- ncvar_get(ncid, ncid$var[[var]])
  TG[i,] <- tmp[60,60,]
  var <- paste("WG",i,sep="")
  tmp<- ncvar_get(ncid, ncid$var[[var]])
  WG[i,] <- tmp[60,60,]
}
nc_close(ncid)

dl <- c(0.001,0.04, 0.1, 0.2,0.4,0.6,0.8,1,1.5,2,3,5,8,12)
depth <- numeric(14)
depth[1] <- dl[1]/2
for (i in 2:14){
  depth[i] <- dl[i-1] + (dl[i]-dl[i-1])/2
}

pdf("figures/other/profile.pdf")
par(mfrow=c(1,2))
plot(TG[,6], -depth,type='o', main="Temperature profile",xlab="temp [K]",ylab="depth [m]")
plot(WG[,6], -depth,type='o', main="Soil moisture profile", xlab="Volumetric water content [m3m-3]",ylab="depth [m]")
dev.off()
