require(ncdf4)

path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/SPINUP/ISBA"
#path <- "/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs038_2"
#path <- "/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMOS/SEKF/obs05_2"
#files <- list.files(path,pattern="ISBA_PROGNOSTIC",full.names=T,recursive=T)
nfiles <- length(files)
WG <- array(NA, dim=c(111,111,6*nfiles,14))
k=1
for (fl in 1:nfiles){
  cat(fl,"\r")
  ncid <- nc_open(files[fl])
  for (i in 1:14){
    var <- paste("WG",i,sep="")
    WG[,,k:(k+5),i] <- ncvar_get(ncid, ncid$var[[var]])
  }
  k <- k+6
  nc_close(ncid)
  flush.console()
}
cat("\n")

wg_ol <- readRDS("RDS_files/wg_ol_2016.rds")
wg_smos <- readRDS("RDS_files/wg_smos_2016.rds")
wg_smap <- readRDS("RDS_files/wg_smap_2016.rds")

smos <- readRDS("RDS_files/SEKF_smos.rds")
smap <- readRDS("RDS_files/SEKF_smap.rds")

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



#### Mean osv ####


mean_p_ol <- apply(wg_ol,4,mean,na.rm=T)
mean_p_smos <- apply(wg_smos,4,mean,na.rm=T)
mean_p_smap <- apply(wg_smap,4,mean,na.rm=T)



smosMask <- which(is.na(smos$yo[,,,1]))
smosInc <- smos$inc

for (l in 1:7){
  tmp                 <- smosInc[,,,l]
  tmp[smosMask]       <- NA
  smosInc[,,,l] <- tmp
}

r <- matrix(NA,2,7)
par(mfrow=c(2,4))
for (l in 1:7){
  #r[,l] <- range(apply(smosInc[,,,l],1:2,mean,na.rm=T),na.rm=T)
  image.plot(apply(smosInc[,,,l],1:2,mean,na.rm=T),zlim=range(r))
}


plot(r[1,],type='n',ylim=c(min(r),max(r)))
polygon(c(1:7,7:1),c(r[1,],rep(0,7)),col="red",border=F)
polygon(c(1:7,7:1),c(r[2,],rep(0,7)),col="blue",border=F)
polygon(c(1:7,7:1),c(r[1,],rep(min(r),7)),col="white",border=F)
lines(r[1,])
lines(r[2,])

med <- apply(smosInc,4,median,na.rm=T)
mea <- apply(smosInc,4,mean,na.rm=T)
std <- apply(smosInc,4,sd,na.rm=T)

incainca <- matrix(NA, 12321*368,7)
for(l in 1:7){
  incainca[,l] <- as.numeric(smosInc[,,,l])
}

plot(NA,xlim=c(min(incainca,na.rm=T),max(incainca,na.rm=T)), ylim=c(0,200))
for(l in 1:4){
  lines(density(incainca[,l],na.rm=T),col=l)
}






