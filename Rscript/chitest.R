source("chi2.R")

smap  <- loadSODA("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs038_2")
smos  <- loadSODA("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMOS/SEKF/obs05_2")

# SMAP
x04  <- loadSODA("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs04")
x05  <- loadSODA("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs05")
#x048 <- loadSODA("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs048")

y04  <- errDiag3(x04,0.5)
y05  <- errDiag3(x05,0.5)
#y048 <- errDiag3(x048,0.5)

x0 <- 0.4
x1 <- 0.6
x2 <- 0.48


y0 <- mean(y04$chi_o,na.rm=T)
y1 <- mean(y06$chi_o,na.rm=T)
y2 <- mean(y048$chi_o,na.rm=T)


linSolv <- function(x0,x1,f0,f1){
  x <- x1 - f1*(x1-x0)/(f1-f0)
  return(x)
}

#####################################
#### Error of representativeness ####

si_o <- sqrt(apply(x04$obserr,1:2,mean,na.rm=T))




#####################################



files <- list.files("/lustre/storeA/users/asmundb/surfex/job1/OBSERVATIONS/",full.names=T)
files <- list.files("/lustre/storeB/users/asmundb/SMAP/OBSERVATIONS_norm",full.names=T)
nfiles <- length(files)


obs <- array(NA,dim=c(111,111,nfiles))

for (i in 1:nfiles){
  tmp <- matrix(read.table(files[i])[,1],111,111)
  obs[,,i] <- tmp
}


### satelitte correlations ###

mat_cor <- function(x,y){
  if (all(dim(x) != dim(y))){
    print("fu")
    stop()
  }
  corr <- array(NA, dim=dim(x)[1:2])

  for (i in 1:dim(x)[2]){
    for (j in 1:dim(x)[1]){
      corr[j,i] <- cor(x[j,i,], y[j,i,],use="na")
    }
  }
  return(corr)
}


### Increments ###

#smapinc <- apply(smap$inc,1:2,mean,na.rm=T)
#smapinc[which(smapinc == 0)] <- NA
#smosinc <- apply(smos$inc,1:2,mean,na.rm=T)
#smosinc[which(smosinc == 0)] <- NA

smapobs <- smap$yo[,,,1]
#smapinc <- smap$inc[,,,2]
#smapinc[which(is.na(smapobs))] <- NA
#smapincmean <- apply(smapinc,1:2,mean,na.rm=T)


smapinc <- smap$inc
smapincmean <- array(NA, dim=c(111,111,7))
for(i in 1:7){
  tmp <- smapinc[,,,i]
  tmp[which(is.na(smapobs))] <- NA
  smapinc[,,,i] <- tmp  # ABS ABS OBS OBS
  smapincmean[,,i] <- apply(tmp,1:2,mean,na.rm=T)
}


smosobs <- smos$yo[,,,1]
#smosinc <- smos$inc[,,,2]
#smosinc[which(is.na(smosobs))] <- NA
#smosincmean <- apply(smosinc,1:2,mean,na.rm=T)
smosinc <- smos$inc
smosincmean <- array(NA, dim=c(111,111,7))
for(i in 1:7){
  tmp <- smosinc[,,,i]
  tmp[which(is.na(smosobs))] <- NA
  smosinc[,,,i] <- tmp  # ABS ABS OBS OBS
  smosincmean[,,i] <- apply(tmp,1:2,mean,na.rm=T)
}

abssmapincmean <- apply(abs(smapinc),4,mean,na.rm=T)
abssmosincmean <- apply(abs(smosinc),4,mean,na.rm=T)



col=two.colors(50,"red","blue","white")#rev(tim.colors())
source("topo.R")

for ( i in 1:7){
  m <- max(abs(c(smosincmean[,,i],smapincmean[,,i])),na.rm=T)

  zlim <- c(-m,m)
  zlim <- c(-1,1)
  pdf(sprintf("figures/2016/rel_smap_mean_inc_%s.pdf",i),width=3,height=2)
  par(mar=c(2,2,1,0.5))
  image.plot(smapincmean[,,i]/abssmosincmean[i],zlim=zlim, col=col,main=sprintf("WG%s SMAP",i),legend.width=0.5,legend.mar=6)
  topo()
  dev.off()

  pdf(sprintf("figures/2016/rel_smos_mean_inc_%s.pdf",i),width=3,height=2)
  par(mar=c(2,2,1,0.5))
  image.plot(smosincmean[,,i]/abssmosincmean[i],zlim=zlim, col=col,main=sprintf("WG%s SMOS",i),legend.width=0.5,legend.mar=6)
  topo()
  dev.off()

}

pdf("figures/2016/meanabsinc.pdf",width=2.5,height=2.5)
par(mar=c(4,4,1,1))
plot(abssmapincmean,log="y",type='o',ylim=c(1e-6,1e-2),main="Mean abs increment", xlab="soil layer",ylab="mean abs inc")
points(abssmosincmean,pch=2)
lines(abssmosincmean,lty=2)
legend("bottomleft", legend=c("SMAP","SMOS"),pch=c(1,2), lty=c(1,2))
dev.off()

### innovations and residuals ###

innov_residue_hist <- function(data,main="innovation, residuals",breaks=51){
  innov <- data$innov[,,,1]
  innov[which(is.na(data$yo))] <- NA
  res <- data$yo[,,,1] - data$xa[,,,2]
  res[which(is.na(data$yo))] <- NA
  par(pin=c(2,2))
  par(las=1)
#  par(ps=20)
  hist(res,breaks=breaks,main=main,xlab="soil moisture",angle=45,density=20,xlim=c(-0.15,0.15),yaxt="n",ylab="Frequency/1000 ")
  hist(innov,add=T,breaks=breaks,border="red")
  legend("topright", legend=c("O-A", "O-F"), fill=c("black","red"), density=c(20,0),bty="o",border=c("black","red"))
  akse <- seq(par()$yaxp[1],par()$yaxp[2],length=par()$yaxp[3]+1)
  axis(side=2,at=akse, labels=akse/1000)
}  

pdf("figures/2016/innov_residu_smap.pdf")
innov_residue_hist(smap,"SMAP")
dev.off()

pdf("figures/2016/innov_residu_smos.pdf")
innov_residue_hist(smos,"SMOS")
dev.off()

### point ###

kise <- 12550
blon <- 10.9583  # 74
blat <- 60.7908  # 102

bij <- fnn_lamb(lon,lat,blon,blat)$ij_out
m <- matrix(1:length(lon),111,111)
ob <- which(m == bij, arr.ind=T)
ob[1] <- 74 # manual correction


# Kise
xi <- 74
yi <- 102


# Munkedal
# max clay : clay=0.19 sand=0.43
xi <- 90
yi <- 11

pdf("figures/2016/Hbox_much_clay.pdf")
H <- smapHO$H[,xi,yi,]
par(pin=c(2,2))
boxplot(t(H),ylim=c(-0.05,0.85),xlab="Soil layer",main="clay=0.19, sand=0.43",ylab="dwg2/dwgi")
dev.off()

sot <- seq(1,368,by=2)
for (i in 1:4){
  pdf(sprintf("figures/2016/HvsSM_clay_%s.pdf",i),width=2.5,height=2)
  par(mar=c(4,4,1,1))
  ylab <- sprintf("dWG2/dWG%d", i)
  plot(smap$xf[xi,yi,sot,2], smapHO$H[i,xi,yi,sot],xlab=paste("WG",2,sep=""), ylab=ylab,ylim=c(0,0.85),main="Clay",xlim=c(0.1,0.3))
  dev.off()
}



#aust-agder north
#max sand  : clay=0.094 sand=0.724
xi <- 10
yi <- 20

pdf("figures/2016/Hbox_much_sand.pdf")
H <- smapHO$H[,xi,yi,]
par(pin=c(2,2))
boxplot(t(H),ylim=c(-0.05,0.85),xlab="Soil layer",main="clay=0.09, sand=0.72",ylab="dwg2/dwgi")
dev.off()

for (i in 1:4){
  pdf(sprintf("figures/2016/HvsSM_sand_%s.pdf",i),width=2.5,height=2)
  par(mar=c(4,4,1,1))
  ylab <- sprintf("dWG2/dWG%d", i)
  plot(smap$xf[xi,yi,sot,2], smapHO$H[i,xi,yi,sot],xlab=paste("WG",2,sep=""), ylab=ylab,ylim=c(0,0.85),main="Sand",xlim=c(0.1,0.3))
  dev.off()
}

# someting in between
#nordmarka : clay=0.142 sand=0.577
xi <- 69
yi <- 69

# BOXPLOT
smapHO <- loadHO("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs038_2")

pdf("figures/2016/Hbox_much_sand.pdf")
H <- smapHO$H[,xi,yi,]
par(pin=c(2,2))
boxplot(t(H),ylim=c(-0.05,0.85),xlab="Soil layer",main="Jacobian, clay=0.09, sand=0.72")
dev.off()

H2 <- matrix(NA, 7, 368)
for (i in 1:7){
  H2[i,] <- as.numeric(smapHO$H[i,xi,yi,])
}




pdf("figures/2016/SMvsH.pdf")
par(mfrow=c(2,2),oma=c(0,0,1.5,0))
for (i in 1:4){
  ylab <- sprintf("dWG2/dWG%d", i)
  plot(smap$xf[xi,yi,sot,2], smapHO$H[i,xi,yi,sot],xlab=paste("WG",2,sep=""), ylab=ylab,ylim=c(0,0.85),)
}
title(main="Soil moisture vs. Jaobians", outer=T,cex.main=2)
dev.off()






