get_obs <- function(path,pattern=""){
  files <- list.files(path=path,pattern=pattern, full.names=T)
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


SMOS      <- get_obs("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS/")          #[,,507:690]
SMOS_norm <- get_obs("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS_norm/") #[,,403:586]
SMAP      <- get_obs("/lustre/storeB/users/asmundb/SMAP/OBSERVATIONS/")          #[,,1:184]
SMAP_norm <- get_obs("/lustre/storeB/users/asmundb/SMAP/OBSERVATIONS_norm/")     #[,,1:184]
 
SMOS      <- SMOS[,,507:690]
SMOS_norm <- SMOS_norm
SMAP      <- SMAP[,,1:184]
SMAP_norm <- SMAP_norm[,,1:184]


### Data coverage
heatMap <- function(obs,cutOff=15,main=""){
  require(fields)
  nobs <- apply(!is.na(obs),1:2,sum)
  nobs[which(nobs < cutOff)] <- NA
#  main <- substitute(obs)
  par(mar=c(2,2,1,0.5))
  image.plot(nobs,col=two.colors(200,"lightblue","orange","blue"),main=main,zlim=c(1,184),legend.width=0.5,legend.mar=4)
  topo()
}

nObs <- function(obs){
  nobs <- apply(!is.na(obs),1:2,sum)
  return(nobs)
}

source("topo.R")
SMOS <- SM2
pdf("figures/SATevaluation/heatMap_SMOS.pdf")
heatMap(SMOS, cutOff=0,main="Raw SMOS")
dev.off()

pdf("figures/SATevaluation/heatMap_SMAP.pdf")
heatMap(SMAP,cutOff=0,main="Raw SMAP")
dev.off()


X <- smos$sm
tab <- table(X)
rmables <- which(X %in% names(tab)[which(tab > 450)])
X[rmables] <- NA

Y <- smos$sm
Y[which(Y < min(wwilt1,na.rm=T) | Y > max(wsat,na.rm=T))] <- NA


pdf("figures/SATevaluation/heatMap_remove.pdf")
par(mfrow=c(2,3))

heatMap(smos$sm,cutOff=0, main="SMOS raw")
heatMap(X,cutOff=0,main="freq based")
heatMap(Y,cutOff=0,main="dynrange based")

heatMap(smos$sm,cutOff=15, main="SMOS raw")
heatMap(X,cutOff=15,main="freq based")
heatMap(Y,cutOff=15,main="dynrange based")
dev.off()


mod <- readRDS("RDS_files/mod_2016.rds")
mod1 <- mod$am
mod2 <- mod$pm

### Normalization

breaks <- 60

pdf("figures/SATevaluation/hist_SMOS.pdf")
hist(SM2, main="SMOS",col="#0055FF",xlim=c(0,1))
dev.off()

pdf("figures/SATevaluation/hist_SMOS_norm.pdf")
hist(SMOS_norm, main="SMOS normalized",col="#0055FF",xlim=c(0,0.4),breaks=breaks)
dev.off()

pdf("figures/SATevaluation/hist_SMAP.pdf")
hist(SMAP, main="SMAP",col="#0055FF",xlim=c(0,1))
dev.off()

pdf("figures/SATevaluation/hist_SMAP_norm.pdf")
hist(SMAP_norm, main="SMAP normalized",col="#0055FF",xlim=c(0,0.4),breaks=breaks)
dev.off()

pdf("figures/SATevaluation/hist_mod.pdf")
hist(c(mod1,mod2), main="SURFEX open loop",col="#0055FF",xlim=c(0,0.4),breaks=breaks)
dev.off()


pdf("figures/SATevaluation/hist_comp.pdf")
hist(c(mod1,mod2),
     main="JJA 2016 soil moisure",
     xlim=c(0,0.4),
     breaks=breaks,
     probability=T,
     ylim=c(0,11),
     xlab="Soil moiture")
lines(density(SMOS_norm,na.rm=T), col="blue", lwd=2)
lines(density(SMAP_norm,na.rm=T), col="red", lwd=2)
legend("topright",
       legend=c("SURFEX ctrl", "SMOS_norm","SMAP_norm"),
       pch=c(0c,NA,NA), lty=c(NA,1,1),col=c(1,"blue","red"))          
dev.off()


model <- array(NA, dim=c(111,111,184))
k <- 1
for (i in 1:92){
  model[,,k]   <- mod1[,,i]
  model[,,k+1] <- mod2[,,i]
  k <- k+2
}


mask1 <- which(! is.na(SMOS_norm))
mask2 <- which(! is.na(SMAP_norm))

pdf("figures/SATevaluation/hist_comp2_smos.pdf",width=2.5,height=3)
par(mar=c(8,2,1,1))
#hist(model,probability=T,ylim=c(0,13),xlim=c(0,0.4),breaks=50,xlab="Soil moisture",main="Distribution")
plot(density(model,na.rm=T),type='l', ylim=c(0,13), xlim=c(0,0.4),lwd=2,main="SMOS",xlab="")
lines(density(model[mask1],na.rm=T),col="blue", lwd=2)
lines(density(SMOS_norm[mask1],na.rm=T),col="red", lwd=2)
abline(v=median(model,na.rm=T),lty=2)
par(xpd=T)
legend("bottom", inset=c(0,-1.2),
       legend=c("SURFEX ctrl", "SFX@SMOS", "SMOS_norm","SFX median"),
       lty=c(1,1,1,2),lwd=2, col=c(1,"blue","red",1),yjust=0)
dev.off()



pdf("figures/SATevaluation/hist_comp2_smap.pdf",width=2.5,height=3)
par(mar=c(8,2,1,1))
plot(density(model,na.rm=T),type='l', ylim=c(0,13), xlim=c(0,0.4),lwd=2,,main="SMAP",xlab="")
lines(density(model[mask2], na.rm=T),col="blue", lwd=2)
lines(density(SMAP_norm[mask2],na.rm=T),col="red", lwd=2)
abline(v=median(model,na.rm=T),lty=2)
par(xpd=T)
legend("bottom", inset=c(0,-1.2),
       legend=c("SURFEX ctrl", "SFX@SMAP","SMAP_norm","SFX median"),
       lty=c(1,1,1,2),lwd=2, col=c(1,"blue","red",1),yjust=0)
dev.off()

############# FLAGS IN SMOS #############
#
# Try to figure out what is going on with the low values
#

source("read_SMOS_nc.R")

path <- "/lustre/storeB/users/asmundb/SMOS/nc/"
files <- list.files(path, recursive=T, full.names=T,pattern="SM_OPER.*")[c(54:145,254:345)]

smos <- loadSMOSTimeserie(files)
smosB <- smos

tab <- table(smos$sm)

x <- which(smos$sm %in% names(tab)[which(tab < 450)])
y <- which(smos$sm > min(wwilt1,na.rm=T) & smos$sm < max(wsat,na.rm=T))

png("figures/SATevaluation/rydding.png")
par(mfrow=c(2,3),pin=c(2,2))

hist(smos$sm,breaks=500)
hist(smos$sm[x],breaks=500)
hist(smos$sm[y],breaks=500,xlim=c(0,1))

plot(smos$sm,smos$sm_dqx, ylim=c(0,1))
plot(smos$sm[x],smos$sm_dqx[x], ylim=c(0,1))
plot(smos$sm[y],smos$sm_dqx[y], ylim=c(0,1),xlim=c(0,1))
dev.off()


smsmalls <- which(smos$sm < 2.1)

png("figures/SATevaluation/SMOS_dqx_scatter_all.png",width=3*72+50,height=3*72+50)
par(pin=c(2,2))
plot(smos$sm, smos$sm_dqx,
     xlab="soil moisture", ylab="soil moisture dqx", main="SMOS soil moisture quality index")
abline(h=0.04,col="red")
lines(rep(min(wwilt1,na.rm=T),2), c(0,1), col="blue")
lines(rep(max(wsat,na.rm=T),2), c(0,1), col="blue")
dev.off()


#################################### 
### screen(m)ing ###


smosB <- smos # backup

source("sciFlags.R")

minimum <- 0.07 #min(wwilt1,na.rm=T)
maximum <- 0.46 #max(wsat,na.rm=T)
FL_mask <- flags$FL_Topo_S | flags$FL_Urban_High | flags$FL_OW | flags$FL_Sea_Ice | flags$FL_Coast | flags$FL_TEC

smos <- smosB
pdf("figures/SATevaluation/screening_histogram.pdf")
par(pin=c(2,2),xpd=T)
smos$sm[which(smos$sm < minimum | smos$sm > maximum)] <- NA
hist(smos$sm,breaks=50,col="black",main="Screening process", xlab="Soil moisture")
smos$sm[which(smos$sm_dqx > 0.2)] <- NA
hist(smos$sm,breaks=50,col="darkred",add=T)
smos$sm[which(smos$sm_dqx > 0.1)] <- NA
hist(smos$sm,breaks=50,col="red",add=T)
smos$sm[which(smos$rfi_prob > 0.3)] <- NA
hist(smos$sm,breaks=50,col="orange",add=T)
smos$sm[FL_mask] <- NA
hist(smos$sm,breaks=50,col="yellow",add=T)
legend("topright",inset=c(-0.5,0),
       legend=c("SMOS (sat-wilt)", "sm_dqx<0.2","sm_dqx<0.1","rfi_prob<0.3","flags"),
       fill=c("black","darkred","red","orange","yellow"))
dev.off()

pdf("figures/SATevaluation/screening_smos_heatmap.pdf",width=2.5,height=2)
heatMap(smos$sm,cutOff=0,main="SMOS after screening")
dev.off()

#########################
source("readh5.R")

path <- "/lustre/storeB/users/asmundb/SMAP/n5eil01u.ecs.nsidc.org/SMAP/SPL3SMP.004/"

files <- list.files(path, pattern="SMAP_L3",full.names=T, recursive=T)
nfiles <- length(files)
ource("sciFlags.R")

flags <- array(NA, dim=c(111,111,nfiles*2))
for (i in 1:nfiles){
  flags{



#### COUNT ####
smos1 <- smos$sm
tab <- table(smos1)
smos1[which(smos1 %in% names(tab)[which(tab > 450)])] <- NA


ntotal <- 111*111*92*2
nsmos  <- sum(!is.na(SMOS))
nsmap  <- sum(!is.na(SMAP))
nsmosn <- sum(!is.na(SMOS_norm))
nsmapn <- sum(!is.na(SMAP_norm))


ntsmos <- apply(!is.na(SMOS_norm),3,sum)
ntsmap <- apply(!is.na(SMAP_norm),3,sum)

time <- seq(as.POSIXlt("2016-06-01 06:00"), as.POSIXlt("2016-08-31 18:00"), by=3600*12)

pdf("figures/SATevaluation/nobs_of_time.pdf",width=2.5,height=2.5)
par(mar=c(2,2,2,1))
plot(time[3:181], MM(ntsmos,3)[3:181],ylim=c(0,4600),type='l',lwd=1,main="Number of obs",xlab="Time",lty=1,col="red")
lines(time[3:181], MM(ntsmap,3)[3:181],lty=2,lwd=2,col="blue")
dev.off()


### SCATTER ###
pdf("figures/SATevaluation/scatter_smosvsmodel.pdf",width=2.5,height=2.5)
par(mar=c(4,4,1,1))
plot(model, smos$sm,pch=".",main="SMOS vs. ctrl",xlab="model",ylab="SMOS")
dev.off()

pdf("figures/SATevaluation/scatter_smosCleanvsmodel.pdf",width=2.5,height=2.5)
par(mar=c(4,4,1,1))
plot(model, smos1,pch=".",main="SMOS* vs. ctrl",xlab="model",ylab="SMOS")
dev.off()

pdf("figures/SATevaluation/scatter_smosNormvsmodel.pdf",width=2.5,height=2.5)
par(mar=c(4,4,1,1))
plot(model, SMOS_norm,pch=".",main="SMOS** vs. ctrl",xlab="model",ylab="SMOS")
dev.off()

pdf("figures/SATevaluation/scatter_smapvsmodel.pdf",width=2.5,height=2.5)
par(mar=c(4,4,1,1))
plot(model, smap,pch=".",main="SMAP vs. ctrl",xlab="model",ylab="SMAP")
dev.off()


pdf("figures/SATevaluation/scatter_smapCleanvsmodel.pdf",width=2.5,height=2.5)
par(mar=c(4,4,1,1))
plot(model, smap1,pch=".",main="SMAP* vs. ctrl",xlab="model",ylab="SMAP")
dev.off()


pdf("figures/SATevaluation/scatter_smapNormvsmodel.pdf",width=2.5,height=2.5)
par(mar=c(4,4,1,1))
plot(model, SMAP_norm,pch=".",main="SMAP** vs. ctrl",xlab="model",ylab="SMAP")
dev.off()

