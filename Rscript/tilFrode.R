require(ncdf4)
require(fields)


source("read_ISBA.R")

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


mat_rmse <- function(x,y){
  if (all(dim(x) != dim(y))){
    print("fu")
    stop()
  }
  rmse <- array(NA, dim=dim(x)[1:2])
  for (i in 1:dim(x)[2]){
    for (j in 1:dim(x)[1]){
      rmse[j,i] <- rmsd(x[j,i,], y[j,i,])
    }
  }
  return(rmse)
}

rmsd <- function(x,y){
  rmsd <- sqrt(sum((x-y)^2,na.rm=T)/length(x))
  return(rmsd)
}



path <- "/lustre/storeA/users/asmundb/surfex/RESULTS/2014/SEKF/obs06_b005/ISBA/"
files1 <- list.files(path,   
                    pattern="ISBA_PROGNOSTIC.OUT.nc",   
                    recursive=T,
                    full.names=T)
vars1 <- c("WG1","WG2", "TG1","TG2")
prog <- load_isba(files1, vars1)

files2 <- list.files(path,   
                    pattern="ISBA_DIAGNOSTICS.OUT.nc",   
                    recursive=T,
                    full.names=T)

vars2 <- c("LE_ISBA","H_ISBA","RN_ISBA","T2M_ISBA")
diag <- load_isba(files2, vars2)



path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2014/SPINUP/ISBA/"
files3 <- list.files(path,
                    pattern="ISBA_PROGNOSTIC.OUT.nc",
                    recursive=T,
                    full.names=T)
files3 <- files3[125:492]
vars3 <- c("WG1","WG2", "TG1","TG2")


prog_ol <- load_isba(files3, vars3)

files4 <- list.files(path,
                    pattern="ISBA_DIAGNOSTICS.OUT.nc",
                    recursive=T,
                    full.names=T)
files4 <- files4[125:492]
vars4 <- c("LE_ISBA","H_ISBA","RN_ISBA","T2M_ISBA")
diag_ol <- load_isba(files4, vars4)


time <- seq(as.POSIXlt("2014-06-01 01:00"), as.POSIXlt("2014-09-01 00:00"), by=3600)









B <-  diag$H_ISBA/diag$LE_ISBA
EF <- 1/(1+B)

june <- 1:719
july <- 720:1463
august <- 1464:2207

julaug <- 1057:1799

EF2 <- diag$LE_ISBA/diag$RN_ISBA


source("topo.R")
stop()

# LE WG

col <- two.colors(11, "blue","red", "#EEEEEE")

pdf("figures/2014/SEKF_06_005/LE_WG1_july.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,july], prog$WG1[,,july]),zlim=c(-1,1),col=col, main="cor(LE, WG1) july 2014")
topo()
dev.off()

pdf("figures/2014/SEKF_06_005//LE_WG1_june.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,june], prog$WG1[,,june]),zlim=c(-1,1),col=col, main="cor(LE, WG1) june 2014")
topo()
dev.off()

pdf("figures/2014/SEKF_06_005//LE_WG1_julaug.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,julaug], prog$WG1[,,julaug]),zlim=c(-1,1),col=col, main="cor(LE, WG1) 15.july-14.aug 2014")
topo()
dev.off()


# EF WG
pdf("figures/2014/SEKF_06_005//EF_WG1_july.pdf")
image.plot( mat_cor(EF2[,,july], prog$WG1[,,july]),zlim=c(-0.4,0.4),col=col, main="cor(EF, WG1) july 2014")
topo()
dev.off()

pdf("figures/2014/SEKF_06_005//EF_WG1_june.pdf")
image.plot( mat_cor(EF2[,,june], prog$WG1[,,june]),zlim=c(-0.4,0.4),col=col, main="cor(EF, WG1) june 2014")
topo()
dev.off()


# LE TG
pdf("figures/2014/SEKF_06_005//LE_TG1_july.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,july], prog$TG1[,,july]), zlim=c(-1,1), col=rev(col), main="cor(LE, TG1) july 2014")
topo()
dev.off()
 
pdf("figures/2014/SEKF_06_005//LE_TG1_julaug.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,julaug], prog$TG1[,,julaug]), zlim=c(-1,1), col=rev(col), main="cor(LE, TG1) 15.july-14.aug 2014")
topo()
dev.off()

pdf("figures/2014/SEKF_06_005//LE_TG1_june.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,june], prog$TG1[,,june]),zlim=c(-1,1), col=rev(col), main="cor(LE, TG1) june 2014")
topo()
dev.off()


pdf("figures/2014/SEKF_06_005//LE_T2M_julaug.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,julaug], diag$T2M_ISBA[,,julaug]), zlim=c(-1,1), col=rev(col), main="cor(LE, T2M) 15.july-14.aug 2014")
topo()
dev.off()




rn <- as.numeric(diag$RN_ISBA)
le <- as.numeric(diag$LE_ISBA)
sm <- as.numeric(prog$WG1)

png("figures/2014/SEKF_06_005/EF.png")
plot(sm,le/rn,main="EF=LE/RN vs soil moisture")
dev.off()

rn <- as.numeric(diag$RN_ISBA[,,june])
le <- as.numeric(diag$LE_ISBA[,,june])
sm <- as.numeric(prog$WG1[,,june])


png("figures/2014/SEKF_06_005/EF_june.png")
plot(sm,le/rn,main="EF=LE/RN vs soil moisture june")
dev.off()


rn <- as.numeric(diag$RN_ISBA[,,july])
le <- as.numeric(diag$LE_ISBA[,,july])
sm <- as.numeric(prog$WG1[,,july])


png("figures/2014/SEKF_06_005/EF_july.png")
plot(sm,le/rn,main="EF=LE/RN vs soil moisture july")
dev.off()

sm <- as.numeric(prog$WG1)
ef <- as.numeric(EF2)

png("figures/2014/SEKF_06_005/EF2.png")
plot(sm, ef,ylim=c(-100,100))
dev.off()


ef <- diag$LE_ISBA[ob[1],ob[2],]/diag$RN_ISBA[ob[1],ob[2],]
ef1 <- ef[!ef %in% boxplot.stats(ef)$out]
sm <- prog$WG1[ob[1],ob[2],][!ef %in% boxplot.stats(ef)$out]

pdf("figures/2014/SEKF_06_005/ef_sm.pdf")
plot(sm,ef1,main="EF vs SM, outliers removed",ylab="EF=LE/RN")
LM <- lm(ef1~sm)
abline(LM,col="red")
abline(v=wwilt1[ob[1],ob[2]])
dev.off()



###
# domain average t2m
ncid <- nc_open("surfex_files/FORCING.nc")
I  <- ncvar_get(ncid, ncid$var$LON)
J  <- ncvar_get(ncid, ncid$var$LAT)
nc_close(ncid)
print("done")

print("read soil parameters from prep file...")
filename <- "surfex_files/PREP_SODA.nc"
ncid <- nc_open(filename)
wwilt1 <- ncvar_get(ncid, ncid$var$WWILT1)
wfc1   <- ncvar_get(ncid, ncid$var$WFC1)
nc_close(ncid)

# Blindern


source("ffunctions.R")

#aas <- 17850 blon <- 10.7818 blat <- 59.6605
#blindern <- 18700 #blon <- 10.719025 blat <- 59.942484
kise <- 12550
blon <- 10.9583
blat <- 60.7908

dagali <- 29720
blon <- 8.5263
blat <- 60.4188


bij <- fnn_lamb(I,J,blon,blat)$ij_out
m <- matrix(1:length(I),111,111)
ob <- which(m == bij, arr.ind=T)
ob[1] <- 74 # manual correction

plot(diag$T2M_ISBA[ob[1],ob[2],],type="l")


getObs <- function(tab, P, fd, td, stnr){
       URL <- sprintf("http://klapp/metnopub/production/metno?re=30&tab=%s&%s&fd=%s&td=%s&split=0&nmt=0&ddel=dot&del=;&ct=text/plain&s=%s", tab, P, fd,td, stnr)
       df <- read.table(URL,na.strings=c("-",".","<NA>"), header=TRUE)
       colnames(df)[2] <- "TIME"
       df[,"TIME"] <- gsub('\\D','\\1',df[,"TIME"])
       return(df)
}

ta_obs <- getObs("T_ADATA","p=TA&p=TAX","01.06.2014","01.09.2014",aas)
t2m_obs <- ta_obs[2:2209,3]
t2m_max <- ta_obs[2:2209,4]

time2 <- seq(as.POSIXlt("2014-06-01 01:00"), as.POSIXlt("2014-09-01 00:00"), by=3600)

pdf("figures/2014/scatter_T2M_obs_mod.pdf")
plot(t2m_obs,diag$T2M_ISBA[ob[1],ob[2],]-273.15,main="T2M Ås 2014 may-aug; r= 0.9552136",xlab="obs",ylab="surfex offline")
abline(0,1,col="red")
dev.off()


pdf("figures/2014/timeserie_T2M_obs_mod.pdf")
plot(time2,t2m_obs,type='l',main="T2M Ås",ylab="T2M [C]")
lines(time2,diag$T2M_ISBA[ob[1],ob[2],]-273.15,col="red")
lines(
legend("topleft",legend=c("obs","sfx offln"), lty=1,col=c("black","red"))
dev.off()

pdf("figures/2014/timeserie_T2M_obs_mod_diff.pdf")
plot(time2,diag$T2M_ISBA[ob[1],ob[2],]-273.15-t2m_obs,type='l',main="T2M difference sfx-obs Ås",ylab="T2M [C]")
legend("topleft",legend=c("obs","sfx offln"), lty=1,col=c("black","red"))
dev.off()


T2Mdiff <- abs(diag$T2M - diag_ol$T2M)
T2Mdiff[is.infinite(T2Mdiff)] <- NA
T2MdiffMax <- apply(T2Mdiff, 1:2, max,na.rm=T)
T2MdiffMax[is.infinite(T2MdiffMax)] <- NA



image.plot(T2MdiffMax)






# SM MAPS
smax <- apply(prog$WG1,1:2,max,na.rm=T)
smin <- apply(prog$WG1,1:2,min,na.rm=T)

smax[is.infinite(smax)] <- NA
smin[is.infinite(smin)] <- NA

zlim <- c(min(smin,na.rm=T),max(smax,na.rm=T))

pdf("figures/2014/sm_maps.pdf")
par(mfrow=c(2,2))

image.plot(apply(prog$WG1,1:2,mean,na.rm=T), col=rev(tim.colors()),main="mean SM",zlim=zlim)
image.plot(apply(prog$WG1,1:2,sd,na.rm=T),col=rev(tim.colors()),main="SM sd",zlim=zlim)
image.plot(smax,col=rev(tim.colors()),main="max SM",zlim=zlim)
image.plot(smin,col=rev(tim.colors()),main="min SM",zlim=zlim)

dev.off()





## Timeseries


rms <- function(x,y){
  if (length(x) == length(y)){
    n <- length(x)
    xrms <- sqrt(sum((x-y)^2)/n)
  } else {
    xrms <- "lengths differ"
  } 
  return(xrms)
}
  

z <- (prog$WG2[,,1650]-prog_ol$WG2[,,1650])/prog$WG2[,,1650]*100
mx <- max(abs(z),na.rm=T)
zlim <- c(-mx,mx)


image.plot(z,col=two.colors(100,"red","blue","white"), 
            main="difference WG2 in percent, DA - open loop",
            zlim=zlim)
topo()





Kg <- matrix(NA, 7, 368)

for (i in 1:7){
  Kg[i,] <- as.numeric(HO07$K[i,ob[1],ob[2],])
}

Kg[which(Kg == 0)] <- NA

pdf("figures/2014/Kbox_07.pdf")
boxplot(t(Kg), xlab="Soil layer", ylab ="Kalman gain", main="Kalman gain at Kise JJA 2014")
dev.off()

kiseObs <- read.table("kise_1.csv",skip=1,sep=",",stringsAsFactors=F,header=T)

tmp <- strsplit(kiseObs$X,"/")
nveTime <- array(dim=length(tmp))
for (i in 1:length(tmp)){
  nveTime[i] <- sprintf("%04d-%02d-%02d 12:00",as.numeric(tmp[[i]][3]),as.numeric(tmp[[i]][1]),as.numeric(tmp[[i]][2]))
}
nveTime <- as.POSIXlt(nveTime)

nvewhich <- which(nveTime > as.POSIXlt("2014-06-01 00:00:00") & nveTime < as.POSIXlt("2014-09-01 00:00:00"))
nveTime <- nveTime[nvewhich]

percentKise <- as.numeric(kiseObs$X.10.cm)

smKise <- 0.01*swi2sm(mm2perc(percentKise), 6, 48)


anaTime <- seq(as.POSIXlt("2014-06-01 06:00"), as.POSIXlt("2014-09-01 00:00"), by=3600*6)

sot <- seq(1,368,by=2)

plot(anaTime,x07$xa[ob[1],ob[2],,3],type='l',ylim=c(0.1, 0.5))
lines(nveTime,smKise[nvewhich],col="blue")
lines(anaTime[seq(1,368,by=2)],x07$yo[ob[1],ob[2],seq(1,368,by=2),1],col="red")

plot(x07$xf[ob[1],ob[2],,2], HO07$K[2,ob[1],ob[2],])

plot(x07$xf[ob[1],ob[2],sot,2], HO07$H[2,ob[1],ob[2],sot])


#### SCATTER PLOT
pdf("figures/2014/SMvsH.pdf")
par(mfrow=c(2,2),oma=c(0,0,1.5,0))
for (i in 1:4){
  ylab <- sprintf("dWG2/dWG%d", i)
  plot(x07$xf[ob[1],ob[2],sot,2], HO07$H[i,ob[1],ob[2],sot],xlab=paste("WG",2,sep=""), ylab=ylab,ylim=c(0,0.85))
}
title(main="Soil moisture vs. Jaobians", outer=T,cex.main=2)
dev.off()


#### MEANS
julaug_meanwg2_DA <- apply(prog$WG2[,,julaug],1:2, mean,na.rm=T)
julaug_meanwg2_OL <- apply(prog_ol$WG2[,,julaug],1:2, mean,na.rm=T)

pdf("figures/2014/julaug_mean_wg2_DA.pdf")
image.plot(julaug_meanwg2_DA, col=two.colors(11,"red","blue","white"), main="mean WG2 July 15 - Aug 14 2014")
topo()
dev.off()

pdf("figures/2014/julaug_mean_diff_wg2.pdf")
image.plot(julaug_meanwg2_DA-julaug_meanwg2_OL,col=two.colors(11,"red","blue","white"),
           main="mean difference DA-OL WG2 July 15 - Aug 14 2014", zlim=c(-0.0004657016,0.0004657016))
topo()
dev.off()






##########################################################

smos <- readRDS("RDS_files/SEKF_smos.rds")
smap <- readRDS("RDS_files/SEKF_smap.rds")

smos_obs <- smos$yo[,,,1]
smos_inc <- smos$inc[,,,2]
smos_innov <- smos$innov[,,,1]

smos_inc[which(is.na(smos_obs))] <- NA
smos_innov[which(is.na(smos_obs))] <- NA
