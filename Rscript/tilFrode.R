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





path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2014/SPINUP/ISBA/"
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



time <- seq(as.POSIXlt("2014-05-01 01:00"), as.POSIXlt("2014-09-01 00:00"), by=3600)


B <-  diag$H_ISBA/diag$LE_ISBA
EF <- 1/(1+B)

june <- 744:1464
july <- 1465:2208
august <- 2209:2952

julaug <- 1801:2519

EF2 <- diag$LE_ISBA/diag$RN_ISBA


source("topo.R")
stop()

# LE WG

col <- two.colors(11, "blue","red", "#EEEEEE")

pdf("figures/2014/LE_WG1_july.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,july], prog$WG1[,,july]),zlim=c(-1,1),col=col, main="cor(LE, WG1) july 2014")
topo()
dev.off()

pdf("figures/2014/LE_WG1_june.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,june], prog$WG1[,,june]),zlim=c(-1,1),col=col, main="cor(LE, WG1) june 2014")
topo()
dev.off()

pdf("figures/2014/LE_WG1_julaug.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,julaug], prog$WG1[,,julaug]),zlim=c(-1,1),col=col, main="cor(LE, WG1) 15.july-14.aug 2014")
topo()
dev.off()


# EF WG
pdf("figures/2014/EF_WG1_july.pdf")
image.plot( mat_cor(EF2[,,july], prog$WG1[,,july]),zlim=c(-0.4,0.4),col=col, main="cor(EF, WG1) july 2014")
topo()
dev.off()

pdf("figures/2014/EF_WG1_june.pdf")
image.plot( mat_cor(EF2[,,june], prog$WG1[,,june]),zlim=c(-0.4,0.4),col=col, main="cor(EF, WG1) june 2014")
topo()
dev.off()


# LE TG
pdf("figures/2014/LE_TG1_july.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,july], prog$TG1[,,july]), zlim=c(-1,1), col=rev(col), main="cor(LE, TG1) july 2014")
topo()
dev.off()
 
pdf("figures/2014/LE_TG1_julaug.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,julaug], prog$TG1[,,julaug]), zlim=c(-1,1), col=rev(col), main="cor(LE, TG1) 15.july-14.aug 2014")
topo()
dev.off()

pdf("figures/2014/LE_TG1_june.pdf")
image.plot( mat_cor(diag$LE_ISBA[,,june], prog$TG1[,,june]),zlim=c(-1,1), col=rev(col), main="cor(LE, TG1) june 2014")
topo()
dev.off()

rn <- as.numeric(diag$RN_ISBA)
le <- as.numeric(diag$LE_ISBA)
sm <- as.numeric(prog$WG1)

png("figures/2014/EF.png")
plot(sm,le/rn,main="EF=LE/RN vs soil moisture")
dev.off()

rn <- as.numeric(diag$RN_ISBA[,,june])
le <- as.numeric(diag$LE_ISBA[,,june])
sm <- as.numeric(prog$WG1[,,june])


png("figures/2014/EF_june.png")
plot(sm,le/rn,main="EF=LE/RN vs soil moisture june")
dev.off()


rn <- as.numeric(diag$RN_ISBA[,,july])
le <- as.numeric(diag$LE_ISBA[,,july])
sm <- as.numeric(prog$WG1[,,july])


png("figures/2014/EF_july.png")
plot(sm,le/rn,main="EF=LE/RN vs soil moisture july")
dev.off()

sm <- as.numeric(prog$WG1)
ef <- as.numeric(EF2)

png("figures/2014/EF2.png")
plot(sm, ef,ylim=c(-100,100))
dev.off()


ef <- diag$LE_ISBA[ob[1],ob[2],]/diag$RN_ISBA[ob[1],ob[2],]
ef1 <- ef[!ef %in% boxplot.stats(ef)$out]
sm <- prog$WG1[ob[1],ob[2],][!ef %in% boxplot.stats(ef)$out]

pdf("figures/2014/ef_sm.pdf")
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
blon <- 10.719025
blat <- 59.942484

bij <- fnn_lamb(I,J,blon,blat)$ij_out
m <- matrix(1:length(I),111,111)
ob <- which(m == bij, arr.ind=T)

plot(diag$T2M_ISBA[ob[1],ob[2],],type="l")


getObs <- function(tab, P, fd, td, stnr){
       URL <- sprintf("http://klapp/metnopub/production/metno?re=30&tab=%s&%s&fd=%s&td=%s&split=0&nmt=0&ddel=dot&del=;&ct=text/plain&s=%s", tab, P, fd,td, stnr)
       df <- read.table(URL,na.strings=c("-",".","<NA>"), header=TRUE)
       colnames(df)[2] <- "TIME"
       df[,"TIME"] <- gsub('\\D','\\1',df[,"TIME"])
       return(df)
}

ta_obs <- getObs("T_ADATA","p=TA&p=TAX","01.05.2014","01.09.2014",18700)
t2m_obs <- ta_obs[2:2953,3]
t2m_max <- ta_obs[2:2953,4]


time2 <- seq(as.POSIXlt("2014-05-01 01:00"), as.POSIXlt("2014-09-01 00:00"), by=3600)

pdf("figures/2014/scatter_T2M_obs_mod.pdf")
plot(t2m_obs,diag$T2M_ISBA[ob[1],ob[2],]-273.15,main="T2M Oslo Blindern 2014 may-aug; r= 0.9552136",xlab="obs",ylab="surfex offline")
abline(0,1,col="red")
dev.off()


pdf("figures/2014/timeserie_T2M_obs_mod.pdf")
plot(time2,t2m_obs,type='l',main="T2M Oslo Blindern",ylab="T2M [C]")
lines(time2,diag$T2M_ISBA[ob[1],ob[2],]-273.15,col="red")
legend("topleft",legend=c("obs","sfx offln"), lty=1,col=c("black","red"))
dev.off()

pdf("figures/2014/timeserie_T2M_obs_mod_diff.pdf")
plot(time2,diag$T2M_ISBA[ob[1],ob[2],]-273.15-t2m_obs,type='l',main="T2M difference sfx-obs Oslo Blindern",ylab="T2M [C]")
legend("topleft",legend=c("obs","sfx offln"), lty=1,col=c("black","red"))
dev.off()




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
