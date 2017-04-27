require(fields)
require(ncdf4)
source("read_SMOS_nc.R")

#  smos <- loadSMOSTimeserie("surfex_files/tmp")
#smos <- loadSMOSTimeserie("/lustre/storeB/users/asmundb/SMOS/nc")

smos <- readRDS("SMOS_400.rds")

#############################################################
### Load useful data, topography, wilting point, porosity etc.

### lat, lon, topo ###
ncid <- nc_open("surfex_files/FORCING.nc")
I  <- ncvar_get(ncid, ncid$var$LON)
J  <- ncvar_get(ncid, ncid$var$LAT)
zs1 <- ncvar_get(ncid, ncid$var$ZS)
nc_close(ncid)
zs <- matrix(zs1, 111,111)

topo <- function(){
  contour(zs, add=T, levels=c(0,2,10,50,100,200,500,1000))
}

### wilt, sat, fc ###
print("read soil parameters from prep file...")
filename <- "surfex_files/PREP_SODA.nc"
ncid <- nc_open(filename)
wwilt1 <- ncvar_get(ncid, ncid$var$WWILT1)
wfc1   <- ncvar_get(ncid, ncid$var$WFC1)
wsat1  <- ncvar_get(ncid, ncid$var$WSAT1)
nc_close(ncid)


### OPEN LOOP ###

path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/OPEN_LOOP_SPINUP/ISBA"
# loadISBA is temporarily located in readOFFLINE.R
#nx=111
#ny=111
#spinup_wg1 <- loadISBA(path=path,
#                   filename="ISBA_PROGNOSTIC",
#                   varname="WG1")
#spinup_wgi1 <- loadISBA(path=path,
#                   filename="ISBA_PROGNOSTIC",
#                   varname="WGI1")
wg1_ol <- readRDS("wg1_long_spinup_open_loop.rds")
wgi1_ol <- readRDS("wgi1_long_spinup_open_loop.rds")

sm_ol <- wg1_ol + wgi1_ol

smMin <- apply(smos$sm, c(1,2), min, na.rm=T)
smMax <- apply(smos$sm, c(1,2), max, na.rm=T)
smMax[is.infinite(smMax)] <- NA

smos_norm <- smos$sm
smos_norm[,,] <- NA
for (i in 1:dim(smos$sm)[3]){
  smos_norm[,,i] <- (smos$sm[,,i]-smMin)*(wsat1-wwilt1)/(smMax-smMin) + wwilt1
}
smos_norm_mean_true <- apply(smos_norm, c(1,2), mean,na.rm=T)


#############################################################

### AVERAGE RETRIEVAL ###

sm_mean <- apply(smos$sm, c(1,2), mean, na.rm=T)
sm_norm_mean <- (sm_mean-min(sm_mean,na.rm=T))*(wsat1-wwilt1)/(max(sm_mean,na.rm=T)-min(sm_mean,na.rm=T)) + wwilt1
wg1_ol_mean <- apply(wg1_ol,c(1,2), mean, na.rm=T)

zlim <- c(min(min(sm_mean,na.rm=T), min(sm_norm_mean,na.rm=T), min(wg1_ol_mean,na.rm=T)),
          max(min(sm_mean,na.rm=T), max(sm_norm_mean,na.rm=T), max(wg1_ol_mean,na.rm=T)))
#zlim=c(0,1)

wet_colo <- rev(tim.colors())
dif_colo <- designer.colors( n=256, col= c("red", "white", "blue"), x=seq(-1,1,length=3) ,alpha=1.0)




#times to compare
ttc <- seq(as.POSIXlt("2016-05-01 06:00"), as.POSIXlt("2016-10-16 18:00"), by=3600*12)
ttc <- format(ttc,format="%Y-%m-%d %H:%M")

# smos - model
smos_tc <- smos$sm[,,which(smos$time %in% ttc)]
offline_tc <- wg1_ol[,,seq(6,by=12,length=length(ttc))]


### CORRELATION ###
smos_cor <- array(dim=c(111,111))
for (i in 1:111){
  for (j in 1:111){
    smos_cor[j,i] <- cor(smos_tc[j,i,], offline_tc[j,i,], use='na')
  }
}
pdf("figures/SMOS_evaluation/smos-offline_cor.pdf")
image.plot(smos_cor, col=calla(smos_cor), main="SMOS OFFLINE correlation")
topo()
dev.off()


### DIFFERENCE ###
smosMoff <- smos_tc - offline_tc
smosMoff_mean <- apply(smosMoff,c(1,2), mean, na.rm=T)

#pdf("figures/SMOS_evaluation/smos-offline_mean.pdf")
#image.plot(smosMoff_mean, col=calla(smosMoff_mean), main="")
#topo()
#dev.off()


### RMSE ###

N <- dim(smos_tc)[3]
rmse <- sqrt(apply(smosMoff^2, c(1,2), sum,na.rm=T)/N)

pdf("figures/SMOS_evaluation/smos-offline_rmse.pdf")
image.plot(rmse,col=c("white",two.colors(start="white",end="black",middle="red")), main="SMOS OFFLINE RMSE")
topo()
dev.off()

#
#pdf("sm_mean.pdf")
#image.plot(sm_mean,
#           col=colo,
#           main="Averaged Soil Moisture Retrieval",
#           zlim=zlim)
#topo()
#dev.off()
#
#
## scaled to model range
#
#pdf("smos_mean_norm.pdf")
#image.plot(sm_norm_mean,
#           col=colo,
#           main="Averaged normalized Soil Moisture Retrieval",
#           zlim=zlim)
#topo()
#dev.off()
##
### open loop
##
##pdf("sm_mean_openloop.pdf")
##image.plot(wg1_ol_mean,
##           col=colo,
##           main="Averaged Soil Moisture modeled",
##           zlim=zlim)
##topo()
##dev.off()
##
##
### diff
##
#pdf("sm_mean_smos-model.pdf")
#image.plot(sm_mean-wg1_ol_mean,
#           col=calla(sm_mean-wg1_ol_mean),
#           main="Difference Averaged Soil Moisture SMOS-modeled")
#topo()
#dev.off()
##
#pdf("sm_mean_smos_norm-model.pdf")
#image.plot(smos_norm_mean_true-wg1_ol_mean,
#           col=calla(smos_norm_mean_true-wg1_ol_mean),
#           main="Difference Averaged Soil Moisture SMOS_norm-modeled")
#topo()
#dev.off()


### Soil Moisture Data Quality indeX ###

#dqx_mean <- apply(smos$sm_dqx,c(1,2), mean, na.rm=T)
#
#pdf("sm_dqx_mean.pdf")
#image.plot(dqx_mean,
#           col=colo,
#           main="Averaged Soil Moisture Retrieval Data Quality indeX")
#topo()
#dev.off()


### Science Flags ###

calla <- function(x, start="red", end="blue", zero="white"){
  N <- 100
  minVal <- min(x,na.rm=T)
  maxVal <- max(x,na.rm=T)
  print("ok")
  xs <- seq(minVal, maxVal, length=N)
  zer  <- which.min(abs(xs))
  dif_colo <- two.colors(n=N,start=start,end=end,middle=zero)
  N <- 100
  if (zer > N/2){
    print("zer > N/2")
    out <- c(1,(3/2)*N - zer)
  } else if (zer < N/2) {
    print("zer < N/2")
    out <- c((N)/2 - zer, N)
  } else {
    print("zer=N/2")
    out <- c(1,N)
  }
  out <- round(out)
  colo <- colTZ2(n=N,minVal=minVal, maxVal=maxVal, start=dif_colo[out[1]], end=dif_colo[out[2]])
  return(colo)
}

colTZ2 <- function(n=256, minVal=-1, maxVal=1, start="red", end="blue", zero="white"){
  colo   <- designer.colors(n=n, c(start, zero, end), x=c(minVal, 0, maxVal))
  return(colo)
} 
