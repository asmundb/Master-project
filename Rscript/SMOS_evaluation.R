require(fields)
source("read_SMOS_nc.R")

#  smos <- loadSMOSTimeserie("surfex_files/tmp")
smos <- loadSMOSTimeserie("/lustre/storeB/users/asmundb/SMOS/nc")

  ncid <- nc_open("surfex_files/FORCING.nc")
  I  <- ncvar_get(ncid, ncid$var$LON)
  J  <- ncvar_get(ncid, ncid$var$LAT)
  zs1 <- ncvar_get(ncid, ncid$var$ZS)
  nc_close(ncid)
  print("done")

  zs <- matrix(zs1, 111,111)
  topo <- function(){
    contour(zs, add=T, levels=c(0,2,10,50,100,200,500,1000))
  }
### AVERAGE RETRIEVAL ###

sm_mean <- apply(smos$sm, c(1,2), mean, na.rm=T)

colo <- rev(tim.colors())

pdf("sm_mean.pdf")
image.plot(sm_mean,
           col=colo,
           main="Averaged Soil Moisture Retrieval")
topo()
dev.off()


### Soil Moisture Data Quality indeX ###

dqx_mean <- apply(smos$sm_dqx,c(1,2), mean, na.rm=T)

pdf("sm_dqx_mean.pdf")
image.plot(dqx_mean,
           col=colo,
           main="Averaged Soil Moisture Retrieval Data Quality indeX")
topo()
dev.off()


### Science Flags ###


