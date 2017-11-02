##############################################################################
#### FMI STATIONS ####

# station coordinates #
lon0 <- 26.63980
lat0 <- 67.37079
#######################

path <- "/lustre/storeA/users/asmundb/Master-project/ISMN/FMI/"


files <- list.files(path, pattern="sm_0.050000",full.names=T,recursive=T)
nfiles <- length(files)

stations <- gsub('.*FMI//(SOD.*)/FMI_.*', '\\1', files)
stations[c(15,17)] <- paste(stations[c(15,17)],"-A",sep="")
stations[c(16,18)] <- paste(stations[c(16,18)],"-B",sep="")
nstations <- length(stations)

sm <- list()

for (i in 1:nfiles){
  x <- read.table(files[i],skip=1,stringsAsFactors=F)
  sm[[stations[i]]] <- x[,1:3]
  sm[[stations[i]]][,3][which(is.nan(sm[[stations[i]]][,3]))] <- NA
}


##########################

plot(NA,NA,xlim=c(0,1),ylim=c(0,100))
par(mfrow=c(3,5))
for (i in 1:nstations){
#  lines(density(sm[[i]][,3],na.rm=T))
  hist(sm[[i]][,3], breaks=100,main=names(sm)[i],xlab="")
#  readline(prompt="Press [enter] to continue")
}


##########################
### match data by time ###


time <- seq(as.POSIXlt("2016-06-01 01:00"), to=as.POSIXlt("2016-09-01 00:00"), by=3600)
ntimes <- length(time)
alltimes <- data.frame(time=as.character(time))
timeMask <- data.frame(time=as.character(time))

for (i in 1:nstations){
  dtmp <- sm[[i]][,1]
  ttmp <- sm[[i]][,2]
  smtimes <- paste(gsub(patter="/",replacement="-",dtmp)," ", ttmp, ":00",sep="")
  
  tmp <- data.frame(time=smtimes,val=sm[[i]][,3])
  tmp2 <- merge(alltimes, tmp, by="time",all.x=T)
  timeMask[stations[i]] <- tmp2[,2]
}



#############################################################################
### SMOS ####
require(ncdf4)
source("ffunctions.R")

getSM <- function(file){
  ncid <- nc_open(file)
  x <- ncvar_get(ncid, ncid$var$Soil_Moisture)
  nc_close(ncid)
  return(x)
}

# files to read
path <- "/lustre/storeB/users/asmundb/SMOS/nc/"
files <- list.files(path, pattern="SM_OPER", full.names=T, recursive=T)
Afiles <- files[54:145]
Dfiles <- files[254:345]
nfiles <- length(Afiles)

# get point
ncid <- nc_open(Afiles[1])
lon  <- ncvar_get(ncid, ncid$dim$lon)
lat  <- ncvar_get(ncid, ncid$dim$lat) 
nc_close(ncid)
ij   <- fnn(lon,lat,lon0,lat0)

# save value
Asmos <- numeric(nfiles)
Dsmos <- numeric(nfiles)

for (i in 1:nfiles){
  tmp      <- getSM(Afiles[i])
  Asmos[i] <- tmp[ij[1],ij[2]]
  tmp      <- getSM(Dfiles[i])
  Dsmos[i] <- tmp[ij[1],ij[2]]
}

#############################################################################
#### SMAP ####

require(h5)

getPoint <- function(PASS, point, dmax=0.2){
  d   <- (PASS$lon-point[1])^2 + (PASS$lat-point[2])^2
  if (min(d) > dmax){
    ij <- NA
  }else{
    ij <- which.min(d)
  }
  return(ij)
}


readDS <- function(f,gr,dat,ext){
  ds <- paste(gr,dat,ext,sep="")
  x <- readDataSet(f[ds])
  return(x)
}


getDataSet <- function(f, pass="AM"){
  gr <- sprintf("/Soil_Moisture_Retrieval_Data_%s",pass)
  ext <- ""
  if (pass == "PM"){ext="_pm"}
  lon <- readDS(f,gr,"/longitude",ext)
  lat <- readDS(f,gr,"/latitude",ext)
  sm  <- readDS(f,gr,"/soil_moisture",ext)
  sm[which(sm < 0)] <- NA

  x <- list( lon=lon, lat=lat, sm=sm)
  return(x)
}



path <- "/lustre/storeB/users/asmundb/SMAP/n5eil01u.ecs.nsidc.org/SMAP/SPL3SMP.004/"

files <- list.files(path, pattern="SMAP_L3",full.names=T, recursive=T)
nfiles <- length(files)


AMsmap <- numeric(nfiles)
PMsmap <- numeric(nfiles)

for (i in 1:nfiles){
  f         <- h5file(files[i])
  tmp       <- getDataSet(f,pass="AM")
  kl        <- getPoint(tmp, c(lon0, lat0))
  AMsmap[i] <- tmp$sm[kl]
  tmp       <- getDataSet(f,pass="PM")
  kl        <- getPoint(tmp, c(lon0, lat0))
  PMsmap[i] <- tmp$sm[kl]
  h5close(f)
}


#################################################################################
#### TIME SERIE ####

time <- seq(as.POSIXlt("2016-06-01 01:00"), as.POSIXlt("2016-09-01 00:00"), by=3600) # hourly
AM   <- grep("06:00",time)
PM   <- grep("18:00",time)


##################################################################################
#### Visualization ####

pdf("figures/SATevaluation/timeserie.pdf")
#plot(time,sm[[1]][2:2209,3],type="n", ylim=c(0,1), ylab="Soil moisutre m3m-3", xlab="",main="FMI soilmoisture network")
par(pin=c(3,3),xpd=T,ps=16,mar=c(10,5,10,1))
plot(time,timeMask[,2],type="n",ylim=c(0,0.5),ylab="Soil moisutre m3m-3", xlab="",main="FMI soil moisture network")

for (i in 1:14){
  lines(time,timeMask[,i+1])
}

stat_mean <- apply(timeMask[,2:15],1,mean,na.rm=T)
lines(time, stat_mean, lwd=2, lty=2)

satCols <- c("red","darkred","blue","darkblue")

points(time[AM], AMsmap[1:92],col=satCols[1], pch=1)
points(time[PM], PMsmap[1:92],col=satCols[2], pch=2)

points(time[AM], Asmos,col=satCols[3], pch=3)
points(time[PM], Dsmos,col=satCols[4], pch=4)

legend("bottom", legend=c("in-situ","in-situ mean","SMAP_am","SMAP_pm","SMOS_am","SMOS_pm"),
       pch=c(NA,NA,1:4),
       lty=c(1,2,NA,NA,NA,NA),
       lwd=c(1,2,NA,NA,NA,NA),
       col=c(1,1,satCols),
       bg="white",
       inset=c(0,-0.65))
dev.off()


##################################################################################
#### Stats ####

corrs <- array(NA, dim=c(6,nstations))
for (st in 1:nstations){
  corrs[1,st] <- cor(timeMask[AM,st+1], AMsmap[1:92], use="na")
  corrs[2,st] <- cor(timeMask[PM,st+1], PMsmap[1:92], use="na")
  corrs[3,st] <- cor(timeMask[AM,st+1], Asmos, use="na")
  corrs[4,st] <- cor(timeMask[PM,st+1], Dsmos, use="na")
}


pdf("figures/SATevaluation/FMI_correlations.pdf")
par(pin=c(3,3),xpd=T,ps=16,mar=c(16,6,10,6))

plot(1:nstations,1:nstations,type="n",ylim=c(-1,1),ylab="correlation", xlab="",xaxt="n",
     main="Correlations for SMOS and SMAP in \n FMI soil moisture network at Sodankylä")
axis(1,at=1:nstations,labels=F)
text(x=1:nstations,y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]),labels=stations,srt=45,adj=1,xpd=T)

for (dt in 1:4){
  for (st in 1:nstations){
    points(st,corrs[dt,st],pch=dt,col=satCols[dt])
  }
#  abline(h=mean(corrs[dt,],na.rm=T),col=dt)
}

legend("bottom",inset=c(0,-1.2), legend=c("SMAP_am(92)","SMAP_pm(91)","SMOS_am(28)","SMOS_pm(5)"), pch=1:4,col=satCols)

dev.off()



#### RMSE ####

rmse <- function(x,y){
  rmse <- sqrt(mean((x-y)^2,na.rm=T))
  return(rmse)
}

corrs <- array(NA, dim=c(6,nstations))
for (st in 1:nstations){
  corrs[1,st] <- rmse(timeMask[AM,st+1], AMsmap[1:92])
  corrs[2,st] <- rmse(timeMask[PM,st+1], PMsmap[1:92])
  corrs[3,st] <- rmse(timeMask[AM,st+1], Asmos)
  corrs[4,st] <- rmse(timeMask[PM,st+1], Dsmos)
}


pdf("figures/SATevaluation/FMI_rmse.pdf")
par(pin=c(3,3),xpd=T,ps=16,mar=c(16,6,10,6))

plot(1:nstations,1:nstations,type="n",ylim=c(-1,1),ylab="rmse", xlab="",xaxt="n",
     main="RMSE for SMOS and SMAP in \n FMI soil moisture network at Sodankylä")
axis(1,at=1:nstations,labels=F)
text(x=1:nstations,y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]),labels=stations,srt=45,adj=1,xpd=T)

for (dt in 1:4){
  for (st in 1:nstations){
    points(st,corrs[dt,st],pch=dt,col=satCols[dt])
  }
#  abline(h=mean(corrs[dt,],na.rm=T),col=dt)
}

legend("bottom",inset=c(0,-1.2), legend=c("SMAP_am(92)","SMAP_pm(91)","SMOS_am(28)","SMOS_pm(5)"), pch=1:4,col=satCols)

dev.off()


#####################################
#### More stats ####
mcorrs <- numeric(4)
mcorrs[1] <- cor(stat_mean[AM], AMsmap[1:92],use="na")
mcorrs[2] <- cor(stat_mean[PM], PMsmap[1:92],use="na")
mcorrs[3] <- cor(stat_mean[AM], Asmos,use="na")
mcorrs[4] <- cor(stat_mean[PM], Dsmos,use="na")

#####################################
