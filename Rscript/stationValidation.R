require(ncdf4)
require(fields)
source("topo.R")
source("ffunctions.R")
source("chi2.R")

# read diagnostic variables #
#source("read_smos_smap_ol.R")
load("RDS_files/DIAG_2016.RDATA")
load("RDS_files/PROG_2016.RDATA")


#smos <- loadSODA("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMOS/SEKF/obs05_2/")
#smap <- loadSODA("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs038_2/")
smos <- readRDS("RDS_files/SEKF_smos.rds")
smap <- readRDS("RDS_files/SEKF_smap.rds")

smapHO <- loadHO("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs038/")
smosHO <- loadHO("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMOS/SEKF/obs05/")



ncid <- nc_open("surfex_files/FORCING.nc")
I  <- ncvar_get(ncid, ncid$var$LON)
J  <- ncvar_get(ncid, ncid$var$LAT)
nc_close(ncid)
print("done")


stations <- c("Kise"  , "Dagali", "Kongsberg", "Blomskog", "Notodden", "Flisa" , "Ørnhaugen", "Aurskog")
stnr     <- c(12550   ,   29720 ,    28380   ,   240800  ,    30650  ,     6020,     7420   ,     2650 )
lons     <- c(10.8055 ,  8.5263 ,   9.6377   ,  12.0779  ,   9.2105  ,  12.0125,    11.4992 ,  11.5801 )
lats     <- c(60.7733 , 60.4188 ,  59.6247   ,  59.2219  ,  59.5667  ,  60.6141,    61.3763 ,  59.9119 )
xx       <- numeric(length(stations))
yy       <- numeric(length(stations))

nstat <- length(stations)

stList <- data.frame(stations,stnr,lons,lats, xx, yy,stringsAsFactors=F)


m <- matrix(1:length(I),111,111)
for (st in 1:nstat){
  bij <- fnn_lamb(I,J,stList[st,"lons"],stList[st,"lats"])$ij_out
  ob <- which(m == bij, arr.ind=T)
  stList$xx[st] <- ob[1]
  stList$yy[st] <- ob[2]
}

### Manual correction ###

stList$xx[1] <- 74                   # Kise is water 
stList$yy[4] <- stList$yy[4] + 1     # Blomskog is water

#########################


### KISE

kise <- read.table("kise_2.csv",skip=1)
t0 <- which(kise$V1 == "2016-06-01" & kise$V2 == "01:00;")[1]
t1 <- which(kise$V1 == "2016-09-01" & kise$V2 == "00:00;")[1]
kise_sm <- kise$V4[which(kise$V3[t0:t1] == "-10;")]
kise_sm[which(kise_sm == -9999)] <- kise_sm[which(kise_sm == -9999) +1]


# SYNOP
getObs <- function(tab, P, fd, td, stnr){
       URL <- sprintf("http://klapp/metnopub/production/metno?re=30&tab=%s&%s&fd=%s&td=%s&split=0&nmt=0&ddel=dot&del=;&ct=text/plain&s=%s", tab, P, fd,td, stnr)
       df <- read.table(URL,na.strings=c("-",".","<NA>"), header=TRUE)
       colnames(df)[2] <- "TIME"
       df[,"TIME"] <- gsub('\\D','\\1',df[,"TIME"])
       return(df)
}


insitu_RH <- list()
insitu_TA <- list()
for (st in 1:nstat){
  if (st==4){
    tab = "T_UTLANDDATA"
  }else{
    tab = "T_ADATA"
  }
  tmp <- getObs(tab,"p=TA&p=UU","01.05.2016","01.09.2016",stList$stnr[st])
  insitu_RH[[stList$stations[st]]] <- tmp[1:2952,"UU"]/100
  insitu_TA[[stList$stations[st]]] <- tmp[1:2952,"TA"] + 273.15
}


nObs <- function(obs){
  nobs <- apply(!is.na(obs),1:2,sum)
  return(nobs)
}

rm_null <- function(x){
  y <- x
  y[which(y == 0)] <- NA
  return(y)
}

stop()
### HEAT MAP ###

nsmap <- rm_null(nObs(smap$yo[,,,1]))
nsmos <- rm_null(nObs(smos$yo[,,,1]))

pdf("figures/2016/heatMap_stations.pdf")
image.plot(nsmos, col=two.colors(100, "lightblue","darkblue", "blue", alpha=0.5),zlim=c(0,180))
image.plot(nsmap, col=two.colors(100, "pink","darkred", "red", alpha=0.5),add=T,zlim=c(0,180))
topo()
points(stList$xx/111,stList$yy/111, col="green", pch=19,cex=1.2)
text(stList$xx/111,stList$yy/111,labels=stList$stations,col="green",pos=1)
dev.off()

### Timeseries ###
st=1
plot(insitu_RH[[stList$stations[st]]],type='l')
lines(diag_ol$HU2M_ISBA[stList$xx[st],stList$yy[st],], col="green")
lines(diag_smos$HU2M_ISBA[stList$xx[st],stList$yy[st],], col="blue")
lines(diag_smap$HU2M_ISBA[stList$xx[st],stList$yy[st],], col="red")



#### CORRELATIONS AND RMSE ####
rmsd <- function(x,y){
  rmsd <- sqrt(sum((x-y)^2,na.rm=T)/length(x))
  return(rmsd)
}



corr_rh <- array(NA,dim=c(nstat,3))
rmse_rh <- array(NA,dim=c(nstat,3))

corr_ta <- array(NA,dim=c(nstat,3))
rmse_ta <- array(NA,dim=c(nstat,3))

for (st in 1:nstat){
  corr_rh[st,1] <- cor(insitu_RH[[stList$stations[st]]], diag_ol$HU2M[stList$xx[st],stList$yy[st],],use="na")
  corr_rh[st,2] <- cor(insitu_RH[[stList$stations[st]]], diag_smos$HU2M[stList$xx[st],stList$yy[st],],use="na")
  corr_rh[st,3] <- cor(insitu_RH[[stList$stations[st]]], diag_smap$HU2M[stList$xx[st],stList$yy[st],],use="na")

  rmse_rh[st,1] <- rmsd(insitu_RH[[stList$stations[st]]], diag_ol$HU2M[stList$xx[st],stList$yy[st],])
  rmse_rh[st,2] <- rmsd(insitu_RH[[stList$stations[st]]], diag_smos$HU2M[stList$xx[st],stList$yy[st],])
  rmse_rh[st,3] <- rmsd(insitu_RH[[stList$stations[st]]], diag_smap$HU2M[stList$xx[st],stList$yy[st],])
 
  corr_ta[st,1] <- cor(insitu_TA[[stList$stations[st]]], diag_ol$T2M[stList$xx[st],stList$yy[st],],use="na")
  corr_ta[st,2] <- cor(insitu_TA[[stList$stations[st]]], diag_smos$T2M[stList$xx[st],stList$yy[st],],use="na")
  corr_ta[st,3] <- cor(insitu_TA[[stList$stations[st]]], diag_smap$T2M[stList$xx[st],stList$yy[st],],use="na")

  rmse_ta[st,1] <- rmsd(insitu_TA[[stList$stations[st]]], diag_ol$T2M[stList$xx[st],stList$yy[st],])
  rmse_ta[st,2] <- rmsd(insitu_TA[[stList$stations[st]]], diag_smos$T2M[stList$xx[st],stList$yy[st],])
  rmse_ta[st,3] <- rmsd(insitu_TA[[stList$stations[st]]], diag_smap$T2M[stList$xx[st],stList$yy[st],])
}


plot_stat <- function(stats ,main="title",ylab="value"){
  nstat <- dim(stats)[1]
  col=c("black","blue","red")
  plot(1:nstat,1:nstat,type='n',ylim=c(min(stats),max(stats)),xlab=NA,ylab=ylab,xaxt='n',main=main)
  text(x=1:nstat,y=par()$usr[3]-0.03*(par()$usr[4]-par()$usr[3]),labels=stations,srt=45,adj=1,xpd=T)
  for(i in 1:3){
    points(stats[,i],pch=i,col=col[i],cex=2)
  }
  legend("topleft",legend=c("open loop","SEKF smos"," SEKF smap"), pch=1:3,col=col)
}

pdf("figures/2016/RH_correlation.pdf")
plot_stat(corr_rh,"RH 2m correlation JJA 2016", "r=")
dev.off()
pdf("figures/2016/RH_rmse.pdf")
plot_stat(rmse_rh,"RH 2m RMSE JJA 2016", "rmse")
dev.off()


pdf("figures/2016/TA_correlation.pdf")
plot_stat(corr_ta,"Tair 2m correlation JJA 2016", "r=")
dev.off()
pdf("figures/2016/TA_rmse.pdf")
plot_stat(rmse_ta,"Tair 2m RMSE JJA 2016", "rmse")
dev.off()

###########################################################


# Dagali

st <- 2
plot(insitu_RH[[stList$stations[st]]],type='l')
lines(diag_ol$HU2M_ISBA[stList$xx[st],stList$yy[st],], col="green")
lines(diag_smos$HU2M_ISBA[stList$xx[st],stList$yy[st],], col="blue")
lines(diag_smap$HU2M_ISBA[stList$xx[st],stList$yy[st],], col="red")

plot(prog_smap$WG2[stList$xx[st],stList$yy[st],]- prog_ol$WG2[stList$xx[st],stList$yy[st],],xlab=diff)


plot(insitu_RH$Dagali[50:300],col="black",type='l')
lines(diag_ol$HU2M_ISBA[stList$xx[st],stList$yy[st],50:300], col="green")
lines(diag_smap$HU2M_ISBA[stList$xx[st],stList$yy[st],50:300],col="blue")


at <- seq(6,length(insitu_RH$Dagali),by=24)
plot(insitu_RH$Dagali[at+1],col="black",type='l')
lines(diag_ol$HU2M_ISBA[stList$xx[st],stList$yy[st],at], col="green")
lines(diag_smap$HU2M_ISBA[stList$xx[st],stList$yy[st],at],col="blue")



MM <- function(x,n){
# Clalculate moving mean for x around i-n:i+n
  N  = length(x) + 2*n-1
  mm = numeric(length=N)
  x  = c(rep(NA,n), x, rep(NA,n))
  for (i in n:(N-n)){
    wn = sum(!is.na(x[(i-n):(i+n)]))
    mm[i] = sum(x[(i-n):(i+n)], na.rm=T)/(wn)
  }
  return(mm[n:(N-n)])
}

### Boxplots 
make_box <- function(dat, datHO, sat, var, title){
  for(st in 1:nstat){
    analys <- which(!is.na(dat$yo[stList$xx[st],stList$yy[st],,1]))
    if (length(analys) == 0) next
    x <- matrix(NA, 7, length(analys))

    for (i in 1:7){
      x[i,] <- as.numeric(datHO[[var]][i,stList$xx[st],stList$yy[st],analys])
    }
    pdf(sprintf("figures/2016/boxplots/%sBox_%s_%s.pdf", var,sat, stList$stations[st]))
    boxplot(t(x),main=sprintf("%s, %s JJA 2016, N=%d", title, stList$stations[st], length(analys)),ylab=var,xlab="soil layer")
    dev.off()
  }
}

make_box(smap,smapHO,"smap","K","Kalman gain")
make_box(smos,smosHO,"smos","K","Kalman gain")
make_box(smap,smapHO,"smap","H","Jacobian")
make_box(smos,smosHO,"smos","H","Jacobian")


### histograms

make_hist <- function(dat,sat,var,title){
  for(st in 1:nstat){
    analys <- which(!is.na(dat$yo[stList$xx[st],stList$yy[st],,1]))
    if (length(analys) == 0) next
    pdf(sprintf("figures/2016/histograms/%s_hist_%s_%s.pdf", var,sat,stList$stations[st]))
    hist(dat[[var]][stList$xx[st],stList$yy[st],analys,2],
         main=sprintf("%s, %s JJA 2016, N=%d", title, stList$stations[st], length(analys)),xlab=var)
    dev.off()
  }
}

make_hist(smap,"smap","inc","Increments")
make_hist(smos,"smos","inc","Increments")




x <- array(NA, dim=c(111,111,7))
for(k in 1:7){
  for(i in 1:111){
    for(j in 1:111){
     x[j,i,k] <- mean(abs(diff(smap$xf[j,i,,k])))
    }
  }
}

plot(1:7,1:7,ylim=c(min(x,na.rm=T),max(x,na.rm=T)),ylab="Difference after 6 hours",xlab="Soil layer",main="mean abs diff")
lines(apply(x,3,range,na.rm=T)[1,])
lines(apply(x,3,range,na.rm=T)[2,])
lines((apply(x,3,range,na.rm=T)[2,] - apply(x,3,range,na.rm=T)[1,])/2 +apply(x,3,range,na.rm=T)[1,])


######################################################################################
#####  autocorrelations #####


# time
st=2

acf(prog_ol$WG2[stList$xx[st],stList$yy[st],],lag.max=100,na.action=na.pass)


# space

acf(prog_ol$WG2[,,100],lag.max=100,na.action=na.pass)




############################################
### Normaler ###

insitu_RH <- list()
insitu_TA <- list()
for (st in 1:nstat){
  if (st==4){
    tab = "T_UTLANDDATA"
  }else{
    tab = "T_ADATA"
  }
  tmp <- getObs(tab,"p=TA&p=UU","01.06.2016","01.09.2016",stList$stnr[st])
  insitu_RH[[stList$stations[st]]] <- tmp[2:2209,"UU"]/100
  insitu_TA[[stList$stations[st]]] <- tmp[2:2209,"TA"] + 273.15
}

normals <- list()
for (st in 1:nstat){
  if (st %in% c(1,2,3,5,6,8)){
      url <- sprintf("http://klapp/metnopub/production/metno?re=28&p=TAM&p=RR&m=6&m=7&m=8&ddel=dot&del=semicolon&ct=text/plain&s=%s&nod=NA",stList$stnr[st])
      tmp <- read.table(url,header=T,sep=";")
      normals[[stList$station[st]]] <- tmp[1:3,c("TAM","RR")]
  }
}




montly <- list()
for (st in 1:nstat){
  if (st %in% c(1,2,3,5,6,8)){
      url <- sprintf("http://klapp/metnopub/production/metno?re=15&p=TAM&p=RR&fd=01.06.2016&td=01.09.2016&del=semicolon&m=6&m=7&m=8&ct=text/plain&s=%s&nod=NA",stList$stnr[st])
      tmp <- read.table(url,header=T,sep=";")
      montly[[stList$station[st]]] <- tmp[1:3,c("TAM","RR")]
  }
}


sts <- c(1,2,3,5,6,8)
mnts <- c("June","July","August")


pdf("figures/other/prec_anom.pdf",height=3,width=4)
par(pin=c(2,2),mar=c(2.1,4.1,2.1,1))
plot(NA,xlim=c(0,4),ylim=c(-100,100),xaxt="n",ylab="Precipitation anomaly [%]",main="Precipitation anomalies 2016",xlab="")
axis(1,at=1:3,labels=mnts)
for(st in 1:length(montly)){
  points((montly[[st]][,"RR"]- normals[[st]][,"RR"])/normals[[st]][,"RR"]*100,pch=st,col="black")
}
legend("topright",legend=stList$station[sts],col="black", pch=1:length(montly))
dev.off()


pdf("figures/other/tmp_anom.pdf",height=3,width=4)
par(pin=c(2,2),mar=c(2.1,4.1,2.1,1))
plot(NA,xlim=c(0,4),ylim=c(-1,5),xaxt="n",ylab="Temperature anomaly [K]",main="Temperature anomalies 2016",xlab="")
axis(1,at=1:3,labels=mnts)
for(st in 1:length(montly)){
  points(montly[[st]][,"TAM"]- normals[[st]][,"TAM"],pch=st,col="black")
}
#legend("topright",legend=stList$station[sts],col="black", pch=1:length(montly))
dev.off()


pdf("figures/other/prec_temp_anom.pdf",height=3,width=4)
par(pin=c(2,2),mar=c(2.1,4.1,2.1,1))
plot(NA,xlim=c(-100,100),ylim=c(-1,5),xlab="Precipitation anomaly [%]",ylab="Temperature anomaly",main="2016")
for(m in 1:3){
for(st in 1:length(montly)){
  points((montly[[st]][m,"RR"]- normals[[st]][m,"RR"])/normals[[st]][m,"RR"]*100,
         montly[[st]][m,"TAM"]- normals[[st]][m,"TAM"],
         pch=st,col=m)
  print("RR")
  print((montly[[st]][m,"RR"]- normals[[st]][m,"RR"])/normals[[st]][m,"RR"]*100)
  print("TA")
  print(montly[[st]][m,"TAM"]- normals[[st]][m,"TAM"])
}
}
abline(h=0)
abline(v=0)
legend("topright",legend=mnts,col=1:3, pch=1)
dev.off()




#########
#domain
ZZ <- matrix(zs,111,111)
ZZ[which(ZZ<2)] <- NA
txtcol="black"
pdf("figures/other/exDomain.pdf",heigh=4,width=4)
#par(pin=c(3,3),xpd=T)
#plot(NA,xlim=c(0,1),ylim=c(0,1),xlab="",ylab="",main="Experimental domain")
#topo()
image.plot(ZZ,col=two.colors(50,"darkgreen","brown","yellow"),zlim=c(2,2600))
par(xpd=T)
points(stList$xx/111,stList$yy/111, col=txtcol, pch=19,cex=1.2)
par(xpd=T)
text(stList$xx/111,stList$yy/111,labels=stList$stations,col=txtcol,pos=4,srt=-25,offset=0.5)
open("surfex_files/FORCING.nc")
I  <- ncvar_get(ncid, ncid$var$LON)
J  <- ncvar_get(ncid, ncid$var$LAT)
nc_close(ncid)
print("done")


stations <- c("Kise"  , "Dagali", "Kongsberg", "Blomskog", "Notodden", "Flisa" , "Ørnhaugen", "Aurskog")
stnr     <- c(12550   ,   29720 ,    28380   ,   240800  ,    30650  ,     6020,     7420   ,     2650 )
lons     <- c(10.8055 ,  8.5263 ,   9.6377   ,  12.0779  ,   9.2105  ,  12.0125,    11.4992 ,  11.5801 )
lats     <- c(60.7733 , 60.4188 ,  59.6247   ,  59.2219  ,  59.5667  ,  60.6141,    61.3763 ,  59.9119 )
xx       <- numeric(length(stations))
yy       <- numeric(length(stations))

nstat <- length(stations)

stList <- data.frame(stations,stnr,lons,lats, xx, yy,stringsAsFactors=F)


m <- matrix(1:length(I),111,111)
for (st in 1:nstat){
  bij <- fnn_lamb(I,J,stList[st,"lons"],stList[st,"lats"])$ij_out
  ob <- which(m == bij, arr.ind=T)
  stList$xx[st] <- ob[1]
  stList$yy[st] <- ob[2]
}

### Manual correction ###

stList$xx[1] <- 74                   # Kise is water 
stList$yy[4] <- stList$yy[4] + 1     # Blomskog is water


