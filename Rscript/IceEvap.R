require(ncdf4)
require(fields)


# Load evap variable from diagnostics file

time <- seq(as.POSIXlt("2016-10-06 01:00"), as.POSIXlt("2016-10-17 00:00"), by=3600)

loadISBA <- function(varname,files){
  nfiles <- length(files)
  nvar   <-  length(varname)
  ntimes <- nfiles*6
  k <- 1
  tmp <- array(NA, dim=c(111,111,nvar,ntimes))
  for (i in 1:nfiles){
    cat(i,"/",nfiles,"\r")
    ncid <- nc_open(files[i])
    for (ivar in 1:nvar){
      tmp[,,ivar,k:(k+5)] <- ncvar_get(ncid, ncid$var[[varname[ivar]]])
    }
    k <- k+6
    nc_close(ncid)
    flush.console()
  }
  cat("\n")
  isba <- list()
  for (ivar in 1:nvar){
    isba[[varname[ivar]]] <- tmp[,,ivar,]
  }
  return(isba)
}


path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/OPENLOOP/ISBA/"

files <- list.files(path=path, pattern="ISBA_DIAGNOSTICS.OUT.nc", recursive=T,full.names=T)
varname <- c("H_ISBA","LE_ISBA","LEI_ISBA","RN_ISBA","EVAP_ISBA","SUBL_ISBA","RN_ISBA","RAINF_ISBA","GFLUX_ISBA","PSN_ISBA")
diag <- loadISBA(varname, files)

# Load WGI1 and WG1 from prognostic file

files <- list.files(path=path, pattern="ISBA_PROGNOSTIC.OUT.nc", recursive=T,full.names=T)
varname <- c("WGI1","WGI2","WG1","WG2","TG1","TG2")
prog <- loadISBA(varname, files)

# ice portion
rho_i <- prog$WGI1/(prog$WGI1+prog$WG1)

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


movie <- function(x){
  zlim <- c(min(x,na.rm=T),max(x,na.rm=T))
  ntimes <- dim(x)[3]
  for (i in 1:ntimes){
    cat(i,"/",ntimes,"\r")
    png(sprintf("tmp/%04d.png",i))
    image.plot(x[,,i],zlim=zlim, main=as.character(time[i]))
    dev.off()
    flush.console()
  }
  cat("\n")
}




movie4 <- function(x1,x2,x3,x4){

  zlim1 <- c(min(x1,na.rm=T),max(x1,na.rm=T))
  zlim2 <- c(min(x2,na.rm=T),max(x2,na.rm=T))
  zlim3 <- c(min(x3,na.rm=T),max(x3,na.rm=T))
  zlim4 <- c(min(x4,na.rm=T),max(x4,na.rm=T))

  title1 <- deparse(substitute(x1))
  title2 <- deparse(substitute(x2))
  title3 <- deparse(substitute(x3))
  title4 <- deparse(substitute(x4))

  ntimes <- dim(x1)[3]
  for (i in 1:ntimes){
    cat(i,"/",ntimes,"\r")
    png(sprintf("tmp/%04d.png",i))
    par(mfrow=c(2,2))
    image.plot(x1[,,i],zlim=zlim1, main=paste(title1, as.character(time[i])))
    image.plot(x2[,,i],zlim=zlim2, main=title2)
    image.plot(x3[,,i],zlim=zlim3, main=title3)
    image.plot(x4[,,i],zlim=zlim4, main=title4)
    dev.off()
    flush.console()
  }
  cat("\n")
}


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


mat_mm <- function(var,n=10){
  x <- array(NA,dim=dim(var))
  for (i in 1:111){
    for (j in 1:111){
      x[j,i,] <- MM(var[j,i,],n)
    }
  }
  return(x)
}


topo <- function(levels=c(0,2,10,50,100,200,500,1000)){
  contour(matrix(zs,111,111), add=T, levels=levels)
}




#############################
wgi_mean <- apply(prog$WGI1,c(1,2), mean,na.rm=T)
evap_mean <- apply(diag$EVAP_ISBA, c(1,2), mean, na.rm=T)
rn_mean <- apply(diag$RN_ISBA, c(1,2), mean, na.rm=T)
tg1_mean <- apply(prog$TGI1,c(1,2), mean,na.rm=T)
wg1_mean <- apply(prog$WG1,c(1,2), mean,na.rm=T)

n <- 12

wgi1_mm <- mat_mm(prog$WGI1,n)
wgi2_mm <- mat_mm(prog$WGI2,n)
evap_mm <- mat_mm(diag$EVAP_ISBA,n)
rn_mm <- mat_mm(diag$RN_ISBA,n)

movie4(wgi1_mm, wgi2_mm, evap_mm, diag$RN_ISBA)


x <- prog$WGI1/(prog$WGI1 + prog$WG1)

ntimes <- dim(x)[3]
# percent ice coverage in domain
ip <- array(0, dim=ntimes)
for (i in 1:ntimes){
  ip[i] <- sum((x[,,i] > 0.1),na.rm=T)/length(x[,,i])*100
}

evap_sum <- apply(diag$EVAP_ISBA[,,which(ip > 1)], c(1,2), sum,na.rm=T)
wgi1_sum <- apply(prog$WGI1[,,which(ip > 1)], c(1,2), sum,na.rm=T)

x_mean <- apply(x, c(1,2), mean, na.rm=T)


par(mfrow=c(2,2))
image.plot(evap_sum)
image.plot(wgi1_sum)



# scatter plot

wg1 <- as.numeric(prog$WG1)
wgi1<- as.numeric(prog$WGI1)
tg1 <- as.numeric(prog$TG1)
evap <- as.numeric(diag$EVAP_ISBA)
rn <- as.numeric(diag$RN_ISBA)
h <- as.numeric(diag$H_ISBA)
le <- as.numeric(diag$LE_ISBA)

pdf("figures/iceEvap/scatter1.pdf")
par(mfrow=c(2,2))
plot(wg1,evap) 
plot(wgi1,evap)
plot(tg1,evap)
plot(rn,evap)
dev.off()



############
t1 <- which(as.character(time) == "2016-10-09 00:00:00")

clouds <- array(FALSE, dim=c(111,111,ntimes))
for (i in 1:ntimes){
  clouds[,,i] <- (cf_arome[,,i] >= 0.95)
}

evap_clear <- array(0,dim=c(111,111))
evap_cloud <- array(0,dim=c(111,111))

kk <- apply(clouds, c(1,2), sum)

for (i in 1:ntimes){
  evap_clear <- evap_clear + diag$EVAP_ISBA[,,i]*(!clouds[,,i])
  evap_cloud <- evap_cloud + diag$EVAP_ISBA[,,i]*(clouds[,,i])
}



pdf("figures/iceEvap/RN_ISBA_mean.pdf")
image.plot(diag$RN_ISBA,main="Averaged net radiation OFFLINE")
dev.off()



# Height corrected

hc <- 6.5*zs_mat/1000
tg_hc <- apply(prog$TG1,1:2,mean,na.rm=T)+hc

pdf("figures/iceEvap/hc_tg_rn.pdf")
par(mfrow=c(2,2))
image.plot(apply(prog$TG1,1:2,mean,na.rm=T),main="TG1")
topo()
image.plot(tg_hc, main="TG1 + 6.5*zs/1000")
topo()
image.plot(apply(diag$RN_ISBA, 1:2, mean,na.rm=T),main="Net radiation")
topo()
image.plot(apply(cf_arome,1:2, mean,na.rm=T),main="cloud cover")
topo()
dev.off()



## Correlations
pdf("figures/iceEvap/correlations_evap_tg_wg.pdf")
par(mfrow=c(2,2))
image.plot(mat_cor(prog$TG1*prog$WG1,diag$EVAP_ISBA),main="cor(TG1*WG1,EVAP_ISBA)")
image.plot(mat_cor(prog$TG1,diag$EVAP_ISBA),main="cor(TG1,EVAP_ISBA)")
image.plot(mat_cor(prog$WG1,diag$EVAP_ISBA),main="cor(WG1,EVAP_ISBA)")
image.plot(mat_cor(prog$WG1,prog$TG1),main="cor(WG1,TG1)")
dev.off()
