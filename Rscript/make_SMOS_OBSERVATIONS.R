#get_obs <- function(files){
#  nfiles<- length(files)
#  obs <- array(dim=c(12321, nfiles))
#  for (i in 1:nfiles){
#    obs[,i] <- read.table(file=files[i])[,1]
#  }
#  obs[which(obs==999)] <- NA
#  obs_m <- array(NA, dim=c(111,111,nfiles))
#  for (i in 1:nfiles){
#    obs_m[,,i] <- matrix(obs[,i],111,111)
#  }
#  return(obs_m)
#}

#path <- "/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS"
#files <- list.files(path=path, full.names=T)
#files <- files[507:690]
#SMOS      <- get_obs(files)

source("read_SMOS_nc.R")

path <- "/lustre/storeB/users/asmundb/SMOS/nc/"
files <- list.files(path, recursive=T, full.names=T,pattern="SM_OPER.*")[c(54:145,254:345)]

smos <- loadSMOSTimeserie(files)


## screening ##
a <-  numeric(5) # count observations after screening
a[1] <- sum(! is.na(smos$sm))

# only dynamic range
minimum <- 0.074 # about the smallest value of wilting point
maximum <- 0.46  # about the largest value of wsat
smos$sm[which(smos$sm < minimum | smos$sm > maximum)] <- NA
a[2] <- sum(! is.na(smos$sm))

#allow std below 0.2
smos$sm[which(smos$sm_dqx > 0.2)] <- NA
a[3] <- sum(! is.na(smos$sm))

# remove vals with rfi_prob > 0.3
smos$sm[which(smos$rfi_prob > 0.3)] <- NA
a[4] <- sum(! is.na(smos$sm))

# if following flags set then remove
source("sciFlags.R")
FL_mask <- flags$FL_Topo_S | flags$FL_Urban_High | flags$FL_OW | flags$FL_Sea_Ice | flags$FL_Coast | flags$FL_TEC
smos$sm[FL_mask] <- NA
a[5] <- sum(! is.na(smos$sm))



SM <- array(NA, dim=c(111,111,2,92))
#SM[,,1,] <- SMOS[,,seq(1,by=2,length=92)]
#SM[,,2,] <- SMOS[,,seq(2,by=2,length=92)]
SM[,,1,] <- smos$sm[,,seq(1,by=2,length=92)]
SM[,,2,] <- smos$sm[,,seq(2,by=2,length=92)]

## Remove data that occur more than enough times (450)
#
tab <- table(SM)
SM2 <- SM
rmables <- which(SM2 %in% names(tab)[which(tab > 450)])
SM2[rmables] <- NA
#tab2 <- table(SM2)

nObs <- function(obs){
  nobs <- apply(!is.na(obs),1:2,sum)
  return(nobs)
}


#SM3 <- SM
#rmables <- which(SM3  < 0.002)
#SM3[rmables] <- NA
#tab3 <- table(SM3)


pdf("figures/SATevaluation/hist1_SMOS.pdf")
par(mfrow=c(2,1))
hist(SM,main="raw data",breaks=50)
hist(SM2,main="modified",breaks=50)
dev.off()

pdf("figures/SATevaluation/mean_SMAP_raw.pdf")
image.plot(apply(SM,1:2,mean,na.rm=T),main="Time averaged soil moisture",col=rev(tim.colors()),zlim=c(0,0.43))
topo()
dev.off()

pdf("figures/SATevaluation/mean_SMAP_clean.pdf")
image.plot(apply(SM2,1:2,mean,na.rm=T),main="Time averaged soil moisture",col=rev(tim.colors()),zlim=c(0,0.43))
topo()
dev.off()


### NEW ###

pdf("figures/SATevaluation/hist1_smos_raw.pdf",width=2.5,height=1.5)
par(mar=c(2,2,1,1))
hist(SM,main="raw SMOS", breaks=50,col="black",border="white")

dev.off()

pdf("figures/SATevaluation/hist1_smos_mod.pdf",width=2.5,height=1.5)
par(mar=c(2,2,1,1))
hist(SM2,main="modified SMOS",breaks=50,col="black",border="white")
dev.off()


pdf("figures/SATevaluation/mean_SMOS_raw.pdf",width=2.5,height=2)
par(mar=c(2,2,1,0.6))
image.plot(apply(SM,1:2,mean,na.rm=T),main="SMOS mean raw",col=rev(tim.colors()),legend.width=0.5,legend.mar=4,zlim=c(0,0.6))
topo()
dev.off()

pdf("figures/SATevaluation/mean_SMOS_clean.pdf", width=2.5,height=2)
par(mar=c(2,2,1,0.6))
image.plot(apply(SM2,1:2,mean,na.rm=T),main="SMOS mean mod",col=rev(tim.colors()),legend.width=0.5,legend.mar=4,zlim=c(0,0.6))
topo()
dev.off()

### WEN ###


plot_flag <- function(x,y,pass=1){
  plot(SM[x,y,pass,],type='n')
  text(SM[x,y,pass,],labels=QF[x,y,pass,],cex=0.8)
}






#"OBSERVATIONS_160801H06.DAT"
outpath <- "/lustre/storeB/users/asmundb/SMAP/OBSERVATIONS/"

write_obs <- function(SM,files,outpath){
  nfiles <- length(files)
  for (i in 1:nfiles){
  
    yymmdd   <- gsub('.*SM_P_20(.*)_R.*','\\1',files[i])
    outfile1 <- sprintf("%sOBSERVATIONS_%sH06.DAT", outpath, yymmdd)
    outfile2 <- sprintf("%sOBSERVATIONS_%sH18.DAT", outpath, yymmdd)
  
    sm1 <- as.numeric(SM[,,1,i])
    sm2 <- as.numeric(SM[,,2,i])
  
    sm1[which(is.na(sm1))] <- 999
    sm2[which(is.na(sm2))] <- 999

    write(sm1, outfile1, ncolumns=1) #write to file
    write(sm2, outfile2, ncolumns=1)
    print(yymmdd)
  }
  return(0)
}

### open loop

path  <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/SPINUP/ISBA"
files <- list.files(path=path,pattern="ISBA_PROGNOSTIC",recursive=T, full.names=T)
nfiles <- length(files)

files <- files[125:nfiles]
nfiles <- length(files)

time <- seq(as.POSIXlt("2014-06-01 01:00"), as.POSIXlt("2014-09-01 00:00"), by=3600)
Atime <- grep("06:00",as.character(time))
Dtime <- grep("18:00",as.character(time))

require(ncdf4)
var <- array(dim=c(111,111,6*nfiles))
k=1
for (i in 1:nfiles){
  ncid <- nc_open(files[i])
  tmp <- ncvar_get(ncid, ncid$var$WG2)
  nc_close(ncid)
  var[,,k:(k+5)] <- tmp
  k <- k+6
}

mod1 <- var[,,Atime]
mod2 <- var[,,Dtime]


lin_rescale <- function(obs,mod){

  mask <- nObs(obs)>15

  for (i in 1:dim(obs)[3]){
    obs[,,i] <- obs[,,i]*mask
  }
  obs[which(obs == 0)] <- NA
  
  mod[which(is.na(obs))] <- NA

  mod_mean <- apply(mod,1:2,mean,na.rm=T)
  mod_sd   <- apply(mod,1:2,sd,na.rm=T)

  obs_mean <- apply(obs,1:2,mean,na.rm=T)
  obs_sd   <- apply(obs,1:2,sd,na.rm=T) 

  ntimes  <- dim(obs)[3]
  obs_new <- array(NA, dim=c(111,111,ntimes))
  
  print(ntimes)

  for(i in 1:ntimes){
    obs_new[,,i] = (obs[,,i] - obs_mean)*(mod_sd/obs_sd) + mod_mean
  }
  return(obs_new)
}

log_rescale <- function(obs,mod){
  obs_new <- exp(lin_rescale(log(obs),log(mod)))
  return(obs_new)
}


#### TEST ####
pdf("figures/obs_rescaling/smap_rescale.pdf")
par(mfrow=c(2,2))
hist(SM2[,,1,],breaks=100,main="SMAP raw")
hist(mod1,xlim=c(0,0.4),breaks=100,main="SURFEX open loop")
hist(lin_rescale(SM2[,,1,],mod1),xlim=c(0,0.4),breaks=100,main="Linear rescaling")
hist(log_rescale(SM2[,,1,],mod1),xlim=c(0,0.4),breaks=100,main="log-transformed rescaling")
dev.off()





SM_norm <- SM2
SM_norm[,,1,] <- lin_rescale(SM2[,,1,],mod1)
SM_norm[,,2,] <- lin_rescale(SM2[,,2,],mod2)
SM_norm[which(SM_norm < 0 )] <- NA

files <- list.files("/lustre/storeB/users/asmundb/SMAP/n5eil01u.ecs.nsidc.org/SMAP/SPL3SMP.004/",
                    pattern="SMAP_L3_SM_P", full.names=T, recursive=T)
nfiles <- length(files)

outpath <- "/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS_norm/"
write_obs(SM_norm,files, outpath)


### verify ###

fls <- list.files("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS_norm/",full.names=T)
n   <- length(fls)

obs <- matrix(NA,111*111,n)
for (i in 1:n){
  obs[,i] <- read.table(fls[i])[,1]
}

obs[which(obs == 999)] <- NA
### OK"





plot_norm_ts <- function(x,y){
  plot(SM[x,y,1,],ylim=c(-0.5,1))
  lines(var[x,y,seq(6,by=12,length=92)])
  points(obs_new1[x,y,],pch=3)
}

pdf("figures/SATevaluation/problem.pdf")

col <- two.colors(10, "lightblue", "darkblue","blue")

par(mfrow=c(2,2))

image.plot(apply(obs_new1<0,1:2,sum,na.rm=T),zlim=c(1,10),col=col,main="smap_am < 0")
topo()

image.plot(apply(obs_new2<0,1:2,sum,na.rm=T),zlim=c(1,10),col=col,main="smap_pm < 0")
topo()

image.plot(obs_sd,col=two.colors(25,"red","blue"),main="smap_sd",zlim=c(0,0.2))
topo()

image.plot(mod_sd,col=two.colors(25,"red","blue"), main="mod_sd",zlim=c(0,0.2))
topo()
dev.off()



###############################

sat_mean <- 0.5
sat_sd   <- 0.6 

mod_mean <- 0.15
mod_sd   <- 0.2

n <- 10000

sat <- rnorm(n=n,mean=sat_mean,sd=sat_sd)
sat[which(sat<0)] <- NA

mod <- rnorm(n=n,mean=mod_mean,sd=mod_sd)
mod[which(mod<0)] <- NA

lin_norm <- function(obs,mod){
  obs_mean <- mean(obs,na.rm=T)
  obs_sd   <- sd(obs,na.rm=T)
  mod_mean <- mean(mod,na.rm=T)
  mod_sd   <- sd(mod,na.rm=T)
  obs_new  <- (obs-obs_mean)*(mod_sd/obs_sd) + mod_mean
  return(obs_new)
}

log_norm <- function(obs,mod){
  lobs <- log(obs)
  lmod <- log(mod)
  lobs_new <- lin_norm(lobs,lmod)
  obs_new <- exp(lobs_new)
  return(obs_new)
}

plot(mod,type='l',ylim=c(0,1))
points(sat,col="red")
points(lin_norm(sat,mod),col="green")
points(log_norm(sat,mod),col="blue")




