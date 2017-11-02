require(h5)


files <- list.files("/lustre/storeB/users/asmundb/SMAP/n5eil01u.ecs.nsidc.org/SMAP/SPL3SMP.004/",
                    pattern="SMAP_L3_SM_P", full.names=T, recursive=T)
files <- files[1:(length(files)-1)]
nfiles <- length(files)


source("readh5.R")


SM <- array(NA, dim=c(111,111,2,nfiles))
QF <- SM

for (i in 1:nfiles){
  x <- readh5(files[i])
  SM[,,1,i] <- x$SM_AM
  SM[,,2,i] <- x$SM_PM
  QF[,,1,i] <- x$QF_AM
  QF[,,2,i] <- x$QF_PM

}

# Remove data that occurs more than enough times (1000)
tab <- table(SM)
SM2 <- SM
rmables <- which(SM2 %in% names(tab)[which(tab > 1000)])
SM2[rmables] <- NA
tab2 <- table(SM2)

SM2 <- SM2[,,,1:92]


nObs <- function(obs){
  nobs <- apply(!is.na(obs),1:2,sum)
  return(nobs)
}


pdf("figures/SATevaluation/hist1_smap_raw.pdf",width=2.5,height=1.5)
par(mar=c(2,2,1,1))
hist(SM,main="raw SMAP", breaks=50,col="black",border="white")
dev.off()

pdf("figures/SATevaluation/hist1_smap_mod.pdf",width=2.5,height=1.5)
par(mar=c(2,2,1,1))
hist(SM2,main="modified SMAP",breaks=50,col="black",border="white")
dev.off()


pdf("figures/SATevaluation/mean_SMAP_raw.pdf",width=2.5,height=2)
par(mar=c(2,2,1,0.6))
image.plot(apply(SM,1:2,mean,na.rm=T),main="SMAP mean raw",col=rev(tim.colors()),legend.width=0.5,legend.mar=4,zlim=c(0,0.6))
topo()
dev.off()

pdf("figures/SATevaluation/mean_SMAP_clean.pdf", width=2.5,height=2)
par(mar=c(2,2,1,0.6))
image.plot(apply(SM2,1:2,mean,na.rm=T),main="SMAP mean mod",col=rev(tim.colors()),legend.width=0.5,legend.mar=4,zlim=c(0,0.6))
topo()
dev.off()





plot_flag <- function(x,y,pass=1){
  plot(SM[x,y,pass,],type='n')
  text(SM[x,y,pass,],labels=QF[x,y,pass,],cex=0.8)
}



### NOT RECOMMENDED FLAG  ###
rm_notrec <- function(sm,qf){

  flags <- array(NA, dim=c(dim(sm)[1], dim(sm)[2], dim(sm)[3], 6))
  for (i in 1:dim(sm)[3]){
    for (j in 1:111){
      for (k in 1:111){
        tmp <- intToBits(qf[k,j,i])
        flags[k,j,i,] <- as.integer(tmp[1:6])
      }
    }
  }
  return(flags)
}

flags <- rm_notrec(SM[,,1,],QF[,,1,])
sm <- array(NA, dim=c(111,111,92))
for (k in 1:92){
  for (j in 1:111){
    for (i in 1:111){
      if (flags[i,j,k,1] == 0){
        sm[i,j,k] <- SM[i,j,1,k]
      } else {
        sm[i,j,k] <- NA
      }
    }
  }
}
flags_pm <- flags <- rm_notrec(SM[,,2,],QF[,,2,])
sm_pm <- sm <- array(NA, dim=c(111,111,92))
for (k in 1:92){
  for (j in 1:111){
    for (i in 1:111){
      if (flags_pm[i,j,k,1] == 0){
        sm_pm[i,j,k] <- SM[i,j,2,k]
      } else {
        sm_pm[i,j,k] <- NA
      }
    }
  }
}


smm <- array(NA, dim=c(111,111,184))
k <- 1
for (i in 1:92){
  smm[,,k] <- sm[,,i]
  smm[,,k+1] <- sm_pm[,,i]
  k <- k+2
}

recpercent <- sum(!is.na(smm))/length(smm)*100

heatMap(smm,cutOff=0,main="Recommended SMAP obs")
nobs <- apply(!is.na(smm),1:2,sum)


pdf("figures/SATevaluation/recommended_smap.pdf",width=2.5,height=2)
par(mar=c(2,2,1,0.5))
image.plot(nobs,
           col=two.colors(200,"lightblue","orange","blue"),
           zlim=c(1,184),
           legend.width=0.5,
           legend.mar=3.5,
           main="Recommended SMAP")
topo()
dev.off()


##############################




#"OBSERVATIONS_160801H06.DAT"
outpath <- "/lustre/storeB/users/asmundb/SMAP/OBSERVATIONS/"

write_obs <- function(SM,files,outpath){
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

require(ncdf4)
isba_path  <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/SPINUP/ISBA"
isba_files <- list.files(path=isba_path,pattern="ISBA_PROGNOSTIC",recursive=T, full.names=T)
isba_nfiles <- length(isba_files)

isba_files <- isba_files[125:isba_nfiles]
isba_nfiles <- length(isba_files)

time <- seq(as.POSIXlt("2014-06-01 01:00"), as.POSIXlt("2014-09-01 00:00"), by=3600)
Atime <- grep("06:00",as.character(time))
Dtime <- grep("18:00",as.character(time))

var <- array(dim=c(111,111,6*isba_nfiles))
k=1
for (i in 1:isba_nfiles){
  ncid <- nc_open(isba_files[i])
  tmp <- ncvar_get(ncid, ncid$var$WG2)
  nc_close(ncid)
  var[,,k:(k+5)] <- tmp
  k <- k+6
}

mod1 <- var[,,Atime]
mod2 <- var[,,Dtime]


lin_rescale <- function(obs,mod){

  mask <- nObs(obs) >= 15

  for (i in 1:dim(obs)[3]){
    obs[,,i] <- obs[,,i]*mask
    mod[,,i] <- mod[,,i]*mask
  }
  obs[which(obs == 0)] <- NA
  mod[which(mod == 0)] <- NA
  
  
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

outpath <- "/lustre/storeB/users/asmundb/SMAP/OBSERVATIONS_norm/"
write_obs(SM_norm,files, outpath)


### verify ###

fls <- list.files("/lustre/storeB/users/asmundb/SMAP/OBSERVATIONS_norm/",full.names=T)
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




