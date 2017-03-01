
read_ANAL_INC <- function(res_path){

  dirs <- list.dirs(res_path,recursive=F)
  ndir <- length(dirs)

  analysis  <- array(dim=c(ndir,7,8)) 
  increment <- array(dim=c(ndir,7,8))
  obs       <- array(dim=c(ndir,8))
  obserr    <- array(dim=c(ndir,8))
  innov     <- array(dim=c(ndir,8))
  Ho        <- array(dim=c(ndir,7,8))
  Kg        <- array(dim=c(ndir,7,8))
  time <- array(dim=ndir)
  for (idir in 1:ndir){
    dtg <- substring(dirs[idir], nchar(dirs[idir])-9,nchar(dirs[idir]))
    tmp1 <- read.table(paste(dirs[idir],"/ANAL_INCR.0000001",sep=""))
	tmp2 <- read.table(paste(dirs[idir],"/OBSout.0000001",sep=""))
	tmp3 <- read.table(paste(dirs[idir],"/OBSERRORout.0000001",sep=""))
	tmp4 <- read.table(paste(dirs[idir],"/INNOV.0000001",sep=""))
	innov[idir,] <- as.matrix(tmp4)
	obserr[idir,] <- as.matrix(tmp3)
	obs[idir,] <- as.matrix(tmp2)
	time[idir] <- as.numeric(dtg)
    for (v in 1:7){
      analysis[idir,8-v,] <- tmp1[,v]
      increment[idir,8-v,]<- tmp1[,v+7]
      tmp5 <- read.table(paste(dirs[idir],"/HO_WG",v,"_v1",sep=""))
	  Ho[idir,v,] <- tmp5[,1]
      Kg[idir,v,] <- tmp5[,2]
    }
  }
  obs[which(obs > 900)] <- NA
  anaNinc <- list(time=time,
                  ana=analysis,
                  inc=increment,
				  obs=obs,
				  obserr=obserr,
				  innov=innov,
				  Ho=Ho,
				  Kg=Kg)
  return(anaNinc)
}

read_SURFEX_RESULT <- function(res_path){

  dirs <- list.dirs(res_path,recursive=F)
  ndir <- length(dirs)
  forecast  <- array(dim=c(ndir,7,8))
  analysis  <- array(dim=c(ndir,7,8))
  increment <- array(dim=c(ndir,7,8))
  obs       <- array(dim=c(ndir,8))
  obserr    <- array(dim=c(ndir,8))
  innov     <- array(dim=c(ndir,8))
  exp_obs   <- array(dim=c(ndir,8))
  obs_out   <- array(dim=c(ndir,8))
#  Ho        <- array(dim=c(ndir,7,8))
  Kg        <- array(dim=c(ndir,7,8))
  time      <- array(dim=ndir)

  for (idir in 1:ndir){
    dtg <- substring(dirs[idir], nchar(dirs[idir])-9,nchar(dirs[idir]))
    tmp1 <- read.table(paste(dirs[idir],"/SURFEX_RESULTS",sep=""),skip=1)
    tmp2 <- read.table(paste(dirs[idir],"/OBSout.0000001",sep=""))
    tmp3 <- read.table(paste(dirs[idir],"/OBSERRORout.0000001",sep=""))
    obserr[idir,] <- as.matrix(tmp3)
    obs[idir,] <- as.matrix(tmp2)
    time[idir] <- as.numeric(dtg)
    exp_obs[idir,] <- tmp1[,8]
	obs_out[idir,] <- tmp1[,9]
	for (v in 1:7){
	  forecast[idir,8-v,] <- tmp1[,v]
      analysis[idir,8-v,] <- tmp1[,v+9]
      increment[idir,8-v,]<- tmp1[,v+16]
    }
  }
  obs[which(obs > 900)] <- NA
  innov <- obs_out - exp_obs
  for  (v in 1:7){
    Kg[,v,] <- increment[,v,]/innov[,]
  }
  anaNinc <- list(time=time,
                  fcst=forecast,
				  ana=analysis,
                  inc=increment,
                  obs=obs,
                  obserr=obserr,
                  innov=innov,
				  exp_obs=exp_obs,
				  obs_out=obs_out,
                  Kg=Kg)
  return(anaNinc)

}


readNVE <- function(file){
  tmp <- read.table(file,skip=1,sep=";",stringsAsFactors=F)
  time <- format(as.POSIXlt(tmp$V1[which(tmp$V2 == -10)]),format="%Y%m%d%H")
  d10 <- tmp$V3[which(tmp$V2 == -10)]
  d20 <- tmp$V3[which(tmp$V2 == -20)]
  d30 <- tmp$V3[which(tmp$V2 == -30)]
  d40 <- tmp$V3[which(tmp$V2 == -40)]
  d60 <- tmp$V3[which(tmp$V2 == -60)]
  d100 <- tmp$V3[which(tmp$V2 == -100)]

  d10[which(d10 < 0  | d10 > 100)] <- NA
  d20[which(d20 < 0  | d20 > 100)] <- NA
  d30[which(d30 < 0  | d30 > 100)] <- NA
  d40[which(d40 < 0  | d40 > 100)] <- NA
  d60[which(d60 < 0  | d60 > 100)] <- NA
  d100[which(d100 < 0  | d100 > 100)] <- NA


  nvedat <- list( time=time, 
                  d10=d10,
				  d20=d20,
				  d30=d30,
				  d40=d40,
				  d60=d60,
				  d100=d100 )
}

mm2perc <- function(x){
  # scale (min,max) -> (0, 100)
  xx <- (x-min(x,na.rm=T))*100/(max(x,na.rm=T)-min(x,na.rm=T))
  return(xx)
}


swi2sm <- function(SWI, wp=0.213320, fc=0.356612){
  SM <- 0.01*SWI*(fc-wp) + wp
  return(SM)
}

sm2swi <- function(SM, wp=0.213320, fc=0.356612){
  SWI = 100*(SM - wp) / (fc - wp)
  return(SWI)
}

maxMin2fcwp <- function(SWI, wp=0.213320, fc=0.356612){
  swi <- 0.01*SWI
  ma <- max(swi, na.rm=T)
  mi <- min(swi, na.rm=T)
  SM <- (swi-mi)*(fc-wp)/(ma-mi) + wp
  return(SM)
}




##########################################################################

################### PLOT #################################################
#plot(ol$ana[,iwg,pnt],type='l', ylim=c(0,1) )
#lines(ekf$ana[,iwg,pnt],col="red")
#points(ekf$obs[,pnt],pch=20,cex=0.4,col="blue")
#abline(h=mean(ekf$obs[,pnt], na.rm=T))


rm_null <- function(x){
  x[which(x == 0)] <- NA
  return(x)
}

BOXPLOT <- function(x,...,rm_null=T){
  if (rm_null){
    y <- rm_null(x)
  }
  boxplot(y,...)
}

make_plots <- function(CASE, pnt, path){

  ekf    <- CASE$ekf
  enkf   <- CASE$enkf
  ol     <- CASE$ol
  ol_ens <- CASE$ol_ens

  ######     BOXPLOTS     ######
  ######                  ######
  ## KALMAN GAIN

  pdf(paste(path,"/ekf_box_Kgain.pdf",sep=""))
  x <- rm_null(apply(ekf$Kg[,,pnt], 2, as.numeric))
  boxplot(rm_null(x), ylim=c(-0.002,0.033))
  xmeans <- colMeans(x,na.rm=T)
  points(xmeans, pch=5)
  title(main="sekf Kalman gain matrix",
        xlab="Soil layer")
  dev.off()
  
  pdf(paste(path,"/enkf_box_Kgain.pdf",sep=""))
  x <- apply(enkf$Kg[,,pnt], 2, as.numeric)
  boxplot(rm_null(x),ylim=c(-0.2,0.3))
  xmeans <- colMeans(x,na.rm=T)
  points(xmeans, pch=5)
  title(main="enkf Kalman gain matrix",
        xlab="Soil layer")
  dev.off()


  ## Jacobian

  pdf(paste(path,"/ekf_box_jac.pdf",sep=""))
  x <- apply(ekf$Ho[,,pnt], 2, as.numeric)
  boxplot(rm_null(x), ylim=c(-0.1,0.95))
  xmeans <- colMeans(x,na.rm=T)
  points(xmeans, pch=5)
  title(main="sekf Jacobian H matrix",
        xlab="Soil layer")
  dev.off()


  ## INCREMENT
  pdf(paste(path,"/ekf_box_increment.pdf",sep=""))
  x <- apply(ekf$inc[,,pnt], 2, as.numeric)
  boxplot(rm_null(x),ylim=c(-0.0012,0.0032))
  xmeans <- colMeans(x,na.rm=T)
  points(xmeans, pch=5)
  title(main="sekf Increment",
        xlab="Soil layer")
  dev.off()
  
  pdf(paste(path,"/enkf_box_increment.pdf",sep=""))
  x <- apply(enkf$inc[,,pnt], 2, as.numeric)
  boxplot(rm_null(x),ylim=c(-0.02,0.02))
  xmeans <- colMeans(x,na.rm=T)
  points(xmeans, pch=5)
  title(main="enkf Increment",
        xlab="Soil layer")
  dev.off()
  
  
  ## ANALYSIS
  for (i in pnt){
    tmp <- sprintf("%s/ekf_box_SM_p%d.pdf", path,i)
    pdf(tmp)
    boxplot(ekf$ana[,,i],ylim=c(0.1,0.4))
    title(main="sekf Soil moisture",
        xlab="Soil layer")
    dev.off()
    tmp <- sprintf("%s/enkf_box_SM_p%d.pdf", path,i)
    pdf(tmp)
    boxplot(enkf$ana[,,i],ylim=c(0.1,0.4))
    title(main="enkf Soil moisture",
        xlab="Soil layer")
    dev.off()
  }
  
  ######    SCATTER PLOT    #######
  ######                    #######
  
  ## GAIN vs ANALYSIS
  pdf(paste(path,"/ekf_scat_K_SM.pdf",sep=""))
  plot(rm_null(ekf$Kg[,2,pnt]),ekf$ana[,2,pnt],
       xlab="", ylab="",
  	 pch=20)
  title(main="sekf K gain vs Soil moisture",
        xlab="Kalman gain",
  	  ylab="soil moisture")
  dev.off()
  
  pdf(paste(path,"/enkf_scat_K_SM.pdf",sep=""))
  plot(rm_null(enkf$Kg[,2,pnt]),enkf$ana[,2,pnt],
       xlab="", ylab="",
     pch=20)
  title(main="enkf K gain vs Soil moisture",
        xlab="Kalman gain",
      ylab="soil moisture")
  dev.off()

#  pdf(paste(path,"/enkf_2_scat_K_SM.pdf")
#  plot(rm_null(enkf$Kg[,2,pnt]),enkf_2$ana[,2,pnt],
#       xlab="", ylab="",
#     pch=20)
#  title(main="enkf K gain vs Soil moisture",
#        xlab="Kalman gain",
#      ylab="soil moisture")
#  dev.off()


  
  ## INC vs ANALYSIS
  pdf(paste(path,"/ekf_scat_inc_SM.pdf",sep=""))
  plot(rm_null(ekf$inc[,2,pnt]),ekf$ana[,2,pnt],
       xlab="", ylab="",
  	 pch=20)
  title(main="sekf increment vs Soil moisture",
        xlab="increment",
  	        ylab="soil moisture")
  dev.off()
  
  pdf(paste(path,"/enkf_scat_inc_SM.pdf",sep=""))
  plot(rm_null(enkf$inc[,2,pnt]),enkf$ana[,2,pnt],
       xlab="", ylab="",
  	      pch=20)
  title(main="enkf increment vs Soil moisture",
        xlab="increment",
  	              ylab="soil moisture")
  dev.off()
  
  pdf(paste(path,"/scat_enkf_ekf_SM.pdf",sep=""))
  plot(enkf$ana[,2,pnt],ekf$ana[,2,pnt],
       xlab="", ylab="",
       pch=20)
  abline(c(0,0), c(1,1))
  title(main="soil moisture",
        xlab="sekf",
        ylab="enkf")
  dev.off()
  

  
  ######     TIMESERIES     #######
  #####                    #######

# timeseries of enkf, sekf, openloop and obs

  tmp <- pnt
  for (pnt in tmp){
    pdf(paste(path,"/timeserie_p",pnt,".pdf",sep=""))
    colo=rainbow(5)
  
    plot(NA, xlim=c(0, 700),ylim=c(0.15,0.45),xlab="hours/6", ylab="soil moisture")
    lines(enkf$ana[,2,pnt],col=colo[3])
    lines(ol$ana[,2,pnt],col=colo[2])
    lines(ol_ens$ana[,2,pnt],col=colo[1])
    lines(ekf$ana[,2,pnt],col=colo[4])
    points(enkf$obs[,pnt],cex=0.5,col=colo[5])
    legend("topleft", legend=c("openloop_ens", "openloop", "enkf_5", "ekf","SMOS"),
         fill=colo)
  #}
    dev.off()
  }
  

#  pdf(paste(path,"/timeserie2.pdf",sep=""))
#  plot(NA, xlim=c(0, 700),ylim=c(0.15,0.45),xlab="hours/6", ylab="soil moisture")
#  lines(enkf$ana[,2,pnt],col=colo[1])
#  lines(ol_ens$ana[,2,pnt],col=colo[2])
#  lines(ol_ens$ana[,2,pnt],col=colo[3])
#  lines(enkf$ana[,2,pnt],col=colo[4])
#  points(enkf$obs[,pnt],cex=0.5,col=colo[5])
#  legend("topleft", legend=c("enkf01", "openloop01", "openloop001", "enkf001","SMOS"),
#         fill=colo)
#  #}
#  dev.off()

}
###################################################################################################

# ol <- read_ANAL_INC("/disk1/asmundb/RESULTS/pre9feb17/normSMOS2/openloop/ANALYSIS")
# ekf<- read_ANAL_INC("/disk1/asmundb/RESULTS/pre9feb17/normSMOS2/ekf/ANALYSIS")

# ol_ens <- read_SURFEX_RESULT("/disk1/asmundb/RESULTS/pre9feb17/normSMOS2/openloop_ens_5/ANALYSIS/")
# enkf <- read_SURFEX_RESULT("/disk1/asmundb/RESULTS/pre9feb17/normSMOS2/enkf_5/ANALYSIS/")

# ol_ens_2 <- read_SURFEX_RESULT("/disk1/asmundb/RESULTS/pre9feb17/smaller_pert/openloop_ens_5/ANALYSIS/")
# enkf_2 <- read_SURFEX_RESULT("/disk1/asmundb/RESULTS/pre9feb17/smaller_pert/enkf_5/ANALYSIS/")
# 


 ol <- read_ANAL_INC("/disk1/asmundb/RESULTS/B_sensTest/CASE1/openloop/ANALYSIS")
 ekf<- read_ANAL_INC("/disk1/asmundb/RESULTS/B_sensTest/CASE1/ekf/ANALYSIS")
 ol_ens <- read_SURFEX_RESULT("/disk1/asmundb/RESULTS/B_sensTest/CASE1/openloop_ens_5/ANALYSIS/")
 enkf <- read_SURFEX_RESULT("/disk1/asmundb/RESULTS/B_sensTest/CASE1/enkf_5/ANALYSIS/")
 
 CASE1 <- list( ol=ol, ol_ens=ol_ens, ekf=ekf, enkf=enkf)



 ol <- read_ANAL_INC("/disk1/asmundb/RESULTS/B_sensTest/CASE2/openloop/ANALYSIS")
 ekf<- read_ANAL_INC("/disk1/asmundb/RESULTS/B_sensTest/CASE2/ekf/ANALYSIS")
 ol_ens <- read_SURFEX_RESULT("/disk1/asmundb/RESULTS/B_sensTest/CASE2/openloop_ens_5/ANALYSIS/")
 enkf <- read_SURFEX_RESULT("/disk1/asmundb/RESULTS/B_sensTest/CASE2/enkf_5/ANALYSIS/")

 CASE2 <- list( ol=ol, ol_ens=ol_ens, ekf=ekf, enkf=enkf)

 # evolving B matrix initial CASE3
 ekf<- read_ANAL_INC("/disk1/asmundb/RESULTS/B_sensTest/CASE3/ekf/ANALYSIS")
 ol_ens <- read_SURFEX_RESULT("/disk1/asmundb/RESULTS/B_sensTest/CASE3/openloop_ens_5/ANALYSIS/")
 enkf <- read_SURFEX_RESULT("/disk1/asmundb/RESULTS/B_sensTest/CASE3/enkf_5/ANALYSIS/")


 CASE3 <- list( ol=ol, ol_ens=ol_ens, ekf=ekf, enkf=enkf)

 # evolving B matrix initial CASE2
 ekf<- read_ANAL_INC("/disk1/asmundb/RESULTS/B_sensTest/CASE4/ekf/ANALYSIS")

 CASE4 <- list( ol=ol, ol_ens=ol_ens, ekf=ekf, enkf=enkf)

obs <- readNVE("2.727_Kise.csv")

wp=0.213320
fc=0.356612


nveobs <- 0.01*swi2sm(mm2perc(obs$d10), 6, 48)

nveobs2 <- 0.01*swi2sm(mm2perc(obs$d10[seq(which(obs$time == "2016050106"), which(obs$time == "2016101700"),by=6)]), 6, 48)
nveobs <- 0.01*swi2sm(mm2perc(obs$d100[seq(which(obs$time == "2016050106"), which(obs$time == "2016101700"),by=6)]), 6, 48)


SMOS_raw <- readRDS("dat/obs/WG2.obs.dat")
#SMOS_raw <- SMOS_raw[,3]
SMOS <- SMOS_raw[which(as.numeric(dimnames(SMOS_raw)[[1]]) == 2016050106) : which(as.numeric(dimnames(SMOS_raw)[[1]]) == 2016101618),3]

SMOS[which(SMOS > 1)] <- NA

 
source("functions.R")

SMOS2 <- linReScale(SMOS, nveobs)

#plot(maxMin2fcwp(obs$d10[seq(which(obs$time == "2016050106"), which(obs$time == "2016101700"),by=6)]),type='l',
#            ylim=c(0.15,0.41))


#pdf("figures/B_sensTest/CASE2/timeserie1.pdf")
#plot(nveobs[seq(which(obs$time == "2016050106"), which(obs$time == "2016101700"),by=6)],type='l',
#            ylim=c(0.1,0.4),
#			ylab="Volumetric soil moisture", xlab="time",
#			main="CASE2")
#lines(nveobs2,col="orange")
#points(seq(1,length(SMOS)*2,by=2), SMOS, col="darkgreen", pch=20)
#lines(seq(1,length(SMOS)*2,by=2), SMOS, col="darkgreen")
#lines(which(! is.na(ekf$obs[,3])),  ekf$obs[which(! is.na(ekf$obs[,3])),3],col="green")
#lines(seq(1,length(SMOS2)*2,by=2), SMOS2, col="blue")


#lines(ekf$ana[,3,3],col="red")
#lines(ol$ana[,3,3],col="darkred")
#lines(enkf$ana[,3,3],col="blue")
#lines(ol_ens$ana[,3,3],col="darkblue")
#lines(c(0,700),rep(fc,2),col="red", lty=2)
#lines(c(0,700),rep(wp,2),col="red", lty=2)
#lines(c(0,700),rep(0.06,2), lty=2)
#lines(c(0,700),rep(0.48,2), lty=2)

#lgnd <- c( "in-situ 10cm", "SMOS", "SMOS-rescale openloop","SMOS-rescale insitu",
#           "SURFEX-openloop 6cm", "model field capacity - wilting point","nve porocity-wilting point")
#lty <-  c( 1, 1, 1, 1, 1, 2, 2)
#colors<- c("black", "darkgreen", "green", "blue", "darkred", "red", "black") 
#
#lgnd <- c( "in-situ 10cm", "SMOS-rescale openloop",
#           "SURFEX-openloop 6cm", )
#lty <-  c( 1, 1, 1, 1, 1, 2, 2)
#colors<- c("black", "green", "green", "blue", "darkred", "red", "black")

#legend("topright", inset=c(0,0), legend=lgnd, lty=lty, col=colors)

#dev.off()

#cor(nveobs2, ekf$obs[,3],use="na")
#cor(nveobs2[seq(1,length(SMOS2)*2,by=2)], SMOS2,use="na")


#make_plots(3)

#x_sigma <- NA*1:7
#vall <- array(dim=c(dim(ol$ana)[1]*8, 7))
#for (i in 1:7){
#  x_sigma[i] <- sd(as.numeric(ol$ana[,i,]))
#  vall[,i] <- as.numeric(ol$ana[,i,])
#}


#pdf("figures/B_sensTest/openloop_sd.pdf")
#plot(x_sigma,type='o', ylim=c(0,0.01) xlab="Ground Layer", ylab="Standard deviation")


##################################################################
################## Save plotting functions #######################


plot1 <- function(v,pnt=3){
  col = rainbow(9)
  if (v <=3){
    nveobs <- 0.01*swi2sm(mm2perc(obs$d10[seq(which(obs$time == "2016050106"), which(obs$time == "2016101700"),by=6)]), 6, 48)
  }else if (v == 4){
	 nveobs <- 0.01*swi2sm(mm2perc(obs$d20[seq(which(obs$time == "2016050106"), which(obs$time == "2016101700"),by=6)]), 6, 48)
  }else if (v == 5){
	 nveobs <- 0.01*swi2sm(mm2perc(obs$d40[seq(which(obs$time == "2016050106"), which(obs$time == "2016101700"),by=6)]), 6, 48)
  }else if (v == 6){
	 nveobs <- 0.01*swi2sm(mm2perc(obs$d60[seq(which(obs$time == "2016050106"), which(obs$time == "2016101700"),by=6)]), 6, 48)
  }else{
     nveobs <- 0.01*swi2sm(mm2perc(obs$d100[seq(which(obs$time == "2016050106"), which(obs$time == "2016101700"),by=6)]), 6, 48)
  }
  plot(nveobs,type='l', col=col[1 ])
  lines(CASE1$ekf$ana[,v,pnt], col=col[2])
  lines(CASE2$ekf$ana[,v,pnt], col=col[3])
  lines(CASE4$ekf$ana[,v,pnt], col=col[4])
  lines(CASE2$enkf$ana[,v,pnt],col=col[5])
  lines(CASE3$enkf$ana[,v,pnt],col=col[6])
  lines(CASE3$enk$ana[,v,pnt],col=col[7])
  lines(CASE2$ol$ana[,v,pnt], col=col[8])
  lines(CASE1$ol_ens$ana[,v,pnt],col=col[9])
  if (v <= 3){
    points(CASE1$ekf$obs[,pnt],pch=20)
  }
  legend("topleft", legend=c("nve","CASE1 sekf", "CASE2 sekf", "CASE4 ekf", "CASE2 enkf",
                              "CASE3 enkf", "CASE3 ekf","open loop", "open loop ens"),
  col=col, lty=1)
}
