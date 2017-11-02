require(fields)

load("RDS_files/2016.VARS")

### Spatial ACF ###
# season average #
wg_mean <- apply(prog_ol$WG2,1:2,mean,na.rm=T)

mlag <- 50
x <- array(NA,dim=c((mlag),111,2))
for(i in 1:111){
    x[,i,1] <- acf(wg_mean[,i],plot=F,lag.max=(mlag-1),na.action=na.pass)$acf
    x[,i,2] <- acf(wg_mean[i,],plot=F,lag.max=(mlag-1),na.action=na.pass)$acf
}

lag <- (0:49)*2.5

pdf("figures/ACF/spatial_mean_acf.pdf")
# 20 km
plot(lag,lag,type='n',ylim=c(-0.6,1),ylab="Autocorrelation",xlab="Distance km",main="Spatial autocorrelation")
for ( i in 1:111){
  lines(lag,x[,i,1],col="red", lwd=0.3)
  lines(lag,x[,i,2],col="blue",lwd=0.3)
}

acfMean <- apply(x,1,mean,na.rm=T)
lines(lag,acfMean,lwd=3)
legend("topright", legend=c("east-west", "north-south","average"),lty=1,col=c("red","blue","black"),lwd=c(0.3,0.3,3))

dev.off()



pdf("figures/ACF/spatial_mean_box_acf.pdf")
boxplot(t(cbind(x[,,2],x[,,1])),xaxt='n',xlab="Distance km",ylab="Autocorrelation", main="Spatial autocorrelation")
axis(1,at=seq(1,50,length=7), labels=lag[seq(1,49,length=7)])
dev.off()


# daily average #
wg_d_mean <- array(NA, dim=c(111,111,92))
k = 1
for (i in 1:92){
  wg_d_mean[,,i] <- apply(prog_ol$WG2[,,k:(k+23)],1:2,mean,na.rm=T)
  k = k+24
}


mlag <- 50
x <- array(NA,dim=c((mlag),111*92,2))
for (j in 1:92){
  for (i in 1:111){
    x[,i,1] <- acf(wg_d_mean[,i,j],plot=F,lag.max=(mlag-1),na.action=na.pass)$acf
    x[,i,2] <- acf(wg_d_mean[i,,j],plot=F,lag.max=(mlag-1),na.action=na.pass)$acf
  }
}

lag <- (0:49)*2.5

pdf("figures/ACF/spatial_daily_mean_acf.pdf")
# 17.5 km
plot(lag,lag,type='n',ylim=c(-0.6,1),ylab="Autocorrelation",xlab="Distance km",main="Spatial autocorrelation, daily mean")
for ( i in 1:111){
  lines(lag,x[,i,1],col="red", lwd=0.3)
  lines(lag,x[,i,2],col="blue",lwd=0.3)
}

acfMean <- apply(x,1,mean,na.rm=T)
lines(lag,acfMean,lwd=3)
legend("topright", legend=c("east-west", "north-south","average"),lty=1,col=c("red","blue","black"),lwd=c(0.3,0.3,3))

dev.off()



### Temporal ACF ###

mlag <- 200
y <- matrix(NA,mlag,12321)
k = 1
for (i in 1:111){
  for (j in 1:111){
    y[,k] <- acf(prog_ol$WG2[j,i,],plot=F,lag.max=(mlag-1),na.action=na.pass)$acf
    k <- k+1
  }
}

lag <- (0:(mlag-1))

pdf("figures/ACF/temporal_acf.pdf")
# 70 hours
plot(lag,lag,type='n',ylim=c(-0.6,1),ylab="Autocorrelation",xlab="Time hours",main="Temporal autocorrelation")
for ( i in 1:12321){
  lines(lag,y[,i], lwd=0.1)
}
acfMean <- apply(y,1,mean,na.rm=T)
lines(acfMean,lwd=3,col="red")
dev.off()


pdf("figures/ACF/temporal_box_acf.pdf")
boxplot(t(y[,]),xaxt='n',xlab="Time hours",ylab="Autocorrelation", main="Temporal autocorrelation")
axis(1,at=seq(1,mlag,length=7), labels=lag[seq(1,mlag-1,length=7)])
dev.off()


### Anomaly ###
mlag <- 200
k = 1
ac <- matrix(NA,mlag,12321)
for (j in 1:111){
  for (i in 1:111){
    x      <- prog_ol$WG2[i,j,]
    xsmud  <- MM(prog_ol$WG2[i,j,],24*15)
    xanomaly  <- x-xsmud
    ac[,k] <- acf(xanomaly,plot=F,na.action=na.pass,lag.max=mlag-1)$acf
    k = k+1
  }
}

lag <- (0:(mlag-1))

pdf("figures/ACF/temporal_acf_anomaly.pdf")
# 45 hours
plot(lag,lag,type='n',ylim=c(-0.6,1),ylab="Autocorrelation",xlab="Time hours",main="Temporal autocorrelation, anomaly")
for ( i in 1:12321){
  lines(lag,ac[,i], lwd=0.1)
}
acfMean <- apply(ac,1,mean,na.rm=T)
lines(acfMean,lwd=3,col="red")
dev.off()



#### Scaling Aanalysis ####


x <- apply(prog_ol$WG2,1:2, mean,na.rm=T)
x[which(is.na(x))] <- mean(x,na.rm=T)


rescale <- function(x,f){
  nx <- dim(x)[1]
  ny <- dim(x)[2]

  nx2 <- nx%/%f
  ny2 <- ny%/%f
  x2 <- matrix(NA, nx2,ny2)
  for(i in 1:nx2){
    for(j in 1:ny2){
      x2[j,i] <- mean(x[((j-1)*f+1):(j*f), ((i-1)*f+1):(i*f)],na.rm=T)
    }
  }
  return(x2)
} 


Is <- numeric(20)

for (i in 1:20){
  xn <- rescale(x,i)
  Is[i] <- SAC(xn,1)
}



