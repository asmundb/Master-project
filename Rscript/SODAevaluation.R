source("readSODAresults.R")

smos <- loadSODA(path="/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMOS/SEKF/obs05_2/ANALYSIS/")
#smap <- loadSODA(path="/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs038_2/ANALYSIS/")


plot(apply(smos$inc[,,,1],3,mean,na.rm=T),type='l')





x <- apply(smos$xf,3:4,mean,na.rm=T)


f <- fft(x)




### Find dry and wet episodes ###

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


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.05, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


mm <- array(NA, dim(111,111,368))
for (j in 1:111){
  for (i in 1:111){
    mm[i,j,] <- MM(smap$xa[i,j,,2],3*4)
  }
}



### K-means clustering ###

sm <- smap$xa[,,,2]
smclean <- sm[which(!is.na(sm))]

kk <- kmeans(smclean,7)
clus <- sm
cent <- sm
clus[which(!is.na(clus))] <- kk$cluster


cldry <- which.min(kk$center)
clwet <- which.max(kk$center)

dry <- which(clus==cldry)
wet <- which(clus==clwet)


###
obsMask <- which(is.na(smap$yo[,,,1]))



Hdry <- matrix(NA,length(dry),7)
Hwet <- matrix(NA,length(wet),7)

for (i in 1:7){
  tmp <- smapHO$H[i,,,]
  tmp[obsMask] <- NA
  Hdry[,i] <- tmp[dry]
  Hwet[,i] <- tmp[wet]
}


pdf("figures/2016/wet_dry_Hbox.pdf",width=2.5,height=2.5)
par(mar=c(2,2,1,1))
boxplot(Hwet,ylim=c(-0.1,0.9),boxwex=0.3,at=1:7+0.2,xaxt="n", outline=F,main="Jacobian")
boxplot(Hdry,add=T,col="red",boxwex=0.3,at=1:7-0.2,xaxt="n",outline=F)
axis(1,at=1:7,labels=1:7)
legend("topright",legend=c("Wet", "dry"), fill=c("white","red"))
dev.off()


pdf("figures/2016/wet_Hbox.pdf",width=2.5,height=2.5)
par(mar=c(2,2,1,1))
boxplot(Hwet,ylim=c(-0.1,0.9), outline=T,main="Wet")
dev.off()


pdf("figures/2016/dry_Hbox.pdf",width=2.5,height=2.5)
par(mar=c(2,2,1,1))
boxplot(Hdry,ylim=c(-0.1,0.9), outline=T,main="Dry")
dev.off()

