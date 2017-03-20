#source("make_OBSERVATIONS.R")




ascend <- seq(1, to=dim(dataA)[1]*2, by=2)
descend<- seq(2, to=dim(dataA)[1]*2, by=2)

data <- array(dim=c(dim(dataA)[1]*2,dim(dataA)[2]))
data[ascend,] <- dataA
data[descend,]<- dataD

stations<- read_stlist(stationlist="stationlist.cfg")



for ( pnt in 1:dim(data)[2]){
  pdf(sprintf("figures/SMOS_obs/SMOS_total_%d.pdf", pnt))
  plot(NA, xlim=c(0, dim(data)[1]), ylim=c(0,0.6),
       main=sprintf("%s",dimnames(stations)[[1]][pnt]),
       xlab="time index",
       ylab="Soil moisture")
  points(ascend, dataA[,pnt], col="blue", pch=20,cex=0.5)
  points(descend, dataD[,pnt], col="green",  pch=20,cex=0.5)
  lines(MM(data[,pnt], 5))
  lines(ascend, MM(dataA[,pnt],5), col="blue")
  lines(descend,MM(dataD[,pnt],5), col="green")
  legend("topright", legend=c("total","ascending","descending"), fill=c("black","blue","green"))
  dev.off()
}
