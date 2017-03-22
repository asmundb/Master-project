source("functions.R")

# load smos from nc
#source("make_OBSERVATIONS.R")

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

swi2sm <- function(SWI, wp=0.213320, fc=0.356612){
  SM <- 0.01*SWI*(fc-wp) + wp
  return(SM)
}

mm2perc <- function(x){
  # scale (min,max) -> (0, 100)
  xx <- (x-min(x,na.rm=T))*100/(max(x,na.rm=T)-min(x,na.rm=T))
  return(xx)
}

#########################################################################################

times <- seq( as.POSIXlt("2016-05-01 06:00"), by=3600*12, len=400)

yyyymmddhh <- format(times, format="%Y%m%d%H")

### NVE obs at kise ###
obs <- readNVE("2.727_Kise.csv")

nveobs <- 0.01*swi2sm(mm2perc(obs$d10[which(obs$time %in% yyyymmddhh)]), 6, 48)



### SMOS ###

#ascend <- seq(1, to=dim(dataA)[1]*2, by=2)
#descend<- seq(2, to=dim(dataA)[1]*2, by=2)
#
#data <- array(dim=c(dim(dataA)[1]*2,dim(dataA)[2]))
#data[ascend,] <- dataA
#data[descend,]<- dataD

data <- readRDS("SMOS.rds")



### stations ### 
stations<- read_stlist(stationlist="stationlist.cfg")


#########################################################
##### Re-scale SMOS data to in-situ  #####

smos_norm <- linReScale(data[,3], nveobs)



#for ( pnt in 1:dim(data)[2]){
#  pdf(sprintf("figures/SMOS_obs/SMOS_total_%d.pdf", pnt))
#  plot(NA, xlim=c(0, dim(data)[1]), ylim=c(0,0.6),
#       main=sprintf("%s",dimnames(stations)[[1]][pnt]),
#       xlab="time index",
#       ylab="Soil moisture")
#  points(ascend, dataA[,pnt], col="blue", pch=20,cex=0.5)
#  points(descend, dataD[,pnt], col="green",  pch=20,cex=0.5)
#  lines(MM(data[,pnt], 5))
#  lines(ascend, MM(dataA[,pnt],5), col="blue")
#  lines(descend,MM(dataD[,pnt],5), col="green")
#  legend("topright", legend=c("total","ascending","descending"), fill=c("black","blue","green"))
#  dev.off()
#}
