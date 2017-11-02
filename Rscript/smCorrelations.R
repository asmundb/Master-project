#source("read_smos_smap_ol.R")
source("chi2.R")

heatMap <- function(obs, ...){
  require(fields)
  nobs <- nObs(obs)
  nobs[which(nobs == 0)] <- NA
  main <- substitute(obs)
  image.plot(nobs,col=two.colors(200,"lightblue","orange","blue"),main=main,zlim=c(0,184), ...)
  topo()
}

nObs <- function(obs){
  nobs <- apply(!is.na(obs),1:2,sum)
  return(nobs)
}


smap <- loadSODA("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMAP/SEKF/obs038/")
smos <- loadSODA("/lustre/storeA/users/asmundb/surfex/RESULTS/2016/SMOS/SEKF/obs05/")



# kise 74 102 : no smap


nsmos <- nObs(smos$yo[,,,1])
nsmap <- nObs(smap$yo[,,,1])

which.max(nsmos*nsmap)
which.max(nsmos+nsmap)
which.max(nsmos)
which.max(nsmap)


which(nsmos == nsmos[7804],arr.ind=T)

ana_times <- seq(1,length=92*2,by=2)
ana_at    <- seq(6,length=length(ana_times),by=12)


### Flest observasjoner fra begge satellitter ###
xx <- 39
yy <- 88

# håpls korrelasjon

### best correlasjon mellom satellittene ###
xx <- 51
yy <- 75

#ingen punkter

xx <- 103
yy <- 90

###
ol <- prog_ol$WG2[xx,yy,ana_at]
smos1 <- smos$yo[xx,yy,ana_times,1]
smap1 <- smap$yo[xx,yy,ana_times,1]

smos2 <- smos$xa[xx,yy,ana_times,2]
smap2 <- smap$xa[xx,yy,ana_times,2]


plot(ol, type="l",ylim=c(0.1,0.4))
lines(smos1,col="blue")
lines(smap1,col="red")


