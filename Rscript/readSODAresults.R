library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools)
gpclibPermit()
library(mapproj)
library(ncdf4)
library(fields)

### Forcing.nc ###
#
#  contains lon, lat, and zs (topography)
#

filename <- "surfex_files/FORCING.nc"
ncid <- nc_open(filename)
lon  <- ncvar_get(ncid, ncid$var$LON)
lat  <- ncvar_get(ncid, ncid$var$LAT)
zs   <- ncvar_get(ncid, ncid$var$ZS)
nc_close(ncid)

### PGD.nc ###
# 
#  contains frac_nature (points which is assimilated
#59.950516, 10.803464
#

filename <- "surfex_files/PGD_2D.nc"
ncid <- nc_open(filename)
frac_nature <- ncvar_get(ncid, ncid$var$FRAC_NATURE)
nx <- ncid$dim$xx$len
ny <- ncid$dim$yy$len
nc_close(ncid)

### PREP.nc ###

filename <- "surfex_files/PREP_SODA.nc"
ncid <- nc_open(filename)
wwilt1 <- ncvar_get(ncid, ncid$var$WWILT1)
wfc1   <- ncvar_get(ncid, ncid$var$WFC1)
nc_close(ncid)

### AROME-METCOOP ###

filename <- "surfex_files/AROME_MetCoOp_00_sfx.nc_20161010"
ncid <- nc_open(filename)
lon_arome <- ncvar_get(ncid, ncid$var$longitude)
lat_arome <- ncvar_get(ncid, ncid$var$latitude)
wg1_arome <- ncvar_get(ncid, ncid$var$WG1)
wgi1_arome <- ncvar_get(ncid, ncid$var$WGI1)
nc_close(ncid)



lonlat0 <- arrayInd(which.min( (lat_arome - min(lat))^2 + (lon_arome - min(lon))^2 ), dim(lat_arome))
lons <- seq(lonlat0[1], by=1, length.out=111)
lats <- seq(lonlat0[2], by=1, length.out=111)
wg1_arome_mygrid <- wg1_arome[lons,lats,]
#rm(wg1_arome)

source("ffunctions.R")
ij <- numeric(length(lon))
dist <- numeric(length(lon))

for ( k in 1:length(lon)){
  x <- fnn_lamb(array(lon_arome), array(lat_arome), lon[k], lat[k])
  ij[k]   <- x$ij_out
  dist[k] <- x$dist[x$ij_out]
}
wg1_arome_mygrid2 <- array(dim=c(111,111,dim(wg1_arome)[3]))
for (i in 1:dim(wg1_arome)[3]){
  wg1_arome_mygrid2[,,i] <- matrix(array(wg1_arome[,,i])[ij], nx, ny)
}

wgi1_arome_mygrid2 <- array(dim=c(111,111,dim(wgi1_arome)[3]))
for (i in 1:dim(wgi1_arome)[3]){
  wgi1_arome_mygrid2[,,i] <- matrix(array(wgi1_arome[,,i])[ij], nx, ny)
}



###########################################################
########## FUNCTIONS ######################################


load001 <- function(path, filename,nx=111,ny=111, maxVal=999){
# load all [filename] in path
#
  files <- list.files(path=path,
                    pattern=filename,
                    recursive=T,
                    full.name=T)
  nfiles <- length(files)
  tmp <- read.table(files[1])
  var    <- array(dim=c(nx,ny,nfiles,dim(tmp)[2]))
  for (i in 1:nfiles){
    tmp <- as.matrix(read.table(files[i]))
    var[,,i,] <- maskNreshape(tmp, frac_nature)
  }
  var[which(var >= maxVal)] <- NA
  return(var)
}

loadSMOS <- function(path, nx=111, ny=111){
# load all OBSERVATIONS.DAT in path
# 
  files <- list.files(path=path,
                      pattern="OBSERVATIONS",
                      full.name=T)
  nfiles <- length(files)
  var <- array(dim=c(nx,ny,nfiles))
  for (i in 1:nfiles){
    tmp <- as.matrix(read.table(files[i]))
    var[,,i] <- matrix(tmp,nx,ny)
  }
  var[which(var >= 999)] <- NA
  return(var)
}

### Reshaping ###

maskNreshape <- function(x, fn){
# reshape array(dim=nx*ny) to array(dim=(nx,ny))
# and distribute to nature points
# 
  nx <- dim(fn)[1]
  ny <- dim(fn)[2]
  nl <- dim(x)[2]
  nat_mask <- array(fn)
  var <- array(dim=c(nx*ny,nl))
  var[which(nat_mask != 0),] <- x
  
  return( array(var,dim=c(nx,ny,nl)) )
}

### rescaling ###
# with field capacity and wiltingpoint
#
#SMOS_min <- apply(SMOS, c(1,2), min, na.rm=T)
#SMOS_min[which(is.infinite(SMOS_min))] <- NA
#SMOS_max <- apply(SMOS, c(1,2), max, na.rm=T)
#SMOS_max[which(is.infinite(SMOS_max))] <- NA
#
#obs_new <- array(dim=c(111,111,20))
#for (i in 1:20){
#  obs_new[,,i] <- (SMOS[,,i] - SMOS_min)*(wfc1-wwilt1)/(SMOS_max-SMOS_min)
#}


### Visualisation ###


movieMap <- function(var,by=1,fps=10, save=F, title=""){
# plot as movie
# 
  d3 <-  dim(var)[3]
  if (length(title) != d3 ){
    title <- paste("frame", 1:d3)
  }

  if (save) {
    system("mkdir tmp")
    k <- seq(1,d3, by=by)
    for (i in 1:length(k)){
      png(sprintf("tmp/tmp_%06d.png",i))
      image.plot(var[,,k[i]], 
                 main=title[k[i]],
                 zlim=c(min(var,na.rm=T),max(var, na.rm=T)),
                 col=rev(tim.colors()))
      dev.off()
    }
    system(sprintf("avconv -r %d -start_number 0 -i tmp/tmp_\\%06d.png -b:v 1000k out.mp4", fps))
    system("rm -r tmp")
  } else {  
    for (i in seq(1,dim(var)[3], by=by)){
      image.plot(var[,,i], main=i, zlim=c(min(var,na.rm=T),max(var, na.rm=T)),
                 col=rev(tim.colors()))
      Sys.sleep(1/fps)
    }
  }

}

plotMap <- function(lon2d, lat2d, var, ...){
# Plot using ggplot2
#
  dat <- data.frame(lon2d,lat2d,var)

  argnames <- names(list(...))
  if (!("xlim" %in% argnames)){
    xlim <- c(min(lon2d),max(lon2d))
  }else{
    xlim <- list(...)[["xlim"]]
  }
  if (!("ylim" %in% argnames)){
    ylim <- c(min(lat2d),max(lat2d))
  }else{
    ylim <- list(...)[["ylim"]]
#59.950516, 10.803464
#59.950516, 10.803464
  }

  # Get the map outlines
  outlines <- as.data.frame(map("world", plot = FALSE,
                              xlim = c(min(lon2d), max(lon2d)),
                              ylim = c(min(lat2d), max(lat2d)))[c("x","y")])
  worldmap <-geom_path(aes(x, y), inherit.aes = FALSE,
                       data = outlines, alpha = 0.8, show.legend = T)

  # The layer for the observed variable
  zsmap <- geom_point(aes(x=lon2d, y=lat2d, colour=var), data=dat, shape=15, size=10)

  # Prjoection
  projection <- coord_map(projection="lambert", lat0=57.5, lat1=57.5, orientation=c(90,0,10.1),
                          xlim=xlim, ylim=ylim)

  # Colour
  colors <- scale_colour_continuous(breaks=c(250,300,350))

  # Plot map
  ggplot() + zsmap + worldmap + projection #+ colors
}

makePDF <- function(var,time){
  for (i in 1:dim(var)[3]){
    varname <- deparse(substitute(var))
#    print(varname)
    varname <- gsub("[^[:alnum:]]","",varname)
#    print(varname)
    filename <- sprintf("figures/20170323/%s_%02d.pdf", varname, i)
#    print(filename)
    pdf(filename)
    image.plot(var[,,i], zlim=c(min(var,na.rm=T), max(var,na.rm=T)), main=time[i])
    contour(matrix(zs, 111,111), add=T, levels=c(0,2,10,50,100,200,500,1000))
    dev.off()
  }
  system(sprintf("pdfunite figures/20170323/%s_* figures/20170323/%s.pdf", varname,varname))
  system(sprintf("rm figures/20170323/%s_*", varname))
}


###########################################################

### Load data ###
path="/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/SEKF/ANALYSIS/"

ana    <- load001(path, "ANAL_INCR")
xf     <- ana[,,,1:7]
inc    <- ana[,,,8:14]
rm(ana)
obsout <- load001(path, "OBSout")
innov  <- load001(path, "INNOV")


sekf <- list( xf=xf,
              inc=inc,
              obsout=obsout,
              innov=innov)




SMOS   <- loadSMOS("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS")[,,3:18]
SMres  <- loadSMOS("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS_rescaled")[,,3:18]


  

stop()
### visualisation ###


# static
image.plot(frac_nature, col=two.colors(n=256,start="blue", end="darkgreen", middle="orange"))
contour(matrix(zs, 111,111), add=T, levels=c(0,2,10,50,100,200,500,1000))

# time 
time <- seq(as.POSIXlt("2016-10-07 06:00"),length=16, by=3600*12)

#time <- 1
#slr  <- 6
# soda increments
#image.plot(inc[,,time,slr])

# soil moisture
#image.plot(xf[,,time,slr])

# innovation
#image.plot(innov[,,time,1])

# obsout
#image.plot(obsout[,,time,1])

 makePDF(SMOS,time)
 makePDF(sekf$obsout[,,seq(1,31,by=2),1],time)
 makePDF(sekf$inc[,,seq(1,31,by=2),6],time)
 makePDF(sekf$xf[,,seq(1,31,by=2),6],time)
 makePDF(sekf$innov[,,seq(1,31,by=2),1],time)
 makePDF(SMres,time)
 makePDF(sekf$xf[,,seq(1,31,by=2),6]-ol[,,seq(1,31,by=2),6],time)
 makePDF(sekf$xf[,,seq(1,31,by=2),5]-ol[,,seq(1,31,by=2),5],time)
 makePDF(sekf$xf[,,seq(1,31,by=2),7]-ol[,,seq(1,31,by=2),7],time)
 
 makeODF(wg1_arome_mygrid2[,,c(6,18)], arome_time[c(6,18)])


 pdf("wg1_arome_101006.pdf")
 image.plot(wg1_arome_mygrid2[,,6]+wgi1_arome_mygrid2[,,6], zlim=c(0.1,0.43), main="wg1+wgi1 AROME-MetCoOp 2016-10-10 06:00")
 contour(matrix(zs, 111,111), add=T, levels=c(0,2,10,50,100,200,500,1000))
 dev.off()

 pdf("wg1_101006.pdf")
 image.plot(sm_tot_ol[,,78], zlim=c(.1,0.43), main="wg1+wgi1 SURFEX offline openloop 2016-10-10 06:00")
 contour(matrix(zs, 111,111), add=T, levels=c(0,2,10,50,100,200,500,1000))
 dev.off()


 pdf("wg1_sekf_101006.pdf")
 image.plot(sm_tot_sekf[,,78], zlim=c(.1,0.43), main="wg1+wgi1 SURFEX offline sekf 2016-10-10 06:00")
 contour(matrix(zs, 111,111), add=T, levels=c(0,2,10,50,100,200,500,1000))
 dev.off()




