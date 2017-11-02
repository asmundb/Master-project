require(h5)
require(fields)
require(ncdf4)

############################################################

# rotate grid to surfex like
rot <- function(x) t(apply(x, 2, rev))

# select points inside my grid
ncid <- nc_open("/lustre/storeB/users/asmundb/surfex/FORCING/domain/FORCING.nc_2016100700")
I  <- ncvar_get(ncid, ncid$var$LON)
J  <- ncvar_get(ncid, ncid$var$LAT)
#zs   <- get_ncvar(ncid, ncid$var$ZS)
nc_close(ncid)


getMyGrid <- function(PASS,dmax=0.2){
  npoints <- length(I)
  ij <- numeric(npoints)
  for (i in 1:npoints){
    d <- (PASS$lon-I[i])^2 + (PASS$lat-J[i])^2
    if (min(d) > dmax){
      ij[i] <- NA
    }else{
      ij[i] <- which.min(d)
    }
  }
  return(ij)
}


# extract soil moisture values within my grid
mySM <- function(ij,sm){
  npoints <- length(ij)
  x <- numeric(npoints)
  for (i in 1:npoints){
    if (is.na(ij[i])){
      x[i] <- NA
    }else{
      x[i] <- sm[ij[i]]
    }
  }
  return(matrix(x,111,111))
}

# 
readDS <- function(f,gr,dat,ext,res=36){
  if (res == 36){
    lonInt=500:520
    latInt=375:383
  }else if (res == 3){
    lonInt=6000:6200
    latInt=4500:4600
  }else if (res == 9){
    lonInt=2000:2080
    latInt=1500:1540
  }else{
    print("unknown dataset")
  }
  ds <- paste(gr,dat,ext,sep="")
#  print(range(lonInt))
#  print(range(latInt))
  x <- rot(readDataSet(f[ds]))[lonInt,latInt]
  return(x)
}


getDataSet <- function(f, pass="AM",res=36){
  if (res==36){
    gr <- sprintf("/Soil_Moisture_Retrieval_Data_%s",pass)
  }else if(res<=10){
    gr <- "/Soil_Moisture_Retrieval_Data"
  }else{
    print("unknown format")
  }
  ext <- ""
  if (pass == "PM"){ext="_pm"}
#  cat(paste(gr,"*",ext,res, sep="/"),"\n")
#  cat("reading lon\n")
  lon <- readDS(f,gr,"/longitude",ext,res)
#  cat("reading lat\n")
  lat <- readDS(f,gr,"/latitude",ext,res)
#  cat("reading sm\n")
  sm  <- readDS(f,gr,"/soil_moisture",ext,res)
  sm[which(sm < 0)] <- NA
  
  qf <- readDS(f,gr,"/retrieval_qual_flag",ext,res)
#  sm_error <- readDS(f,gr,"/soil_moisutre_error",ext,res)

  x <- list( lon=lon, lat=lat, sm=sm,qf=qf)#,sm_e=sm_error)
  return(x)
}

# read h5 file
readh5 <- function(file){
  f <- h5file(file) #"../h5/SMAP_L3_SM_P_20160701_R14010_001.h5")
 
#  if (length(grep("SM_A_",file)) == 1){
#    res = 3
#    cat("reading file with resolution: ", res,"km...\n")
#3    AM <- getDataSet(f,"AM",res)
#    cat("converting to my grid...\n")
#    SM_AM <- mySM(getMyGrid(AM,0.02),AM$sm)
#    out <- array(NA,dim=c(111,111,1))
#    out[,,1] <- SM_AM
#  }else if(length(grep("SM_P_",file)) == 1){
    res=36
    cat("reading file with resolution: ", res,"km...\n")
    AM <- getDataSet(f,"AM",res)
    PM <- getDataSet(f,"PM",res)
    cat("converting to my grid...\n")
    SM_AM <- mySM(getMyGrid(AM,0.2),AM$sm)
    SM_PM <- mySM(getMyGrid(PM,0.2),PM$sm)
    QF_AM <- mySM(getMyGrid(AM,0.2),AM$qf)
    QF_PM <- mySM(getMyGrid(PM,0.2),PM$qf)

#    out <- array(NA,dim=c(111,111,2))
#    out[,,1] <- SM_AM
#    out[,,2] <- SM_PM
    out <- list(SM_AM=SM_AM, SM_PM=SM_PM, QF_AM=QF_AM, QF_PM=QF_PM)

#  }else if(length(grep("SM_AP_",file)) == 1){
#    res=9
#    cat("reading file with resolution: ", res,"km...\n")
#    AM <- getDataSet(f,"AM",res)
##    cat("converting to my grid...\n")
#    SM_AM <- mySM(getMyGrid(AM,0.08),AM$sm)
#    out <- array(NA,dim=c(111,111,1))
#    out[,,1] <- SM_AM

#  }else{
#    print("unknown format")
#  }
#  cat("done\n")
  return(out)
}


#stop()
#########################################################################
source("topo.R")
## compare with SMOS
#x <- readh5("../h5/SMAP_L3_SM_P_20160701_R14010_001.h5")
#
#
#f1 <- read.table("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS/OBSERVATIONS_160701H06.DAT")
#
#f2 <- read.table("/lustre/storeB/users/asmundb/SMOS/OBSERVATIONS/OBSERVATIONS_160701H18.DAT")
#
#SMOS_A <- matrix(as.numeric(f1[,1]),111,111)
#SMOS_D <- matrix(as.numeric(f2[,1]),111,111)
#
#SMOS_A[which(SMOS_A > 10)] <- NA
#SMOS_D[which(SMOS_D > 10)] <- NA
#
#
#col<- rev(tim.colors())
#
#pdf("SMAPvsSMOS.pdf")
#
#par(mfrow=c(2,2))
#image.plot(x[,,1],zlim=c(0,1),main="SMAP_am", col=col)
#topo()
#
#image.plot(SMOS_A,zlim=c(0,1),main="SMOS_am", col=col)
#topo()
#
#image.plot(x[,,2],zlim=c(0,1),main="SMAP_pm", col=col)
#topo()
#
#image.plot(SMOS_D,zlim=c(0,1),main="SMOS_am", col=col)
#topo()
#dev.off()
#
#############################################################################
#
### compare active and passive
#
#passive <- readh5("/lustre/storeB/users/asmundb/SMAP/n5eil01u.ecs.nsidc.org/SMAP/SPL3SMP.004/2015.07.01/SMAP_L3_SM_P_20150701_R14010_001.h5")
#
#active <- readh5("/lustre/storeB/users/asmundb/SMAP/n5eil01u.ecs.nsidc.org/SMAP/SPL3SMA.003/2015.07.01/SMAP_L3_SM_A_20150701_R13080_001.h5")
#
#
#pdf("SMAP_products.pdf")
#par(mfrow=c(2,2))
#
#image.plot(passive[,,1],zlim=c(0,1), col=col,main="passive_am")
#topo()
#
#image.plot(passive[,,2],zlim=c(0,1), col=col,main="passive_pm")
#topo()
#
#image.plot(active[,,1],zlim=c(0,1), col=col,main="active")
#topo()
#
#image.plot(actpass[,,1],zlim=c(0,1), col=col,main="active passive")
#topo()
#
#dev.off()
