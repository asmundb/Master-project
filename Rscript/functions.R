#Functions


#### nn ####
nn <- function(i,j,I,J){
  # return nearest neighbour
  d <- outer((I-i)^2 , (J-j)^2)
  x <- which(min(d) == d, arr.ind=T)
  return(x)
}


#### fortDp2Float ####
fortDp2Float <- function(str){
  # extract number from fortran double print.
  # 0.xxxxxxD+yy ---> 0.xxxxxx*10^yy 
  # 
  # to improve: generalize length of xs by using regex.
  des = as.numeric(substr(str,1,8))
  pow = as.numeric(substr(str,10,12))
  return(des*10^pow)
}

#### get_var_TXT ####  
get_var_TXT <- function(files){
  # read .TXT output files from offline run
  #
  # read files
  var <- c()
  for (i in 1:length(files)){
    var <- c(var, read.table(files[i],stringsAsFactors=F)[[1]])
  }
  # conver to numeric
  x <- array(dim=length(var))
  for (i in 1:length(x)){
    x[i] <- fortDp2Float(var[i])
  }
  return(x)
}

#### coords_from_stlist ####
coords_from_stlist <- function(stationlist){
  # Read coordinates from stationlist.cfg file
  #
  #
  x = read.table(stationlist,header=F,skip=1,comment.char="#")
  lon = x$V4
  lat = x$V3
  alt = x$V5
  return(rbind(lon,lat,alt))
}

#### read_stlist ####
read_stlist <- function(stationlist){
  # Read name stnr lon lat alt from stationlist.cfg file
  #
  #
  x = read.table(stationlist,header=F,skip=1,comment.char="#")
  snr = x$V1
  stnr= x$V2
  name= x$V6
  lon = x$V4
  lat = x$V3
  alt = x$V5
  x   = cbind(stnr,lon,lat,alt)
  dimnames(x)[[1]] <- name
  return(x)
}

#### read_OBSERVATION ####
read_OBSERVATION <- function(OBS_file){
  # Read OBSERVATIONS.DAT file used by SODA
  # 
  #
  tstr       = regmatches(OBS_file, regexec("_(.*).DAT", OBS_file))[[1]][2]
  yyyymmddhh = paste("20", gsub("H","",tstr),sep="")
  x          = t(as.matrix(read.table(file=OBS_file)))
  dimnames(x)[[1]]  = yyyymmddhh
  return(x) 
}
#### get_var ####
get_var <- function(file, varname, reduse=T){
  # 
  # open [filename] and read [varname]
  # returns array (ntime, npoints)
  if (reduse){
    system(sprintf("fimex --extract.selectVariables %s --input.file %s --input.type netcdf  --output.file tmp.nc",varname, file))
	file    = "tmp.nc"
  }
  ncid      = nc_open(file)
  npoints   = ncid$dim$Number_of_points$len
  # time from ncid
  ntimes    = ncid$dim$time$len
  timeUnit  = ncid$dim$time$units
  itimes    = ncid$dim$time$vals
  tmp       = gsub("hours since ", "", timeUnit)
  startTime = as.POSIXlt(tmp)
  # analysis or prognosis
  if (ntimes == 1){ # analysis
    times = startTime
  } else { # prognosis
    times = startTime + itimes*3600
  }
  num_times = as.numeric(format(times,"%Y%m%d%H"))
  # get var
  var       = t(ncvar_get(ncid, ncid$var[[varname]]))
  dimnames(var)[[2]] = c(sprintf("point_%d",1:npoints))
  dimnames(var)[[1]] = num_times
  stat = nc_close(ncid)
  system("rm -f tmp.nc")
  return(var)
}

#### get_analysis ####
get_analysis <- function(file, varname,reduse=T){
  #
  # read ISBA_PROGNOSTICS.OUT.NC created by homemade EnKF
  # 
  filename = file
  if (reduse){
    system(sprintf("fimex --extract.selectVariables %s --input.file %s --input.type netcdf --output.file tmp.nc",varname, file))
    file    = "tmp.nc"
  }
  ncid       = nc_open(file)
  npoints    = ncid$dim$Number_of_points$len
# filename   = ncid$filename
  yyyymmddhh = gsub(".nc", "", gsub(".*nc_", "", filename))
  num_times  = as.numeric(strsplit(yyyymmddhh,split="_")[[1]][1])
  var        = t(ncvar_get(ncid, ncid$var[[varname]]))
  dimnames(var)[[2]] = c(sprintf("point_%d",1:npoints))
  dimnames(var)[[1]] = num_times
  stat = nc_close(ncid)
  system("rm -f tmp.nc")
  return(var)
}

#### prt2pntReshape ####
prt2pntReshape <- function(x){
  # reshape list
  # x  = list[[perturbation]][time,point]
  # xx = array[time, perturbation, point]
  #
  npoints <- dim(x[[1]])[2]
  nprt    <- length(x)
  ntimes  <- dim(x[[1]])[1]
  xx <- array(NA, dim=c(ntimes,nprt, npoints))                              
  for (prt in 1:nprt){
    for (pnt in 1:npoints){
	  xx[,prt,pnt] <- x[[prt]][,pnt]
	}
  }
  dimnames(xx)[[1]] <- dimnames(x[[1]])[[1]]
  dimnames(xx)[[2]] <- c(sprintf("pert_%d",1:nprt))
  dimnames(xx)[[3]] <- dimnames(x[[1]])[[2]]
  return(xx)
}


##### STATISTICAL #####

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

linReScale <- function(x,y){
  # Return rescaled x to y's mean and spread
  sigma_x = sd(x,na.rm=T)
  sigma_y = sd(y,na.rm=T)
  x_mean  = mean(x,na.rm=T)
  y_mean  = mean(y,na.rm=T)
  x_new = (x - x_mean)*(sigma_y/sigma_x) + y_mean
#  cat(sprintf("=========== STATION: %d =============\n", pnt))
#  cat(sprintf("old min  : %5.3f | old max  : %.3f \n",min(x,na.rm=T)  , max(x,na.rm=T)))
#  cat(sprintf("model min: %5.3f | model max: %.3f \n",min(y,na.rm=T)  , max(y,na.rm=T)))
#  cat(sprintf("new min  : %5.3f | new max  : %.3f \n",min(x_new,na.rm=T) , max(x_new,na.rm=T)))
  return(x_new)
}


minMaxReScale <- function(x,y){
  # scale to min max
  x_max <- max(x,na.rm=T)
  x_min <- min(x,na.rm=T)
  y_max <- max(y,na.rm=T)
  y_min <- min(y,na.rm=T)
  x_new <- (x-x_min)*(y_max-y_min)/(x_max-x_min) + y_min
  return(x_new)
}

minReScale <- function(x,y){
  # scale to min max
  x_max <- max(x,na.rm=T)
  x_min <- min(x,na.rm=T)
  y_max <- max(y,na.rm=T)
  y_min <- min(y,na.rm=T)
  x_new <- (x-x_min)*(1-y_min)/(x_max-x_min) + y_min
  return(x_new)
}

