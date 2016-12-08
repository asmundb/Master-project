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
  x = read.table(file=OBS_file)
  return(x) 
}
#### get_var ####
get_var <- function(file, varname){
  # 
  # open [filename] and read [varname]
  # returns array (ntime, npoints)
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
  dimnames(var)[[2]] = c(sprintf("point_%d",1:8))
  dimnames(var)[[1]] = num_times
  stat = nc_close(ncid)
  return(var)
}

#### get_analysis ####
get_analysis <- function(file, varname){
  #
  # read ISBA_PROGNOSTICS.OUT.NC created by homemade EnKF
  # 
  ncid       = nc_open(file)
  npoints    = ncid$dim$x$len
  ntimes     = ncid$dim$y$len
  filename   = ncid$filename
  yyyymmddhh = gsub(".nc", "", gsub(".*nc_", "", filename))
  num_times  = as.numeric(yyyymmddhh)
  var        = t(ncvar_get(ncid, ncid$var[[varname]]))
  dimnames(var)[[2]] = c(sprintf("point_%d",1:8))
  dimnames(var)[[1]] = num_times
  stat = nc_close(ncid)
  return(var)
}
