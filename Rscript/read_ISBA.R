# 
require(ncdf4)

load_isba <- function(files, vars){
  nfiles <- length(files)
  nvar <- length(vars)
  var <- array(NA, dim=c(111,111,nfiles*6,nvar))
  k   <- 1
  for (ifile in 1:nfiles){
    cat(ifile, "/", nfiles, "\r")
    ncid <- nc_open(files[ifile])
    for (ivar in 1:nvar){
      tmp <- ncvar_get(ncid, ncid$var[[vars[ivar]]])
      var[,,k:(k+5),ivar] <- tmp
    }
    k <- k+6
    flush.console()
  }
  out <- list()
  for (ivar in 1:nvar){
     out[[ivar]] <- var[,,,ivar]
  }
  names(out) <- vars
  return(out)


}

##time <- seq(as.POSIXlt("2016-09-10 01:00"), as.POSIXlt("2016-10-17 00:00"), by=3600)

#vars <- c("WG1","WG2", "TG1", "TG2")


# open loop 
#path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2014/SPINUP/ISBA/"
#files1 <- list.files(path,   
#                    pattern="ISBA_PROGNOSTIC.OUT.nc",   
##                    recursive=T,
#                    full.names=T)
#ol <- load_isba(files1, vars)

# SEKF
#path <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/obserr/SEKF_obs07/ISBA/"
#files2 <- list.files(path,   
#                    pattern="ISBA_PROGNOSTIC.OUT.nc",   
#                    recursive=T,
#                    full.names=T)
#
#sekf <- load_isba(files2, vars)
