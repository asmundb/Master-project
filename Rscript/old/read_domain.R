library(ncdf4)

### ISBA Prognostic ###

path_sekf <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/SEKF/"
path_ol <- "/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/openloop/"



load_ISBA_var <- function(path,var){
  files <- list.files(path=path, pattern="ISBA_PROGNOSTIC", recursive=T, full.name=T)
  nfiles <- length(files)

  var <- array(dim=c(111,111,nfiles*6))
  k <- 1
  for (ifile in 1:nfiles){
    ncid <- nc_open(files[ifile])
    var[,,k:(k+5)] <- ncvar_get(ncid, ncid$var$WG2)
  }
  return(var)
}
 

