# Read NetCDF files created by SURFEX

library("ncdf4")

# test
path  <- "/uio/hume/student-u88/aasmunba/Master-project/OUTPUT"

files <- list.files(path=path, pattern="PREP_OFFLINE*",full.name=T) # test if nc mabye
files <- c(files, paste(path,"/PREP_SODA_EKF.nc",sep=""))
nfiles<- length(files)

vars <- c("TG1","TG2","WG1","WG2")
nvars<- length(vars)
                         #    npoints    
                         #       v
x    <- array(dim=c(nfiles,nvars,2))
dimnames(x) <- list(letters[1:nfiles])

for (i in 1:nfiles){
  ncid    <- nc_open(files[i])
  filename <- ncid$filename
  filename <- gsub(".*/","",filename)
  filename <- gsub("*.nc","",filename)
  dimnames(x)[[1]][i] <- filename
  for (j in 1:nvars){
    x[i,j,]    <- ncvar_get(ncid,ncid$var[[vars[j]]])
  }
  nc_close(ncid)
}
dimnames(x)[[2]] <- vars


