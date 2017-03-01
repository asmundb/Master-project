library("ncdf4")
source("functions.R")

load_ISBA_timeserie <- function(files, varname){
  
  nfiles <- length(files)
  
  
  #var <- array(dim=c(nfiles*6, 8))
  var = list()
  for (j in 1:length(varname)){
    var[[varname[j]]] <- array(dim=c(nfiles*6, 8))
  }
  time <- numeric(length=nfiles*6)
  
  k <-1
  for (i in 1:nfiles){
    tmp <- get_var(files[i], varname)
    time[k:(k+6-1)] <- tmp$time
    for (j in 1:length(tmp)){
      var[[varname[j]]][k:(k+6-1),] <- tmp[[varname[j]]]
    }
    k <- k + 6 
  }
  data <- list( time=time, var=var )
  return(data)

}




get_var <- function(file, varname, reduse=T){
  # 
  # open [filename] and read [varname]
  # returns array (ntime, npoints)
  if (reduse){
    extractvar <- paste("--extract.selectVariables", varname, collapse=' ') 
    system(sprintf("fimex %s --input.file %s --input.type netcdf  --output.file tmp.nc",extractvar, file))
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
  var <- list()
  for (i in 1:length(varname)){
    tmpvar       = t(ncvar_get(ncid, ncid$var[[varname[i]]]))
    var[[varname[i]]] <- tmpvar
  }
  #dimnames(var)[[2]] = c(sprintf("point_%d",1:npoints))
  #dimnames(var)[[1]] = num_times
  var$time <- num_times
  stat = nc_close(ncid)
  system("rm -f tmp.nc")
  return(var)
}

unitize <- function(x){
  y <- x/max(abs(x))
  return(y)
}


stlst <- read_stlist(stationlist="stationlist.cfg")



diag_vars <- c("T2M_ISBA", "Q2M_ISBA","GFLUX_ISBA","H_ISBA","LE_ISBA","RN_ISBA")
prog_vars <- c("WG1", "WG2", "TG1", "TG2")


path_diag <- "/disk1/asmundb/RESULTS/20170228_run/openloop/ISBA/"
files_diag <- list.files(path_diag, pattern="ISBA_DIAGNOSTICS", full.names=T, recursive=T)
files_prog <- list.files(path_diag, pattern="ISBA_PROGNOSTIC", full.names=T, recursive=T)
prog_ol <- load_ISBA_timeserie(files_prog,prog_vars)
diag_ol <- load_ISBA_timeserie(files_diag,diag_vars)



path_diag <- "/disk1/asmundb/RESULTS/20170228_run/ekf/ISBA/"
files_diag <- list.files(path_diag, pattern="ISBA_DIAGNOSTICS", full.names=T, recursive=T)
files_prog <- list.files(path_diag, pattern="ISBA_PROGNOSTIC", full.names=T, recursive=T)
prog_ekf <- load_ISBA_timeserie(files_prog,prog_vars)
diag_ekf <- load_ISBA_timeserie(files_diag,diag_vars)




# Energy balance
#plot( diag_ol$var$H_ISBA[,1] + diag_ol$var$LE_ISBA[,1] + diag_ol$var$GFLUX_ISBA[,1] - diag$var$RN_ISBA[,1])

# Difference (increment)
pnt=1
plot(unitize(diag_ekf$var$H_ISBA[,pnt] - diag_ol$var$H_ISBA[,pnt]),
     ylim=c(-1,1))
lines(unitize(prog_ekf$var$TG1[,pnt]- prog_ol$var$TG1[,pnt]),col="blue")
lines(unitize(prog_ekf$var$WG1[,pnt]- prog_ol$var$WG1[,pnt]),col="red")
lines(unitize(diag_ekf$var$LE_ISBA[,pnt] - diag_ol$var$LE_ISBA[,pnt]), col="green")


# regimes

wp=0.213320
fc=0.356612

for (i in 1:8){
  pdf(sprintf("EF_%d.pdf",i))
  plot(prog_ol$var$WG1[,i],
       diag_ol$var$LE_ISBA[,i]/abs(diag_ol$var$RN_ISBA[,i]), ylim=c(0,7),
       xlab="soil moisture", ylab="LE/RN",
	   main=dimnames(stlst)[[1]][i])
  abline(v=wp, col="red")
  abline(v=fc, col="red")
  dev.off()
}
