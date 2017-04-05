require(ncdf4)
require(fields)

path <- "/lustre/storeB/users/asmundb/surfex/SMOSREX/RESULT/"

files <- list.files(path=path,
                    pattern="ISBA_PROGNOSTIC",
                    recursive=T,
                    full.names=T)

nfiles <- length(files)

varnames <- paste("WG",1:14,sep="")
varnames2 <- paste("WGI",1:14,sep="")
nr <- nfiles*8
nc <- 14
wg <- array(dim=c(nr,nc))
wgi <- array(dim=c(nr,nc))

k <- 1
for (i in 1:nfiles){
  ncid <- nc_open(files[i])
  for (j in 1:nc){
    wg[k:(k+7), j] <- ncvar_get(ncid, ncid$var[[varnames[j]]])[10,]
    wgi[k:(k+7), j] <- ncvar_get(ncid, ncid$var[[varnames2[j]]])[10,]
  }
  nc_close(ncid)
  k <- k+8
}
sm <- wg + wgi

# soil parameters
clay <- 0.317
sand <- 0.231
wwilt <- 37.1342*10^(-3)*clay^0.5
wsat  <- (-1.08*sand + 494.305)*10^(-3)
wfc   <- 89.0467*10^(-3)*clay^(0.3496)

ncid <- nc_open("/lustre/storeB/users/asmundb/surfex/SMOSREX/RUN/RUN_OFFLINE/SURFOUT.nc")

wwiltp <- ncvar_get(ncid, ncid$var$WWILT1)
wsatp  <- ncvar_get(ncid, ncid$var$WSAT1)
wfcp   <- ncvar_get(ncid, ncid$var$WFC1)

nc_close(ncid)


times <- seq(as.POSIXlt("2001-01-01 07:00"), as.POSIXlt("2007-01-01 06:00"), by=3600*3)
at <- seq(1, nr, by=8*14)

### PLOT ###


plot_timeserie <- function(layers){
  par(mar=c(5.1,4.1,4.1,8.1), xpd=T)
  plot(1:nr, 
       ylim=c(wwiltp-0.01,0.5), #c(min(wg,na.rm=T), max(wg,na.rm=T)),
       main="SMOSREX total soil moisture (wg+wgi)",
       xlab="",
       ylab="",
       xaxt="n",
       type='n')
 
  axis(1,at=at, labels=format(times[at],format="%b-%y"))

  colo <- tim.colors(8)

  for (i in layers){
    lines(sm[,i], col=colo[i])
  }
  lines(c(1,nr), rep(wwiltp,2),lty=2)
  lines(c(1,nr), rep(wsatp,2),lty=3) 
  lines(c(1,nr), rep(wfcp,2),lty=4) 

#  par(xpd=T)
  legend("topright", inset=c(-0.24,0), wsatp,       legend=varnames[layers],         lty=1,         col=colo[layers])
  legend("bottomright", inset=c(-0.24,0),   legend=c("wsat","wfc","wwilt"),    lty=c(3,4,2))
}
