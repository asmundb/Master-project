## Rescale data from SMOSREX

require(ncdf4)


#### wilt-sat ####

# read a prepfile to obtain soil parameters 


prepfile <- "/lustre/storeB/users/asmundb/surfex/SMOSREX/RUN/RUN_PREP/PREP.nc"
ncid <- nc_open(prepfile)

wwilt <- ncvar_get(ncid, ncid$var$WWILT1)
wsat <- ncvar_get(ncid, ncid$var$WSAT1)

nc_close(ncid)

##################

### read SMOSREX-grassland files ###

files <- list.files(path="SMOSREX_Jostein/",
                    pattern="SMOSREX-grassland",
                    full.names=T)

nfiles <- length(files)

tmp <- list()
for (i in 1:nfiles){
  tmp[[i]] <- read.table(files[i], na.strings=-9.999)
#  nobs <- nobs + dim(tmp[[i]])[1]
}

obs <- do.call("rbind", tmp)
rm(tmp)

nobs <- dim(obs)[1]

#mins <- matrix(as.numeric(apply(obs[,4:13], 2, min, na.rm=T)),ncol=10,nrow=nobs,byrow=T)
#maxs <- matrix(as.numeric(apply(obs[,4:13], 2, max, na.rm=T)),ncol=10,nrow=nobs,byrow=T)

mins <- as.numeric(apply(obs[,4:13], 2, min, na.rm=T))
maxs <- as.numeric(apply(obs[,4:13], 2, max, na.rm=T))

#sat  <- matrix(wsat,ncol=10,nrow=nobs,byrow=T)
#wilt <- matrix(wwilt,ncol=10,nrow=nobs,byrow=T)

newobs <- obs
tmp <- sweep(obs[,4:13], 2, mins,'-')
tmp2<- sweep(tmp, 2, (wsat-wwilt)/(maxs-mins), '*')
newobs[,4:13] <- tmp2 + wwilt


pdf("rescale.pdf")
plot(obs[[4]],type='l', ylim=c(0.05,0.6), 
     main="SMOSREX normalized to model range",
     xlab="half hours since 2001-01-03 00:00",
     ylab="Soil moisture m3m-3")
lines(newobs[[4]], col="red")
abline(h=wwilt, lty=2)
abline(h=wsat, lty=3)
legend("topright", 
       legend=c("SMOSREX soil moisture", "normalized","wsat","wwilt"),
       lty=c(1,1,3,2), col=c("black", "red","black","black"))
dev.off()


#####  write files  #####

# to path
path <- "/lustre/storeB/users/asmundb/surfex/SMOSREX/OBSERVATIONS"

use <- which(newobs[,2] %in% c(0,600,1200,1800))
dates <- as.character(newobs[use,1])
hours <- substr(sprintf("%04d",newobs[use,2]),1,2)
obsout <- newobs[use,4]
obsout[is.na(obsout)] <- 999

i=1
for (i in 1:length(obsout)){
  tmp1 <- strsplit(dates[i], "/")[[1]]
  filename <- sprintf("%s/OBSERVATIONS_%s%s%sH%s.DAT",path, substr(tmp1[3],3,4), tmp1[2], tmp1[1], hours[i])
  write(obsout[i],file=filename, ncolumns=1)
}













