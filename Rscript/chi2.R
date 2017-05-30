# Diagnostics on backround, observation and analysis errors

# dbo: innovation  y^o - h(x^b)
# dba: (A-B)
# dao: (O-A)

# yo, xa, inc, and innov are read in readSODAresults.R
source("readSODAresults.R")

loadSODA <- function(path){
  print("read analysis files")
  ana    <- load001(path, "ANAL_INCR")  
  xa     <- ana[,,,7:1]                  # Analysis
  inc    <- ana[,,,14:8]                 # increment
  xf     <- xa-inc                       # background
  rm(ana)

  yo     <- load001(path, "OBSout")      # observation
  innov  <- load001(path, "INNOV")       # innovation
  obserr <- load001(path, "OBSERRORout") # observation error
  
  x <- list( xa=xa,
             xf=xf,
             yo=yo,
             inc=inc,
             innov=innov,
             obserr=obserr )
  return(x)
}

# Scaling of error in SODA ZCOEF
PCOFSWI <- function(clay){
  PCOFSWI <- 0.001 * (89.0467 * ((100.*clay)^0.3496) - 37.1342*((100.*clay)^0.5))
  return(PCOFSWI)
}


errDiag <- function(x, sigma_o, sigma_b){
  # Errors set in namelist
#  sigma_o <- 0.4      # XOBSERR
#  sigma_b <- 0.049871 # XSIGMA_B

  dbo <- as.numeric(x$yo[,,,1]-x$xf[,,,2])  # INNOVATION Yo- H*Xf
  dba <- as.numeric(x$xa[,,,2]-x$xf[,,,2])  # (A-B) 
  dao <- as.numeric(x$yo[,,,1]-x$xa[,,,2])  # (O-A)
  
  E_dba_dbo <- mean(dba*dbo,na.rm=T)    # E(dba*dbo)  ##= HBH^T = B , H=1 in my case  
  E_dao_dbo <- mean(dao*dbo,na.rm=T)    # E(dao*dbo)  ##= R
  E_dbo_dbo <- mean(dbo*dbo,na.rm=T)
  
  sigma_o_sq <- mean(x$obserr,na.rm=T)                     # R: specified obs error in soda
  sigma_b_sq <- mean(sigma_b^2*PCOFSWI(clay)^2 ,na.rm=T)   # B: specified bg error in soda
  
  # Diagnostics of obs error
  
  chi2o <- E_dao_dbo/sigma_o_sq # should be close to one
  chi2b <- E_dba_dbo/sigma_b_sq  
  chi2bobo <- E_dbo_dbo/(sigma_o_sq + sigma_b_sq)

  y <- list( dbo   = dbo, 
             dba   = dba,
             dao   = dao,
             Ebabo = E_dba_dbo,
             Eaobo = E_dao_dbo,
             Ebobo = E_dbo_dbo,
             z2_o_out = sigma_o_sq,
             z2_b_out = sigma_b_sq,
             chi2o = chi2o,
             chi2b = chi2b )
  return(y)
}

XSIGMA_B <- rev(c(0.035399,  0.038154,  0.040139,  0.042590,  0.045341,  0.049871,  0.058721))
B <- matrix(0, 7, 7)
for (i in 1:7){
  B[i,i] <- XSIGMA_B[i]^2
}



# More complicated computation with jacobian H


errDiag2 <- function(x, HO){

  daodbo <- array(NA, dim=c(111,111,44))    # dao*dbo
  y <- array(NA, dim=c(7,111,111,44))       # HBH^T 
                                            # B is fixed (SEKF)
                                            # H is printed from SODA

  for (l in 1:44){
    for (k in 1:111){
      for (j in 1:111){
        dao <- x$yo[j,k,l,1] - HO$H[,j,k,l] %*% x$xa[j,k,l,]  # (O-A)
        dbo <- x$yo[j,k,l,1] - HO$H[,j,k,l] %*% x$xf[j,k,l,]  # (O-B) innovation
        daodbo[j,k,l] <- dao %*% t(dbo)                       # dao*dbo^T
        y[,j,k,l] <- HO$H[,j,k,l] %*% B*PCOFSWI(clay[j,k])^2 %*% t(HO$H[,j,k,l]) # HBH^T
      }
    }
  }
  yy <- apply(y,c(1),mean,na.rm=T)  # E[HBH^T]

  dba1 <- x$xa - x$xf               # (A-B)
  dbo1 <- x$yo[,,,1] - x$xf[,,,2]   # innovation

  dbadbo <- array(NA, dim=c(7,111,111,44))   # dba*dbo
  for (l in 1:44){
    for (k in 1:111){
      for (j in 1:111){
        dbadbo[,j,k,l] <- dba1[j,k,l,] * dbo1[j,k,l]
      }
    }
  }
  zz <- apply(dbadbo,1,mean,na.rm=T) # E[dba*dbo]

  out <- list( hbht=yy, dbadbo=zz)
  return(out)
}



path="/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/SEKF_sat-wilt/ANALYSIS/"
x04 <- loadSODA(path)
y04 <- errDiag(x04,0.4,0.049871)
HO04 <- loadHO(path)
z04 <- errDiag2(x04,HO04)

path="/lustre/storeB/users/asmundb/surfex/RESULTS/2016/obserr/SEKF_08/ANALYSIS"
x08 <- loadSODA(path)
y08 <- errDiag(x08,0.8,0.049871)
HO08 <- loadHO(path)
z08 <- errDiag2(x08,HO08)

path="/lustre/storeB/users/asmundb/surfex/RESULTS/2016/obserr/SEKF_07/ANALYSIS"
x07 <- loadSODA(path)
y07 <- errDiag(x07,0.7,0.049871)
HO07 <- loadHO(path)
z07 <- errDiag2(x07,HO07)


stop("hei")


#plot(yy/zz)

#
#plot(yy/zz,type='o', ylab=expression(chi^2), main=expression(chi^2 : E(d[b]^a*d[b]^o) / HBH^T))
#legend("topright",legend=sprintf("%s%d%s%f",expression(sigma^2),1:7,"=",XSIGMA_B))



plot(z04$dbadbo/z04$hbht,type='o', main=expression(chi^2 : E(d[b]^a*d[b]^o) / HBH^T), ylab=expression(chi^2),ylim=c(0,20))
lines(z08$dbadbo/z08$hbht,type='o',col="red")
lines(z07$dbadbo/z07$hbht,type='o',col="blue")
legend("topleft",legend=paste("obserr=",c("0.4","0.8","0.7"),sep=""), lty=1, fill=c("black","red","blue"))
