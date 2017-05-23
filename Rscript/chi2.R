# Diagnostics on backround, observation and analysis errors

# dbo: innovation  y^o - h(x^b)
# dba: (A-B)
# dao: (O-A)

# yo, xa, inc, and innov are read in readSODAresults.R
source("readSODAresults.R")

loadSODA <- function(path){
  print("read analysis files")
  ana    <- load001(path, "ANAL_INCR")  
  xa     <- ana[,,,1:7]                  # Analysis
  inc    <- ana[,,,8:14]                 # increment
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

  dbo <- as.numeric(x$yo[,,,1]-x$xf[,,,6])  # INNOVATION Yo- H*Xf
  dba <- as.numeric(x$xa[,,,6]-x$xf[,,,6])  # (A-B) 
  dao <- as.numeric(x$yo[,,,1]-x$xa[,,,6])  # (O-A)
  
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


path="/lustre/storeB/users/asmundb/surfex/RESULTS/2016/lowcloud/SEKF_sat-wilt/ANALYSIS/"
x04 <- loadSODA(path)
y04 <- errDiag(x04,0.4,0.049871)

path="/lustre/storeB/users/asmundb/surfex/RESULTS/2016/obserr/SEKF_08/ANALYSIS"
x08 <- loadSODA(path)
y08 <- errDiag(x08,0.8,0.049871)



x <- array(NA, dim=c(111,111,44))
for (l in 1:44){
  for (k in 1:111){
    for (j in 1:111){
      dao <- x08$yo[j,k,l,1] - x08$xa[j,k,l,6]
      dbo <- x08$yo[j,k,l,1] - x08$xf[j,k,l,6]
      x[j,k,l] <- dao %*% t(dbo)
    }
  }
}
print(mean(x,na.rm=T))



x <- array(NA, dim=c(111,111,44))
      dao <- x08$yo[,,,1] - x08$xa[,,,6]
      dbo <- x08$yo[,,,1] - x08$xf[,,,6]
      x <- dao*dbo
print(mean(x,na.rm=T))
