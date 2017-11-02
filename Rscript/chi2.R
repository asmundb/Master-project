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


makeB <- function(XSIGMA_B){
 #XSIGMA_B <- rev(c(0.035399,  0.038154,  0.040139,  0.042590,  0.045341,  0.049871,  0.058721))
  B <- matrix(0, 7, 7)
  for (i in 1:7){
    B[i,i] <- XSIGMA_B[i]^2
  }
  return(B)
}


# More complicated computation with jacobian H


errDiag2 <- function(x, HO, B){

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

# CHI^2 in time and space

errDiag3 <- function(x,berr){
  dbo <- x$yo[,,,1] - x$xf[,,,2]
  dao <- x$yo[,,,1] - x$xa[,,,2]
  dba <- x$xa[,,,2] - x$xf[,,,2]
  xx <- dao*dbo
  yy <- dba*dbo
  ntimes <- dim(xx)[3]
  R <- array(NA, dim=c(111,111))
  B <- array(NA, dim=c(111,111))
  chi2_o_tot <- array(NA, dim=c(111,111,ntimes))
  chi2_b_tot <- array(NA, dim=c(111,111,ntimes))

  for (i in 1:111){
    for (j in 1:111){
      R[j,i] <- x$obserr[j,i,1,1]#^2*PCOFSWI(clay[j,i])^2
      B[j,i] <- berr^2*PCOFSWI(clay[j,i])^2
    }
  }

  for (i in 1:ntimes){
    chi2_o_tot[,,i] <- xx[,,i]/R
    chi2_b_tot[,,i] <- yy[,,i]/B
  }


  out <- list(chi_o=chi2_o_tot, chi_b=chi2_b_tot)
  return(out)

}


#stop()

#B <- makeB(rev(c(0.035399,  0.038154,  0.040139,  0.042590,  0.045341,  0.049871,  0.058721)))

#path="/lustre/storeB/users/asmundb/surfex/RESULTS/2016/obserr/SEKF_obs08/ANALYSIS"
#x08 <- loadSODA(path)
#y08 <- errDiag(x08,0.8,0.049871)
#OFSWI(clay=clay)
#z08 <- errDiag2(x08,HO08,B)
#y08 <- errDiag3(x08,0.4)


#path="/lustre/storeB/users/asmundb/surfex/RESULTS/2016/obserr/SEKF_obs07/ANALYSIS"
#x07 <- loadSODA(path)
#y07 <- errDiag(x07,0.7,0.049871)
#HO07 <- loadHO(path)
#z07 <- errDiag2(x07,HO07,B)
#y07 <- errDiag3(x07,0.4)

#B2 <- makeB(rev(c(0.3,  0.2,  0.15,  0.1,  0.1,  0.06,  0.1)))

#path="/lustre/storeB/users/asmundb/surfex/RESULTS/2016/obserr/SEKF_obs06/ANALYSIS"
#x07b <- loadSODA(path)
#y07b <- errDiag(x07b,0.7,0.06)
#HO07b <- loadHO(path)
#z07b <- errDiag2(x07b,HO07b,B2)
#y07b <- errDiag3(x07b,0.4)

#B2 <- makeB(rev(c(0.1,  0.1,  0.1,  0.1,  0.1,  0.1,  0.1)))

#B2 <- makeB(rev(c(0.2,  0.2,  0.2,  0.2,  0.2,  0.4,  0.2)))

#path="/lustre/storeB/users/asmundb/surfex/RESULTS/2016/moderr/SEKF_obs07_3/ANALYSIS"
#x07b3 <- loadSODA(path)
#y07b2 <- errDiag(x07b2,0.7,0.1)
#HO07b2 <- loadHO(path)
#z07b2 <- errDiag2(x07b2,HO07b2,B2)
#y07b3 <- errDiag3(x07b3)

#path="/lustre/storeA/users/asmundb/surfex/RESULTS/2014/SEKF/obs06_b005/ANALYSIS"
#x06 <- loadSODA(path)
#y06 <- errDiag3(x06,0.05)
#
#
#path="/lustre/storeA/users/asmundb/surfex/RESULTS/2014/SEKF/obs06_b005/ANALYSIS"
#x06b005 <- loadSODA(path)
#y06b005 <- errDiag3(x06b005,0.05)
#
#path="/lustre/storeA/users/asmundb/surfex/RESULTS/2014/SEKF/obs06_b02/ANALYSIS"
#x06b02 <- loadSODA(path)
#y06b02 <- errDiag3(x06b02,0.2)
#
#path="/lustre/storeA/users/asmundb/surfex/RESULTS/2014/SEKF/obs07_b005/ANALYSIS"
#x07b005 <- loadSODA(path)
#y07b005 <- errDiag3(x07b005,0.05)
#
#path="/lustre/storeA/users/asmundb/surfex/RESULTS/2014/SEKF/obs07_b02/ANALYSIS"
#x07b02 <- loadSODA(path)
#y07b02 <- errDiag3(x07b02,0.2)
#
#path="/lustre/storeA/users/asmundb/surfex/RESULTS/2014/SEKF/obs08_b005/ANALYSIS"
#x08b005 <- loadSODA(path)
#y08b005 <- errDiag3(x08b005,0.05)
#
#path="/lustre/storeA/users/asmundb/surfex/RESULTS/2014/SEKF/obs08_b02/ANALYSIS"
#x08b02 <- loadSODA(path)
#y08b02 <- errDiag3(x08b02,0.2)
#
#
#stop("hei")
#
#
##plot(yy/zz)
#
##
##plot(yy/zz,type='o', ylab=expression(chi^2), main=expression(chi^2 : E(d[b]^a*d[b]^o) / HBH^T))
##legend("topright",legend=sprintf("%s%d%s%f",expression(sigma^2),1:7,"=",XSIGMA_B))
#
#
#pdf("figures/chi2/HBHT.pdf")
#plot(z04$dbadbo/z04$hbht,type='o', main=expression(chi^2 : E(d[b]^a*d[b]^o) / HBH^T), ylab=expression(chi^2),log="y", ylim=c(1,2000))
#lines(z08$dbadbo/z08$hbht,type='o',col="red")
#lines(z07$dbadbo/z07$hbht,type='o',col="blue")
#lines(z07b$dbadbo/z07b$hbht,type='o',col="darkblue")
#legend("topleft",legend=paste("obserr=",c("0.4","0.8","0.7","0.7 b"),sep=""), lty=1, fill=c("black","red","blue","darkblue"))
#dev.off()
#
#
## Statistical significance
#
#
#converg <- function(A){
#  iters <- dim(A)[3]
#   
#  S <- numeric(iters)
#  N <- numeric(iters)
#
#  S[1] <- sum(A[,,1],na.rm=T)
#  N[1] <- sum(!is.na(A[,,1]))
#  
#  for (i in 2:iters){
#    S[i] <- S[i-1] + sum(A[,,i],na.rm=T)
#    N[i] <- N[i-1] + sum(!is.na(A[,,i]))
#  }
#  
#  M <- S/N
#  M[which(is.infinite(M))] <- NA
#
#  return(M)
#}
#
## CONVERGENCE OF CHI^2 EXPECTATION VALUE
#
#con <- converg(A)[seq(1,368,by=2)]
#
#its <- 5:22
#LM <- lm(log(conv2[5:22])~log(its))
#P <- predict(LM, data.frame(its = 1:100))
#X <- exp(P)
#
#plot(y,xlim=c(0,100))
#lines(X)
#
#
#pdf("figures/chi2/convergence_of_chisquare_06.pdf")
#plot(apply(y06$chi_o, 3 , mean, na.rm=T), main=expression(chi^2-test: "obserr=0.6"), xlab="analysis i", ylab=expression(chi^2))
#lines(x$mean, lty=1)
#abline(h=1, lty=2, lwd=0.5)
#legend("topright",legend=c("mean at t=i", "mean from t=1 to t=i", "chi^2=1" ), lty=c(NA,1,2), pch=c(1,NA,NA))
#dev.off()
#
#
## Table
#
#
#oe <- c(0.6,0.7 ,0.7,0.7,0.7,0.7,0.8)
#be <- c(0.4,0.06,0.1,0.2,0.4,0.8,0.4)
#chio <- c(mean(y06$chi_o,na.rm=T), 
#          mean(y07b1$chi_o,na.rm=T), 
#          mean(y07b2$chi_o,na.rm=T),
#          mean(y07b02$chi_o,na.rm=T),
#          mean(y07b3$chi_o,na.rm=T),
#          mean(y07b08$chi_o,na.rm=T), 
#          mean(y08$chi_o,na.rm=T))
#
#chib <- c(mean(y06$chi_b,na.rm=T), 
#          mean(y07b1$chi_b,na.rm=T), 
#          mean(y07b2$chi_b,na.rm=T),
#          mean(y07b02$chi_b,na.rm=T),
#          mean(y07b3$chi_b,na.rm=T),
#          mean(y07b08$chi_b,na.rm=T),
#          mean(y08$chi_b,na.rm=T))
#
#
#
## Polynomial interpolation
#p <- function(x,xi,yi){
#  len <- length(x)
#  n <- length(xi)
#  p <- numeric(len)
#  for (i in 1:n){  # sum
#    m <- rep(1,len)
#    for (j in 1:n){ # multiply
#      if (j != i){
#        m <- m * (x-xi[j])/(xi[i]-xi[j])
#      }
#    }
#    p <- p + m*yi[i]
#  }
#  return(p)
#}
#
#
#
## RUN TIME
#
#nproc <- c(1,2,4,8,16,32)
#tim <- c(2*60+36.961, 51.74, 41.241, 32.827, 30.327, 39.041)
#pdf("figures/runtime.pdf")
#plot(nproc,tim, xlab="processors", ylab="elapsed time [s]", main="OFFLINE run time", type='o')
#dev.off()
#

