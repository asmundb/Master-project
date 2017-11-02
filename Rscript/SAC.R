SAC <- function(x,stp){
  n    <- length(x)
  nx   <- dim(x)[1]
  ny   <- dim(x)[2]
  xbar <- mean(x,na.rm=T)

  smax <- max(stp)
  xx   <- matrix(NA, nx + smax*2, ny + smax*2)
  xx[(1:nx)+smax,(1:ny)+smax] <- x

  stps <- c(-rev(stp),stp)
  
  S    <- 0
  nas  <- 0
  for (k in (1:ny)+smax){
    for (j in (1:nx)+smax){
      dx1 <- xx[j,k] - xbar
      hos <- dx1*(xx[j + stps, k] - xbar)
      ves <- dx1*(xx[j, k + stps] - xbar)
      S   <- S + sum(c(hos,ves), na.rm=T)         
    }
  }

  D <- sum((x-xbar)^2,na.rm=T)
  nas <- sum(is.na(x)) 
  r <- (n/(n*length(stp)*4-(2*nx*sum(stp)+2*ny*sum(stp) + nas )))*S/D
  return(r)
}


### Moran's Index of spatial autocorrelation ### 

MI <- function(y,w){

  n <- length(y)
  ybar <- mean(y,na.rm=T)
  ss1 <- 0
  ss2 <- 0
  nas <- 0
  for(i in 1:n){
    for (j in 1:n){
      tmp <- w[i,j]*(y[i]-ybar)*(y[j]-ybar)
      if (!is.na(tmp)){
        ss1 <- ss1 + tmp
        ss2 <- ss2 + w[i,j]
      } else {
        nas <- nas+1
      }
    }
  }
  n1  <- n-nas
  a1  <- n1/sum((y-ybar)^2,na.rm=T)
  I   <- a1*ss1/ss2                     # Moran's I
  EI  <- -1/(n-1)                     # Expected value
  ### Variance ###
  # S1
  S1  <- 0
  for (i in 1:n){1
    for(j in 1:n){
      S1 <- S1 + (w[i,j]+w[j,i])^2
    }
  }
  S1 <- 0.5*S1
  # S2
  S2 <- 0
  for (i in 1:n){
    S2 <- S2 + (sum(w[i,]) + sum(w[,i]))^2
  }
  # S3
  S3 <- ((1/n)*sum((y-ybar)^4,na.rm=T))/(((1/n)*sum((y-ybar)^2,na.rm=T))^2)
  # S4
  S4 <- (n^2 - 3*n + 3)*S1 - n*S2 + 3*ss2^2
  # S5
  S5 <- (n^2-n)*S1 - 2*n*S2 + 6*ss2^2
  # Var(I)
  V   <- (n*S4 - S3*S5)/( (n-1)*(n-2)*(n-3)*ss2^2) - EI^2
  # Z-score
  z   <- (I-EI)/sqrt(V)


  out <- list(I=I,EI=EI,Zscore=z,Var=V)
  return(out)
}

make_w <- function(nx,ny){
  n  <- nx*ny

  w  <- matrix(0,n,n)

  nl=TRUE
  for(i in 1:n){
    if(i%/%nx == i/nx){
      w[i,i-1] <- 1
      nl <- TRUE
    } else if (nl){
      w[i,i+1] <- 1
      nl <- FALSE
    }else{
      w[i,i-1] <- 1
      w[i,i+1] <- 1
    }
    if (i < n-nx){
      w[i,i+nx] <- 1
    }
    if (i > nx){
      w[i,i-nx] <- 1
    }
  }
  return(w)
}


make_w2 <- function(sw, nx, ny){
  n <- nx*ny
  w <- matrix(0,n,n)

  l <- 0     # current line

  for (i in 1:n){
    ### Left step ###
    if (i >= l*nx + 1 + sw){
      w[i,i-sw] <- 1      # OK
    }
    ### Right step ###
    if (i <= (l+1)*nx - sw){
      w[i,i+sw] <- 1     # OK
    }
    ### Down step ###
    if (l+1 > sw){
      w[i,i-nx*sw] <- 1  # OK
    }
    ### Up step ###
    if (ny-l > sw){
      w[i,i+nx*sw] <- 1  # OK
    }
    l <- i %/% nx
  }
  return(w)

}



nx <- 11
ny <- 11

# perfect dispersion = -1

m1 <- matrix(0, nx,ny)
k1=1
k2=2
k=k1
for(i in 1:nx){
  m1[i,seq(k,by=2,to=nx)] <- 1
  k1=k2
  k2=k
  k=k1
}

# perfect correlations = 1
m2 <- matrix(0, nx,ny)
m2[1:((nx+1)/2)-1,] <- 1


# random = 0
m3 <- matrix(rnorm(nx*ny),nx,ny)


# mountain top

x <- sin(seq(0,pi,length=nx))
m4 <- outer(x,x)



# Real example


load("RDS_files/2016.VARS")
x <- prog_ol$WG2[,,100]

col=rev(tim.colors())

pdf("figures/ACF/morans/wg2_domain0.pdf")
image.plot(x,col=col,zlim=c(min(x,na.rm=T),max(x,na.rm=T)))
lines(c(1,40,40,1,1)/111, c(31,31,70,70,31)/111)
dev.off()

pdf("figures/ACF/morans/wg2_domain1.pdf")
x1 <- x[1:40,31:70]
image.plot(x1,col=col,zlim=c(min(x,na.rm=T),max(x,na.rm=T)))
lines(c(6,10,10,6,6)/10,c(0,0,4,4,0)/10)
dev.off()

x2 <- x1[24:40,1:16]



Isteps <- function(x,stpMax=10){
  Is <- numeric(stpMax)
  zs <- numeric(stpMax)
  for(i in 1:stpMax){
#    cat(i,"/",stpMax,"\r")
    w <- make_w2(i,dim(x)[1],dim(x)[2])
    print("w done")
    tmp <- fMI(x,w)
    Is[i] <- tmp$I
    zs[i] <- tmp$Zscore
#    flush.console()
  }
#  cat("\n")
  out <- list(I=Is, z=zs)
  return(out)
}
 
is <- Isteps(xx,stpMax=90)
pdf("Morans_RESULT.pdf")
plot(is$I)
dev.off()

SACs <- function(x,stpMax=10){
  Is <- numeric(stpMax)
  for (i in 1:stpMax){
    cat(i,"/",stpMax,"\r")
    Is[i] <- SAC(x,1:i)
    flush.console()
  }
  cat("done!","\n")
  return(Is)
}

SACs2 <- function(x,stpMax=10){
  Is <- numeric(stpMax)
  for (i in 1:stpMax){
    cat(i,"/",stpMax,"\r")
    Is[i] <- SAC(x,i)
    flush.console()
  }
  cat("done!","\n")
  return(Is)
}













