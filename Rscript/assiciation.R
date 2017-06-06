
milk     <- c(1,0,0,1,0)
bread    <- c(1,0,0,1,1)
butter   <- c(0,1,0,1,0)
beer     <- c(0,0,1,0,0)
diapers  <- c(0,0,1,0,0)

             # 1      2     3     4    5
Tset <- cbind(milk,bread,butter,beer,diapers)

#      beer, diapers
X <- c(   4,    5)


supp <- function(X,Tset){
  m <- dim(Tset)[1]
  n <- length(X)
  x <- numeric(m)
  x[] <- 1
  for (i in 1:n){
    x <- x * (Tset[,X[i]] == 1)
    
  }
  s <- sum(x,na.rm=T)
  supp <- s/m 
  return(supp)
}

conf <- function(X,Y,Tset){
  conf <- supp(cbind(X,Y),Tset)/supp(X,Tset)
  return(conf)
}
  

lift <- function(X,Y,Tset){
  lift <- supp(cbind(X,Y),Tset)/(supp(X,Tset)*supp(Y,Tset))
  return(lift)
}

conv <- function(X,Y,Tset){
  conv <- (1-supp(Y,Tset))/(1-conf(X,Y,Tset))
  return(conv)
}
