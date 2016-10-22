#Functions

#### nn ####
nn <- function(i,j,I,J){
  # return nearest neighbour
  d <- outer((I-i)^2 , (J-j)^2)
  x <- which(min(d) == d, arr.ind=T)
  return(x)
}


#### fortDp2Float ####
fortDp2Float <- function(str){
  # extract number from fortran double print.
  # 0.xxxxxxD+yy ---> 0.xxxxxx*10^yy 
  # 
  # to improve: generalize length of xs by using regex.
  des = as.numeric(substr(str,1,8))
  pow = as.numeric(substr(str,10,12))
  return(des*10^pow)
}

#### get_var_TXT ####  
get_var_TXT <- function(files){
  # read .TXT output files from offline run
  #
  # read files
  var <- c()
  for (i in 1:length(files)){
    var <- c(var, read.table(files[i],stringsAsFactors=F)[[1]])
  }
  # conver to numeric
  x <- array(dim=length(var))
  for (i in 1:length(x)){
    x[i] <- fortDp2Float(var[i])
  }
  return(x)
}
