#Functions

nn <- function(i,j,I,J){
  # return nearest neighbour
  d <- outer((I-i)^2 , (J-j)^2)
  x <- which(min(d) == d, arr.ind=T)
  return(x)
}

