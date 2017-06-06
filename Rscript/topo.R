zs <- readRDS("RDS_files/zs.rds")
topo <- function(levels=c(0,2,10,50,100,200,500,1000)){
  contour(matrix(zs,nx,ny), add=T, levels=levels)
}
