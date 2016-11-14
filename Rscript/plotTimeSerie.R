# Load variables from .dat file
# and plot a timeserie
#

args <- commandArgs(trailingOnly=T)
nargs <- length(args) 
if (nargs == 0){
  stop("please specify input .dat file and output .pdf file")
} else if (nargs == 1) {
  var.dat <- args[1]
  var <- gsub(".dat","",var.dat)
  filename <- sprintf("%s.pdf",var)
} else {
  var.dat <- args[1]
  filename<- args[2]
  points2plot <- as.numeric(c(args[3:nargs]))
}
print(points2plot)
print(class(points2plot))

load(var.dat)

var <- gsub(".dat","",var.dat)

npoints <- dim(var.offline)[2]
nanalysis <- dim(var.analysis)[1]
ntimes <- dim(var.offline)[1]
nobs <- dim(obs)[1]

if (is.null(nanalysis)){
  nanalysis <- 0
}

col = rainbow(npoints)

#points2plot <- 2

pdf(filename)

ylim = c( min(c(min(var.analysis), min(var.offline)), na.rm=T),
          max(c(max(var.analysis), max(var.offline)), na.rm=T))

plot(NA,xlim=c(0,ntimes),ylim=ylim)
for (i in points2plot){
  for (j in 1:(ntimes/6)){
    lines(1:6+(j-1)*6, var.offline[1:6+(j-1)*6,i],col=col[i])
  }
# lines(c(6,7), c(var.analysis[1,i], var.offline[7,i]),col=col[i])
# lines(c(12,13), var.offline[12:13,i],col=col[i])
# lines(c(18,19), c(var.analysis[2,i], var.offline[19,i]),col=col[i])
  if (nanalysis > 0){
    points(seq(6,nanalysis*12,12),var.analysis[,i],col=col[i])
  }
#
}
#
if (var == "WG1"){		
  for (i in 1:nobs){
    points(rep(12*(i-1)+6,8), obs[i,], bg=col, pch=24)
  }
}
#points(rep(18,8), obs2, bg=col, pch=24)

#abline(v=c(6,18),lwd=0.1)

#legend("topleft", legend=dimnames(stations)[[1]], fill=col)
#legend("bottomleft", legend=c("SMOS", "analysis","offline"),

dev.off()
