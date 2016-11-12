# Load variables from .dat file
# and plot a timeserie
#

args <- commandArgs(trailingOnly=T)
if (length(args) == 0){
  stop("please specify input .dat file and output .pdf file")
} else if (length(args) == 1) {
  var.dat <- args[1]
  var <- gsub(".dat","",var.dat)
  filename <- sprintf("%s.pdf",var)
} else {
  var.dat <- args[1]
  filename<- args[2]
}

load(var.dat)

var <- gsub(".dat","",var.dat)

npoints <- dim(var.analysis)[2]
nanalysis <- dim(var.analysis)[1]
ntimes <- dim(var.offline)[1]

col = rainbow(npoints)

pdf(filename)

ylim = c( min(c(min(var.analysis), min(var.offline))),
          max(c(max(var.analysis), max(var.offline))))

plot(NA,xlim=c(0,ntimes),ylim=ylim)
for (i in 1:npoints){
  for (j in 1:(ntimes/6)){
    lines(1:6+(j-1)*6, var.offline[1:6+(j-1)*6,i],col=col[i])
  }
# lines(c(6,7), c(var.analysis[1,i], var.offline[7,i]),col=col[i])
# lines(c(12,13), var.offline[12:13,i],col=col[i])
# lines(c(18,19), c(var.analysis[2,i], var.offline[19,i]),col=col[i])
  points(seq(6,nanalysis*12,12),var.analysis[,i],col=col[i])
#
}
#
for (i in 1:nanalysis){
  points(rep(12*(i-1)+6,8), obs[i,], bg=col, pch=24)
}
#points(rep(18,8), obs2, bg=col, pch=24)

#abline(v=c(6,18),lwd=0.1)

#legend("topleft", legend=dimnames(stations)[[1]], fill=col)
#legend("bottomleft", legend=c("SMOS", "analysis","offline"),

dev.off()
