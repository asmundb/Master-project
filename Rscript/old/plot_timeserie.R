# plot_timeserie.R
# 
# Stages:
#
#  -Read commandArgs
#  -Load variables from .dat files provided in argument
#  -Check loaded variables
#  -Plot variables
#
# Options: 
# -o  : name of output file
# -p  : plot this variable as points every 6 hours (analysis or observation)
#
#  Example:
#
#  $> Rscript plot_timeseries.R var1.dat var2.dat output.pdf

#####################################################################
##################### ARG PARSER ####################################

args <- commandArgs(trailingOnly=T)
nargs <- length(args)

## Default setting when no arguments passed
if(nargs < 1) {
  args <- c("--help")
}
 
## Help section
if("--help" %in% args) {
  cat("The R Script
      Arguments:
      -o name of outpyt file
      -p [n file] plot var as point skip n timesteps
      --help              - print this text
      Example:
      ./test.R var1.dat var2.dat -p 3 var3.dat -o out.pdf \n\n")
  q(save="no")
}

 
## Sort indecies out                                    ### debug if not present
ipoints    <- which(args == "-p") + 2                   # ipoint = numeric(0)
iskip      <- ipoints - 1                               # iskip  = numeric(0)
iout       <- which(args == "-o") + 1                   # iout   = numeric(0)
ifiles     <- grep(pattern=".dat", args)                # ifiles = integer(0)
ifiles     <- ifiles[which(ifiles != ipoints)]
nlfiles    <- length(ifiles)                            # nfiles = 0
npfiles    <- length(ipoints)                           # npfiles= 0

## Read arguments and load variables
if (length(iout) > 0){
  outfile    <- args[iout]
}else{
  outfile <- "out.pdf"
}
if (npfiles > 0){
  pointfiles <- args[ipoints]
  skip       <- args[iskip]
  try(npoints    <- length(readRDS(pointfiles[1])))
  pointvars  <- array(NA, dim=c(npoints,npfiles))
  for (i in 1:npfiles){
    result <- try(pointvars[,i] <- readRDS(pointfiles[i]))
	if (class(result) == "try-error"){
      print(sprintf("%s did not read successfully", pointfiles[i]))
    }
  }

}
if (nlfiles > 0){
  linefiles  <- args[ifiles]
  try(ntime      <- length(readRDS(linefiles[1])))  # guessing all files are of same length
  linevars   <- array(NA, dim=c(ntime,nlfiles))
  for (i in 1:nlfiles){
    result <- try(linevars[,i] <- readRDS(linefiles[i]))
	if (class(result) == "try-error"){
	  print(sprintf("%s did not read successfully", linefiles[i]))
	}
  }
}

#for (i in ls()){
#  print(i)
#  print(get(i))
#}

#stop("Test went ok i guess")

#####################################################################
##################### PLOT  VARS ####################################

npoints <- dim(var.offline)[2]
nanalysis <- dim(var.analysis)[1]
ntimes <- dim(var.offline)[1]
nobs <- dim(obs)[1]

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
