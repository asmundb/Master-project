require(fields)

stations <- read.table("ftp://ftp.wmo.int/wmo-ddbs/VolA_New/Pub9volA160415x.flatfile",sep="\t",fill=T,header=T,stringsAsFactors=F)

n <- dim(stations)[1]

lon <- numeric(n)
lat <- numeric(n)

getDegDecimal <- function(str){
  deg  <- as.numeric(str[[1]][1])
  min  <- as.numeric(str[[1]][2])
  sec  <- as.numeric(gsub("\\D","",str[[1]][3],perl=T))
  dir  <- gsub("\\d","",str[[1]][3],perl=T)
  if (dir %in% c("E","N")){
    sign <- 1
  } else {
    sign <- -1
  }
  dec  <- sign*(deg + min/60 + sec/3600)
  return(dec)
}


stns <- rea




for (i in 1:n){
  tmp1 <- strsplit(stations$Longitude[i],split=" ")
  tmp2 <- strsplit(stations$Latitude[i],split=" ")
  lon[i] <- getDegDecimal(tmp1)
  lat[i] <- getDegDecimal(tmp2)
}




s <- read.table("http://old.wetterzentrale.de/klima/stnlst.html",skip=20,stringsAsFactors=F, fill=T)

n <- dim(s)[1]

lon <- numeric(n)
lat <- numeric(n)

f <- function(str){
  if (substr(str,5,5) %in% c("E","N")){
    sign <- 1
  } else {
    sign <- -1
  }
  deg <- sign*(as.numeric(substr(str,1,2)) + 0.1*as.numeric(substr(str,3,4)))
  return(deg)
}



for (i in 1:n){
  lon[i] <- f(s[i,7])
  lat[i] <- f(s[i,6])
}
