source("libLoc_precise.R")
library("miIO", lib.loc=libLoc)
stas <- miDVHstations()
stas  <- stas[,match(c("STNR", "WMO_NO", "LON_DEC", "LAT_DEC", "AMSL", "ST_NAME"), names(stas))]
names(stas)  <- c( "STNR", "NUMBER", "LON", "LAT", "synop.HOH", "NAME")
stas <- stas[!is.na(stas$LAT),]
stas <- stas[!is.na(stas$LON),]
stas <- stas[stas$LAT>58 & stas$LAT<65,]
stas <- stas[stas$LON>5 & stas$LON<13,]
stas <- stas[!is.na(stas$NUMBER),]

ii <- c(11, 12, 13 ,14,16,24,28,29,32,184,186,192,193,195,198,80,110)


these <- stas[ii,]

rownames(this) <- 1:nrow(this)

#  SNR   STNR  LAT_DEC LON_DEC  AMSL  ST_NAME

stationlist <- sprintf("%5i%7i%9.5f%9.5f%5i  '%s'", this$NUMBER,this$STNR,this$LAT,this$LON,this$synop.HOH,this$NAME)
