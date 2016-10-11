stas <- miDVHstations()
stas  <- stas[,match(c("STNR", "WMO_NO", "LON_DEC", "LAT_DEC", "AMSL", "ST_NAME"), names(stas))]
names(stas)  <- c( "STNR", "NUMBER", "LON", "LAT", "synop.HOH", "NAME")
stas <- stas[!is.na(stas$LAT),]
stas <- stas[!is.na(stas$LON),]
stas <- stas[stas$LAT>58,]
stas <- stas[stas$LON>8 & stas$LON<13,]
stas <- stas[!is.na(stas$NUMBER),]


#  SNR   STNR  LAT_DEC LON_DEC  AMSL  ST_NAME
