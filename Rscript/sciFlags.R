### Science Flags ###

flag_names <- c( "FL_Non_Nom",      #         
            "FL_Scene_T",           #
            "FL_Barren",            #
            "FL_Topo_S",            #
            "FL_Topo_M",            #
            "FL_OW",                #
            "FL_Snow_Mix",          #
            "FL_Snow_Wet",          #
            "FL_Snow_Dry",          #
            "FL_Forest",            # 10
            "FL_Nominal",           #
            "FL_Frost",             #
            "FL_Ice",               #
            "FL_Wetlands",          #
            "FL_Flood_Prob",        #
            "FL_Urban_Low",         #
            "FL_Urban_High",        #
            "FL_Sand",              #
            "FL_Sea_Ice",           #
            "FL_Coast",             # 20
            "FL_Occur_T",           #
            "FL_Litter",            #
            "FL_PR",                #
            "FL_Intercep",          #
            "FL_External",          #
            "FL_Rain",              #
            "FL_TEC",               #
            "FL_TAU_FO",            #
            "FL_WINTER_FOREST",     #
            "FL_DUAL_RETR_FNO_FFO", # 30
            "spare_bit1",
            "spare_bit2" )             

nflags <- 1:32

flags <- list()
ntimes <- length(smos$time)
for (i in 1:32){
  flags[[flag_names[i]]] <- array(NA,dim=c(111,111,ntimes))
  for(l in 1:ntimes){
    for (k in 1:111){
      for(j in 1:111){
        flags[[i]][j,k,l] <- as.logical(intToBits(smos$sci_flag[j,k,l]))[i]
      }
    }
  }
}
flags[["time"]] <- smos$time


#plot_flag <- function(flags, time, flag){
#  print(range(flags[[flag]],na.rm=T))
#  II <- which(flags$time == time)
#  image.plot(smos$sm[,,II], col=rev(tim.colors(256)), main=names(flags)[flag])
#  image(flags[[flag]][,,II],col=c("#00000000", "#00000055"), add=T)
#  topo()
#}


#for (i in 1:30){
#  pdf(sprintf("figures/SMOS_evaluation/sciFlags/%02d_%s.pdf",i,flag_names[i]))
#  plot_flag(flags, "2016-10-09 06:00", i)
#  dev.off()
#}

