require(ncdf4)


readSFX <- function(file, vars){

  nvar = length(vars)

  ncid = nc_open(file)
  nx   = ncid$dim$xx$len
  ny   = ncid$dim$yy$len 
  
  pgd        = vector("list", nvar)
  names(pgd) = vars
  tmp        = array(dim=c(nx,ny)) 

  for (i in 1:nvar){
    varname = vars[i]
    tmp = ncvar_get(ncid, ncid$var[[varname]])
    pgd[[varname]] = tmp
  }
  nc_close(ncid)
  return(pgd)
}



# PGD 
pgd_vars     <- c("SAND","CLAY","FRAC_NATURE","ZS")
pgd_fao  <- readSFX(file="surfex_files/PGD_2D.nc",pgd_vars)
pgd_HWSD <- readSFX(file="surfex_files/PGD_hwsd.nc",pgd_vars)

zs <- pgd_fao$ZS
# PREP
prep_vars      <- c("WWILT1", "WSAT1", "WFC1")
prep_fao  <- readSFX(file="fao_hwsd/PREP_fao.nc",prep_vars)
prep_HWSD <- readSFX(file="fao_hwsd/PREP_HWSD_MOY.nc",prep_vars)

#### PLOTTING ####

require(fields)

pdf("figures/other/fao_clay.pdf", width=2.5, height=2)
par(mar=c(2,2,1.7,1))
image.plot(pgd_fao$CLAY,col=tim.colors(),main="FAO clay fraction",zlim=c(0.04,0.33),legend.width=0.5, legend.mar=4)
dev.off()

pdf("figures/other/hwsd_clay.pdf", width=2.5, height=2)
par(mar=c(2,2,1.7,1))
image.plot(pgd_HWSD$CLAY,col=tim.colors(),main="HWSD clay fraction",zlim=c(0.04,0.33),legend.width=0.5, legend.mar=4)
dev.off()


plotAllVars <- function(varList){
  listName <- deparse(substitute(varList))
  colo <- rev(tim.colors())

  for (i in 1:length(varList)){
    title <- sprintf("%s_%s",listName, names(varList)[i])
    pdf(paste(title,".pdf",sep=""))
    image.plot(varList[[i]], main=title)
    contour(zs, add=T, levels=c(0,2,10,50,100,200,500,1000))
    dev.off()
  }
  cmd1 <- sprintf("pdfunite %s_* %s.pdf", listName, listName)
  system(cmd1)
  cmd2 <- sprintf("rm %s_*", listName)
  system(cmd2)
}

