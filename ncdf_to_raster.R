# ========================================================================
# ncdf_to_raster.R    -   Naia Ormaza Zulueta   -  Apr 2022
# This file converts air quality pm2.5 data in netcdf format to a raster
# Analysis period: Mean over 2016-2020 yearly average values.
# file GTiff format
# ========================================================================

library(raster)
library(rasterVis)
library(ncdf4)
library(lattice)

ncpath <- "/Users/naiacasina/Documents/ENVS/Codes and Data/Air Quality/Global"
varname <- 'GWRPM25'

f_toxics <- "/Users/naiacasina/Documents/SEM2/Toxics Layer/Input layers/Air quality/V5GL02.HybridPM25.Global.201601-201612.nc"

setwd(ncpath)
f <- list.files(getwd()) 
ras <- lapply(f,raster, varname=varname, band=1) 
STACK1 <- stack(ras)
meanPM25 <- calc(STACK1, base::mean)

# quick view for the dataset
png("F:\\plot2019.png",
    height = 15,
    width = 20,
    units = 'cm',
    res = 1000)
print(levelplot(meanPM25))
dev.off()


outpath <- "/Users/naiacasina/Documents/ENVS/Codes and Data/Air Quality/Raster/"
outname <- "airQuality"
output <- paste(outpath, outname, ".tif", sep="")
# write raster to file
writeRaster(meanPM25,output,format = 'GTiff',overwrite = TRUE)

