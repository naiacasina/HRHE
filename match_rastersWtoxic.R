# ========================================================================
# match_rastersWtoxic.R    -   Naia Ormaza Zulueta   -  Sep 2022-Sep 2023
# In this file:
# - We gather all the variables of interest for the analysis 
# - Read them into R through the terra package to check extents and resolutions
# - Match the resolution.
# - Match the extent.
# - Check CRS.
# - Create a raster file for each and save.
# ========================================================================

# Clear the environment
rm(list=ls()) 
# Load libraries
packages <- c("terra", "raster", "tidyverse", "rasterVis", "ncdf4", 
              "lattice", "foreign", "rworldmap", "readxl")
lapply(packages, require, character=TRUE)


# ------ Open (already processed) raster data ------
# Population layers
pop1 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/World-Pop/ppp_2020_1km_Aggregated.tif")
pop2 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/World-Pop/2020_2pt5_min_tif/gpw_v4_population_count_rev11_2020_2pt5_min.tif")
# Population projections
pop_projSSP2_2050 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/World-Pop/Projections/SPP2/SSP2_2050.tif")
pop_projSSP5_2050 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/World-Pop/Projections/SPP5/SSP5_2050.tif")
# GDP projections
gdp_projSSP2_2050 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/GDP/Projections/GDP_SSP2_1km/GDP2050_ssp2.tif")
gdp_projSSP5_2050 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/GDP/Projections/GDP_SSP5_1km/GDP2050_ssp5.tif")
# PM2.5 global estimates average over years [2016,2020]
airQuality <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Air Quality/Raster/airQuality.tif")
# BII
biodiv <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Biodiversity/lbii.asc")
# Heatstress - WBGT and HeatIndex year 2016
heat <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/heatstress103.tif")
hi <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/hi_annual_av.tif")
wbgt <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/wbgt_max.tif")
# Tree coverage 2000
treecov <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/Tree Coverage Raster/treecoverage_all.tif")
# Flood hazard map of the World - 10-year return period
flood <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Floods/floodMapGL_rp10y/floodMapGL_rp10y.tif")
# Banned or soon-to-be-banned pesticide spraying as of 2015
food.pest <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Pesticides/pesticides_all.tif")
# WFP HungerMap: Prevalence of insufficient food consumption
food.hm <- rast("/Users/naiacasina/Documents/ENVS/Hungermap/Processed Raster/hungermap_no_reprojected.tif")
# IPC food
food.ipc <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Food Security/IPC/World/Merged_IPC.tif")
# FCS food
food.fcs <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/fcscore_max.tif")
# Stunding prev food
food.st <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Food Security/Stunting/Codebooks/Stunting Prevalence [GeoTIFF]/IHME_LMIC_CGF_2000_2017_STUNTING_PREV_MEAN_2017_Y2020M01D08.TIF")
# Annual average monthly blue water scarcity (access to water) 1995-2005
w.suf <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Water/WS_blue_monthly_rasters/WSbl_monthly_30m/ws_avg/hdr.adf")
# Aridity Index
w.arid <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Water/Aridity Index and Evapotransportation/Global-AI_ET0_v3_annual/ai_v3_yr.tif")
# Access to any improved water (sanitation) 2017
w.san <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Water (building)/w_access.tif")
gdp <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/GDP/Gridded Map/GDP2005_1km.tif")
rwi <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/RWI/raster_rwi2.tif")

food.pest2 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Pesticides/pesticides_all_lower.tif")

# Choose heat projection
ssp <- "SSP585"
deg <- "28C"
heat_path1<- paste('/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/CHIRTS Projections/','Extremes_2030','_',ssp,'_',deg,'.tif',sep="")
heat_path2<- paste('/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/CHIRTS Projections/','Extremes_2050','_',ssp,'_',deg,'.tif',sep="")

heat_proj1 <- rast(heat_path1)
heat_proj2 <- rast(heat_path2)
heat_2016 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/CHIRTS Projections/Extremes_2016_28C.tif")

# Set to 0 any pixel in which the pop count < 1
pop1[pop1<1] <- 0
#pop2[pop2<1] <- 0
food.ipc[food.ipc>=5] <- NA
food.st[food.st>1] <- 1

# ------ Aggregation/Disaggregation ------
# Methods of interpolation: bilinear for continuous data and NN for categorical
newres <- 0.1
pop1 <- aggregate(pop1,newres/res(pop1),fun=sum, na.rm=TRUE)
pop2 <- aggregate(pop2,newres/res(pop2),fun=sum, na.rm=TRUE)
pop_projSSP2_2050 <- aggregate(pop_projSSP2_2050, newres/res(pop_projSSP2_2050), fun=sum, na.rm=T)
pop_projSSP5_2050 <- aggregate(pop_projSSP5_2050, newres/res(pop_projSSP5_2050), fun=sum, na.rm=T)
gdp_projSSP2_2050 <- aggregate(gdp_projSSP2_2050, newres/res(gdp_projSSP2_2050), fun=mean, na.rm=T)
gdp_projSSP5_2050 <- aggregate(gdp_projSSP5_2050, newres/res(gdp_projSSP5_2050), fun=mean, na.rm=T)
biodiv <- aggregate(biodiv,newres/res(biodiv),fun=mean, na.rm=TRUE)
flood <- aggregate(flood, newres/res(flood), fun=mean, na.rm=TRUE)
w.suf <- disagg(w.suf, res(w.suf)/newres, method="bilinear")
w.arid <- aggregate(w.arid, newres/res(w.arid), fun=mean, na.rm=TRUE)
heat <- aggregate(heat, newres/res(heat), fun=mean)
wbgt <- aggregate(wbgt, newres/res(wbgt), fun=mean)
heat_proj1 <- aggregate(heat_proj1, newres/res(heat_proj1), fun=mean)
heat_proj2 <- aggregate(heat_proj2, newres/res(heat_proj2), fun=mean)
heat_2016 <- aggregate(heat_2016, newres/res(heat_2016), fun=mean)
gdp <- aggregate(gdp, newres/res(gdp), fun=mean)
rwi <- aggregate(rwi, newres/res(rwi), fun=mean, na.rm=T)
# Already at the newres
airQuality <- aggregate(airQuality, fact=newres/res(airQuality), fun=mean)
treecov <- aggregate(treecov, newres/res(treecov), fun=mean, na.rm=T)
food.pest <- aggregate(food.pest, newres/res(food.pest), fun=mean, na.rm=T)
food.hm <- aggregate(food.hm, newres/res(food.hm), fun=median, na.rm=T)
food.ipc <- aggregate(food.ipc, newres/res(food.ipc), fun=max, na.rm=T)
food.fcs <- aggregate(food.fcs, newres/res(food.fcs), fun=max, na.rm=T)
food.st <- aggregate(food.st, newres/res(food.st), fun=mean, na.rm=T)
w.san <- aggregate(w.san, res(w.san)/newres, fun=mean)

food.pest2 <- aggregate(food.pest, newres/res(food.pest), fun=mean)
food.pest2 <- resample(food.pest2,pop1)
food.pest2[is.na(food.pest2)] <- 0
writeRaster(food.pest2, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_pest_lower.tif", overwrite=T)

# ------ Match extents ------
#ext(pop2) <- ext(w.san)
# Get the extent of pop1
ext <- ext(pop1)
reso <- res(pop1)

pop2 <- terra::crop(pop2, ext)
pop2 <- terra::resample(pop2, pop1, method="bilinear")

# USING POP1 FORM WORLD POP HUB
airQuality <- resample(airQuality,pop1)
biodiv <- resample(biodiv, pop1)
heat <- resample(heat,pop1)
wbgt <- resample(wbgt,pop1)
treecov <- resample(treecov,pop1)
flood <- resample(flood,pop1)
food.hm <- resample(food.hm,pop1)
food.ipc <- resample(food.ipc, pop1, method="near")
food.fcs <- resample(food.fcs, pop1, method="near")
food.st <- resample(food.st, pop1)
food.pest <- resample(food.pest,pop1)
w.suf <- resample(w.suf,pop1)
w.arid <- resample(w.arid,pop1)
w.san <- resample(w.san,pop1)
pop2 <- resample(pop2,pop1,method="med")
pop_projSSP2_2050 <- resample(pop_projSSP2_2050, pop1)
pop_projSSP5_2050  <- resample(pop_projSSP5_2050, pop1)
gdp_projSSP2_2050  <- resample(gdp_projSSP2_2050, pop1)
gdp_projSSP5_2050  <- resample(gdp_projSSP5_2050, pop1)
heat_proj1 <- resample(heat_proj1,pop1)
heat_proj2 <- resample(heat_proj2,pop1)
heat_2016 <- resample(heat_2016,pop1)
rwi <- resample(rwi, pop1)
gdp <- resample(gdp, pop1)

# USING GPW POPULATION
airQuality <- resample(airQuality,pop2)
biodiv <- resample(biodiv, pop2)
heat <- resample(heat,pop2)
wbgt <- resample(wbgt,pop2)
treecov <- resample(treecov,pop2)
flood <- resample(flood,pop2)
food.hm <- resample(food.hm,pop2)
food.ipc <- resample(food.ipc, pop2, method="near")
food.fcs <- resample(food.fcs, pop2, method="near")
food.st <- resample(food.st, pop2)
food.pest <- resample(food.pest,pop2)
w.suf <- resample(w.suf,pop2)
w.arid <- resample(w.arid,pop2)
w.san <- resample(w.san,pop2)
pop1 <- resample(pop1,pop2)
pop_projSSP2_2050 <- resample(pop_projSSP2_2050, pop2)
pop_projSSP5_2050  <- resample(pop_projSSP5_2050, pop2)
gdp_projSSP2_2050  <- resample(gdp_projSSP2_2050, pop2)
gdp_projSSP5_2050  <- resample(gdp_projSSP5_2050, pop2)
heat_proj1 <- resample(heat_proj1,pop2)
heat_proj2 <- resample(heat_proj2,pop2)
heat_2016 <- resample(heat_2016,pop2)
gdp <- resample(gdp, pop2)


# Mask heatmap with tree coverage
heat[treecov>60] <- 0
# Remove NAs and set as 0
airQuality[is.na(airQuality)] <- 0
flood[is.na(flood)] <- 0
heat[is.na(heat)] <- 0
wbgt[is.na(wbgt)] <- 0
biodiv[is.na(biodiv)] <- 0
w.suf[is.na(w.suf)] <- 0
w.arid[is.na(w.arid)] <- 0
w.san[is.na(w.san)] <- 0
food.hm[is.na(food.hm)] <- 0
food.ipc[is.na(food.ipc)] <- 0
food.fcs[is.na(food.fcs)] <- 0
food.st[is.na(food.st)] <- 0
food.pest[is.na(food.pest)] <- 0
pop1[is.na(pop1)] <- 0
pop2[is.na(pop2)] <- 0
pop_projSSP2_2050[is.na(pop_projSSP2_2050)] <- 0
pop_projSSP5_2050[is.na(pop_projSSP5_2050)] <- 0
gdp_projSSP2_2050[is.na(gdp_projSSP2_2050)] <- 0
gdp_projSSP5_2050[is.na(gdp_projSSP5_2050)] <- 0
heat_proj1[is.na(heat_proj1)] <- 0
heat_proj2[is.na(heat_proj2)] <- 0
heat_2016[is.na(heat_2016)] <- 0
gdp[is.na(gdp)] <- 0
rwi[is.na(rwi)] <- 0


# ------ Write raster files ------
writeRaster(airQuality, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/airQuality.tif", overwrite=TRUE)
writeRaster(biodiv, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/biodiv.tif", overwrite=TRUE)
writeRaster(heat, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/heat103_1.tif", overwrite=TRUE)
writeRaster(wbgt, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/wbgt2016_max.tif", overwrite=TRUE)
writeRaster(pop1, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/pop1.tif", overwrite=TRUE)
writeRaster(pop2, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/pop2.tif", overwrite=TRUE)
writeRaster(pop_projSSP2_2050, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/pop_projSSP2_2050.tif", overwrite=TRUE)
writeRaster(pop_projSSP5_2050, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/pop_projSSP5_2050.tif", overwrite=TRUE)
writeRaster(gdp_projSSP2_2050, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/gdp_projSSP2_2050.tif", overwrite=TRUE)
writeRaster(gdp_projSSP5_2050, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/gdp_projSSP5_2050.tif", overwrite=TRUE)
writeRaster(flood, filename= "/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/flood.tif", overwrite=TRUE)
writeRaster(food.hm, filename= "/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_hm.tif", overwrite=TRUE)
writeRaster(food.ipc, filename= "/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_IPC.tif", overwrite=TRUE)
writeRaster(food.fcs, filename= "/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_FCS.tif", overwrite=TRUE)
writeRaster(food.st, filename= "/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_ST.tif", overwrite=TRUE)
writeRaster(food.pest, filename= "/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_pest.tif", overwrite=TRUE)
writeRaster(w.san, filename= "/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/w_san.tif", overwrite=TRUE)
writeRaster(w.arid, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/w_ai.tif", overwrite=TRUE)
writeRaster(w.suf, filename= "/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/w_suf.tif", overwrite=TRUE)
writeRaster(gdp, filename= "/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/gdp.tif", overwrite=TRUE)
writeRaster(rwi, filename="/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/rwi.tif", overwrite=TRUE)
out_path1<- paste('/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/','Extremes_2030','_',ssp,'_',deg,'.tif',sep="")
writeRaster(heat_proj1,out_path1, overwrite=TRUE)
out_path2<- paste('/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/','Extremes_2050','_',ssp,'_',deg,'.tif',sep="")
writeRaster(heat_proj2,out_path2, overwrite=TRUE)
writeRaster(heat_2016, filename= "/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/Extremes_2016.tif", overwrite=TRUE)


