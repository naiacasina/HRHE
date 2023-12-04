# ========================================================================
# Main_file.R    -   Naia Ormaza Zulueta   -  Sep 2022 - Sep 2023
# In this file:
# - We gather all the variables of interest for the analysis (matched already)
# - Create a common stack of all the data.
# - Make calculations in the converted df.
# - Create a raster file with the calculations and export it.
# - Make bivariate plots that capture both the HRV in the area & population
# ========================================================================

# Clear the environment
rm(list=ls()) 

# Load libraries
packages <- c("terra", "raster", "tidyverse", "rasterVis", "ncdf4", 
              "lattice", "foreign", "rworldmap", "data.table",
              "classInt", "patchwork", "matrixStats", "dplyr", "sf",
              "hutilscpp")
lapply(packages, require, character=TRUE)

# ------------------------ BIVARIATE PLOTTING FUNCTIONS ------------------------

# The function that produces the colour matrix
colmat <- function(nbreaks = 6, breakstyle = "fisher",
                   upperleft = "#0096EB", upperright = "#820050", 
                   bottomleft = "#BEBEBE", bottomright = "#FFE60F",
                   xlab = "x label", ylab = "y label", plotLeg = TRUE,
                   saveLeg = FALSE) {
  # TODO - replace any tidyr, dplyr etc. functions with data.table #
  library(tidyverse)
  require(ggplot2)
  require(classInt)
  library(dplyr)
  if (breakstyle == "sd") {
    warning("SD breaks style cannot be used.\nWill not always return the correct number of breaks.\nSee classInt::classIntervals() for details.\nResetting to quantile",
            call. = FALSE, immediate. = FALSE)
    breakstyle <- "quantile"}
  
  my.data <- seq(0, 1, .01)
  my.class <- classInt::classIntervals(my.data,
                                       n = nbreaks,
                                       style = breakstyle,
  )
  my.pal.1 <- classInt::findColours(my.class, c(upperleft, bottomleft))
  my.pal.2 <- classInt::findColours(my.class, c(upperright, bottomright))
  col.matrix <- matrix(nrow = 101, ncol = 101, NA)
  for (i in 1:101) {
    my.col <- c(paste(my.pal.1[i]), paste(my.pal.2[i]))
    col.matrix[102 - i, ] <- classInt::findColours(my.class, my.col)
  }
  # need to convert this to data.table at some stage.
  col.matrix.plot <- col.matrix %>%
    as.data.frame(.) %>% 
    dplyr::mutate("Y" = row_number()) %>%
    dplyr::mutate_at(.tbl = ., .vars = vars(starts_with("V")), .funs = list(as.character)) %>% 
    pivot_longer(data = ., cols = -Y, names_to = "X", values_to = "HEXCode") %>% 
    dplyr::mutate("X" = as.integer(sub("V", "", .$X))) %>%
    distinct(as.factor(HEXCode), .keep_all = TRUE) %>%
    mutate(Y = rev(.$Y)) %>% 
    dplyr::select(-c(4)) %>%
    dplyr::mutate("Y" = rep(seq(from = 1, to = nbreaks, by = 1), each = nbreaks),
           "X" = rep(seq(from = 1, to = nbreaks, by = 1), times = nbreaks)) %>%
    dplyr::mutate("UID" = row_number())
  # Use plotLeg if you want a preview of the legend
  if (plotLeg) {
    p <- ggplot(col.matrix.plot, aes(X, Y, fill = HEXCode)) +
      geom_tile() +
      scale_fill_identity() +
      coord_equal(expand = FALSE) +
      theme_void() +
      theme(aspect.ratio = 1,
            axis.title = element_text(size = 12, colour = "black",hjust = 0.5, 
                                      vjust = 1),
            axis.title.y = element_text(angle = 90, hjust = 0.5)) +
      xlab(bquote(.(xlab) ~  symbol("\256"))) +
      ylab(bquote(.(ylab) ~  symbol("\256")))
    print(p)
    assign(
      x = "BivLegend",
      value = p,
      pos = .GlobalEnv
    )
  }
  # Use saveLeg if you want to save a copy of the legend
  if (saveLeg) {
    ggsave(filename = "bivLegend.pdf", plot = p, device = "pdf",
           path = "./", width = 4, height = 4, units = "in",
           dpi = 300)
  }
  seqs <- seq(0, 100, (100 / nbreaks))
  seqs[1] <- 1
  col.matrix <- col.matrix[c(seqs), c(seqs)]
  attr(col.matrix, "breakstyle") <- breakstyle
  attr(col.matrix, "nbreaks") <- nbreaks
  return(col.matrix)
}

bivariate.map <- function(rasterx, rastery, colourmatrix = col.matrix,
                          export.colour.matrix = TRUE,
                          outname = paste0("colMatrix_rasValues", names(rasterx))) {
  # TO DO - replace raster with terra #
  require(raster)
  require(classInt)
  # export.colour.matrix will export a data.frame of rastervalues and RGB codes 
  # to the global environment outname defines the name of the data.frame
  quanx <- getValues(rasterx)
  tempx <- data.frame(quanx, quantile = rep(NA, length(quanx)))
  brks <- with(tempx, classIntervals(quanx,
                                     n = attr(colourmatrix, "nbreaks"),
                                     style = attr(colourmatrix, "breakstyle"))$brks)
  ## Add (very) small amount of noise to all but the first break
  ## https://stackoverflow.com/a/19846365/1710632
  brks[-1] <- brks[-1] + seq_along(brks[-1]) * .Machine$double.eps
  r1 <- within(tempx, quantile <- cut(quanx,
                                      breaks = brks,
                                      labels = 2:length(brks),
                                      include.lowest = TRUE))
  quantr <- data.frame(r1[, 2])
  quany <- getValues(rastery)
  tempy <- data.frame(quany, quantile = rep(NA, length(quany)))
  brksy <- with(tempy, classIntervals(quany,
                                      n = attr(colourmatrix, "nbreaks"),
                                      style = attr(colourmatrix, "breakstyle"))$brks)
  brksy[-1] <- brksy[-1] + seq_along(brksy[-1]) * .Machine$double.eps
  r2 <- within(tempy, quantile <- cut(quany,
                                      breaks = brksy,
                                      labels = 2:length(brksy),
                                      include.lowest = TRUE
  ))
  quantr2 <- data.frame(r2[, 2])
  as.numeric.factor <- function(x) {
    as.numeric(levels(x))[x]
  }
  col.matrix2 <- colourmatrix
  cn <- unique(colourmatrix)
  for (i in 1:length(col.matrix2)) {
    ifelse(is.na(col.matrix2[i]),
           col.matrix2[i] <- 1, col.matrix2[i] <- which(
             col.matrix2[i] == cn
           )[1]
    )
  }
  # Export the colour.matrix to data.frame() in the global env
  # Can then save with write.table() and use in ArcMap/QGIS
  # Need to save the output raster as integer data-type
  if (export.colour.matrix) {
    # create a dataframe of colours corresponding to raster values
    exportCols <- as.data.frame(cbind(
      as.vector(col.matrix2), as.vector(colourmatrix),
      t(col2rgb(as.vector(colourmatrix)))
    ))
    # rename columns of data.frame()
    colnames(exportCols)[1:2] <- c("rasValue", "HEX")
    # Export to the global environment
    assign(
      x = outname,
      value = exportCols,
      pos = .GlobalEnv
    )
  }
  cols <- numeric(length(quantr[, 1]))
  for (i in 1:length(quantr[, 1])) {
    a <- as.numeric.factor(quantr[i, 1])
    b <- as.numeric.factor(quantr2[i, 1])
    cols[i] <- as.numeric(col.matrix2[b, a])
  }
  r <- rasterx
  r[1:length(r)] <- cols
  return(r)
}
#=========================================================
# Choose heat projection
year <- "2050"
deg <- "28C"
heat_path_50_2 <- paste('/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/','Extremes_',year,'_SSP245_',deg,'.tif',sep="")
heat_path_50_5 <- paste('/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/','Extremes_',year,'_SSP585_',deg,'.tif',sep="")

# Define threshold type (loose vs. restrictive)
th_loose <- F
th_restr <- F

# Open raster data
airQuality <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/airQuality.tif")
biodiv <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/biodiv.tif")
if(th_loose){
  heat <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/heat_loose.tif")
}else if (th_restr){
  heat <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/heat_restrictive.tif")
}else{
  heat <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/heat103_1.tif")
}
hi <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/hi.tif")
wbgt <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/wbgt2016_max.tif")
pop1 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/pop1.tif")
pop2 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/pop2.tif")
#flood <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/flood.tif")
food.hm <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_hm.tif")
food.ipc <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_IPC.tif")
food.fcs <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_FCS.tif")
food.st <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_ST.tif")
if (th_loose){
  food.pest <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_pest_loose.tif")
}else{
  food.pest <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/food_pest.tif")}
w.san <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/w_san.tif")
w.ai <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/w_ai.tif")
w.suf <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/w_suf.tif")
gdp <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/gdp.tif")
rwi <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/rwi.tif")
# Projections data
heat_proj_2 <- rast(heat_path_50_2)
heat_proj_5 <- rast(heat_path_50_5)
heat_2016 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/Extremes_2016.tif")
gdp_2 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/gdp_projSSP2_2050.tif")
gdp_5 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/gdp_projSSP5_2050.tif")
pop_2 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/pop_projSSP2_2050.tif")
pop_5 <- rast("/Users/naiacasina/Documents/ENVS/Codes and Data/Prepared Rasters/pop_projSSP5_2050.tif")

# W\ flood
s <- c(airQuality, biodiv, heat, pop1, pop2, food.hm, food.pest, w.san, w.ai, w.suf, gdp, rwi, food.ipc, food.fcs, food.st, wbgt, heat_2016, heat_proj_2, heat_proj_5,
       gdp_2, gdp_5, pop_2, pop_5)

# ---------- ADD SHAPEFILES -----------
# Indigenous Lands ----
v <- vect("/Users/naiacasina/Documents/ENVS/Codes and Data/IPL_IndigenousPeoplesLands_2017/01_Data/IPL_IndigenousPeoplesLands_2017/IPL_2017.shp")
v <- project(v,s)
masked1 <- mask(crop(s,v),v)
# IndComs raster to df
df_ind <- as.data.frame(masked1, xy=TRUE)

# Mining sites ----
v <- vect("/Users/naiacasina/Documents/ENVS/Codes and Data/Non-Toxic Environments/Mining Sites/buffers_mining_polygons.shp")
v <- project(v,s)
masked2 <- mask(crop(s,v),v)
writeRaster(masked2, filename = "/Users/naiacasina/Documents/ENVS/Codes and Data/Non-Toxic Environments/Mining Sites/raster001.tif", overwrite=TRUE)
# Mining sites raster to df
df_ms <- as.data.frame(masked2, xy=TRUE)

# GPW ----
v <- vect("/Users/naiacasina/Documents/ENVS/Codes and Data/Non-Toxic Environments/GPW/Shapefile/gpw.shp")
v <- project(v,s)
masked3 <- mask(crop(s,v),v)
writeRaster(masked3, filename = "/Users/naiacasina/Documents/ENVS/Codes and Data/Non-Toxic Environments/GPW/raster_gpw.tif", overwrite=TRUE)
# Mining sites raster to df
df_gpw <- as.data.frame(masked3, xy=TRUE)


# Main raster to df
df <- as.data.frame(s, xy=TRUE)
cols <- c("x", "y", "airQuality", "lbii", "heat", "ppp_2020_1km_Aggregated", "gpw_v4_population_count_rev11_2020_2pt5_min",
          "class", "pesticides_all", "w_access", "w_ai", "ws_avg", "GDP2005_1km", "rwi", "Merged_IPC", "fcscore_max", "stunting", 
          "wbgt", "heat_16", "heat_50_245", "heat_50_585", "gdp_50_245", "gdp_50_585", "pop_50_245", "pop_50_585")

# Merge Ind Com
df <- merge(df, df_ind, by = c('x', 'y'), all.x=T)
# Ind Com Dummy
df$ind_com <- ifelse(is.na(df$ws_avg.y),0,1)
# Drop cols
df <- df[,c(1:25,49)]
# Rename columns
colnames(df)[1:length(cols)] <- cols

# Merge Mining sites
df <- merge(df, df_ms, by = c('x', 'y'), all.x=T )
# Mining Sites Dummy
df$mining <- ifelse(is.na(df$ws_avg.y),0,1)
# Drop cols
df <- df[,c(1:26,50)]
# Rename columns
colnames(df)[1:length(cols)] <- cols

# Merge GPW
df <- merge(df, df_gpw, by = c('x', 'y'), all.x=T )
# GPW Dummy
df$gpw <- ifelse(is.na(df$ws_avg.y),0,1)
# Drop cols
df <- df[,c(1:27,51)]
# Rename columns
colnames(df)[1:length(cols)] <- cols

save(df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainfile.R")
# save(df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainfile_pop2.R")
# save(df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainfile_loose.Rdata")
# save(df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainfile_restrictive.R")
# -------------------------------------------------
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainfile.R")

df <- na.omit(df)

# ------ ADD POC LOCATIONS -----
data <- read.csv(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Informal Settlements/UNHCR/Location of persons of concern.csv")
# Filter asylum seekers and returnees
data <- data[(data$pop_formula %in% c("Refugee", "IDP")), ]

coords <- data[,c("longitude_D", "latitude_D")]

# Create empty columns
# Dummy for refugee site
df$ref <- 0
df$dist_ref <- 0
  
for (i in 1:541){
  result <- match_nrst_haversine(data$latitude_D[i], data$longitude_D[i], df$y, df$x)
  df$ref[result$pos] <- 1 + df$ref[result$pos]
  df$dist_ref[result$pos] <- result$dist
  print(i)
}
for (i in 543:11562){
  result <- match_nrst_haversine(data$latitude_D[i], data$longitude_D[i], df$y, df$x)
  df$ref[result$pos] <- 1 + df$ref[result$pos]
  df$dist_ref[result$pos] <- result$dist
  print(i)
}
for (i in 11564:dim(data)[1]){
  result <- match_nrst_haversine(data$latitude_D[i], data$longitude_D[i], df$y, df$x)
  df$ref[result$pos] <- 1 + df$ref[result$pos]
  df$dist_ref[result$pos] <- result$dist
  print(i)
}
# Multiply by the population in that area
df$ref_pop <- ifelse(df$ref>0, df$ppp_2020_1km_Aggregated, 0)

if(th_restr){save(df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_main_poc_restrictive.R")}
if(th_loose){save(df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_main_poc_loose.Rdata")}
save(df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_main_poc.R")
# save(df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_main_poc_pop2.R")
load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_main_poc.R")
#save(df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_main_poc_loose.R")

# ABS THRESHOLDS
# WHO 2021 threshold for PM2.5 annual means
if(th_loose){
  df$air <- ifelse(df$airQuality>35, 1, 0)
}else{
  df$air <- ifelse(df$airQuality>5, 1, 0)
}
if(th_loose){
  df$biodiv <- ifelse(df$lbii<0.70,1,0)
}else if (th_restr){
  df$biodiv <- ifelse(df$lbii<0.90,1,0)
} else{
  df$biodiv <- ifelse(df$lbii<0.80,1,0)
}
# Water Scarcity > 2, Severe; >1.5 significant
if(th_restr){
  df$w_suf <- ifelse(df$ws_avg>1.5,1,0)
}else{
  df$w_suf <- ifelse(df$ws_avg>2,1,0)
}
df$safecl <- ifelse((df$heat>=1),1,0)
df$pest <- ifelse(df$pesticides_all>=5,1,0)
# Aridity index; <0.05 hyper arid. <0.2 arid; <0.5 semi-arid
if(th_loose){
  df$w_arid_i <- ifelse(df$w_ai*0.0001<=0.4,1,0)
}else if(th_restr){
  df$w_arid_i <- ifelse(df$w_ai*0.0001<=0.35,1,0)
}else{
  df$w_arid_i <- ifelse(df$w_ai*0.0001<=0.2,1,0)
}
# Turn percentage into index for access to san facilities
df$w_access_index <- (100-df$w_access)/100

# FOOD: Hungermap
# check missing values on HM and set stunting to 0
df[df$class<1, "stunting"] <- 0

# FOOD: IPC
#df[df$Merged_IPC<1, "class"] <- 0 # uncomment if sens analysis of IPC

# Hungermap classification
df$class <- ifelse(df$class>=1, df$class-1, df$class)
low_hm <- c(0,0.05,0.1,0.2,0.3,0.4)
high_hm <- c(0.05,0.1,0.2,0.3,0.4,0.4)

df$class_high <-replace(df$class,df$class==1,0.05)
df$class_low <-replace(df$class,df$class==1,0)
for (i in 2:length(high_hm)) {
  df$class_high <-replace(df$class_high,i-1<df$class_high&df$class_high<=i,high_hm[i])
  df$class_low <-replace(df$class_low,i-1<df$class_low&df$class_low<=i,low_hm[i])
}

df$ipc <- ifelse(df$Merged_IPC>=2, 1, 0)

# Trick to get HRV all over the map
df[,"ppp_2020_1km_Aggregated"] <- ifelse(df[,"ppp_2020_1km_Aggregated"]==0, 0.000001, df[,"ppp_2020_1km_Aggregated"])
#df[,"gpw_v4_population_count_rev11_2020_2pt5_min"] <- ifelse(df[,"gpw_v4_population_count_rev11_2020_2pt5_min"]==0, 0.000001, df[,"gpw_v4_population_count_rev11_2020_2pt5_min"])


# Pop count facing the HRV
df$pop_pest <- df$ppp_2020_1km_Aggregated*df$pest
df$pop_hm_low <- df$ppp_2020_1km_Aggregated*df$class_low
df$pop_hm_high <- df$ppp_2020_1km_Aggregated*df$class_high
df$pop_st <- df$ppp_2020_1km_Aggregated*df$stunting
df$pop_ipc <- df$ppp_2020_1km_Aggregated*df$ipc
df$pop_wsuf <- df$ppp_2020_1km_Aggregated*df$w_suf
df$pop_wsan <- df$ppp_2020_1km_Aggregated*df$w_access_index
df$pop_wai <- df$ppp_2020_1km_Aggregated*df$w_arid_i
df$pop_air <- df$ppp_2020_1km_Aggregated*df$air
df$pop_biodiv <- df$ppp_2020_1km_Aggregated*df$biodiv
df$pop_safecl <- df$ppp_2020_1km_Aggregated*df$safecl
df$pop_nontox <- df$ppp_2020_1km_Aggregated*(df$mining|df$gpw)
# If pesticides pop up, then HR to healthy food is violated
df$pop_food_l <-  ifelse(df$pop_pest>0, df$pop_pest, df$pop_hm_low)
df$pop_food_h <-  ifelse(df$pop_pest>0, df$pop_pest, df$pop_hm_high)
df$pop_food_st <- ifelse(df$pop_pest>0, df$pop_pest, df$pop_st)
df$pop_food_ipc <- ifelse(df$pop_pest>0, df$pop_pest, df$pop_ipc)
# If access to sufficient blue water is violated, then HR to water is violated
df$pop_water <- ifelse(df$pop_wai>0, df$pop_wai, df$pop_wsan)
#df$pop_water <- ifelse(df$pop_wsuf>0, df$pop_wsuf, df$pop_wsan)

# Define a function to calculate the HRV values for a row
df$HRV1 <- pmax(df$pop_air, df$pop_biodiv, df$pop_safecl, df$pop_water, df$pop_food_h)
df$HRV2 <- apply(df[, c("pop_air", "pop_biodiv", "pop_safecl", "pop_water", "pop_food_h")], 1, function(row) {
  sorted_pop <- sort(row, decreasing = TRUE)
  if (sorted_pop[1] > 0) sorted_pop[2] else 0
})
df$HRV3 <- apply(df[, c("pop_air", "pop_biodiv", "pop_safecl", "pop_water", "pop_food_h")], 1, function(row) {
  sorted_pop <- sort(row, decreasing = TRUE)
  if (sorted_pop[1] > 0) sorted_pop[3] else 0
})
df$HRV4 <- apply(df[, c("pop_air", "pop_biodiv", "pop_safecl", "pop_water", "pop_food_h")], 1, function(row) {
  sorted_pop <- sort(row, decreasing = TRUE)
  if (sorted_pop[1] > 0) sorted_pop[4] else 0
})
df$HRV5 <- apply(df[, c("pop_air", "pop_biodiv", "pop_safecl", "pop_water", "pop_food_h")], 1, function(row) {
  sorted_pop <- sort(row, decreasing = TRUE)
  if (sorted_pop[1] > 0) sorted_pop[5] else 0
})


# Save Data Frame

if(th_restr){save(df,file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_merged_restrictive.Rdata")}
if(th_loose){save(df,file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_merged_loose.Rdata")}
save(df,file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_merged.Rdata")
#save(df,file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_merged_pop2.Rdata")


#df_merged_loose, df_merged

# ------------- ADD REGIONS AND INCOME GROUPS -------------
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_merged.Rdata")
coords2country = function(points)
{  
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  indices$ADMIN  
}

big_number <- function(x, digits = 2) {
  abbr <- c("", "k", "M", "B", "T")
  mags <- 10^((seq_along(abbr) - 1) * 3)
  x_s <- signif(x, digits = digits)
  idx <- findInterval(abs(x_s), mags)
  idx <- ifelse(idx == 0, 1, idx)
  suffix <- purrr::map_chr(idx, ~abbr[.])
  if (any(abs(x_s) >= max(mags) * 1e3)) {
    warning("Standard abbreviations only go up to one trillion. Larger numbers will just be shown in terms of trillions; consider scientific notation instead.", call. = FALSE)
  }
  main <- x_s / mags[idx]
  paste0(main, suffix)
}


coords <- df[c("x","y")]
df$country <- coords2country(coords)


# Drop islands
df <- df[!(df$country%in%c("Cook Islands" ,"Federated States of Micronesia","Niue",
                             "Northern Mariana Islands","Tonga","Siachen Glacier",
                             "American Samoa","Kiribati","Norfolk Island","Samoa",
                             "Wallis and Futuna","French Polynesia","Bahrain",
                             "East Timor","Falkland Islands","Guernsey",
                             "Guinea Bissau","Jersey","Saint Helena","Seychelles",
                             "Vanuatu","Solomon Islands","New Caledonia","Fiji",
                             "Faroe Islands","The Bahamas","Trinidad and Tobago",
                             "Marshall Islands","Antigua and Barbuda","Aruba",
                             "British Virgin Islands","Grenada","Cayman Islands",
                             "Curacao","Guam","Malta","Aland","Saint Pierre and Miquelon" ,
                             "United States Virgin Islands",
                             "Anguilla","Sint Maarten","Saint Lucia","Saint Kitts and Nevis"
                             ,"French Southern and Antarctic Lands","British Indian Ocean Territory"
                             ,"Saint Vincent and the Grenadines","Heard Island and McDonald Islands",
                             "South Georgia and South Sandwich Islands", "Sao Tome and Principe",
                             "Comoros", "Mauritius", "Palau", "Taiwan", "Antarctica", "Bermuda", "Cape Verde",
                             "Isle of Man", "Greenland", "Dominica", "Barbados","Cyprus",
                             "Hong Kong S.A.R.", "Kosovo","Greenland", "Iceland", "Djibouti") ),]

if(th_restr){save(df,file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_merged_restrictive.Rdata")}
if(th_loose){save(df,file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_merged_loose.Rdata")}

# Omit countries with a smaller area than certain threshold
land <- read.csv(file="/Users/naiacasina/Documents/ENVS/Sensitivity/Area/land-area-km.csv")
# Take last year and only countries
land <- land[land$Year==2020&land$Code!="",]
countries_all <- data.frame(country=land$Entity)
# Save countries with a land area below the threshold
th <- 20000
countries_th <- land[land$Land.area..sq..km.<th, "Entity"]

df <- df[!df$country%in%countries_th, ]
df <- na.omit(df)
# WARNING: EDITS FOR WACCESS, BII and AI for the axes to be coherent with the story
df[, "w_access"] <- 100-df[, "w_access"]
df$lbii[df$lbii>1] <- 1
df[, "lbii"] <- 1-df[, "lbii"]
df$w_ai <- df$w_ai*0.0001
df[, "w_ai"] <- 1 - df[, "w_ai"]



# Normalize
normalized<-function(y) {
  x<-y[!is.na(y)]
  x<-(x - min(x)) / (max(x) - min(x))
  y[!is.na(y)]<-x
  # y<-(y - min(y)) / (max(y) - min(y))
  return(y)
}

# Normalized population
df$pop_norm <- normalized(df$ppp_2020_1km_Aggregated)



# Load WB file
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/wb.R")

# Left merge df with regions and income groups
df_m <- merge(wb_ir, df, by = 'country', all = TRUE)

save(df_m, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainHRV.R")
#save(df_m, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainHRV_pop2.R")
write.csv(df_m, file = "/Users/naiacasina/Documents/ENVS/Atlas/dataframe.csv")
save(df_m, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainHRV_loose.Rdata")
# save(df_m, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainHRV_restrictive.R")

# --------------- LOAD -------------
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainHRV.R")

df <- df_m
# Order df wrt coordinates to merge with NaNs afterwards
df <- df[order( df[,2], df[,3] ),]
df <- na.omit(df)
df$maxpopHRV_count <- apply(df[, c("HRV1", "HRV2", "HRV3", "HRV4", "HRV5")], 1, max)
# Create a new column 'maxpopHRVval' with the HRV column index of the maximum value
df$maxpopHRVval <- apply(df[, c("HRV1", "HRV2", "HRV3", "HRV4", "HRV5")], 1, function(x) {
  max_val <- max(x)
  if (max_val == 0) {
    0  # Set maxpopHRVval to 0 if maxpopHRV_count is 0
  } else {
    max_HRV <- tail(which(x == max_val), 1)  # Find the highest HRV column index with the maximum value
    if (is.na(max_HRV)) {
      0  # Set maxpopHRVval to 0 if no HRV column has the maximum value
    } else {
      max_HRV  # Assign the index of the highest HRV column
    }
  }
})

df$logpop <- log10(df$maxpopHRV_count)
df <- na.omit(df)
df[(df$logpop==-Inf),"logpop"] <- 0
df[(df$logpop<0),"logpop"] <- 0.0000001


# ------- Dfs into rasters -------
# Separate
df_rasterx <- df[,c('x','y','maxpopHRVval')]
df_rastery <- df[,c('x','y','logpop')]
# Ens
df_rast <- df[,c('x','y','maxpopHRVval','logpop')]

# ------ Create rasters -------
# Separate
rasterx <- rast(df_rasterx, type="xyz")
rastery <- rast(df_rastery, type="xyz")

# Ens
raster_HRV <- rast(df_rast, type="xyz")
# write rasters
writeRaster(raster_HRV, filename="/Users/naiacasina/Documents/ENVS/Results/Outcome/HRV_all.tif", overwrite=TRUE)
writeRaster(rasterx, filename="/Users/naiacasina/Documents/ENVS/Results/Outcome/HRV_all_layer1.tif", overwrite=TRUE)
writeRaster(rastery, filename="/Users/naiacasina/Documents/ENVS/Results/Outcome/HRV_all_layer2.tif", overwrite=TRUE)


# Reproject
rasterx <- rast("/Users/naiacasina/Documents/ENVS/Results/Outcome/HRV_all_layer1.tif")
rasterx_reprojected = project(rasterx, "EPSG:54009", method = "near")

writeRaster(rasterx_reprojected, filename="/Users/naiacasina/Documents/ENVS/Results/Outcome/HRV_all_layer1_repr.tif")


rastery <- raster("/Users/naiacasina/Documents/ENVS/Results/Outcome/HRV_all_layer2.tif")
res_rastery <- raster::res(rastery)

rastery_reprojected <- projectRaster(rastery, res=res_rasterx, crs="+proj=eck4 +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0",method="bilinear", over = TRUE)
writeRaster(rasterx_reprojected, filename="/Users/naiacasina/Documents/ENVS/Results/Outcome/HRV_all_layer2_repr.tif")


# --------------------- BIVARIATE PLOTTING ---------------------

# Clip to Globe
clipExt <- extent(-180, 180, -55, 70)
# Clip to Europe
clipExt_eur <- extent(-15, 40, 30, 68)
# Clip to North America and Central America
clipExt_NA <- extent(-160,-40, 5, 65)
# Clip to SE Asia
clipExt_SEA <- extent(45,140,-10,45)


# Define the number of breaks
nBreaks <- 6

# Create the colour matrix
col.matrixQ <- colmat(nbreaks = nBreaks, breakstyle = "equal",
                      xlab = "HRV", ylab = "Population", 
                      bottomright = "#F7900A", upperright = "#993A65",
                      bottomleft = "#44B360", upperleft = "#3A88B5",
                      saveLeg = FALSE, plotLeg = TRUE)

# create the bivariate raster
rasterx_br <- brick(rasterx)
rastery_br <- brick(rastery)

bivmapQ <- bivariate.map(rasterx = rasterx_br, rastery =  rastery_br,
                         export.colour.matrix = FALSE,
                         colourmatrix = col.matrixQ)

# Convert to dataframe for plotting with ggplot
bivMapDFQ <- setDT(as.data.frame(bivmapQ, xy = TRUE))
colnames(bivMapDFQ)[3] <- "BivValue"
bivMapDFQ <- melt(bivMapDFQ, id.vars = c("x", "y"),
                  measure.vars = "BivValue",
                  value.name = "bivVal",
                  variable.name = "Variable")

# Make the map using ggplot
map_q <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_y_continuous(breaks = seq(-75, 90, by = 20), 
                     labels = paste0(seq(-75, 90, 20), "°")) +
  scale_x_continuous(breaks = seq(-180,180,30), 
                     labels = paste0(seq(-180,180,30), "°")) +
  scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.2) +
  coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude")


map_q_eur <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_y_continuous(breaks = seq(30,68, by = 10), 
                     labels = paste0(seq(30,68, 10), "°")) +
  scale_x_continuous(breaks = seq(-15, 40, 5), 
                     labels = paste0(seq(-15, 40, 5), "°")) +
  scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.2) +
  coord_quickmap(expand = FALSE, xlim = clipExt_eur[1:2], ylim = clipExt_eur[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude")

map_q_na <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_y_continuous(breaks = seq(5, 65, by = 20), 
                     labels = paste0(seq(5, 65, 20), "°")) +
  scale_x_continuous(breaks = seq(-160,-40,20), 
                     labels = paste0(seq(-160,-40,20), "°")) +
  scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.2) +
  coord_quickmap(expand = FALSE, xlim = clipExt_NA[1:2], ylim = clipExt_NA[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude")

map_q_sea <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_y_continuous(breaks = seq(-10, 45, by = 20), 
                     labels = paste0(seq(-10, 45, 20), "°")) +
  scale_x_continuous(breaks = seq(45,140,20), 
                     labels = paste0(seq(45,140,20), "°")) +
  scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.2) +
  coord_quickmap(expand = FALSE, xlim = clipExt_SEA[1:2], ylim = clipExt_SEA[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude")


# -------------------- Build figures--------------------
fig_global <- {map_q + ggtitle("Human Rights Violations and Population")} + 
  inset_element(BivLegend + theme(plot.background = element_rect(fill = "white",
                                                                 colour = NA)), 
                left = 0.1, bottom = 0.2, right = 0.3, top = 0.5,
                align_to = "full") +
  plot_annotation(caption = "Capt")

fig_global <- {map_q + ggtitle("Human Rights Violations and  Population")}
fig_global

fig_eur <- {map_q_eur + ggtitle("HRV Europe")} + 
  inset_element(BivLegend + theme(plot.background = element_rect(fill = "white",
                                                                 colour = NA)), 
                left = 0.15, bottom = 0.77, right = 0.3, top = 0.92,
                align_to = "full") +
  plot_annotation(caption = "Capt")
fig_eur

fig_na <- {map_q_na + ggtitle("HRV North and Central America")} + 
  inset_element(BivLegend + theme(plot.background = element_rect(fill = "white",
                                                                 colour = NA)), 
                left = 0.73, bottom = 0.65, right = 0.98, top = 0.90,
                align_to = "full") +
  plot_annotation(caption = "Capt")
#fig_na

fig_sea <- {map_q_sea + ggtitle("HRV South and Southeast Asia")} + 
  inset_element(BivLegend + theme(plot.background = element_rect(fill = "white",
                                                                 colour = NA)), 
                left = 0.1, bottom = 0.2, right = 0.3, top = 0.5,
                align_to = "full") +
  plot_annotation(caption = "Capt")
#fig_sea

# Save
ggsave(plot = fig_global,
       filename = "Map_Global_pop2.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Main Maps/",
       width = 10, height = 6, units = "in",
       dpi = 320)
# Save restrictive/loose
ggsave(plot = fig_global,
       filename = "Map_Global_loose_arid.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Main Maps/",
       width = 10, height = 6, units = "in",
       dpi = 320)

# ggsave(plot = fig_global,
#        filename = "BivariatePlot_Global_ggsave.pdf",
#        device = "pdf", path = "/Users/naiacasina/Documents/ENVS/Results/Outcome/Final/",
#        width = 6, height = 7, units = "in",
#        dpi = 320)

ggsave(plot = fig_eur,
       filename = "Map_Europe.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Main Maps/",
       width = 6, height = 7, units = "in",
       dpi = 320)

# ggsave(plot = fig_eur,
#        filename = "BivariatePlot_Europe.pdf",
#        device = "pdf", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/Latest/",
#        width = 6, height = 7, units = "in",
#        dpi = 320)

ggsave(plot = fig_na,
       filename = "Map_NA.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Main Maps/",
       width = 6, height = 7, units = "in",
       dpi = 320)

# ggsave(plot = fig_na,
#        filename = "BivariatePlot_NorthCentralAmerica.pdf",
#        device = "pdf", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/Latest/",
#        width = 6, height = 7, units = "in",
#        dpi = 320)

ggsave(plot = fig_sea,
       filename = "Map_SEA.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Main Maps/",
       width = 6, height = 7, units = "in",
       dpi = 320)

ggsave(plot = fig_sea,
       filename = "BivariatePlot_SEAsia.pdf",
       device = "pdf", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/Latest/",
       width = 6, height = 7, units = "in",
       dpi = 320)



# ======================== Previous Code ============================

# Human Rights Violations
df$HRV <- rowSums(df[,c(11:14)])
df$logpop <- ifelse(log(df$ppp_2020_1km_Aggregated)>0,(df$ppp_2020_1km_Aggregated),0)


# Save Data Frame
save(df,file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/No Food/stack_layers.Rdata")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/No Food/stack_layers.Rdata")


# ------- Dfs into rasters -------
# Separate
df_rasterx <- df[,c('x','y','HRV')]
#df_rastery <- df[,c('x','y','ppp_2020_1km_Aggregated')]
df_rastery <- df[,c('x','y','logpop')]
# Ens
#df_rast <- df[,c('x','y','HRV','ppp_2020_1km_Aggregated')]
df_rast <- df[,c('x','y','HRV','logpop')]

# ------ Create rasters -------
# Separate
rasterx <- rast(df_rasterx, type="xyz")
rastery <- rast(df_rastery, type="xyz")
# Ens
raster_HRV <- rast(df_rast, type="xyz")
# write rasters
writeRaster(raster_HRV, filename="/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/HRV_2.tif", overwrite=TRUE)
writeRaster(rasterx, filename="/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/HRV_Layer1.tif", overwrite=TRUE)
writeRaster(rastery, filename="/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/Population_Layer2.tif", overwrite=TRUE)

rasterx <- rast("/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/HRV_Layer1.tif")
rastery <- rast("/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/Population_Layer2.tif")

# --------------------- BIVARIATE PLOTTING ---------------------


# Clip to Globe
clipExt <- extent(-180, 180, -55, 70)
# Clip to Europe
clipExt_eur <- extent(-15, 40, 30, 68)
# Clip to North America and Central America
clipExt_NA <- extent(-160,-40, 5, 65)
# Clip to SE Asia
clipExt_SEA <- extent(45,140,-10,45)


# Define the number of breaks
nBreaks <- 5

# Create the colour matrix
col.matrixQ <- colmat(nbreaks = nBreaks, breakstyle = "equal",
                      xlab = "HRV", ylab = "Population", 
                      bottomright = "#F7900A", upperright = "#993A65",
                      bottomleft = "#44B360", upperleft = "#3A88B5",
                      saveLeg = FALSE, plotLeg = TRUE)

# create the bivariate raster
rasterx_br <- brick(rasterx)
rastery_br <- brick(rastery)

bivmapQ <- bivariate.map(rasterx = rasterx_br, rastery =  rastery_br,
                         export.colour.matrix = FALSE,
                         colourmatrix = col.matrixQ)

# Convert to dataframe for plotting with ggplot
bivMapDFQ <- setDT(as.data.frame(bivmapQ, xy = TRUE))
colnames(bivMapDFQ)[3] <- "BivValue"
bivMapDFQ <- melt(bivMapDFQ, id.vars = c("x", "y"),
                  measure.vars = "BivValue",
                  value.name = "bivVal",
                  variable.name = "Variable")

# Make the map using ggplot
map_q <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_y_continuous(breaks = seq(-55, 75, by = 20), 
                     labels = paste0(seq(-55, 75, 20), "°")) +
  scale_x_continuous(breaks = seq(-180,180,30), 
                     labels = paste0(seq(-180,180,30), "°")) +
  scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.2) +
  coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude")


map_q_eur <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_y_continuous(breaks = seq(30,68, by = 10), 
                     labels = paste0(seq(30,68, 10), "°")) +
  scale_x_continuous(breaks = seq(-15, 40, 5), 
                     labels = paste0(seq(-15, 40, 5), "°")) +
  scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.2) +
  coord_quickmap(expand = FALSE, xlim = clipExt_eur[1:2], ylim = clipExt_eur[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude")

map_q_na <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_y_continuous(breaks = seq(5, 65, by = 20), 
                     labels = paste0(seq(5, 65, 20), "°")) +
  scale_x_continuous(breaks = seq(-160,-40,20), 
                     labels = paste0(seq(-160,-40,20), "°")) +
  scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.2) +
  coord_quickmap(expand = FALSE, xlim = clipExt_NA[1:2], ylim = clipExt_NA[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude")

map_q_sea <- ggplot(bivMapDFQ, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_y_continuous(breaks = seq(-10, 45, by = 20), 
                     labels = paste0(seq(-10, 45, 20), "°")) +
  scale_x_continuous(breaks = seq(45,140,20), 
                     labels = paste0(seq(45,140,20), "°")) +
  scale_fill_gradientn(colours = col.matrixQ, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.2) +
  coord_quickmap(expand = FALSE, xlim = clipExt_SEA[1:2], ylim = clipExt_SEA[3:4]) +
  theme(legend.position = "none",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude")

# Using different breaks algorithm -----------------------------------
# Create the colour matrix
col.matrixF <- colmat(nbreaks = nBreaks, breakstyle = "fisher",
                      xlab = "Temperature", ylab = "Precipiation", 
                      bottomright = "#F7900A", upperright = "#993A65",
                      bottomleft = "#44B360", upperleft = "#3A88B5",
                      saveLeg = FALSE, plotLeg = TRUE)

# create the bivariate raster
bivmapF <- bivariate.map(rasterx = rasterx_br, rastery = rastery_br,
                         export.colour.matrix = FALSE,
                         colourmatrix = col.matrixF)
bivMapDFF <- setDT(as.data.frame(bivmapF, xy = TRUE))
colnames(bivMapDFF)[3] <- "BivValue"
bivMapDFF <- melt(bivMapDFF, id.vars = c("x", "y"),
                  measure.vars = "BivValue",
                  value.name = "bivVal",
                  variable.name = "Variable")

# Make the map using ggplot
map_F <- ggplot(bivMapDFF, aes(x = x, y = y)) +
  geom_raster(aes(fill = bivVal)) +
  scale_y_continuous(breaks = seq(-20, 70, by = 20), 
                     labels = paste0(seq(-20, 70, 20), "°")) +
  scale_x_continuous(breaks = seq(50,175,25), 
                     labels = paste0(seq(50,175,25), "°")) +
  scale_fill_gradientn(colours = col.matrixF, na.value = "transparent") + 
  theme_bw() +
  theme(text = element_text(size = 10, colour = "black")) +
  borders(colour = "black", size = 0.2) +
  coord_quickmap(expand = FALSE, xlim = clipExt[1:2], ylim = clipExt[3:4]) +
  theme(legend.position = "left",
        plot.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  labs(x = "Longitude", y = "Latitude")


# -------------------- Build figures--------------------
fig_global <- {map_q + ggtitle("Human Rights Violations and Population")} + 
  inset_element(BivLegend + theme(plot.background = element_rect(fill = "white",
                                                                 colour = NA)), 
                left = 0.1, bottom = 0.2, right = 0.3, top = 0.5,
                align_to = "full") +
  plot_annotation(caption = "Capt")
#fig_global

fig_eur <- {map_q_eur + ggtitle("HRV Europe")} + 
  inset_element(BivLegend + theme(plot.background = element_rect(fill = "white",
                                                                 colour = NA)), 
                left = 0.15, bottom = 0.77, right = 0.3, top = 0.92,
                align_to = "full") +
  plot_annotation(caption = "Capt")
#fig_eur

fig_na <- {map_q_na + ggtitle("HRV North and Central America")} + 
  inset_element(BivLegend + theme(plot.background = element_rect(fill = "white",
                                                                 colour = NA)), 
                left = 0.73, bottom = 0.65, right = 0.98, top = 0.90,
                align_to = "full") +
  plot_annotation(caption = "Capt")
#fig_na

fig_sea <- {map_q_sea + ggtitle("HRV South and Southeast Asia")} + 
  inset_element(BivLegend + theme(plot.background = element_rect(fill = "white",
                                                                 colour = NA)), 
                left = 0.1, bottom = 0.2, right = 0.3, top = 0.5,
                align_to = "full") +
  plot_annotation(caption = "Capt")
#fig_sea

# Save
ggsave(plot = fig_global,
       filename = "BivariatePlot_Global_ggsave_logpop_equal.png",
       device = "png", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/",
       width = 6, height = 7, units = "in",
       dpi = 320)

ggsave(plot = fig_global,
       filename = "BivariatePlot_Global_ggsave.pdf",
       device = "pdf", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/",
       width = 6, height = 7, units = "in",
       dpi = 320)

ggsave(plot = fig_eur,
       filename = "BivariatePlot_Europe_logpop_equal.png",
       device = "png", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/",
       width = 6, height = 7, units = "in",
       dpi = 320)

ggsave(plot = fig_eur,
       filename = "BivariatePlot_Europe.pdf",
       device = "pdf", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/",
       width = 6, height = 7, units = "in",
       dpi = 320)

ggsave(plot = fig_na,
       filename = "BivariatePlot_NorthCentralAmerica_fisher.png",
       device = "png", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/",
       width = 6, height = 7, units = "in",
       dpi = 320)

ggsave(plot = fig_na,
       filename = "BivariatePlot_NorthCentralAmerica.pdf",
       device = "pdf", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/",
       width = 6, height = 7, units = "in",
       dpi = 320)

ggsave(plot = fig_sea,
       filename = "BivariatePlot_SEAsia_fisher.png",
       device = "png", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/",
       width = 6, height = 7, units = "in",
       dpi = 320)

ggsave(plot = fig_sea,
       filename = "BivariatePlot_SEAsia.pdf",
       device = "pdf", path = "/Users/naiacasina/Documents/COLORADO/ENVS/Results/Outcome/",
       width = 6, height = 7, units = "in",
       dpi = 320)


