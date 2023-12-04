# ========================================================================
# second_panel_inequalities.R    -   Naia Ormaza Zulueta   -  Jan 2023
# This file computes the figures needed for the second panel. Steps:
#
#       1. Inequality VS median HRV
#       2. Sankey Diagram
#       3. Violin plots
#
# with Hunger Map data, using different sets of countries for heat,
# access to sanitation facilities and food analyses
# ========================================================================

# Clear the environment
rm(list=ls()) 
# Load libraries
packages <- c("terra", "raster", "tidyverse", "rasterVis", "ncdf4", 
              "lattice", "foreign", "rworldmap", "readxl",
              "modi", "sf","data.table", "networkD3", "qpcR")
lapply(packages, require, character=TRUE)

# Food layer: HM, IPC or FSC
food_n <- 2
food_layer <- c("class", "Merged_IPC", "fcscore_max")

dim_food <- food_layer[food_n]

# ------------------------------------------------
# --------------- First plot ---------------
# ------------------------------------------------
# 
# ---- LOAD ----
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainHRV.R")
df <- na.omit(df_m)

# Omit countries with a smaller area than certain threshold
land <- read.csv(file="/Users/naiacasina/Documents/ENVS/Sensitivity/Area/land-area-km.csv")
# Take last year and only countries
land <- land[land$Year==2020&land$Code!="",]
countries_all <- data.frame(country=land$Entity)
# Save countries with a land area below the threshold
th <- 100000
countries_th <- land[land$Land.area..sq..km.<th, "Entity"]

df <- df[!df$country%in%countries_th, ]
df <- na.omit(df)

# Create ineq df
c.all <- unique(df$country)
df_ineq <- data.frame(country=c.all)
df_ineq <- na.omit(df_ineq)


# ---- Countries in the food analysis ----
# food
if (food_n==1){
  load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_HM.R")
}else if (food_n==2){
  load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_IPC.R")
}else{
  load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_FSC.R")
}
c.food <- setdiff(c.food, c("Lebanon", "Vanuatu"))
c.food <- c.food[c.food%in%c.all]
df_ineq.food <- data.frame(country=c.food)
df_ineq.food <- na.omit(df_ineq.food)


# ---- Countries that have subnational stats for the access to san facilities ----
c_lmic <- read_excel(path="/Users/naiacasina/Documents/ENVS/Codes and Data/Water (building)/Countries_LMIC.xlsx")
countries_wacc <- colnames(c_lmic)
countries_wacc <- countries_wacc[countries_wacc%in%c.all]
countries_wacc <- countries_wacc[!countries_wacc %in% c(setdiff(countries_wacc,df_ineq$country))]
df_ineq.wa <- data.frame(country=countries_wacc)
df_ineq.wa <- na.omit(df_ineq.wa)


# ------ Drop countries where the right to safe climate has not been violated ------
countries <- unique(as.character(df$country))
df_heat <- data.frame(country=countries)
df_heat$tot_pop <- 0
df_heat$heat_pop <- 0

for (i in 1:length(countries)) {
  df_heat[i, "tot_pop"] <- sum(df[df$country==countries[i], "ppp_2020_1km_Aggregated"])
  df_heat[i, "heat_pop"] <- sum(df[df$country==countries[i]&df$safecl!=0, "ppp_2020_1km_Aggregated"])
}
df_heat$prop <- df_heat$heat_pop / df_heat$tot_pop
df_heat <- na.omit(df_heat)
countries_heat <- df_heat[df_heat$prop>=0.1, "country"]
countries_heat <- countries_heat[!countries_heat%in%c("Hong Kong S.A.R.", "Kosovo", "Greenland", "Iceland")]

df_ineq.heat <- data.frame(country=countries_heat)


# Save countries for water san access, food (IPC and FSC) and heat
save(countries_wacc, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/countries_wacc.R")
save(c.food, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_IPC.R")
load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_FSC.R")
#save(c.food_FSC, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_FSC.R")
save(countries_heat, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_heat.R")


# ---- MAIN INEQUALITY PLOT ----

fun_ineq1 <- function(x,qu,df,dimension) {
  quant <- weighted.quantile(x=df[df$country==x,dimension], 
                             w=df[df$country==x,c("pop_norm")], 
                             prob = qu)
  return(quant)
}

# Normalize
normalized<-function(y) {
  x<-y[!is.na(y)]
  x<-(x - min(x)) / (max(x) - min(x))
  y[!is.na(y)]<-x
  return(y)
}

df_ineq_tot <- df_ineq
dim <- "airQuality"
df_ineq_tot$median_pm25 <- apply(df_ineq,1,fun_ineq1,0.5,df, dim)
df_ineq_tot$median_pm25 <- normalized(df_ineq_tot[,"median_pm25"])
df_ineq_tot$q_20_pm25 <- apply(df_ineq,1,fun_ineq1,0.2,df, dim)
df_ineq_tot$q_80_pm25 <- apply(df_ineq,1,fun_ineq1,0.8,df, dim)
df_ineq_tot$diff_pm25 <- df_ineq_tot$q_80_pm25 - df_ineq_tot$q_20_pm25
df_ineq_tot$diff_pm25 <- normalized(df_ineq_tot[,"diff_pm25"])

# lbii !! I take the difference
dim <- "lbii"
df_ineq_tot$median_bii <- apply(df_ineq,1,fun_ineq1,0.5,df, dim)
df_ineq_tot$median_bii <- normalized(df_ineq_tot[,"median_bii"])
df_ineq_tot$q_20_bii <- apply(df_ineq,1,fun_ineq1,0.2,df, dim)
df_ineq_tot$q_80_bii <- apply(df_ineq,1,fun_ineq1,0.8,df, dim)
df_ineq_tot$diff_bii <- df_ineq_tot$q_80_bii-df_ineq_tot$q_20_bii
df_ineq_tot$diff_bii <- normalized(df_ineq_tot[,"diff_bii"])

# w_access !! I take the difference
df_ineq_tot.wa <- df_ineq.wa
dim <- "w_access"
df_ineq_tot.wa$median_waccess <- apply(df_ineq.wa,1,fun_ineq1,0.5,df, dim)
df_ineq_tot.wa$median_waccess <- normalized(df_ineq_tot.wa[,"median_waccess"])
df_ineq_tot.wa$q_20_waccess <- apply(df_ineq.wa,1,fun_ineq1,0.2,df, dim)
df_ineq_tot.wa$q_80_waccess <- apply(df_ineq.wa,1,fun_ineq1,0.8,df, dim)
df_ineq_tot.wa$diff_waccess <- df_ineq_tot.wa$q_80_waccess-df_ineq_tot.wa$q_20_waccess
df_ineq_tot.wa$diff_waccess <- normalized(df_ineq_tot.wa[,"diff_waccess"])

# w_suf !! I take the difference
dim <- "ws_avg"
df_ineq_tot$median_wsuf <- apply(df_ineq,1,fun_ineq1,0.5,df, dim)
df_ineq_tot$median_wsuf <- normalized(df_ineq_tot[,"median_wsuf"])
df_ineq_tot$q_20_wsuf <- apply(df_ineq,1,fun_ineq1,0.2,df, dim)
df_ineq_tot$q_80_wsuf <- apply(df_ineq,1,fun_ineq1,0.8,df, dim)
df_ineq_tot$diff_wsuf <- df_ineq_tot$q_80_wsuf-df_ineq_tot$q_20_wsuf
df_ineq_tot$diff_wsuf <- normalized(df_ineq_tot[,"diff_wsuf"])

# pesticides !! I take the difference
dim <- "pesticides_all"
df_ineq_tot$median_pest <- apply(df_ineq,1,fun_ineq1,0.5,df, dim)
df_ineq_tot$median_pest <- normalized(df_ineq_tot[,"median_pest"])
df_ineq_tot$q_20_pest <- apply(df_ineq,1,fun_ineq1,0.2,df, dim)
df_ineq_tot$q_80_pest <- apply(df_ineq,1,fun_ineq1,0.8,df, dim)
df_ineq_tot$diff_pest <- df_ineq_tot$q_80_pest-df_ineq_tot$q_20_pest
df_ineq_tot$diff_pest <- normalized(df_ineq_tot[,"diff_pest"])


# heat stress
df_ineq_tot.heat <- df_ineq.heat
dim <- "heat"
df_ineq_tot.heat$median_heat <- apply(df_ineq.heat,1,fun_ineq1,0.5,df, dim)
df_ineq_tot.heat$median_heat <- normalized(df_ineq_tot.heat[,"median_heat"])
df_ineq_tot.heat$q_20_heat <- apply(df_ineq.heat,1,fun_ineq1,0.2,df, dim)
df_ineq_tot.heat$q_80_heat <- apply(df_ineq.heat,1,fun_ineq1,0.8,df, dim)
df_ineq_tot.heat$diff_heat <- df_ineq_tot.heat$q_80_heat-df_ineq_tot.heat$q_20_heat
df_ineq_tot.heat$diff_heat <- normalized(df_ineq_tot.heat[,"diff_heat"])

# food
df <- na.omit(df)
df_ineq_tot.food <- df_ineq.food
df_ineq.food$country[df_ineq.food$country == "Côte d'Ivoire"] <- "Ivory Coast"
df_ineq_tot.food$country[df_ineq_tot.food$country == "Côte d'Ivoire"] <- "Ivory Coast"
df_ineq_tot.food$country[df_ineq_tot.food$country == "Tanzania"] <- "United Republic of Tanzania" 
df_ineq.food$country[df_ineq.food$country == "Tanzania"] <- "United Republic of Tanzania" 
dim <- "Merged_IPC"
df_ineq_tot.food$median_food <- apply(df_ineq.food,1,fun_ineq1,0.5,df, dim)
df_ineq_tot.food$median_food <- normalized(df_ineq_tot.food[,"median_food"])
df_ineq_tot.food$q_20_food <- apply(df_ineq.food,1,fun_ineq1,0.2,df, dim)
df_ineq_tot.food$q_80_food <- apply(df_ineq.food,1,fun_ineq1,0.8,df, dim)
df_ineq_tot.food$diff_food <- df_ineq_tot.food$q_80_food-df_ineq_tot.food$q_20_food
df_ineq_tot.food$diff_food <- normalized(df_ineq_tot.food[,"diff_food"])

# GDP
dim <- "GDP2005_1km"
df_ineq_tot$median_gdp <- apply(df_ineq,1,fun_ineq1,0.5,df, dim)
df_ineq_tot$q_20_gdp <- apply(df_ineq,1,fun_ineq1,0.2,df, dim)
df_ineq_tot$q_80_gdp <- apply(df_ineq,1,fun_ineq1,0.8,df, dim)
df_ineq_tot$diff_gdp <- df_ineq_tot$q_80_gdp/df_ineq_tot$q_20_gdp

rwi = TRUE
if (rwi==T){
  df_rwi <- load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_rwi.R")
  cols <- c("x", "y", "airQuality", "lbii", "heat", "ppp_2020_1km_Aggregated", "gpw_v4_population_count_rev11_2020_2pt5_min",
            "class", "pesticides_all", "w_access", "w_ai", "ws_avg", "GDP2005_1km", "raster_rwi2", "Merged_IPC", "fcscore_max", "stunting", 
            "wbgt", "heat_16", "heat_50_245", "heat_50_585", "gdp_50_245", "gdp_50_585", "pop_50_245", "pop_50_585")
  colnames(df_rwi) <- cols
  # Merge the dataframes
  df_merged <- df %>%
    left_join(dplyr::select(df_rwi, x, y, raster_rwi2), by = c("x", "y"))
  df_merged <- na.omit(df_merged)
  
  df_merged <- df_merged %>%
    group_by(country) %>%
    filter(n() >= 50) %>%  # Filter groups (countries) with counts >= 50
    ungroup() 
  
  c.all <- unique(df_merged$country)
  df_ineq <- data.frame(country=c.all)
  df_ineq <- na.omit(df_ineq)
  df_ineq.wa <- data.frame(country=countries_wacc[countries_wacc%in%c.all])
  df_ineq.wa <- na.omit(df_ineq.wa)
  df_ineq.heat <- data.frame(country=countries_heat[countries_heat%in%c.all])
  df_ineq.food <- data.frame(country=c.food[c.food%in%c.all])
  
  df_ineq_tot <- df_ineq
  dim <- "airQuality"
  df_ineq_tot$median_pm25 <- apply(df_ineq,1,fun_ineq1,0.5,df_merged, dim)
  df_ineq_tot$median_pm25 <- normalized(df_ineq_tot[,"median_pm25"])
  df_ineq_tot$q_20_pm25 <- apply(df_ineq,1,fun_ineq1,0.2,df_merged, dim)
  df_ineq_tot$q_80_pm25 <- apply(df_ineq,1,fun_ineq1,0.8,df_merged, dim)
  df_ineq_tot$diff_pm25 <- df_ineq_tot$q_80_pm25 - df_ineq_tot$q_20_pm25
  df_ineq_tot$diff_pm25 <- normalized(df_ineq_tot[,"diff_pm25"])
  
  # lbii !! I take the difference
  dim <- "lbii"
  df_ineq_tot$median_bii <- apply(df_ineq,1,fun_ineq1,0.5,df_merged, dim)
  df_ineq_tot$median_bii <- normalized(df_ineq_tot[,"median_bii"])
  df_ineq_tot$q_20_bii <- apply(df_ineq,1,fun_ineq1,0.2,df_merged, dim)
  df_ineq_tot$q_80_bii <- apply(df_ineq,1,fun_ineq1,0.8,df_merged, dim)
  df_ineq_tot$diff_bii <- df_ineq_tot$q_80_bii-df_ineq_tot$q_20_bii
  df_ineq_tot$diff_bii <- normalized(df_ineq_tot[,"diff_bii"])
  
  # w_access !! I take the difference
  df_ineq_tot.wa <- df_ineq.wa
  dim <- "w_access"
  df_ineq_tot.wa$median_waccess <- apply(df_ineq.wa,1,fun_ineq1,0.5,df_merged, dim)
  df_ineq_tot.wa$median_waccess <- normalized(df_ineq_tot.wa[,"median_waccess"])
  df_ineq_tot.wa$q_20_waccess <- apply(df_ineq.wa,1,fun_ineq1,0.2,df_merged, dim)
  df_ineq_tot.wa$q_80_waccess <- apply(df_ineq.wa,1,fun_ineq1,0.8,df_merged, dim)
  df_ineq_tot.wa$diff_waccess <- df_ineq_tot.wa$q_80_waccess-df_ineq_tot.wa$q_20_waccess
  df_ineq_tot.wa$diff_waccess <- normalized(df_ineq_tot.wa[,"diff_waccess"])
  
  # w_suf !! I take the difference
  dim <- "ws_avg"
  df_ineq_tot$median_wsuf <- apply(df_ineq,1,fun_ineq1,0.5,df_merged, dim)
  df_ineq_tot$median_wsuf <- normalized(df_ineq_tot[,"median_wsuf"])
  df_ineq_tot$q_20_wsuf <- apply(df_ineq,1,fun_ineq1,0.2,df_merged, dim)
  df_ineq_tot$q_80_wsuf <- apply(df_ineq,1,fun_ineq1,0.8,df_merged, dim)
  df_ineq_tot$diff_wsuf <- df_ineq_tot$q_80_wsuf-df_ineq_tot$q_20_wsuf
  df_ineq_tot$diff_wsuf <- normalized(df_ineq_tot[,"diff_wsuf"])
  
  # pesticides !! I take the difference
  dim <- "pesticides_all"
  df_ineq_tot$median_pest <- apply(df_ineq,1,fun_ineq1,0.5,df_merged, dim)
  df_ineq_tot$median_pest <- normalized(df_ineq_tot[,"median_pest"])
  df_ineq_tot$q_20_pest <- apply(df_ineq,1,fun_ineq1,0.2,df_merged, dim)
  df_ineq_tot$q_80_pest <- apply(df_ineq,1,fun_ineq1,0.8,df_merged, dim)
  df_ineq_tot$diff_pest <- df_ineq_tot$q_80_pest-df_ineq_tot$q_20_pest
  df_ineq_tot$diff_pest <- normalized(df_ineq_tot[,"diff_pest"])
  
  
  # heat stress
  df_ineq_tot.heat <- df_ineq.heat
  dim <- "heat"
  df_ineq_tot.heat$median_heat <- apply(df_ineq.heat,1,fun_ineq1,0.5,df_merged, dim)
  df_ineq_tot.heat$median_heat <- normalized(df_ineq_tot.heat[,"median_heat"])
  df_ineq_tot.heat$q_20_heat <- apply(df_ineq.heat,1,fun_ineq1,0.2,df_merged, dim)
  df_ineq_tot.heat$q_80_heat <- apply(df_ineq.heat,1,fun_ineq1,0.8,df_merged, dim)
  df_ineq_tot.heat$diff_heat <- df_ineq_tot.heat$q_80_heat-df_ineq_tot.heat$q_20_heat
  df_ineq_tot.heat$diff_heat <- normalized(df_ineq_tot.heat[,"diff_heat"])
  
  # food
  df_ineq_tot.food <- df_ineq.food
  df_ineq.food$country[df_ineq.food$country == "Côte d'Ivoire"] <- "Ivory Coast"
  df_ineq_tot.food$country[df_ineq_tot.food$country == "Côte d'Ivoire"] <- "Ivory Coast"
  df_ineq_tot.food$country[df_ineq_tot.food$country == "Tanzania"] <- "United Republic of Tanzania" 
  df_ineq.food$country[df_ineq.food$country == "Tanzania"] <- "United Republic of Tanzania" 
  dim <- "Merged_IPC"
  df_ineq_tot.food$median_food <- apply(df_ineq.food,1,fun_ineq1,0.5,df_merged, dim)
  df_ineq_tot.food$median_food <- normalized(df_ineq_tot.food[,"median_food"])
  df_ineq_tot.food$q_20_food <- apply(df_ineq.food,1,fun_ineq1,0.2,df_merged, dim)
  df_ineq_tot.food$q_80_food <- apply(df_ineq.food,1,fun_ineq1,0.8,df_merged, dim)
  df_ineq_tot.food$diff_food <- df_ineq_tot.food$q_80_food-df_ineq_tot.food$q_20_food
  df_ineq_tot.food$diff_food <- normalized(df_ineq_tot.food[,"diff_food"])
  
  # GDP
  dim <- "GDP2005_1km"
  df_ineq_tot$median_gdp <- apply(df_ineq,1,fun_ineq1,0.5,df_merged, dim)
  df_ineq_tot$q_20_gdp <- apply(df_ineq,1,fun_ineq1,0.2,df_merged, dim)
  df_ineq_tot$q_80_gdp <- apply(df_ineq,1,fun_ineq1,0.8,df_merged, dim)
  df_ineq_tot$diff_gdp <- df_ineq_tot$q_80_gdp/df_ineq_tot$q_20_gdp
  
  # RWI
  dim <- "rwi"
  df_ineq_tot$median_rwi <- apply(df_ineq,1,fun_ineq1,0.5,df_merged, dim)
  df_ineq_tot$q_20_rwi <- apply(df_ineq,1,fun_ineq1,0.2,df_merged, dim)
  df_ineq_tot$q_80_rwi <- apply(df_ineq,1,fun_ineq1,0.8,df_merged, dim)
  df_ineq_tot$diff_rwi <- df_ineq_tot$q_80_rwi/df_ineq_tot$q_20_rwi
  
}

# Merge with World Bank regions
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/wb.R")
df_ineq_m <- merge(wb_ir, df_ineq_tot, by = 'country')
df_ineq_m.food <- merge(wb_ir, df_ineq_tot.food, by="country")
df_ineq_m.wa <- merge(wb_ir, df_ineq_tot.wa, by="country")
df_ineq_m.heat <- merge(wb_ir, df_ineq_tot.heat, by="country")
df_ineq_m.food <- df_ineq_m.food[!duplicated(df_ineq_m.food), ]


save(df_ineq_m, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq30000_sens_all.R")
save(df_ineq_m.food, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq30000_sens_IPC.R")
save(df_ineq_m.wa, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq30000_sens_wa.R")
save(df_ineq_m.heat, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq30000_sens_heat.R")

# ----------------------- Plot ------------------------- 
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_all.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_IPC.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_wa.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_heat.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/wb.R")


df_ineq_m_pm <- df_ineq_m[!df_ineq_m$country%in%c("Greenland", "Bermuda"),]

df_ineq_m_bii <- df_ineq_m[df_ineq_m$median_bii>0,]


df_medians <- c(df_ineq_m$median_pm25,df_ineq_m_bii$median_bii,
                df_ineq_m$median_pest, df_ineq_m.wa$median_waccess,
                df_ineq_m$median_wsuf, df_ineq_m.heat$median_heat,
                df_ineq_m.food$median_food)

df_diffs <- c(df_ineq_m$diff_pm25,df_ineq_m_bii$diff_bii,
              df_ineq_m$diff_pest, df_ineq_m.wa$diff_waccess,
              df_ineq_m$diff_wsuf, df_ineq_m.heat$diff_heat,
              df_ineq_m.food$diff_food)

df_dims <- c(rep("pm2.5",dim(df_ineq_m)[1]), rep("bii",dim(df_ineq_m_bii)[1]),
             rep("pest",dim(df_ineq_m)[1]),
             rep("waccess",dim(df_ineq_m.wa)[1]), rep("wsuf",dim(df_ineq_m)[1]),
             rep("heat",dim(df_ineq_m.heat)[1]), rep("food",dim(df_ineq_m.food)[1]))

df_plots <- data.frame(medians=df_medians, diffs=df_diffs, dim=df_dims)

# ----------- Plot ------------

light <-  c("#67AB4F","#BF5841", "#D9751E","#3A88B5", "#5D9671","#D9981E",  "#864975")
light.ordered <-  c("#67AB4F","#864975", "#BF5841", "#D9981E","#D9751E", "#3A88B5","#5D9671")


p <- ggplot(df_plots, aes(x=medians,y=diffs, color=dim)) + 
  #geom_point()+
  geom_quantile(quantiles = 0.5, size = 1.7, alpha = 0.7)+
  #geom_quantile(quantiles = 0.5, colour = "grey", method = "rq", size = 1, alpha = 0.8)+
  #stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "red") + 
  #stat_summary(fun.y = mean, geom = "point", colour = "red") +
  xlab("Median") +
  ylab("Inequality") +
  ylim(0,1) +
  theme_minimal()+
  scale_color_manual(values=c(as.character(light.ordered[1:7])))

fig_first <- p + theme(axis.text.x = element_text(size=22), axis.text.y = element_text(size=22),
                       legend.position = "none",
                       axis.title.y = element_blank(), axis.title.x = element_blank())

#With x and y labels
fig_first <- p + theme(axis.text.x = element_text(size=22), axis.text.y = element_text(size=22),
                       axis.title.y = element_text(size = 17),
                       axis.title.x = element_text(size = 17))

fig_first
# Save
ggsave(plot = fig_first,
       filename = "ineq_vs_median20000_heat_corrected_IPC.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Second panel/First figure/",
       width = 8.5, height = 6, units = "in",
       dpi = 320)



# ------------------------------------------------
# --------------- Second: Sankey plot ---------------
# ------------------------------------------------

# ---- LOAD ----
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_sankey_countries.R")
c_ineq <- sort(unique(df_ineq_m$country))

# Merge food prevalence df with main df
df.m <- merge(df, food.df, by=c("x","y"), all.x=T)
save(df.m,file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_sankey_countries_IPC.R")

dims <- c("pm25", "bii", "waccess", "wsuf", "pest", "food", "heat")
dimension <- c("airQuality", "lbii", "w_access", "ws_avg", "pesticides_all", "world", "heat")

# --------- Create dataframe --------- 
df_sankey <- data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("country", "source", "target", "group", "value"))))

for (i in 1:length(c_ineq)) {
  # Top 20 richest
  df_rich <- df.m[(df.m$country==c_ineq[i]) & (df.m$GDP2005_1km>=df_ineq_m[df_ineq_m$country==c_ineq[i], "q_80_gdp"]), ]
  # Loop over each dimension
  for (j in 1:length(dims)) {
    if(dims[j]=="food"&(c_ineq[i]%in%c.food)){df_ineq_m.sankey <- df_ineq_m.food}
    else if(dims[j]=="waccess"&(c_ineq[i]%in%countries_wacc)){df_ineq_m.sankey <- df_ineq_m.wa}
    else if(dims[j]=="heat"&(c_ineq[i]%in%countries_heat)){df_ineq_m.sankey <- df_ineq_m.heat}
    else{df_ineq_m.sankey <- df_ineq_m}
    tot_pop <- sum(df_rich[df_rich$country==c_ineq[i], "ppp_2020_1km_Aggregated"  ], na.rm = T)
    
    # Best
    dim_char <- paste(as.character(dims[j]),"_Best", sep = "")
    q20 <- df_ineq_m.sankey[df_ineq_m.sankey$country==c_ineq[i], paste("q_20_",dims[j],sep="") ]
    ppl_below <- ifelse(sum(df_rich[, dimension[j]]<=q20) == 0, 0, sum(df_rich[(df_rich$country==c_ineq[i]) & (df_rich[, dimension[j]]<=q20), "ppp_2020_1km_Aggregated"], na.rm = T)) 
    prop_below <- ppl_below/tot_pop*100
    if(dims[j]=="food"&(!c_ineq[i]%in%c.food)){
      prop_below<-0
      print(i)}
    if(dims[j]=="waccess"&(!c_ineq[i]%in%countries_wacc)){prop_below<-0}
    if(dims[j]=="heat"&(!c_ineq[i]%in%countries_heat)){prop_below<-0}
    df_sankey <- rbind(df_sankey, c(c_ineq[i], "Top20", dim_char, "Top20", round(prop_below)))
    
    # Worst
    dim_char <- paste(as.character(dims[j]),"_Worst", sep = "")
    q80 <- df_ineq_m.sankey[df_ineq_m.sankey$country==c_ineq[i], paste("q_80_",dims[j],sep="") ]
    ppl_above <- ifelse(sum(df_rich[, dimension[j]]>q80) == 0, 0, sum(df_rich[(df_rich$country==c_ineq[i]) & (df_rich[, dimension[j]]>q80), "ppp_2020_1km_Aggregated"], na.rm = T))
    prop_above <- ppl_above/tot_pop*100
    if(dims[j]=="food"&(!c_ineq[i]%in%c.food)){prop_above<-0}
    if(dims[j]=="waccess"&(!c_ineq[i]%in%countries_wacc)){prop_above<-0}
    if(dims[j]=="heat"&(!c_ineq[i]%in%countries_heat)){prop_above<-0}
    df_sankey <- rbind(df_sankey, c(c_ineq[i], "Top20", dim_char, "Top20", round(prop_above)))
  }
  
  
  # Bottom 20 poorest
  df_poorest <- df.m[(df.m$country==c_ineq[i]) & (df.m$GDP2005_1km<=df_ineq_m[df_ineq_m$country==c_ineq[i], "q_20_gdp"]), ]
  # Loop over each dimension
  for (j in 1:length(dims)) {
    if(dims[j]=="food"&(c_ineq[i]%in%c.food)){
      df_ineq_m.sankey <- df_ineq_m.food}
    else if(dims[j]=="waccess"&(c_ineq[i]%in%countries_wacc)){
        df_ineq_m.sankey <- df_ineq_m.wa}
    else if(dims[j]=="heat"&(c_ineq[i]%in%countries_heat)){
      df_ineq_m.sankey <- df_ineq_m.heat}
    else{
          df_ineq_m.sankey <- df_ineq_m}
    tot_pop <- sum(df_poorest[df_poorest$country==c_ineq[i], "ppp_2020_1km_Aggregated"  ], na.rm = T)
    
    # Best
    dim_char <- paste(as.character(dims[j]),"_Best", sep = "")
    q20 <- df_ineq_m.sankey[df_ineq_m.sankey$country==c_ineq[i], paste("q_20_",dims[j],sep="") ]
    ppl_below <- ifelse(sum(df_poorest[, dimension[j]]<q20) == 0, 0, sum(df_rich[(df_poorest$country==c_ineq[i]) & (df_poorest[, dimension[j]]<q20), "ppp_2020_1km_Aggregated"], na.rm = T)) 
    prop_below <- ppl_below/tot_pop*100
    if(dims[j]=="food"&(!c_ineq[i]%in%c.food)){prop_below<-0}
    if(dims[j]=="waccess"&(!c_ineq[i]%in%countries_wacc)){prop_below<-0}
    if(dims[j]=="heat"&(!c_ineq[i]%in%countries_heat)){prop_below<-0}
    df_sankey <- rbind(df_sankey, c(c_ineq[i], "Bott20", dim_char, "Bott20", prop_below))
    
    # Worst
    dim_char <- paste(as.character(dims[j]),"_Worst", sep = "")
    q80 <- df_ineq_m.sankey[df_ineq_m.sankey$country==c_ineq[i], paste("q_80_",dims[j],sep="") ]
    ppl_above <- ifelse(sum(df_poorest[, dimension[j]]>=q80) == 0, 0, sum(df_poorest[(df_poorest$country==c_ineq[i]) & (df_poorest[, dimension[j]]>=q80), "ppp_2020_1km_Aggregated"], na.rm = T)) 
    prop_above <- ppl_above/tot_pop*100
    if(dims[j]=="food"&(!c_ineq[i]%in%c.food)){prop_above<-0}
    if(dims[j]=="waccess"&(!c_ineq[i]%in%countries_wacc)){prop_above<-0}
    if(dims[j]=="heat"&(!c_ineq[i]%in%countries_heat)){prop_above<-0}
    df_sankey <- rbind(df_sankey, c(c_ineq[i], "Bott20", dim_char, "Bott20", prop_above))
  }
  
}

colnames(df_sankey) <- c("country", "source", "target", "group", "value")
df_sankey$value <- as.integer(df_sankey$value)

# -------- Links ---------
links <- setDT(df_sankey[,2:5])
links_df <- df_sankey[,2:5]

# -------- Nodes ---------
nodes <- data.table(name=c(as.character(links$source), as.character(links$target)) %>% unique())

nodes_order <- c("Top20", "Bott20","bii_Best", "heat_Best", "pm25_Best", "waccess_Best", "wsuf_Best", "pest_Best", "food_Best", 
                 "bii_Worst", "heat_Worst", "pm25_Worst", "waccess_Worst", "wsuf_Worst", "pest_Worst",  "food_Worst")

nodes <- nodes[match(nodes_order, nodes$name), ]

nodes$value <- c(sum(links_df[links_df$source==nodes_order[1], "value"]), 
                 sum(links_df[links_df$source==nodes_order[2], "value"]), 
                 sum(links_df[links_df$target==nodes_order[3], "value"]),
                 sum(links_df[links_df$target==nodes_order[4], "value"]),
                 sum(links_df[links_df$target==nodes_order[5], "value"]),
                 sum(links_df[links_df$target==nodes_order[6], "value"]),
                 sum(links_df[links_df$target==nodes_order[7], "value"]),
                 sum(links_df[links_df$target==nodes_order[8], "value"]),
                 sum(links_df[links_df$target==nodes_order[9], "value"]),
                 sum(links_df[links_df$target==nodes_order[10], "value"]),
                 sum(links_df[links_df$target==nodes_order[11], "value"]),
                 sum(links_df[links_df$target==nodes_order[12], "value"]),
                 sum(links_df[links_df$target==nodes_order[13], "value"]),
                 sum(links_df[links_df$target==nodes_order[14], "value"]),
                 sum(links_df[links_df$target==nodes_order[15], "value"]),
                 sum(links_df[links_df$target==nodes_order[16], "value"]))

# Delete links with no value attached to them
missing_links <- which(links$value < 1 | is.na(links$value))

links <- links[!missing_links]

# Create IDs for sources and targets
links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

links <- as.data.frame(links)
nodes <- as.data.frame(nodes)


# customize color for each node
#light <- colour("light")
# light <-  c("#67AB4F","#BF5841", "#D9751E","#3A88B5", "#5D9671","#D9981E",  "#864975")

# Colors from the map: "#B0E6B2", "#D7DADE","#67AB4F","#BF5841", "#D9751E","#3A88B5", "#5D9671","#D9981E",  "#864975" , "#67AB4F","#BF5841", "#D9751E","#3A88B5", "#5D9671","#D9981E",  "#864975"

# Light colors: "#DF9D65", "#D7DADE","#77AADD","#EE8866","#EEDD88","#FFAABB","#99DDFF","#44BB99","#BBCC33" , "#77AADD","#EE8866","#EEDD88","#FFAABB","#99DDFF","#44BB99","#BBCC33"

# First node: #D0A594, 

#library(sankeyD3)

networkD3::sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget",
                         Value = "value", NodeID = "name", LinkGroup = "group",
                         colourScale = JS(
                           'd3.scaleOrdinal() 
                .range(["slategray", "#D7DADE","#67AB4F","#BF5841", "#D9751E","#3A88B5", "#5D9671","#D9981E",  "#864975" , "#67AB4F","#BF5841", "#D9751E","#3A88B5", "#5D9671","#D9981E",  "#864975"])'),
                         nodePadding = 8.5,fontSize=0, fontFamily="Arial", 
                         iterations=0)



# ------------------------------------------------
# --------------- Third: Violin plots ---------------
# ------------------------------------------------

# LOAD dataframe
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_sankey_countries_IPC.R")
# LOAD countries in water access and stunting prev. datasets
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/countries_wacc.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_IPC.R")

# Save sankey df to df_panel3
save(df.m, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel3_prev_IPC.R")
# --------
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel3_prev_IPC.R")

df <- df.m
# ------------------------ Poorest ----------------------

# Create dummy for the pixels that are below 20% poorest of GDP
fun_poorest <- function(x, df) {
  # if GDP is smaller than 20% poorest
  country <- as.character(x[1])
  # Return the highest number of HRV for the max pop
  return(ifelse(as.double(x[2]) < df[df$country==country,"q_20_gdp"], 1, 0))
}

df$dummy <- apply(df[,c("country","GDP2005_1km")], 1, fun_poorest, df_ineq_m)

df$poorest_ppl <- df[, "ppp_2020_1km_Aggregated"] * as.integer(df[, "dummy"])

# Check that it is indeed the 20 percent of the global population
frac <- sum(df$poorest_ppl, na.rm=T)/sum(df$ppp_2020_1km_Aggregated)*100

# Dataframe with poorest people
df_poorest <- df[df$dummy==1,]
df_poorest.food <- df_poorest[df_poorest$country%in%c.food,]
df_poorest.wa <- df_poorest[df_poorest$country%in%countries_wacc,]
df_poorest.heat <- df_poorest[df_poorest$country%in%countries_heat,]

save(df_poorest, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_poorest_prev_IPC.R")

# --- Load ---
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_poorest_prev_IPC.R")

c_all <- sort(unique(df_poorest$country))
# Compute 20th worst percentile for each dimension across countries
dims <- c("pm25", "bii", "waccess", "wsuf", "pest", "food", "heat")
dimension <- c("airQuality", "lbii", "w_access", "ws_avg", "pesticides_all", "world", "heat")
proportions <- c()
ci_lower <- c()
ci_upper <- c()
mean_dim <- c()


# Create dataframe
i <- 1
proportions <- c()
if(dims[i]=="food"){
  c.iter <- c.food} else if(dims[i]=="waccess"){
    c.iter <- countries_wacc
  } else if(dims[i]=="heat"){
    c.iter <- countries_heat
  }else{
    c.iter <- c_all}
for (j in 1:length(c.iter)) {
  # Take 80th worst perc value
  q80 <- df_ineq_m[df_ineq_m$country==c.iter[j], paste("q_80_",dims[i],sep="") ]
  # Take all the poorest population sum
  pop_tot_in <- sum(df_poorest[df_poorest$country==c.iter[j], "ppp_2020_1km_Aggregated"  ], na.rm = T)
  # Take all the poorest population sum that faces MORE than the 80th worst value
  freq_il_above <- sum(df_poorest[df_poorest$country==c.iter[j] & df_poorest[,dimension[i]]>=q80, "ppp_2020_1km_Aggregated" ], na.rm = T)
  # Is that percentage greater than 20%?
  #prop <- ifelse(freq_il_below==0&freq_il_above>0, 100, freq_il_above/pop_tot_in*100)
  prop <- freq_il_above/pop_tot_in*100
  proportions <- c(proportions, prop)
}

df_plot1 <- data.frame(pm25 = proportions)


for (i in 2:length(dims)) {
  proportions <- c()
  if(dims[i]=="food"){
    c.iter <- c.food
    df_ineq_m.all <- df_ineq_m.food
    df_poorest.all <- df_poorest.food} else if(dims[i]=="waccess"){
      c.iter <- countries_wacc
      df_ineq_m.all <- df_ineq_m.wa
      df_poorest.all <- df_poorest.wa
    } else if(dims[i]=="heat"){
      c.iter <- countries_heat
      df_ineq_m.all <- df_ineq_m.heat
      df_poorest.all <- df_poorest.heat
    }else{
      c.iter <- c_all
      df_ineq_m.all <- df_ineq_m
      df_poorest.all <- df_poorest}
  for (j in 1:length(c.iter)) {
    # Take 80th worst perc value
    q80 <- df_ineq_m.all[df_ineq_m.all$country==c.iter[j], paste("q_80_",dims[i],sep="") ]
    # Take all the poorest population sum
    pop_tot_in <- sum(df_poorest.all[df_poorest.all$country==c.iter[j], "ppp_2020_1km_Aggregated"  ], na.rm = T)
    # Take all the poorest population sum that faces MORE than the 80th worst value
    freq_il_above <- sum(df_poorest.all[df_poorest.all$country==c.iter[j] & df_poorest.all[,dimension[i]]>=q80, "ppp_2020_1km_Aggregated" ], na.rm = T)
    # Is that percentage greater than 20%?
    #prop <- ifelse(freq_il_below==0&freq_il_above>0, 100, freq_il_above/pop_tot_in*100)
    prop <- freq_il_above/pop_tot_in*100
    proportions <- c(proportions, prop)
  }
  df_plot1 <- qpcR:::cbind.na(df_plot1, proportions)
}
dims <- c("pm25", "bii", "waccess", "wsuf", "pest", "food", "heat")
colnames(df_plot1) <- dims

save(df_plot1, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_plot1_prev_heat.R")

# --------- Bootstrap loop ---------
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_plot1_prev_IPC.R")
iter <- 10000
indices <- c(1:length(c_all))
indices.food <- c(1:length(c.food))
indices.wa <- c(1:length(countries_wacc))
indices.heat <- c(1:length(countries_heat))

# Containers for the coefficients
df.pm25 <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.bii <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.waccess <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.wsuf <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.pest <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.food <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.heat <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))

for (i in 1:iter) {
  sample_i <- sample(x= indices, size=length(indices), replace = T)
  sample_i.food <- sample(x=indices.food, size=length(indices.food), replace = T)
  sample_i.wa <- sample(x= indices.wa, size=length(indices.wa), replace = T)
  sample_i.heat <- sample(x= indices.heat, size=length(indices.heat), replace = T)
  model_boot.pm25 <- lm(df_plot1[sample_i, dims[1]] ~ 1)
  model_boot.bii <- lm(df_plot1[sample_i, dims[2]] ~ 1) 
  model_boot.waccess <- lm(df_plot1[sample_i.wa, dims[3]] ~ 1) 
  model_boot.wsuf <- lm(df_plot1[sample_i, dims[4]] ~ 1) 
  model_boot.pest <- lm(df_plot1[sample_i, dims[5]] ~ 1) 
  model_boot.food <- lm(df_plot1[sample_i.food, dims[6]] ~ 1) 
  model_boot.heat <- lm(df_plot1[sample_i.heat, dims[7]] ~ 1) 
  
  ci.pm25 <- confint(model_boot.pm25, level=0.95)
  ci.bii <- confint(model_boot.bii, level=0.95)
  ci.waccess <- confint(model_boot.waccess, level=0.95)
  ci.wsuf <- confint(model_boot.wsuf, level=0.95)
  ci.pest <- confint(model_boot.pest, level=0.95)
  ci.food <- confint(model_boot.food, level=0.95)
  ci.heat <- confint(model_boot.heat, level=0.95)
  
  df.pm25 <- rbind(df.pm25, c(model_boot.pm25$coefficients[[1]], ci.pm25[1], ci.pm25[2]))
  df.bii <- rbind(df.bii, c(model_boot.bii$coefficients[[1]], ci.bii[1], ci.bii[2]))
  df.waccess <- rbind(df.waccess, c(model_boot.waccess$coefficients[[1]], ci.waccess[1], ci.waccess[2]))
  df.wsuf <- rbind(df.wsuf, c(model_boot.wsuf$coefficients[[1]], ci.wsuf[1], ci.wsuf[2]))
  df.pest <- rbind(df.pest, c(model_boot.pest$coefficients[[1]], ci.pest[1], ci.pest[2]))
  df.food <- rbind(df.food, c(model_boot.food$coefficients[[1]], ci.food[1], ci.food[2]))
  df.heat <- rbind(df.heat, c(model_boot.heat$coefficients[[1]], ci.heat[1], ci.heat[2]))
  
}

colnames(df.pm25) <- c("mean","lower","upper")
colnames(df.bii) <- c("mean","lower","upper")
colnames(df.waccess) <- c("mean","lower","upper")
colnames(df.wsuf) <- c("mean","lower","upper")
colnames(df.pest) <- c("mean","lower","upper")
colnames(df.food) <- c("mean","lower","upper")
colnames(df.heat) <- c("mean","lower","upper")


df.food_alt <- data.frame(mean=rep(NA,), lower= , upper= )


# Df with the means of all
df.means <- data.frame(dim=rep(dims, each=iter), mean=c(df.pm25$mean, df.bii$mean, df.waccess$mean, df.wsuf$mean, df.pest$mean, df.food$mean, df.heat$mean), 
                       lower = c(df.pm25$lower, df.bii$lower, df.waccess$lower, df.wsuf$lower, df.pest$lower, df.food$lower,df.heat$lower),
                       upper = c(df.pm25$upper, df.bii$upper, df.waccess$upper, df.wsuf$upper, df.pest$upper, df.food$upper,df.heat$upper))

save(df.means, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_means1_prev_heat.R")


# ---- Plot ----
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_means1.R")

# Change order so that hm is last
df.means[df.means$dim=="bii", "dim"] <- "a_bii" 
df.means[df.means$dim=="heat", "dim"] <- "b_heat" 
df.means[df.means$dim=="pm25", "dim"] <- "c_pm25" 
df.means[df.means$dim=="waccess", "dim"] <- "d_waccess" 
df.means[df.means$dim=="wsuf", "dim"] <- "e_wsuf" 
df.means[df.means$dim=="pest", "dim"] <- "f_pest" 
df.means[df.means$dim=="hm", "dim"] <- "g_food" 

#light <- colour("light")

light <-  c("#67AB4F","#BF5841", "#D9751E","#3A88B5", "#5D9671","#D9981E",  "#864975")

#light<-c(light(6),light(6))
stats <- df.means %>% 
  group_by(dim) %>%
  summarise(Mean = mean(mean), SD = sd(mean),
            CI_L = Mean - (SD * 2.576)/sqrt(6),
            CI_U = Mean + (SD * 2.576)/sqrt(6))

fig_poorest <- ggplot() +
  geom_violin(df.means, mapping = aes(x = dim, y = mean, fill=dim, color=dim), size=0.2, alpha=0.7) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange",
               colour = "red") +
  geom_point(stats, mapping = aes(dim, Mean), colour="white") +
  geom_errorbar(stats, mapping = aes(x = dim, ymin = CI_L, ymax = CI_U), width = 0.2, colour="white", size=0.7) +
  ggtitle("Poorest pop facing the worst conditions") +
  ylab("")+
  xlab("Dimension") +
  ylim(0,100) +
  geom_hline(yintercept=20, linetype="dashed", color = "grey", size=2, alpha=0.9)+
  theme_minimal() + 
  scale_fill_manual(values=c(as.character(light[1:7])))+ 
  scale_color_manual(values=c(as.character(light[1:7])))

fig_poorest <- fig_poorest + theme(axis.text.x = element_text(size=22), axis.text.y = element_text(size=22),
                                   plot.title = element_text(size=22),
                                   axis.title.y = element_blank(), axis.title.x = element_blank())

fig_poorest
ggsave(plot = fig_poorest,
       filename = "poorest20perc_IPC_heat.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/",
       width = 9, height = 7, units = "in",
       dpi = 420)


# ------------------------
# ------------------------ Indigenous Lands ----------------------------
# ------------------------

# Read gpkg files with areas
world = st_read("/Users/naiacasina/Documents/ENVS/Codes and Data/IPL_IndigenousPeoplesLands_2017/area_world.gpkg")
ind_lands <- st_read("/Users/naiacasina/Documents/ENVS/Codes and Data/IPL_IndigenousPeoplesLands_2017/area_ind_com.gpkg")
# gpkg to area
iso3 <- world$iso3
areas  <- world$area_world
c_world <- world$name
world <- data.frame(country=iso3, c_name=c_world, area_country=areas)

c_names <- ind_lands$Name_
areas <- ind_lands$area_ind
ind_lands <- data.frame(country=c_names, area=areas)
# Check that all country iso3 names in indigenous lands are in the world df
inters <- intersect(iso3, c_names)
length(inters)==length(c_names)

# Only keep countries with Indigenous Lands
world <- world[world$country%in%c_names,]

# Merge IL df to have the country name
merger <- merge(ind_lands,world,by="country", all.x=T)
merger$frac <- merger$area/merger$area_country

# Edit country names for the merger with the MAIN df
merger[merger$c_name=="Lao People's Democratic Republic", "c_name"] <- "Laos"
merger[merger$c_name=="Libyan Arab Jamahiriya", "c_name"] <- "Libya"
merger[merger$c_name=="Russian Federation", "c_name"] <- "Russia"
merger[merger$c_name=="Côte d'Ivoire", "c_name"] <- "Ivory Coast"
merger[merger$c_name=="Congo", "c_name"] <- "Republic of the Congo"
merger[merger$c_name=="Iran (Islamic Republic of)", "c_name"] <- "Iran"


# Keep countries that have at least 10% of each
c_threshold <- merger[merger$frac<0.9&merger$frac>0.1, "c_name"]

# Consider only countries in c_threshold from the main dataset to compute statistics
# Quantile data: df_ineq_m
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_all.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_IPC.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_wa.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_heat.R")

# Main df plus countries
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel3_prev_heat.R")

# All countries in the main df
c_all <- unique(df$country)

# Check intersection
inters <- sort(intersect(c_all, c_threshold))

# Take only countries with at least 10% of IL & NIL
df_ind <- df[df$country%in%c_threshold, ]

save(df_ind, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_ind_prev_IPC.R")

# --- Load ---
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_ind_prev_IPC.R")

c_all <- inters
countries_wacc.ind <- countries_wacc[countries_wacc%in%inters]
c.food.ind <- c.food[c.food%in%inters]
countries_heat.ind <- countries_heat[countries_heat%in%inters]

df_ind.food <- df_ind[df_ind$country%in%c.food.ind,]
df_ind.wa <- df_ind[df_ind$country%in%countries_wacc.ind,]
df_ind.heat <- df_ind[df_ind$country%in%countries_heat.ind,]

dims <- c("pm25", "bii", "waccess", "wsuf", "pest", "food", "heat")
dimension <- c("airQuality", "lbii", "w_access", "ws_avg", "pesticides_all", "world", "heat")

# Create dataframe
df_plot2 <- data.frame()
i <- 1
proportions <- c()
if(dims[i]=="food"){
  c.iter <- c.food.ind} else if(dims[i]=="waccess"){
    c.iter <- countries_wacc.ind
  } else if(dims[i]=="heat"){
    c.iter <- countries_heat.ind
  }else{
    c.iter <- c_all}
for (j in 1:length(c.iter)) {
  # Take 80th worst perc value
  q80 <- df_ineq_m[df_ineq_m$country==c.iter[j], paste("q_80_",dims[i],sep="") ]
  # Take all the poorest population sum
  pop_tot_in <- sum(df_ind[df_ind$country==c.iter[j] & df_ind$ind_com==1, "ppp_2020_1km_Aggregated"  ], na.rm = T)
  # Take all the poorest population sum that faces MORE than the 80th worst value
  freq_il_above <- sum(df_ind[df_ind$country==c.iter[j] & df_ind$ind_com==1 & df_ind[,dimension[i]]>=q80, "ppp_2020_1km_Aggregated" ], na.rm = T)
  # Is that percentage greater than 20%?
  #prop <- ifelse(freq_il_below==0&freq_il_above>0, 100, freq_il_above/pop_tot_in*100)
  prop <- freq_il_above/pop_tot_in*100
  proportions <- c(proportions, prop)
}

df_plot2 <- data.frame(pm25 = proportions)


for (i in 2:length(dims)) {
  proportions <- c()
  if(dims[i]=="food"){
    c.iter <- c.food.ind
    df_ineq_m.all <- df_ineq_m.food
    df_ind.all <- df_ind.food} else if(dims[i]=="waccess"){
      c.iter <- countries_wacc.ind
      df_ineq_m.all <- df_ineq_m.wa
      df_ind.all <- df_ind.wa
    } else if(dims[i]=="heat"){
      c.iter <- countries_heat.ind
      df_ineq_m.all <- df_ineq_m.heat
      df_ind.all <- df_ind.heat
    }else{
      c.iter <- c_all
      df_ineq_m.all <- df_ineq_m
      df_ind.all <- df_ind}
  for (j in 1:length(c.iter)) {
    # Take 80th worst perc value
    q80 <- df_ineq_m.all[df_ineq_m.all$country==c.iter[j], paste("q_80_",dims[i],sep="") ]
    # Take all the poorest population sum
    pop_tot_in <- sum(df_ind.all[df_ind.all$country==c.iter[j] & df_ind.all$ind_com==1, "ppp_2020_1km_Aggregated"], na.rm = T)
    # Take all the poorest population sum that faces MORE than the 80th worst value
    freq_il_above <- sum(df_ind.all[df_ind.all$country==c.iter[j] & df_ind.all$ind_com==1 & df_ind.all[,dimension[i]]>=q80, "ppp_2020_1km_Aggregated" ], na.rm = T)
    # Is that percentage greater than 20%?
    #prop <- ifelse(freq_il_below==0&freq_il_above>0, 100, freq_il_above/pop_tot_in*100)
    prop <- freq_il_above/pop_tot_in*100
    proportions <- c(proportions, prop)
  }
  df_plot2 <- qpcR:::cbind.na(df_plot2, proportions)
}
dims <- c("pm25", "bii", "waccess", "wsuf", "pest", "food", "heat")
colnames(df_plot2) <- dims

save(df_plot2, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_plot2_prev_IPC_heat.R")



# --------- Bootstrap loop ---------
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_plot2_prev_IPC.R")
iter <- 10000
indices <- c(1:length(c_all))
indices.food <- c(1:length(c.food.ind))
indices.wa <- c(1:length(countries_wacc.ind))
indices.heat <- c(1:length(countries_heat.ind))

# Containers for the coefficients
df.pm25 <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.bii <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.waccess <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.wsuf <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.pest <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.food <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.heat <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))

for (i in 1:iter) {
  sample_i <- sample(x=indices, size=length(indices), replace = T)
  sample_i.food <- sample(x=indices.food, size=length(indices.food), replace = T)
  sample_i.wa <- sample(x=indices.wa, size=length(indices.wa), replace = T)
  sample_i.heat <- sample(x=indices.heat, size=length(indices.heat), replace = T)
  model_boot.pm25 <- lm(df_plot2[sample_i, dims[1]] ~ 1)
  model_boot.bii <- lm(df_plot2[sample_i, dims[2]] ~ 1) 
  model_boot.waccess <- lm(df_plot2[sample_i.wa, dims[3]] ~ 1) 
  model_boot.wsuf <- lm(df_plot2[sample_i, dims[4]] ~ 1) 
  model_boot.pest <- lm(df_plot2[sample_i, dims[5]] ~ 1) 
  model_boot.food <- lm(df_plot2[sample_i.food, dims[6]] ~ 1) 
  model_boot.heat <- lm(df_plot2[sample_i, dims[7]] ~ 1) 
  
  ci.pm25 <- confint(model_boot.pm25, level=0.95)
  ci.bii <- confint(model_boot.bii, level=0.95)
  ci.waccess <- confint(model_boot.waccess, level=0.95)
  ci.wsuf <- confint(model_boot.wsuf, level=0.95)
  ci.pest <- confint(model_boot.pest, level=0.95)
  ci.food <- confint(model_boot.food, level=0.95)
  ci.heat <- confint(model_boot.heat, level=0.95)
  
  df.pm25 <- rbind(df.pm25, c(model_boot.pm25$coefficients[[1]], ci.pm25[1], ci.pm25[2]))
  df.bii <- rbind(df.bii, c(model_boot.bii$coefficients[[1]], ci.bii[1], ci.bii[2]))
  df.waccess <- rbind(df.waccess, c(model_boot.waccess$coefficients[[1]], ci.waccess[1], ci.waccess[2]))
  df.wsuf <- rbind(df.wsuf, c(model_boot.wsuf$coefficients[[1]], ci.wsuf[1], ci.wsuf[2]))
  df.pest <- rbind(df.pest, c(model_boot.pest$coefficients[[1]], ci.pest[1], ci.pest[2]))
  df.food <- rbind(df.food, c(model_boot.food$coefficients[[1]], ci.food[1], ci.food[2]))
  df.heat <- rbind(df.heat, c(model_boot.heat$coefficients[[1]], ci.heat[1], ci.heat[2]))
  
}

colnames(df.pm25) <- c("mean","lower","upper")
colnames(df.bii) <- c("mean","lower","upper")
colnames(df.waccess) <- c("mean","lower","upper")
colnames(df.wsuf) <- c("mean","lower","upper")
colnames(df.pest) <- c("mean","lower","upper")
colnames(df.food) <- c("mean","lower","upper")
colnames(df.heat) <- c("mean","lower","upper")

# Df with the means of all
df.means <- data.frame(dim=rep(dims, each=iter), mean=c(df.pm25$mean, df.bii$mean, df.waccess$mean, df.wsuf$mean, df.pest$mean, df.food$mean, df.heat$mean), 
                       lower = c(df.pm25$lower, df.bii$lower, df.waccess$lower, df.wsuf$lower, df.pest$lower, df.food$lower,df.heat$lower),
                       upper = c(df.pm25$upper, df.bii$upper, df.waccess$upper, df.wsuf$upper, df.pest$upper, df.food$upper,df.heat$upper))

save(df.means, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_means2_prev_IPC_heat.R")


# ---- Plot ----
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_means2_prev_IPC.R")

df.means[df.means$dim=="bii", "dim"] <- "a_bii" 
df.means[df.means$dim=="heat", "dim"] <- "b_heat" 
df.means[df.means$dim=="pm25", "dim"] <- "c_pm25" 
df.means[df.means$dim=="waccess", "dim"] <- "d_waccess" 
df.means[df.means$dim=="wsuf", "dim"] <- "e_wsuf" 
df.means[df.means$dim=="pest", "dim"] <- "f_pest" 
df.means[df.means$dim=="food", "dim"] <- "g_food" 

#light <- colour("light")

light <-  c("#67AB4F","#BF5841", "#D9751E","#3A88B5", "#5D9671","#D9981E",  "#864975")



stats <- df.means %>% 
  group_by(dim) %>%
  summarise(Mean = mean(mean), SD = sd(mean),
            CI_L = Mean - (SD * 2.576)/sqrt(6),
            CI_U = Mean + (SD * 2.576)/sqrt(6))

fig_ind <- ggplot() +
  geom_violin(df.means, mapping = aes(x = dim, y = mean, fill=dim, color=dim), size=0.2, alpha=0.7) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange",
               colour = "red") +
  geom_point(stats, mapping = aes(dim, Mean), colour="white") +
  geom_errorbar(stats, mapping = aes(x = dim, ymin = CI_L, ymax = CI_U), width = 0.2, colour="white", size=0.7) +
  ggtitle("People in IL facing the worst conditions") +
  ylab("")+
  xlab("Dimension") +
  ylim(0,100) +
  geom_hline(yintercept=20, linetype="dashed", color = "grey", size=2) +
  theme_minimal() + 
  scale_color_manual(values=c(as.character(light[1:7]))) +
  scale_fill_manual(values=c(as.character(light[1:7])))

fig_ind <- fig_ind + theme(axis.text.x = element_text(size=22), axis.text.y = element_text(size=22),
                           plot.title = element_text(size=22),
                           axis.title.y = element_blank(), axis.title.x = element_blank())

fig_ind
ggsave(plot = fig_ind,
       filename = "indigenous_land_IPC_heat.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/",
       width = 9, height = 7, units = "in",
       dpi = 420)



# ------------------------ POC Locations ----------------------------

load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/refugees.R")

# Check number of "Location of persons of concern" for each country
c_ref <- unique(df[df$ref!=0, "country"])

sum_ref <- c()

for (i in 1:length(c_ref)) {
  s <- sum(df[df$country==c_ref[i], "ref"])
  sum_ref <- c(sum_ref, s)
}

df_ref <- data.frame(country=c_ref, ref=sum_ref)

# Choose threshold for number of ref camps
th <- 50

c_th <- df_ref[df_ref$ref>=th,]
c_th <- c_th$country

# Quantile data: df_ineq_m
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_all.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_IPC.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq20000_sens_wa.R")
# Main df plus countries
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel3_prev_IPC.R")

df_merge <- merge(df,df.m,by=c("x","y"))
df <- df_merge
df <- df[,c(1:48,64)]
colnames(df)[which(names(df) == "country.x")] <- "country"
colnames(df)[which(names(df) == "airQuality.x")] <- "airQuality"
colnames(df)[which(names(df) == "lbii.x")] <- "lbii"
colnames(df)[which(names(df) == "w_access.x")] <- "w_access"
colnames(df)[which(names(df) == "ws_avg.x")] <- "ws_avg"
colnames(df)[which(names(df) == "pesticides_all.x")] <- "pesticides_all"
colnames(df)[which(names(df) == "heat.x")] <- "heat"
# Take only countries with at least 10% of IL & NIL
df_ref <- df[df$country%in%c_th, ]

save(df_ref, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_ref_prev50_IPC.R")

# --- Load ---
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_ref_prev50_IPC.R")

# Countries in the dataframe for POC Locations
c_all <- c_th
countries_wacc.poc <- countries_wacc[countries_wacc%in%c_th]
c.food.poc <- c.food[c.food%in%c_th]
countries_heat.poc <- countries_heat[countries_heat%in%c_th]

df_ref.food <- df_ref[df_ref$country%in%c.food.poc,]
df_ref.wa <- df_ref[df_ref$country%in%countries_wacc.poc,]
df_ref.heat <- df_ref[df_ref$country%in%countries_heat.poc,]

dims <- c("pm25", "bii", "waccess", "wsuf", "pest", "food", "heat")
dimension <- c("airQuality", "lbii", "w_access", "ws_avg", "pesticides_all", "world", "heat")

# Create dataframe
df_plot3 <- data.frame()
i <- 1
proportions <- c()
if(dims[i]=="food"){
  c.iter <- c.food.poc} else if(dims[i]=="waccess"){
    c.iter <- countries_wacc.poc
  } else if(dims[i]=="heat"){
    c.iter <- countries_heat.poc
  }else{
    c.iter <- c_all}
for (j in 1:length(c.iter)) {
  # Take 80th worst perc value
  q80 <- df_ineq_m[df_ineq_m$country==c.iter[j], paste("q_80_",dims[i],sep="") ]
  # Take all the poorest population sum
  pop_tot_in <- sum(df_ref[df_ref$country==c.iter[j] & df_ref$ref==1, "ppp_2020_1km_Aggregated.x"  ], na.rm = T)
  # Take all the poorest population sum that faces MORE than the 80th worst value
  freq_il_above <- sum(df_ref[df_ref$country==c.iter[j] &df_ref$ref==1 & df_ref[,dimension[i]]>=q80, "ppp_2020_1km_Aggregated.x" ], na.rm = T)
  # Is that percentage greater than 20%?
  #prop <- ifelse(freq_il_below==0&freq_il_above>0, 100, freq_il_above/pop_tot_in*100)
  prop <- freq_il_above/pop_tot_in*100
  proportions <- c(proportions, prop)
}

df_plot3 <- data.frame(pm25 = proportions)


for (i in 2:length(dims)) {
  proportions <- c()
  if(dims[i]=="food"){
    c.iter <- c.food.poc
    df_ineq_m.all <- df_ineq_m.food
    df_ref.all <- df_ref.food} else if(dims[i]=="waccess"){
      c.iter <- countries_wacc.poc
      df_ineq_m.all <- df_ineq_m.wa
      df_ref.all <- df_ref.wa
    } else if(dims[i]=="heat"){
      c.iter <- countries_heat.poc
      df_ineq_m.all <- df_ineq_m.heat
      df_ref.all <- df_ref.heat
    }else{
      c.iter <- c_all
      df_ineq_m.all <- df_ineq_m
      df_ref.all <- df_ref}
  for (j in 1:length(c.iter)) {
    # Take 80th worst perc value
    q80 <- df_ineq_m.all[df_ineq_m.all$country==c.iter[j], paste("q_80_",dims[i],sep="") ]
    # Take all the poorest population sum
    pop_tot_in <- sum(df_ref.all[df_ref.all$country==c.iter[j] & df_ref.all$ref==1, "ppp_2020_1km_Aggregated.x"  ], na.rm = T)
    # Take all the poorest population sum that faces MORE than the 80th worst value
    freq_il_above <- sum(df_ref.all[df_ref.all$country==c.iter[j] &df_ref.all$ref==1 & df_ref.all[,dimension[i]]>=q80, "ppp_2020_1km_Aggregated.x" ], na.rm = T)
    # Is that percentage greater than 20%?
    prop <- freq_il_above/pop_tot_in*100
    proportions <- c(proportions, prop)
  }
  df_plot3 <- qpcR:::cbind.na(df_plot3, proportions)
}
dims <- c("pm25", "bii", "waccess", "wsuf", "pest", "food", "heat")
colnames(df_plot3) <- dims

save(df_plot3, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_plot3_prev50_IPC_heat.R")


# --------- Bootstrap loop ---------
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_plot3_prev50_IPC.R")
iter <- 10000
indices <- c(1:length(c_all))
indices.food <- c(1:length(c.food.poc))
indices.wa <- c(1:length(countries_wacc.poc))
indices.heat <- c(1:length(countries_heat.poc))

# Containers for the coefficients
df.pm25 <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.bii <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.waccess <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.wsuf <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.pest <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.food <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))
df.heat <- data.frame(matrix(ncol=3,nrow=0, dimnames=list(NULL, c("mean", "lower", "upper"))))

for (i in 1:iter) {
  sample_i <- sample(x=indices, size=length(indices), replace = T)
  sample_i.food <- sample(x=indices.food, size=length(indices.food), replace = T)
  sample_i.wa <- sample(x=indices.wa, size=length(indices.wa), replace = T)
  sample_i.heat <- sample(x=indices.heat, size=length(indices.heat), replace = T)
  model_boot.pm25 <- lm(df_plot3[sample_i, dims[1]] ~ 1)
  model_boot.bii <- lm(df_plot3[sample_i, dims[2]] ~ 1) 
  model_boot.waccess <- lm(df_plot3[sample_i.wa, dims[3]] ~ 1) 
  model_boot.wsuf <- lm(df_plot3[sample_i, dims[4]] ~ 1) 
  model_boot.pest <- lm(df_plot3[sample_i, dims[5]] ~ 1) 
  model_boot.food <- lm(df_plot3[sample_i.food, dims[6]] ~ 1) 
  model_boot.heat <- lm(df_plot3[sample_i.heat, dims[7]] ~ 1) 
  
  ci.pm25 <- confint(model_boot.pm25, level=0.95)
  ci.bii <- confint(model_boot.bii, level=0.95)
  ci.waccess <- confint(model_boot.waccess, level=0.95)
  ci.wsuf <- confint(model_boot.wsuf, level=0.95)
  ci.pest <- confint(model_boot.pest, level=0.95)
  ci.food <- confint(model_boot.food, level=0.95)
  ci.heat <- confint(model_boot.heat, level=0.95)
  
  df.pm25 <- rbind(df.pm25, c(model_boot.pm25$coefficients[[1]], ci.pm25[1], ci.pm25[2]))
  df.bii <- rbind(df.bii, c(model_boot.bii$coefficients[[1]], ci.bii[1], ci.bii[2]))
  df.waccess <- rbind(df.waccess, c(model_boot.waccess$coefficients[[1]], ci.waccess[1], ci.waccess[2]))
  df.wsuf <- rbind(df.wsuf, c(model_boot.wsuf$coefficients[[1]], ci.wsuf[1], ci.wsuf[2]))
  df.pest <- rbind(df.pest, c(model_boot.pest$coefficients[[1]], ci.pest[1], ci.pest[2]))
  df.food <- rbind(df.food, c(model_boot.food$coefficients[[1]], ci.food[1], ci.food[2]))
  df.heat <- rbind(df.heat, c(model_boot.heat$coefficients[[1]], ci.heat[1], ci.heat[2]))
  
}

colnames(df.pm25) <- c("mean","lower","upper")
colnames(df.bii) <- c("mean","lower","upper")
colnames(df.waccess) <- c("mean","lower","upper")
colnames(df.wsuf) <- c("mean","lower","upper")
colnames(df.pest) <- c("mean","lower","upper")
colnames(df.food) <- c("mean","lower","upper")
colnames(df.heat) <- c("mean","lower","upper")

# Df with the means of all
df.means <- data.frame(dim=rep(dims, each=iter), mean=c(df.pm25$mean, df.bii$mean, df.waccess$mean, df.wsuf$mean, df.pest$mean, df.food$mean, df.heat$mean), 
                       lower = c(df.pm25$lower, df.bii$lower, df.waccess$lower, df.wsuf$lower, df.pest$lower, df.food$lower,df.heat$lower),
                       upper = c(df.pm25$upper, df.bii$upper, df.waccess$upper, df.wsuf$upper, df.pest$upper, df.food$upper,df.heat$upper))

save(df.means, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_means3_prev50_IPC_heat.R")


# ---- Plot ----
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_means3_prev50_IPC.R")

df.means[df.means$dim=="bii", "dim"] <- "a_bii" 
df.means[df.means$dim=="heat", "dim"] <- "b_heat" 
df.means[df.means$dim=="pm25", "dim"] <- "c_pm25" 
df.means[df.means$dim=="waccess", "dim"] <- "d_waccess" 
df.means[df.means$dim=="wsuf", "dim"] <- "e_wsuf" 
df.means[df.means$dim=="pest", "dim"] <- "f_pest" 
df.means[df.means$dim=="food", "dim"] <- "g_food" 

#light <- colour("light")

light <-  c("#67AB4F","#BF5841", "#D9751E","#3A88B5", "#5D9671","#D9981E",  "#864975")


stats <- df.means %>% 
  group_by(dim) %>%
  summarise(Mean = mean(mean), SD = sd(mean),
            CI_L = Mean - (SD * 2.576)/sqrt(6),
            CI_U = Mean + (SD * 2.576)/sqrt(6))

fig_poc <- ggplot() +
  geom_violin(df.means, mapping = aes(x = dim, y = mean, fill=dim, colour=dim), size=0.2, alpha=0.7) +
  stat_summary(fun.data = "mean_cl_boot", geom = "pointrange",
               colour = "red") +
  geom_point(stats, mapping = aes(dim, Mean), colour="white") +
  geom_errorbar(stats, mapping = aes(x = dim, ymin = CI_L, ymax = CI_U), width = 0.2, colour="white", size=0.7) +
  ggtitle("POC loations facing the worst conditions") +
  ylab("")+
  xlab("Dimension") +
  ylim(0,100) +
  geom_hline(yintercept=20, linetype="dashed", color = "grey", size=2)+
  theme_minimal() + 
  scale_color_manual(values=c(as.character(light[1:7]))) +
  scale_fill_manual(values=c(as.character(light[1:7])))

fig_poc <- fig_poc + theme(axis.text.x = element_text(size=22), axis.text.y = element_text(size=22),
                           plot.title = element_text(size=22),
                           axis.title.y = element_blank(), axis.title.x = element_blank())


fig_poc
ggsave(plot = fig_poc,
       filename = "Location_POC_prev50_IPC_heat.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/",
       width = 9, height = 7, units = "in",
       dpi = 420)
