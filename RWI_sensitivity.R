# Compute third plot of the second panel
# Same as SecondPanel_LastPLot.R but built for RWI-limited area


# --------------- Clear the environment and load packages ---------------
rm(list=ls()) 

# Load libraries
packages <- c("terra", "raster", "tidyverse", "rasterVis", "ncdf4", 
              "lattice", "foreign", "rworldmap", "data.table",
              "classInt", "patchwork", "matrixStats", "dplyr",
              "xtable", "BMisc", "latex2exp", "readxl", "sf", "networkD3",
              'car', "khroma", "gapminder", "modi", "boot", "pracma",
              "hutilscpp", "vioplot", "dplyr", "robustbase", "qgam")
lapply(packages, require, character=TRUE)
# ----------------------- Plot ------------------------- 

# ---- LOAD ----
# Run the second_panel_inequalities with rwi set as TRUE and then run this script

df <- df_merged

# --------------
# -------------- SUBNATIONAL STATISTICS ---------------
# --------------

df_norm <- df %>%
  group_by(country) %>%
  mutate(normalized_population = (ppp_2020_1km_Aggregated - min(ppp_2020_1km_Aggregated)) / (max(ppp_2020_1km_Aggregated) - min(ppp_2020_1km_Aggregated)))

fun_ineq1 <- function(values,weights,percentile) {
  quant <- weighted.quantile(x=values, 
                             w=weights, 
                             prob = percentile)
  return(quant)
}


# Count the frequency of each country
country_counts <- df_norm %>%
  group_by(country) %>%
  summarise(count = n())

# Filter rows where the country count is less than 10
filtered_df <- df_norm %>%
  inner_join(country_counts, by = "country") %>%
  as.data.frame() %>%
  filter(count >= 100) %>%
  dplyr::select(-count)

# Group by country and calculate the percentiles
result_df <- filtered_df %>%
  group_by(country) %>%
  summarize(RWI_20th_percentile = fun_ineq1(rwi, normalized_population, 0.2),
            RWI_80th_percentile = fun_ineq1(rwi, normalized_population, 0.8),
            WBGT_20th_percentile = fun_ineq1(wbgt, normalized_population, 0.2),
            WBGT_80th_percentile = fun_ineq1(wbgt, normalized_population, 0.8),
            Air_20th_percentile = fun_ineq1(airQuality, normalized_population, 0.2),
            Air_80th_percentile = fun_ineq1(airQuality, normalized_population, 0.8),
            Water_20th_percentile = fun_ineq1(w_ai, normalized_population, 0.2),
            Water_80th_percentile = fun_ineq1(w_ai, normalized_population, 0.8),
            Pesticides_20th_percentile = fun_ineq1(pesticides_all, normalized_population, 0.2),
            Pesticides_80th_percentile = fun_ineq1(pesticides_all, normalized_population, 0.8),
            BII_20th_percentile = fun_ineq1(lbii, normalized_population, 0.2),
            BII_80th_percentile = fun_ineq1(lbii, normalized_population, 0.8),
            Food_20th_percentile = fun_ineq1(Merged_IPC, normalized_population, 0.2),
            Food_80th_percentile = fun_ineq1(fcscore_max, normalized_population, 0.8),
            Water2_20th_percentile = fun_ineq1(w_access, normalized_population, 0.2),
            Water2_80th_percentile = fun_ineq1(w_access, normalized_population, 0.8))

save(result_df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/percentiles_countries_rwi.R")

countries <- unique(df_norm$country)

# --------------- RICHEST ------------------
new_df <- df_norm %>%
  left_join(result_df, by = "country") %>%
  mutate(
    above_gdp_above_wbgt = if_else(rwi > RWI_80th_percentile & wbgt > WBGT_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_air_quality = if_else(rwi > RWI_80th_percentile & airQuality > Air_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_blue_water = if_else(rwi > RWI_80th_percentile & w_ai > Water_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_pesticide_use = if_else(rwi > RWI_80th_percentile & pesticides_all > Pesticides_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_bii_use = if_else(rwi > RWI_80th_percentile & lbii > BII_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_food = if_else(rwi > RWI_80th_percentile & Merged_IPC > Food_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_water_access = if_else(rwi > RWI_80th_percentile & w_access > Water2_80th_percentile, ppp_2020_1km_Aggregated, 0)
  ) %>%
  group_by(country) %>%
  summarise(
    a__bii = sum(above_gdp_above_bii_use) / sum(ppp_2020_1km_Aggregated[rwi > RWI_80th_percentile]),
    b__wbgt = sum(above_gdp_above_wbgt) / sum(ppp_2020_1km_Aggregated[rwi > RWI_80th_percentile]),
    c__air = sum(above_gdp_above_air_quality) / sum(ppp_2020_1km_Aggregated[rwi > RWI_80th_percentile]),
    d__blue_water = sum(above_gdp_above_blue_water) / sum(ppp_2020_1km_Aggregated[rwi > RWI_80th_percentile]),
    e__waccess = sum(above_gdp_above_water_access) / sum(ppp_2020_1km_Aggregated[rwi > RWI_80th_percentile]),
    f__pestidices = sum(above_gdp_above_pesticide_use) / sum(ppp_2020_1km_Aggregated[rwi > RWI_80th_percentile]),
    g__food = sum(above_gdp_above_food) / sum(ppp_2020_1km_Aggregated[rwi > RWI_80th_percentile])
  )


new_df$country <- countries
df_rich <- na.omit(new_df)
df_rich <- merge(df_rich, filtered_df[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_rich <- df_rich[!duplicated(df_rich), ]

df_rich[!df_rich$country%in%c.food, "g__food"] <- NA
df_rich[!df_rich$country%in%countries_wacc, "e__waccess"] <- NA
df_rich[df_rich$country%in%c("Sierra Leone","Senegal", "Uganda"), "e__waccess"] <- NA
# Compute robust medians using lmrob
medians <- apply(df_rich[2:8], 2, function(x) lmrob(x~1, na.action = na.omit)$coefficients[1])

# Compute 95% confidence intervals using lmrob
conf_intervals <- apply(df_rich[2:8], 2, function(x) confint(lmrob(x~1), level = 0.95))
conf_intervals[conf_intervals<0] <- 0

# Create a dataframe with the summary statistics
summary_df_rich <- data.frame(dimension = colnames(df_rich[2:8]),
                              median = medians,
                              lower_ci = conf_intervals[1, ],
                              upper_ci = conf_intervals[2, ])


# ------------- POOREST ---------------
# Heat

new_df <- df_norm %>%
  left_join(result_df, by = "country") %>%
  mutate(
    below_gdp_above_wbgt = if_else(rwi < RWI_20th_percentile & wbgt > WBGT_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_air_quality = if_else(rwi<RWI_20th_percentile & airQuality > Air_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_water_access = if_else(rwi<RWI_20th_percentile & w_ai > Water_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_pesticide_use = if_else(rwi<RWI_20th_percentile & pesticides_all > Pesticides_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_bii_use = if_else(rwi<RWI_20th_percentile & lbii > BII_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_food = if_else(rwi<RWI_20th_percentile & fcscore_max > Food_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_water_access = if_else(rwi<RWI_20th_percentile & w_access > Water2_80th_percentile, ppp_2020_1km_Aggregated, 0)
  ) %>%
  group_by(country) %>%
  summarise(
    a_bii = sum(below_gdp_above_bii_use) / sum(ppp_2020_1km_Aggregated[rwi<RWI_20th_percentile]),
    b_wbgt = sum(below_gdp_above_wbgt) / sum(ppp_2020_1km_Aggregated[rwi<RWI_20th_percentile]),
    c_air = sum(below_gdp_above_air_quality) / sum(ppp_2020_1km_Aggregated[rwi<RWI_20th_percentile]),
    d_blue_water = sum(below_gdp_above_water_access) / sum(ppp_2020_1km_Aggregated[rwi<RWI_20th_percentile]),
    e_waccess = sum(below_gdp_above_water_access) / sum(ppp_2020_1km_Aggregated[rwi<RWI_20th_percentile]),
    f_pestidices = sum(below_gdp_above_pesticide_use) / sum(ppp_2020_1km_Aggregated[rwi<RWI_20th_percentile]),
    g_food = sum(below_gdp_above_food) / sum(ppp_2020_1km_Aggregated[rwi<RWI_20th_percentile])
  )


new_df$country <- countries
df_poor <- na.omit(new_df)
df_poor <- merge(df_poor, filtered_df[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_poor <- df_poor[!duplicated(df_poor), ]

df_poor[!df_poor$country%in%c.food, "g_food"] <- NA
df_poor[!df_poor$country%in%countries_wacc, "e_waccess"] <- NA

# Compute robust medians using lmrob
medians <- apply(df_poor[2:8], 2, function(x) lmrob(x~1)$coefficients[1])

# Compute 95% confidence intervals using lmrob
conf_intervals <- apply(df_poor[2:8], 2, function(x) confint(lmrob(x~1), level = 0.99))
conf_intervals[conf_intervals<0] <- 0

# Create a dataframe with the summary statistics
summary_df_poor <- data.frame(dimension = colnames(df_poor[2:8]),
                              median = medians,
                              lower_ci = conf_intervals[1, ],
                              upper_ci = conf_intervals[2, ])

# -------- Poor and rich, all in one --------

# Add a "group" column to the summary dataframes
summary_df_rich <- mutate(summary_df_rich, group = "rich")
summary_df_poor <- mutate(summary_df_poor, group = "poor")

# Combine the summary dataframes
summary_df_combined <- rbind(summary_df_rich, summary_df_poor)


group_colors <-c("#67AB4F","#67AB4F","#BF5841","#BF5841","#D9751E","#D9751E", 
                 "#3A88B5","#3A88B5","#5D9671","#5D9671","#D9981E","#D9981E",
                 "#864975", "#864975") 

combined_plot <- ggplot(summary_df_combined, aes(x = dimension, y = median, ymin = lower_ci, ymax = upper_ci)) +
  geom_errorbar(aes(color = dimension), width = 0.2, size=1.8, alpha=0.8,position = position_dodge(0.9)) +
  geom_point(aes(fill = group, shape = group, color = dimension), size = 4, position = position_dodge(0.9)) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("rich" = "black", "poor" = "white")) +
  scale_shape_manual(values = c("rich" = 16, "poor" = 21)) +
  scale_color_manual(values = group_colors) +
  labs(x = "Group", y = "Exposure") +
  ylim(0, 0.6) +
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )


combined_plot
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/poorest_richest_sens_RWI.png", plot = combined_plot, width = 12, height = 4, dpi = 300)


# ---------------- POC Locations ----------------

# Group by country and calculate the percentiles
result_df <- filtered_df %>%
  group_by(country) %>%
  summarize(RWI_20th_percentile = fun_ineq1(rwi, normalized_population, 0.2),
            RWI_80th_percentile = fun_ineq1(rwi, normalized_population, 0.8),
            WBGT_20th_percentile = fun_ineq1(wbgt, normalized_population, 0.2),
            WBGT_80th_percentile = fun_ineq1(wbgt, normalized_population, 0.8),
            Air_20th_percentile = fun_ineq1(airQuality, normalized_population, 0.2),
            Air_80th_percentile = fun_ineq1(airQuality, normalized_population, 0.8),
            Water_20th_percentile = fun_ineq1(w_ai, normalized_population, 0.2),
            Water_80th_percentile = fun_ineq1(w_ai, normalized_population, 0.8),
            Pesticides_20th_percentile = fun_ineq1(pesticides_all, normalized_population, 0.2),
            Pesticides_80th_percentile = fun_ineq1(pesticides_all, normalized_population, 0.8),
            BII_20th_percentile = fun_ineq1(lbii, normalized_population, 0.2),
            BII_80th_percentile = fun_ineq1(lbii, normalized_population, 0.8),
            Food_20th_percentile = fun_ineq1(Merged_IPC, normalized_population, 0.2),
            Food_80th_percentile = fun_ineq1(fcscore_max, normalized_population, 0.8),
            Water2_20th_percentile = fun_ineq1(w_access, normalized_population, 0.2),
            Water2_80th_percentile = fun_ineq1(w_access, normalized_population, 0.8))

save(result_df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/percentiles_countries_rwi.R")


# --------------- GDP - RWI-limited area  ------------------
result_df <- filtered_df %>%
  group_by(country) %>%
  summarize(GDP_20th_percentile = fun_ineq1(GDP2005_1km, normalized_population, 0.2),
            GDP_80th_percentile = fun_ineq1(GDP2005_1km, normalized_population, 0.8),
            WBGT_20th_percentile = fun_ineq1(wbgt, normalized_population, 0.2),
            WBGT_80th_percentile = fun_ineq1(wbgt, normalized_population, 0.8),
            Air_20th_percentile = fun_ineq1(airQuality, normalized_population, 0.2),
            Air_80th_percentile = fun_ineq1(airQuality, normalized_population, 0.8),
            Water_20th_percentile = fun_ineq1(w_ai, normalized_population, 0.2),
            Water_80th_percentile = fun_ineq1(w_ai, normalized_population, 0.8),
            Pesticides_20th_percentile = fun_ineq1(pesticides_all, normalized_population, 0.2),
            Pesticides_80th_percentile = fun_ineq1(pesticides_all, normalized_population, 0.8),
            BII_20th_percentile = fun_ineq1(lbii, normalized_population, 0.2),
            BII_80th_percentile = fun_ineq1(lbii, normalized_population, 0.8),
            Food_20th_percentile = fun_ineq1(Merged_IPC, normalized_population, 0.2),
            Food_80th_percentile = fun_ineq1(fcscore_max, normalized_population, 0.8),
            Water2_20th_percentile = fun_ineq1(w_access, normalized_population, 0.2),
            Water2_80th_percentile = fun_ineq1(w_access, normalized_population, 0.8))

new_df <- df_norm %>%
  left_join(result_df, by = "country") %>%
  mutate(
    above_gdp_above_wbgt = if_else(GDP2005_1km > GDP_80th_percentile & wbgt > WBGT_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_air_quality = if_else(GDP2005_1km > GDP_80th_percentile & airQuality > Air_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_blue_water = if_else(GDP2005_1km > GDP_80th_percentile & w_ai > Water_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_pesticide_use = if_else(GDP2005_1km > GDP_80th_percentile & pesticides_all > Pesticides_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_bii_use = if_else(GDP2005_1km > GDP_80th_percentile & lbii > BII_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_food = if_else(GDP2005_1km > GDP_80th_percentile & Merged_IPC > Food_80th_percentile, ppp_2020_1km_Aggregated, 0),
    above_gdp_above_water_access = if_else(GDP2005_1km > GDP_80th_percentile & w_access > Water2_80th_percentile, ppp_2020_1km_Aggregated, 0)
  ) %>%
  group_by(country) %>%
  summarise(
    a__bii = sum(above_gdp_above_bii_use) / sum(ppp_2020_1km_Aggregated[GDP2005_1km > GDP_80th_percentile]),
    b__wbgt = sum(above_gdp_above_wbgt) / sum(ppp_2020_1km_Aggregated[GDP2005_1km > GDP_80th_percentile]),
    c__air = sum(above_gdp_above_air_quality) / sum(ppp_2020_1km_Aggregated[GDP2005_1km > GDP_80th_percentile]),
    d__blue_water = sum(above_gdp_above_blue_water) / sum(ppp_2020_1km_Aggregated[GDP2005_1km > GDP_80th_percentile]),
    e__waccess = sum(above_gdp_above_water_access) / sum(ppp_2020_1km_Aggregated[GDP2005_1km > GDP_80th_percentile]),
    f__pestidices = sum(above_gdp_above_pesticide_use) / sum(ppp_2020_1km_Aggregated[GDP2005_1km > GDP_80th_percentile]),
    g__food = sum(above_gdp_above_food) / sum(ppp_2020_1km_Aggregated[GDP2005_1km > GDP_80th_percentile])
  )


new_df$country <- countries
df_rich <- na.omit(new_df)
df_rich <- merge(df_rich, filtered_df[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_rich <- df_rich[!duplicated(df_rich), ]

df_rich[!df_rich$country%in%c.food, "g__food"] <- NA
df_rich[!df_rich$country%in%countries_wacc, "e__waccess"] <- NA
df_rich[df_rich$country%in%c("Sierra Leone","Senegal", "Uganda"), "e__waccess"] <- NA
# Compute robust medians using lmrob
medians <- apply(df_rich[2:8], 2, function(x) lmrob(x~1, na.action = na.omit)$coefficients[1])

# Compute 95% confidence intervals using lmrob
conf_intervals <- apply(df_rich[2:8], 2, function(x) confint(lmrob(x~1), level = 0.95))
conf_intervals[conf_intervals<0] <- 0

# Create a dataframe with the summary statistics
summary_df_rich <- data.frame(dimension = colnames(df_rich[2:8]),
                              median = medians,
                              lower_ci = conf_intervals[1, ],
                              upper_ci = conf_intervals[2, ])


# ------ POOR -------
new_df <- df_norm %>%
  left_join(result_df, by = "country") %>%
  mutate(
    below_gdp_above_wbgt = if_else(GDP2005_1km < GDP_20th_percentile & wbgt > WBGT_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_air_quality = if_else(GDP2005_1km<GDP_20th_percentile & airQuality > Air_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_water_access = if_else(GDP2005_1km<GDP_20th_percentile & w_ai > Water_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_pesticide_use = if_else(GDP2005_1km<GDP_20th_percentile & pesticides_all > Pesticides_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_bii_use = if_else(GDP2005_1km<GDP_20th_percentile & lbii > BII_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_food = if_else(GDP2005_1km<GDP_20th_percentile & fcscore_max > Food_80th_percentile, ppp_2020_1km_Aggregated, 0),
    below_gdp_above_water_access = if_else(GDP2005_1km<GDP_20th_percentile & w_access > Water2_80th_percentile, ppp_2020_1km_Aggregated, 0)
  ) %>%
  group_by(country) %>%
  summarise(
    a_bii = sum(below_gdp_above_bii_use) / sum(ppp_2020_1km_Aggregated[GDP2005_1km<GDP_20th_percentile]),
    b_wbgt = sum(below_gdp_above_wbgt) / sum(ppp_2020_1km_Aggregated[GDP2005_1km<GDP_20th_percentile]),
    c_air = sum(below_gdp_above_air_quality) / sum(ppp_2020_1km_Aggregated[GDP2005_1km<GDP_20th_percentile]),
    d_blue_water = sum(below_gdp_above_water_access) / sum(ppp_2020_1km_Aggregated[GDP2005_1km<GDP_20th_percentile]),
    e_waccess = sum(below_gdp_above_water_access) / sum(ppp_2020_1km_Aggregated[GDP2005_1km<GDP_20th_percentile]),
    f_pestidices = sum(below_gdp_above_pesticide_use) / sum(ppp_2020_1km_Aggregated[GDP2005_1km<GDP_20th_percentile]),
    g_food = sum(below_gdp_above_food) / sum(ppp_2020_1km_Aggregated[GDP2005_1km<GDP_20th_percentile])
  )


new_df$country <- countries
df_poor <- na.omit(new_df)
df_poor <- merge(df_poor, filtered_df[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_poor <- df_poor[!duplicated(df_poor), ]

df_poor[!df_poor$country%in%c.food, "g_food"] <- NA
df_poor[!df_poor$country%in%countries_wacc, "e_waccess"] <- NA

# Compute robust medians using lmrob
medians <- apply(df_poor[2:8], 2, function(x) lmrob(x~1)$coefficients[1])

# Compute 95% confidence intervals using lmrob
conf_intervals <- apply(df_poor[2:8], 2, function(x) confint(lmrob(x~1), level = 0.99))
conf_intervals[conf_intervals<0] <- 0

# Create a dataframe with the summary statistics
summary_df_poor <- data.frame(dimension = colnames(df_poor[2:8]),
                              median = medians,
                              lower_ci = conf_intervals[1, ],
                              upper_ci = conf_intervals[2, ])

# -------- Poor and rich, all in one --------

# Add a "group" column to the summary dataframes
summary_df_rich <- mutate(summary_df_rich, group = "rich")
summary_df_poor <- mutate(summary_df_poor, group = "poor")

# Combine the summary dataframes
summary_df_combined <- rbind(summary_df_rich, summary_df_poor)


group_colors <-c("#67AB4F","#67AB4F","#BF5841","#BF5841","#D9751E","#D9751E", 
                 "#3A88B5","#3A88B5","#5D9671","#5D9671","#D9981E","#D9981E",
                 "#864975", "#864975") 

combined_plot <- ggplot(summary_df_combined, aes(x = dimension, y = median, ymin = lower_ci, ymax = upper_ci)) +
  geom_errorbar(aes(color = dimension), width = 0.2, size=1.8, alpha=0.8,position = position_dodge(0.9)) +
  geom_point(aes(fill = group, shape = group, color = dimension), size = 4, position = position_dodge(0.9)) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("rich" = "black", "poor" = "white")) +
  scale_shape_manual(values = c("rich" = 16, "poor" = 21)) +
  scale_color_manual(values = group_colors) +
  labs(x = "Group", y = "Exposure") +
  ylim(0, 0.6) +
  coord_cartesian(ylim = c(0, 0.6)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )


combined_plot
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/poorest_richest_sens_GDP.png", plot = combined_plot, width = 12, height = 4, dpi = 300)
