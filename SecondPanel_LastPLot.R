# Compute third plot of the second panel


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
# Main dataframe
load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainHRV.R")

# HM vs IPC vs FCS
food_n <- 1
food_layer <- c("class", "Merged_IPC", "fcscore_max")
dim_food <- food_layer[food_n]

# Countries
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/countries_wacc.R")
# food
if (food_n==1){
  load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_HM.R")
}else if (food_n==2){
  load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_IPC.R")
}else{
  load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_FSC.R")
}
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_heat.R")
c.food <- c.food[!c.food%in%c("Nicaragua")]

df <- df_m

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

save(result_df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/percentiles_countries.R")

countries <- unique(df_norm$country)

# --------------- RICHEST ------------------
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
# Compute robust medians using lmrob
medians <- apply(df_rich[2:8], 2, function(x) lmrob(x~1)$coefficients[1])

# Compute 95% confidence intervals using lmrob
conf_intervals <- apply(df_rich[2:8], 2, function(x) confint(lmrob(x~1), level = 0.95))
conf_intervals[conf_intervals<0] <- 0

# Create a dataframe with the summary statistics
summary_df_rich <- data.frame(dimension = colnames(df_rich[2:8]),
                         median = medians,
                         lower_ci = conf_intervals[1, ],
                         upper_ci = conf_intervals[2, ])

light <-  c("#67AB4F","#BF5841","#D9751E", "#3A88B5","#D9981E", "#5D9671","#864975") 

# food and sanitation access"#864975", "#5D9671")

# Create the violin plot
plot_richest <- ggplot(summary_df_rich, aes(x = dimension, y = median)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.4, color = light[1:7], size = 1.4, alpha=0.8) +
  geom_point(aes(y = median), color = "black", size = 2) +
  labs(x = "Dimension", y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ggtitle("Errorbars with Robust Median and 99% Confidence Intervals") +
  ylim(0, 1) +  # Set the y-axis limits
  theme_minimal()+
  theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),
          plot.title = element_text(size=22),
          axis.title.y = element_blank(), axis.title.x = element_blank())

plot_richest 
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/richest20.png", plot = plot_richest, width = 4, height = 4, dpi = 300)


# ------- RICH BY INCOME GROUPS ---------
dimension_cols <- colnames(df_rich)[2:7]
region_col <- "region"
income_col <- "IG"

# Add column of interest
# Create new column "IG" based on "income group" values
df_rich$IG <- ifelse(df_rich$`income group` %in% c("Low income", "Lower middle income"), "LIC-LMIC",
                     ifelse(df_rich$`income group` %in% c("Upper middle income", "High income"), "UIC-UMIC", NA))

# Compute medians and confidence intervals per region and dimension
# Subset the dataframe for the two groups in "IG"
group1 <- subset(df_rich, IG == "LIC-LMIC")
group2 <- subset(df_rich, IG == "UIC-UMIC")

# Group 1
median_group1 <- apply(group1[2:7], 2, function(x) lmrob(x~1)$coefficients[1])
conf_intervals1 <-  apply(group1[2:7], 2, function(x) confint(lmrob(x~1), level = 0.95))
conf_intervals1[conf_intervals1<0] <- 0

# Group 2
# Compute medians using lmrob, handling missing values
median_group2 <- apply(group2[2:7], 2, function(x) lmrob(x~1)$coefficients[1])
conf_intervals2 <-  apply(group2[2:7], 2, function(x) confint(lmrob(x~1), level = 0.95))
conf_intervals2[conf_intervals2<0] <- 0

# Create a summary dataframe
summary_df <- data.frame(
  IG = c("LIC-LMIC","LIC-LMIC","LIC-LMIC","LIC-LMIC","LIC-LMIC","LIC-LMIC", 
         "UIC-UMIC","UIC-UMIC","UIC-UMIC","UIC-UMIC","UIC-UMIC","UIC-UMIC"),
  median = c(median_group1, median_group2),
  lower_ci = c(conf_intervals1[1,], conf_intervals2[1,]),
  upper_ci = c(conf_intervals1[2,], conf_intervals2[2,]),
  dimension =  colnames(group1[2:7])
)


summary_df <- summary_df %>%
  filter(!is.na(IG))

# Assign colors to dimensions
dimension_colors <- c("#67AB4F", "#BF5841", "#D9751E", "#3A88B5", "#5D9671", "#D9981E")

# Create the plot with medians for all regions and dimensions
plot_rich_IG <- ggplot(summary_df, aes(x = factor(IG), y = median, color = dimension)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = position_dodge(width = 0.8), width = 0.3, size = 1.4, alpha = 0.8) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  labs(x = "Dimension", y = "Proportions", color = "Dimension") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.95) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = dimension_colors)


plot_rich_IG
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/richest_IG.png", plot = plot_rich_IG, width = 4, height = 4, dpi = 300)



# ------------- POOREST ---------------
# Heat

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

light <-  c("#67AB4F","#BF5841","#D9751E", "#3A88B5","#5D9671","#D9981E", "#864975") 


# Create the violin plot
plot_poorest <- ggplot(summary_df_poor, aes(x = dimension, y = median)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.4, color = light[1:7], size = 1.4, alpha=0.8) +
  geom_point(aes(y = median), color = "black", size = 2) +
  labs(x = "Dimension", y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ggtitle("Errorbars with Robust Median and 99% Confidence Intervals") +
  ylim(0, 1) +  # Set the y-axis limits
  theme_minimal()+
  theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),
        plot.title = element_text(size=22),
        axis.title.y = element_blank(), axis.title.x = element_blank())


plot_poorest
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/poorest20.png", plot = plot_poorest, width = 4, height = 4, dpi = 300)


# ------- POOR BY INCOME GROUPS ---------
dimension_cols <- colnames(df_poor)[2:8]
region_col <- "region"
income_col <- "IG"

# Add column of interest
# Drop rows with NA values in "income group" column
df_poor <- na.omit(df_poor)

# Create new column "IG" based on "income group" values
df_poor$IG <- ifelse(df_poor$`income group` %in% c("Low income", "Lower middle income"), "LIC-LMIC",
                     ifelse(df_poor$`income group` %in% c("Upper middle income", "High income"), "UIC-UMIC", NA))

# Compute medians and confidence intervals per region and dimension
# Subset the dataframe for the two groups in "IG"
group1 <- subset(df_poor, IG == "LIC-LMIC")
group2 <- subset(df_poor, IG == "UIC-UMIC")

# Group 1
median_group1 <- apply(group1[2:8], 2, function(x) lmrob(x~1)$coefficients[1])
conf_intervals1 <-  apply(group1[2:8], 2, function(x) confint(lmrob(x~1), level = 0.95))
conf_intervals1[conf_intervals1<0] <- 0

# Group 2
median_group2 <- apply(group2[2:8], 2, function(x) lmrob(x~1)$coefficients[1])
conf_intervals2 <-  apply(group2[2:8], 2, function(x) confint(lmrob(x~1), level = 0.95))
conf_intervals2[conf_intervals2<0] <- 0

# Create a summary dataframe
summary_df <- data.frame(
  IG = c("LIC-LMIC", "UIC-UMIC"),
  median = c(median_group1, median_group2),
  lower_ci = c(conf_intervals1[1,], conf_intervals2[1,]),
  upper_ci = c(conf_intervals1[2,], conf_intervals2[2,]),
  dimension =  colnames(group1[2:8])
)


summary_df <- summary_df %>%
  filter(!is.na(IG))

# Assign colors to dimensions
dimension_colors <- c("#67AB4F", "#BF5841", "#D9751E", "#3A88B5", "#5D9671", "#D9981E", "#864975")

# Create the plot with medians for all regions and dimensions
plot_poor_IG <- ggplot(summary_df, aes(x = factor(IG), y = median, color = dimension)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = position_dodge(width = 0.8), width = 0.3, size = 1.4, alpha = 0.8) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  labs(x = "Dimension", y = "Proportions", color = "Dimension") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.95) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = dimension_colors)


plot_poor_IG
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/poorest_IG.png", plot = plot_poor_IG, width = 4, height = 4, dpi = 300)



# -------- Poor and rich, all in one --------

# Add a "group" column to the summary dataframes
summary_df_rich <- mutate(summary_df_rich, group = "rich")
summary_df_poor <- mutate(summary_df_poor, group = "poor")

# Combine the summary dataframes
summary_df_combined <- rbind(summary_df_rich, summary_df_poor)

dimension_cols <- c("a_bii","b_wbgt", "c_air", "d_blue_water", "e_waccess", "f_pesticides", "g_food")
region_col <- "region"

summary_df_combined$dimension <- gsub("1$", "", summary_df_combined$dimension)

# Reshape the data to long format
summary_df <- summary_df_combined %>%
  select(all_of(c("country", dimension_cols, region_col))) %>%
  pivot_longer(cols = all_of(dimension_cols), names_to = "dimension", values_to = "value")

summary_df <- summary_df_combined %>%
  pivot_longer(cols = c(median, lower_ci, upper_ci), names_to = "metric", values_to = "value") %>%
  select(dimension, metric, value, group)

# Compute medians and confidence intervals per region and dimension
summary_df <- df_prop_long %>%
  group_by(region, dimension) %>%
  summarize(
    median = median(value, na.rm=T),
    lower_ci = max(0, quantile(value, 0.005, na.rm=T)),
    upper_ci = min(1, quantile(value, 0.995, na.rm=T))
  ) %>%
  ungroup()


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
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/poorest_richest.png", plot = combined_plot, width = 12, height = 4, dpi = 300)

# Print the combined plot
print(combined_plot)



# ---------------- POC Locations ----------------

# Group by 'country' and calculate the sum of 'ref' within each group
df_sum <- aggregate(ref ~ country, data = df, FUN = sum)
# Filter the groups where the sum of 'ref' is greater than or equal to 5
c_poc <- subset(df_sum, ref >= 20)$country
# Filter the original dataframe based on the selected groups
df_ref <- subset(df, country %in% c_poc)


# Define countries that have food inseq and access to san data 
c.food.poc <- c_poc[c_poc%in%c.food]
countries_wacc.poc <- c_poc[c_poc%in%countries_wacc]

dims <- c("lbii","wbgt","airQuality", "w_ai", "w_access",  "pesticides_all", dim_food)
dimensions <- c("BII_80th_percentile","WBGT_80th_percentile","Air_80th_percentile", 
                "Water_80th_percentile", "Water2_80th_percentile", 
                "Pesticides_80th_percentile", 
                "Food_80th_percentile")

# Create dataframe
df_plot3 <- data.frame()
i <- 1
proportions <- c()
if(dims[i]==dim_food){
  c.iter <- c.food.poc} else if(dims[i]=="waccess"){
    c.iter <- countries_wacc.poc
  }else{
    c.iter <- c_poc}
for (j in 1:length(c.iter)) {
  # Take 80th worst perc value
  q80 <- result_df[result_df$country==c.iter[j], dimensions[i] ][[1]]
  # Take all the POC
  pop_tot_in <- sum(df_ref[df_ref$country==c.iter[j] & df_ref$ref>=1, "ppp_2020_1km_Aggregated"  ], na.rm = T)
  # Take all the poorest population sum that faces MORE than the 80th worst value
  freq_il_above <- sum(df_ref[df_ref$country==c.iter[j] &df_ref$ref>=1 & df_ref[,dims[i]]>=q80, "ppp_2020_1km_Aggregated" ], na.rm = T)
  # Is that percentage greater than 20%?
  #prop <- ifelse(freq_il_below==0&freq_il_above>0, 100, freq_il_above/pop_tot_in*100)
  prop <- freq_il_above/pop_tot_in
  proportions <- c(proportions, prop)
}

df_plot3 <- data.frame(country = c_poc, pm25 = proportions)


for (i in 2:length(dims)) {
  proportions <- c()
  if(dims[i]==dim_food){
    c.iter <- c.food.poc} else if(dims[i]=="waccess"){
      c.iter <- countries_wacc.poc
    }else{
      c.iter <- c_poc}
  for (j in 1:length(c.iter)) {
    # Take 80th worst perc value
    q80 <- result_df[result_df$country==c.iter[j], dimensions[i] ][[1]]
    # Take all the poorest population sum
    pop_tot_in <- sum(df_ref[df_ref$country==c.iter[j] & df_ref$ref>=1, "ppp_2020_1km_Aggregated"  ], na.rm = T)
    # Take all the poorest population sum that faces MORE than the 80th worst value
    freq_il_above <- sum(df_ref[df_ref$country==c.iter[j] &df_ref$ref>=1 & df_ref[,dims[i]]>=q80, "ppp_2020_1km_Aggregated" ], na.rm = T)
    # Is that percentage greater than 20%?
    if(dims[i]==dim_food){
      c.iter <- c.food.poc} else if(dims[i]=="waccess"){
        c.iter <- countries_wacc.poc
      }else{
        c.iter <- c_poc}
    prop <- freq_il_above/pop_tot_in
    proportions <- c(proportions, prop)
  }
  df_plot3 <- qpcR:::cbind.na(df_plot3, proportions)
}
dims <- c("country","a_bii","b_wbgt","c_air", "e_blue_water", "f_waccess",  "g_pesticides",  "h_food")

colnames(df_plot3) <- dims


df_prop <- df_plot3
df_prop[!df_prop$country%in%c.food, "h_food"] <- NA
df_prop[!df_prop$country%in%countries_wacc, "f_waccess"] <- NA


df_prop2 <- df_prop[rowSums(df_prop == 0, na.rm = T) < 3, ]
df_prop2 <- merge(df_prop2, filtered_df[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_prop2 <- df_prop2[!duplicated(df_prop2), ]


# Compute robust medians using lmrob
medians <- apply(df_prop2[2:8], 2, function(x) lmrob(x~1)$coefficients[1])

# Compute 95% confidence intervals using lmrob
conf_intervals <- apply(df_prop2[2:8], 2, function(x) confint(lmrob(x~1), level = 0.99))
conf_intervals <- apply(df_prop[2:8], 2, function(x) {
  fit <- lmrob(x ~ 1)
  lower <- max(0, confint(fit, level = 0.99)[1])
  upper <- min(1, confint(fit, level = 0.99)[2])
  c(lower, upper)
})


# Create a dataframe with the summary statistics
summary_df <- data.frame(dimension = colnames(df_prop2[2:8]),
                         median = medians,
                         lower_ci = conf_intervals[1, ],
                         upper_ci = conf_intervals[2, ])


light <-  c("#67AB4F","#BF5841","#D9751E", "#3A88B5","#5D9671","#D9981E", "#864975") 

# food and sanitation access"#864975", "#5D9671")

# Create the violin plot
plot_POC <- ggplot(summary_df, aes(x = dimension, y = median)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.4, color = light, size = 1.4, alpha=0.8) +
  geom_point(aes(y = median), color = "black", size = 2) +
  labs(x = "Dimension", y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 1) +  # Set the y-axis limits
  theme_minimal()+
  theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),
        plot.title = element_text(size=22),
        axis.title.y = element_blank(), axis.title.x = element_blank())

plot_POC

ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/POC_25_w_ai.png", plot = plot_POC, width = 4, height = 4, dpi = 300)

# ------- POCs by Income Group -------
df_prop2 <- df_prop2[df_prop2$country!="Burkina Faso",]
# Create new column "IG" based on "income group" values
df_prop2$IG <- ifelse(df_prop2$`income group` %in% c("Low income", "Lower middle income"), "LIC-LMIC",
                     ifelse(df_prop2$`income group` %in% c("Upper middle income", "High income"), "UIC-UMIC", NA))

# Compute medians and confidence intervals per region and dimension
# Subset the dataframe for the two groups in "IG"
group1 <- subset(df_prop2, IG == "LIC-LMIC")
group2 <- subset(df_prop2, IG == "UIC-UMIC")

# Group 1
median_group1 <- apply(group1[2:8], 2, function(x) lmrob(x~1)$coefficients[1])
conf_intervals1 <-  apply(group1[2:8], 2, function(x) confint(lmrob(x~1), level = 0.95))
conf_intervals1[conf_intervals1<0] <- 0
conf_intervals1[conf_intervals1>1] <- 1

# Group 2
median_group2 <- apply(group2[2:8], 2, function(x) lmrob(x~1)$coefficients[1])
conf_intervals2 <-  apply(group2[2:8], 2, function(x) confint(lmrob(x~1), level = 0.95))
conf_intervals2[conf_intervals2<0] <- 0

# Create a summary dataframe
summary_df_POC <- data.frame(
  IG = c("LIC-LMIC","LIC-LMIC","LIC-LMIC","LIC-LMIC","LIC-LMIC","LIC-LMIC","LIC-LMIC", 
         "UIC-UMIC","UIC-UMIC","UIC-UMIC","UIC-UMIC","UIC-UMIC","UIC-UMIC","UIC-UMIC"),
  median = c(median_group1, median_group2),
  lower_ci = c(conf_intervals1[1,], conf_intervals2[1,]),
  upper_ci = c(conf_intervals1[2,], conf_intervals2[2,]),
  dimension =  colnames(group1[2:8])
)

# Assign colors to dimensions
dimension_colors <- c("#67AB4F", "#BF5841", "#D9751E", "#3A88B5", "#5D9671", "#D9981E", "#864975")

# Create the plot with medians for all regions and dimensions
plot_POC_IG <- ggplot(summary_df_POC, aes(x = factor(IG), y = median, color = dimension)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = position_dodge(width = 0.8), width = 0.3, size = 1.4, alpha = 0.8) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  labs(x = "Dimension", y = "Proportions", color = "Dimension") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = dimension_colors)


plot_POC_IG
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/POC_IG.png", plot = plot_POC_IG, width = 4, height = 4, dpi = 300)




# ---------------- Indigenous Lands ----------------

# Only consider countries with at least 10% of IL AND 10% of non-IL
df_IL <- data.frame(country=sort(unique(df$country)))

fun_IL <- function(x,df) {
  all <- sum(df[df$country==x, "ppp_2020_1km_Aggregated"])
  IL <- sum(df[df$country==x&df$ind_com==1, "ppp_2020_1km_Aggregated"])
  fraction <- IL/all * 100
  return(fraction)
}

df_IL$prop_IL <- apply(df_IL,1,fun_IL,df)
df_IL <- na.omit(df_IL)
c_threshold <- df_IL[df_IL$prop_IL>5&df_IL$prop_IL<95, "country"]
c_threshold <- na.omit(c(c_threshold))
c_threshold <- c_threshold[!c_threshold%in%c("Nicaragua", "Honduras")]
c_threshold <- c_threshold[!c_threshold%in%c("Sweden", "Gaza")]

df_ind <- filtered_df %>%
  filter(country %in% c_threshold)

df_proportions <- df_ind %>%
  filter(ind_com == 1) %>%
  group_by(country) %>%
  summarize(
    a_bii = sum(ppp_2020_1km_Aggregated[lbii > result_df$BII_80th_percentile]) / sum(ppp_2020_1km_Aggregated),
    b_wbgt = sum(ppp_2020_1km_Aggregated[wbgt > result_df$WBGT_80th_percentile]) / sum(ppp_2020_1km_Aggregated),
    c_air = sum(ppp_2020_1km_Aggregated[airQuality > result_df$Air_80th_percentile]) / sum(ppp_2020_1km_Aggregated),
    d_blue_water = sum(ppp_2020_1km_Aggregated[w_ai > result_df$Water_80th_percentile]) / sum(ppp_2020_1km_Aggregated),
    e_waccess = sum(ppp_2020_1km_Aggregated[w_access > result_df$Water2_80th_percentile]) / sum(ppp_2020_1km_Aggregated),
    f_pesticides = sum(ppp_2020_1km_Aggregated[pesticides_all > result_df$Pesticides_80th_percentile]) / sum(ppp_2020_1km_Aggregated),
    g_food = sum(ppp_2020_1km_Aggregated[class > result_df$Food_80th_percentile]) / sum(ppp_2020_1km_Aggregated)
    
  )

df_proportions$country <- c_threshold
df_prop <- na.omit(df_proportions)

df_prop <- merge(df_prop, filtered_df[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_prop <- df_prop[!duplicated(df_prop), ]

df_prop[!df_prop$country%in%c.food, "g_food"] <- NA
df_prop[!df_prop$country%in%countries_wacc, "e_waccess"] <- NA
# Compute robust medians using lmrob
medians <- apply(df_prop[2:8], 2, function(x) lmrob(x~1)$coefficients[1])

# Compute 99% confidence intervals using lmrob
conf_intervals <- apply(df_prop[2:8], 2, function(x) {
  fit <- lmrob(x ~ 1)
  lower <- max(0, confint(fit, level = 0.99)[1])
  upper <- min(1, confint(fit, level = 0.99)[2])
  c(lower, upper)
})


# Create a dataframe with the summary statistics
summary_df <- data.frame(dimension = colnames(df_prop[2:8]),
                         median = medians,
                         lower_ci = conf_intervals[1, ],
                         upper_ci = conf_intervals[2, ])

light <-  c("#67AB4F","#BF5841","#D9751E", "#3A88B5","#5D9671","#D9981E", "#864975") 

# food and sanitation access"#864975", "#5D9671")

# Create the violin plot
plot_IL <- ggplot(summary_df, aes(x = dimension, y = median)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.4, color = light, size = 1.4, alpha=0.8) +
  geom_point(aes(y = median), color = "black", size = 2) +
  labs(x = "Dimension", y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ggtitle("Errorbars with Robust Median and 99% Confidence Intervals") +
  ylim(0, 1) +  # Set the y-axis limits
  theme_minimal()+
  theme(axis.text.x = element_text(size=16), axis.text.y = element_text(size=16),
        plot.title = element_text(size=22),
        axis.title.y = element_blank(), axis.title.x = element_blank())


plot_IL
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/IL_w_HM_5_95.png", plot = plot_IL, width = 4, height = 4, dpi = 300)


# ------- IL by Income Group -------
# Create new column "IG" based on "income group" values
df_prop$IG <- ifelse(df_prop$`income group` %in% c("Low income", "Lower middle income"), "LIC-LMIC",
                     ifelse(df_prop$`income group` %in% c("Upper middle income", "High income"), "UIC-UMIC", NA))

# Compute medians and confidence intervals per region and dimension
# Subset the dataframe for the two groups in "IG"
group1 <- subset(df_prop, IG == "LIC-LMIC")
group2 <- subset(df_prop, IG == "UIC-UMIC")

# Group 1
median_group1 <- apply(group1[2:7], 2, function(x) lmrob(x~1)$coefficients[1])
conf_intervals1 <-  apply(group1[2:7], 2, function(x) confint(lmrob(x~1), level = 0.99))
conf_intervals1[conf_intervals1<0] <- 0

# Group 2
median_group2 <- apply(group2[2:7], 2, function(x) lmrob(x~1)$coefficients[1])
conf_intervals2 <-  apply(group2[2:7], 2, function(x) confint(lmrob(x~1), level = 0.99))
conf_intervals2[conf_intervals2<0] <- 0

# Create a summary dataframe
summary_df <- data.frame(
  IG = c("LIC-LMIC","LIC-LMIC","LIC-LMIC","LIC-LMIC","LIC-LMIC","LIC-LMIC", 
         "UIC-UMIC","UIC-UMIC","UIC-UMIC","UIC-UMIC","UIC-UMIC","UIC-UMIC"),
  median = c(median_group1, median_group2),
  lower_ci = c(conf_intervals1[1,], conf_intervals2[1,]),
  upper_ci = c(conf_intervals1[2,], conf_intervals2[2,]),
  dimension =  colnames(group1[2:7])
)


summary_df <- summary_df %>%
  filter(!is.na(IG))

# Assign colors to dimensions
dimension_colors <- c("#67AB4F", "#BF5841", "#D9751E", "#3A88B5", "#5D9671", "#D9981E")

# Create the plot with medians for all regions and dimensions
plot_IL_IG <- ggplot(summary_df, aes(x = factor(IG), y = median, color = dimension)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = position_dodge(width = 0.8), width = 0.3, size = 1.4, alpha = 0.8) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  labs(x = "Dimension", y = "Proportions", color = "Dimension") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = dimension_colors)


plot_IL_IG
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/IL_IG.png", plot = plot_IL_IG, width = 4, height = 4, dpi = 300)




# ------ IL by regions -------
dimension_cols <- c("a_bii","b_wbgt", "c_air", "d_blue_water", "e_waccess", "f_pesticides", "g_food")
region_col <- "region"
income_col <- "IG"



# Compute medians and confidence intervals per region and dimension
summary_df <- df_prop_long %>%
  group_by(region, dimension) %>%
  summarize(
    median = median(value, na.rm=T),
    lower_ci = max(0, quantile(value, 0.005, na.rm=T)),
    upper_ci = min(1, quantile(value, 0.995, na.rm=T))
  ) %>%
  ungroup()


region_labels <- c("East Asia & Pacific" = "EAP",
                   "North America" = "NA",
                   "Middle East & North Africa" = "MENA",
                   "South Asia" = "SA",
                   "Sub-Saharan Africa" = "SSA",
                   "Europe & Central Asia" = "EAC",
                   "Latin America & Caribbean" = "LAC"
                   # Add more replacements as needed
)

# Replace the values in the "region" column with the new labels
summary_df <- summary_df %>%
  mutate(region = case_when(
    region %in% names(region_labels) ~ region_labels[region],
    TRUE ~ region
  ))


# Assign colors to dimensions
dimension_colors <- c("#67AB4F", "#BF5841", "#D9751E", "#3A88B5", "#5D9671", "#D9981E", "#864975")

# Create the plot with medians for all regions and dimensions
plot_IL <- ggplot(summary_df, aes(x = factor(region), y = median, color = dimension)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = position_dodge(width = 0.8), width = 0.3, size = 1.4, alpha = 0.8) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  labs(x = "Dimension", y = "Proportions", color = "Dimension") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ggtitle("Medians with Confidence Intervals by Region and Dimension for Indigenous Lands") +
  ylim(0, 1) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) +
  scale_color_manual(values = dimension_colors)


plot_IL
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Second panel/Third figure/Errorbars/IL_5_95_regions.png", plot = plot_IL, width = 10, height = 4, dpi = 300)



# --------------
# -------------- NATIONAL STATISTICS ---------------
# --------------

fun_ineq1 <- function(values,weights,percentile) {
  quant <- weighted.quantile(x=values, 
                             w=weights, 
                             prob = percentile)
  return(quant)
}

df <- na.omit(df)
df$pop_norm2 <- (df$ppp_2020_1km_Aggregated - min(df$ppp_2020_1km_Aggregated)) / (max(df$ppp_2020_1km_Aggregated) - min(df$ppp_2020_1km_Aggregated))

# Group by country and calculate the percentiles
df.food <- df[df$country%in%c.food, ]
df.food$pop_norm2 <- (df.food$ppp_2020_1km_Aggregated - min(df.food$ppp_2020_1km_Aggregated)) / (max(df.food$ppp_2020_1km_Aggregated) - min(df.food$ppp_2020_1km_Aggregated))
result_df_food <- df.food %>%
  summarize(Food_20th_percentile = fun_ineq1(Merged_IPC, pop_norm2, 0.2),
            Food_80th_percentile = fun_ineq1(Merged_IPC, pop_norm2, 0.8))

result_df <- df %>%
  summarize(GDP_20th_percentile = fun_ineq1(GDP2005_1km, pop_norm2, 0.2),
            GDP_80th_percentile = fun_ineq1(GDP2005_1km, pop_norm2, 0.8),
            WBGT_20th_percentile = fun_ineq1(wbgt, pop_norm2, 0.2),
            WBGT_80th_percentile = fun_ineq1(wbgt, pop_norm2, 0.8),
            Air_20th_percentile = fun_ineq1(airQuality, pop_norm2, 0.2),
            Air_80th_percentile = fun_ineq1(airQuality, pop_norm2, 0.8),
            Water_20th_percentile = fun_ineq1(w_ai, pop_norm2, 0.2),
            Water_80th_percentile = fun_ineq1(w_ai, pop_norm2, 0.8),
            Pesticides_20th_percentile = fun_ineq1(pesticides_all, pop_norm2, 0.2),
            Pesticides_80th_percentile = fun_ineq1(pesticides_all, pop_norm2, 0.8),
            BII_20th_percentile = fun_ineq1(lbii, pop_norm2, 0.2),
            BII_80th_percentile = fun_ineq1(lbii, pop_norm2, 0.8),
            Water2_20th_percentile = fun_ineq1(w_access, pop_norm2, 0.2),
            Water2_80th_percentile = fun_ineq1(w_access, pop_norm2, 0.8))

result_df$Food_20th_percentile <- result_df_food$Food_20th_percentile
result_df$Food_80th_percentile <- result_df_food$Food_80th_percentile


save(result_df, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/percentiles_international.R")


# --------------- RICHEST ------------------
new_df <- df %>%
  mutate(
    above_80th_GDP = GDP2005_1km > result_df$GDP_80th_percentile,
    above_80th_WBGT = wbgt > result_df$WBGT_80th_percentile,
    above_80th_Air = airQuality > result_df$Air_80th_percentile,
    above_80th_Water = w_ai > result_df$Water_80th_percentile,
    above_80th_Pesticides = pesticides_all > result_df$Pesticides_80th_percentile,
    above_80th_BII = lbii > result_df$BII_80th_percentile,
    above_80th_Food = Merged_IPC > result_df_food$Food_80th_percentile,
    above_80th_Water2 = w_access > result_df$Water2_80th_percentile
  ) %>%
  summarise(
    a_bii = sum(ppp_2020_1km_Aggregated[above_80th_GDP & above_80th_BII]) / sum(ppp_2020_1km_Aggregated),
    b_wbgt = sum(ppp_2020_1km_Aggregated[above_80th_GDP & above_80th_WBGT]) / sum(ppp_2020_1km_Aggregated),
    c_air = sum(ppp_2020_1km_Aggregated[above_80th_GDP & above_80th_Air]) / sum(ppp_2020_1km_Aggregated),
    d_blue_water = sum(ppp_2020_1km_Aggregated[above_80th_GDP & above_80th_Water]) / sum(ppp_2020_1km_Aggregated),
    e_waccess = sum(ppp_2020_1km_Aggregated[above_80th_GDP & above_80th_Water2]) / sum(ppp_2020_1km_Aggregated),
    f_pesticides = sum(ppp_2020_1km_Aggregated[above_80th_GDP & above_80th_Pesticides]) / sum(ppp_2020_1km_Aggregated),
    g_food = sum(ppp_2020_1km_Aggregated[above_80th_GDP & above_80th_Food]) / sum(ppp_2020_1km_Aggregated)
  )


# --------------- POOREST ------------------

poorest_df <- df %>%
  mutate(
    below_20th_GDP = GDP2005_1km <= result_df$GDP_20th_percentile,
    above_80th_WBGT = wbgt >= result_df$WBGT_80th_percentile,
    above_80th_Air = airQuality >= result_df$Air_80th_percentile,
    above_80th_Water = w_ai >= result_df$Water_80th_percentile,
    above_80th_Pesticides = pesticides_all >= result_df$Pesticides_80th_percentile,
    above_80th_BII = lbii >= result_df$BII_80th_percentile,
    above_80th_Food = Merged_IPC >= result_df$Food_80th_percentile,
    above_80th_Water2 = w_access >= result_df$Water2_80th_percentile
  ) %>%
  summarise(
    a_bii = sum(ppp_2020_1km_Aggregated[below_20th_GDP & above_80th_BII]) / sum(ppp_2020_1km_Aggregated),
    b_wbgt = sum(ppp_2020_1km_Aggregated[below_20th_GDP & above_80th_WBGT]) / sum(ppp_2020_1km_Aggregated),
    c_air = sum(ppp_2020_1km_Aggregated[below_20th_GDP & above_80th_Air]) / sum(ppp_2020_1km_Aggregated),
    d_blue_water = sum(ppp_2020_1km_Aggregated[below_20th_GDP & above_80th_Water]) / sum(ppp_2020_1km_Aggregated),
    e_waccess = sum(ppp_2020_1km_Aggregated[below_20th_GDP & above_80th_Water2]) / sum(ppp_2020_1km_Aggregated),
    f_pesticides = sum(ppp_2020_1km_Aggregated[below_20th_GDP & above_80th_Pesticides]) / sum(ppp_2020_1km_Aggregated),
    g_food = sum(ppp_2020_1km_Aggregated[below_20th_GDP & above_80th_Food]) / sum(ppp_2020_1km_Aggregated)
  )



# ----------------- POCS --------------------
# Group by 'country' and calculate the sum of 'ref' within each group
df_sum <- aggregate(ref ~ country, data = df, FUN = sum)
# Filter the groups where the sum of 'ref' is greater than or equal to 5
c_poc <- subset(df_sum, ref >= 25)$country
# Filter the original dataframe based on the selected groups
df_ref <- subset(df, country %in% c_poc)


# Define countries that have food inseq and access to san data 
c.food.poc <- c_poc[c_poc%in%c.food]
countries_wacc.poc <- c_poc[c_poc%in%countries_wacc]

dims <- c("lbii","wbgt","airQuality", "w_ai", "w_access",  "pesticides_all", dim_food)
dimensions <- c("BII_80th_percentile","WBGT_80th_percentile","Air_80th_percentile", 
                "Water_80th_percentile", "Water2_80th_percentile", 
                "Pesticides_80th_percentile", 
                "Food_80th_percentile")

# Create dataframe
proportions <- c()
i <- 7
if(dims[i]==dim_food){
  c.iter <- c.food.poc} else if(dims[i]=="waccess"){
    c.iter <- countries_wacc.poc
  }else{
    c.iter <- c_poc}
# Take 80th worst perc value
q80 <- result_df[, dimensions[i] ][[1]]
# Take all the POC
pop_tot_in <- sum(df_ref[df_ref$ref>=1, "ppp_2020_1km_Aggregated"  ], na.rm = T)
# Take all the poorest population sum that faces MORE than the 80th worst value
freq_il_above <- sum(df_ref[df_ref$ref>=1 & df_ref[,dims[i]]>=q80, "ppp_2020_1km_Aggregated" ], na.rm = T)
# Is that percentage greater than 20%?
#prop <- ifelse(freq_il_below==0&freq_il_above>0, 100, freq_il_above/pop_tot_in*100)
prop <- freq_il_above/pop_tot_in
proportions <- c(proportions, prop)


# Take all the poorest population sum
pop_tot_in <- sum(df_ref[df_ref$ref>=1, "ppp_2020_1km_Aggregated"  ], na.rm = T)

for (i in 2:length(dims)) {
  proportions <- c()
  if(dims[i]==dim_food){
    c.iter <- c.food.poc} else if(dims[i]=="waccess"){
      c.iter <- countries_wacc.poc
    }else{
      c.iter <- c_poc}
  # Take 80th worst perc value
  q80 <- result_df[, dimensions[i] ][[1]]
  # Take all the poorest population sum that faces MORE than the 80th worst value
  freq_il_above <- sum(df_ref[df_ref$ref>=1 & df_ref[,dims[i]]>=q80, "ppp_2020_1km_Aggregated" ], na.rm = T)
  # Is that percentage greater than 20%?
  if(dims[i]==dim_food){
    c.iter <- c.food.poc} else if(dims[i]=="waccess"){
      c.iter <- countries_wacc.poc
    }else{
      c.iter <- c_poc}
  prop <- freq_il_above/pop_tot_in
  proportions <- c(proportions, prop)
}
dims <- c("country","a_bii","b_wbgt","c_air", "e_blue_water", "f_waccess",  "g_pesticides",  "h_food")

