# This file downloads CHIRTS data
rm(list=ls()) 

# Load libraries
packages <- c("terra", "raster", "tidyverse", "rasterVis", "ncdf4", 
              "lattice", "foreign", "rworldmap","dplyr",
              'car', "khroma", "gapminder", "modi", "boot", "pracma",
              "hutilscpp", "vioplot", "dplyr", "robustbase", "qgam",
              "rvest")
lapply(packages, require, character=TRUE)

# ------------- Download CHRTS data -----------
# Define the URL of the webpage containing the files
url <- "https://data.chc.ucsb.edu/experimental/CHC_CMIP6/2050_SSP585/wbgtmax/2016/"
# url <- "https://data.chc.ucsb.edu/experimental/CHC_CMIP6/2030_SSP585/wbgtmax/2016/"
# Specify the path of the folder where you want to save the downloaded files
folder_path <- "/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/CHIRTS Projections/2050_SSP585_WBGTmax"

# Read the webpage and extract the links to the .tif files
webpage <- read_html(url)
file_links <- webpage %>%
  html_nodes("a[href$='.tif']") %>%
  html_attr("href")

# Download all the .tif files from the webpage
for (file_link in file_links) {
  file_url <- paste0(url, file_link)
  file_name <- basename(file_url)
  file_path <- file.path(folder_path, file_name)
  download.file(file_url, destfile = file_path, mode = "wb")
}


# ------------ 
ssp <- "SSP245"
year <- "2030"
deg <- "28C"

# Set the path to the folder containing the raster files
folder_path <- paste("/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/CHIRTS Projections/Extremes_",year,"_",ssp,"_WBGT/", sep="")
#folder_path <- paste("/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/CHIRTS Projections/Extremes_2016_WBGT/", sep="")

# Get the list of files in the folder
file_list <- list.files(path = folder_path, pattern = ".tif", full.names = TRUE)

# Initialize an empty raster stack
raster_stack <- stack()

# Loop through the files and add matching rasters to the stack
for (file_path in file_list) {
  file_name <- basename(file_path)
  
  # Check if the filename contains '28C'
  if (grepl(deg, file_name)) {
    print(file_name)
    # Read the raster and add it to the stack
    raster_layer <- raster(file_path)
    raster_layer[raster_layer < 0] <- 0
    print(raster_layer)
    raster_stack <- addLayer(raster_stack, raster_layer)
  }
}

# Calculate the sum of all the rasters in the stack
sum_raster <- sum(raster_stack)

# Print the sum raster
out_path <- paste('/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/CHIRTS Projections/','Extremes_',year,'_',ssp,'_',deg,'.tif',sep="")
#out_path <- paste('/Users/naiacasina/Documents/ENVS/Codes and Data/Heat/CHIRTS Projections/','Extremes_2016_28C.tif',sep="")
raster::writeRaster(sum_raster, out_path, overwrite=T)

# -------------------
# ------------------- INEQUALITIES ----------------------
# -------------------- main plots -----------------------
# -------------------
# ---- LOAD ----
# Main dataframe
load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainHRV.R")
df <- df_m

# Add delta for the analysis
df$delta_50_16_2 <- df$heat_50_245 - df$heat_16
df$delta_50_16_5 <- df$heat_50_585 - df$heat_16

df_norm <- df

df_norm <- df %>%
  group_by(country) %>%
  mutate(norm_pop_245 = (pop_50_245 - min(pop_50_245)) / (max(pop_50_245) - min(pop_50_245)))

df_norm <- df_norm %>%
  group_by(country) %>%
  mutate(norm_pop_585 = (pop_50_585 - min(pop_50_585)) / (max(pop_50_585) - min(pop_50_585)))


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

# Compute the sum of population where heat_proj30 > 0, grouped by country
sum_population <- filtered_df %>%
  filter(heat_50_245 > 0) %>%
  group_by(country) %>%
  summarise(sum_population = sum(pop_50_245))

# Compute the sum of total population, grouped by country
total_population <- filtered_df %>%
  group_by(country) %>%
  summarise(total_population = sum(pop_50_245))

# Merge the two dataframes by country
result <- merge(sum_population, total_population, by = "country")

# Compute the ratio by dividing sum_population by total_population
result$population_ratio <- result$sum_population / result$total_population

# Filter countries that have above 50% of population suffering from hs
df_heat_filtered <- filtered_df %>%
  filter(country %in% result$country & result$population_ratio > 0.50) %>%
  filter(!country %in% c("Western Sahara"))

df_heat <- df_heat_filtered %>%
  group_by(country) %>%
  summarize(GDP_245_20th_perc = fun_ineq1(gdp_50_245, norm_pop_245, 0.2),
            GDP_245_80th_perc = fun_ineq1(gdp_50_245, norm_pop_245, 0.8),
            GDP_585_20th_perc = fun_ineq1(gdp_50_585, norm_pop_585, 0.2),
            GDP_585_80th_perc = fun_ineq1(gdp_50_585, norm_pop_585, 0.8),
            heat_245_20th_perc = fun_ineq1(heat_50_245, norm_pop_245, 0.2),
            heat_245_80th_perc = fun_ineq1(heat_50_245, norm_pop_245, 0.8),
            heat_585_20th_perc = fun_ineq1(heat_50_585, norm_pop_585, 0.2),
            heat_585_80th_perc = fun_ineq1(heat_50_585, norm_pop_585, 0.8),
            heat_D_245_20th_perc = fun_ineq1(delta_50_16_2, norm_pop_245, 0.2),
            heat_D_245_80th_perc = fun_ineq1(delta_50_16_2, norm_pop_245, 0.8),
            heat_D_585_20th_perc = fun_ineq1(delta_50_16_5, norm_pop_585, 0.2),
            heat_D_585_80th_perc = fun_ineq1(delta_50_16_5, norm_pop_585, 0.8)) 


# ----- POOREST ABSOLUTES -----
df_poorest <- df_norm %>%
  left_join(df_heat, by = "country") %>%
  mutate(
    below_gdp_above_2 = if_else(gdp_50_245 < GDP_245_20th_perc & heat_50_245 > heat_245_80th_perc, pop_50_245, 0),
    below_gdp_above_5 = if_else(gdp_50_585 < GDP_585_20th_perc & heat_50_585 > heat_585_80th_perc, pop_50_585, 0)
  ) %>%
  group_by(country) %>%
  summarise(
    a_SSP2 = sum(below_gdp_above_2) / sum(pop_50_245[gdp_50_245<GDP_245_20th_perc]),
    b_SSP5 = sum(below_gdp_above_5) / sum(pop_50_585[gdp_50_585<GDP_585_20th_perc]))

df_poorest <- merge(df_poorest, df_heat_filtered[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_poorest <- df_poorest[!duplicated(df_poorest), ]
df_poorest <- na.omit(df_poorest)
df_poorest <- subset(df_poorest, !apply(df_poorest == 0, 1, any))

# ----- POOREST DELTAS -----
df_poorest_D <- df_norm %>%
  left_join(df_heat, by = "country") %>%
  mutate(
    below_gdp_above_2 = if_else(gdp_50_245 < GDP_245_20th_perc & delta_50_16_2 > heat_D_245_80th_perc, pop_50_245, 0),
    below_gdp_above_5 = if_else(gdp_50_585 < GDP_585_20th_perc & delta_50_16_5 > heat_D_585_80th_perc, pop_50_585, 0)
  ) %>%
  group_by(country) %>%
  summarise(
    a_SSP2 = sum(below_gdp_above_2) / sum(pop_50_245[gdp_50_245<GDP_245_20th_perc]),
    b_SSP5 = sum(below_gdp_above_5) / sum(pop_50_585[gdp_50_585<GDP_585_20th_perc]))

df_poorest_D <- merge(df_poorest_D, df_heat_filtered[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_poorest_D <- df_poorest_D[!duplicated(df_poorest_D), ]
df_poorest_D <- na.omit(df_poorest_D)
df_poorest_D <- subset(df_poorest_D, !apply(df_poorest_D == 0, 1, any))


# ------ POOREST DELTAS BY INCOME GROUPS -----------

# Add column of interest
# Create new column "IG" based on "income group" values
df_poorest_D$IG <- ifelse(df_poorest_D$`income group` %in% c("Low income", "Lower middle income"), "LIC-LMIC",
                     ifelse(df_poorest_D$`income group` %in% c("Upper middle income", "High income"), "UIC-UMIC", NA))

# Compute medians and confidence intervals per region and dimension
# Subset the dataframe for the two groups in "IG"
group1 <- subset(df_poorest_D, IG == "LIC-LMIC")
group2 <- subset(df_poorest_D, IG == "UIC-UMIC")

# Group 1
median_group1 <- lmrob(group1$a_SSP2~1)$coefficients[1]
conf_intervals1 <- confint(lmrob(group1$a_SSP2~1), level = 0.99)
conf_intervals1[conf_intervals1<0] <- 0

# Group 2
# Compute medians using lmrob, handling missing values
median_group2 <- lmrob(group2$a_SSP2~1)$coefficients[1]
conf_intervals2 <-  confint(lmrob(group2$a_SSP2~1), level = 0.99)
conf_intervals2[conf_intervals2<0] <- 0

# Create a summary dataframe
summary_df_poor <- data.frame(
  IG = c("LIC-LMIC","UIC-UMIC"),
  median = c(median_group1, median_group2),
  lower_ci = c(conf_intervals1[1], conf_intervals2[1]),
  upper_ci = c(conf_intervals1[2], conf_intervals2[2]),
  dimension =  colnames(group1[2])
)


# Assign colors to dimensions
dimension_colors <- c("#67AB4F", "#BF5841", "#D9751E", "#3A88B5", "#5D9671", "#D9981E")

# Create the plot with medians for all regions and dimensions
plot_poor_IG <- ggplot(summary_df, aes(x = factor(IG), y = median, color = IG)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = position_dodge(width = 0.8), width = 0.3, size = 1.4, alpha = 0.8) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  labs(x = "Dimension", y = "Proportions", color = "Dimension") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.4) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) 


plot_poor_IG
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/poorest_IG.png", plot = plot_poor_IG, width = 4, height = 4, dpi = 300)





# -------------- RICHEST ABSOLUTES -----------------
df_richest <- df_norm %>%
  left_join(df_heat, by = "country") %>%
  mutate(
    above_gdp_above_2 = if_else(gdp_50_245 >= GDP_245_80th_perc & heat_50_245 > heat_245_80th_perc, pop_50_245, 0),
    above_gdp_above_5 = if_else(gdp_50_585 >= GDP_585_80th_perc & heat_50_585 > heat_585_80th_perc, pop_50_585, 0)
  ) %>%
  group_by(country) %>%
  summarise(
    a_SSP2 = sum(above_gdp_above_2) / sum(pop_50_245[gdp_50_245>GDP_245_80th_perc]),
    b_SSP5 = sum(above_gdp_above_5) / sum(pop_50_585[gdp_50_585>GDP_585_80th_perc]))

df_richest <- merge(df_richest, df_heat_filtered[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_richest <- df_richest[!duplicated(df_richest), ]
df_richest <- na.omit(df_richest)

# -------------- RICHEST DELTAS -----------------
df_richest_D <- df_norm %>%
  left_join(df_heat, by = "country") %>%
  mutate(
    above_gdp_above_2 = if_else(gdp_50_245 >= GDP_245_80th_perc & delta_50_16_2 > heat_D_245_80th_perc, pop_50_245, 0),
    above_gdp_above_5 = if_else(gdp_50_585 >= GDP_585_80th_perc & delta_50_16_5 > heat_D_585_80th_perc, pop_50_585, 0)
  ) %>%
  group_by(country) %>%
  summarise(
    a_SSP2 = sum(above_gdp_above_2) / sum(pop_50_245[gdp_50_245>GDP_245_80th_perc]),
    b_SSP5 = sum(above_gdp_above_5) / sum(pop_50_585[gdp_50_585>GDP_585_80th_perc]))

df_richest_D <- merge(df_richest_D, df_heat_filtered[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_richest_D <- df_richest_D[!duplicated(df_richest_D), ]
df_richest_D <- na.omit(df_richest_D)


# ----- RICHEST DELTAS BY INCOME GROUPS ------
# Add column of interest
# Create new column "IG" based on "income group" values
df_richest_D$IG <- ifelse(df_richest_D$`income group` %in% c("Low income", "Lower middle income"), "LIC-LMIC",
                          ifelse(df_richest_D$`income group` %in% c("Upper middle income", "High income"), "UIC-UMIC", NA))

# Compute medians and confidence intervals per region and dimension
# Subset the dataframe for the two groups in "IG"
group1 <- subset(df_richest_D, IG == "LIC-LMIC")
group2 <- subset(df_richest_D, IG == "UIC-UMIC")

# Group 1
median_group1 <- lmrob(group1$a_SSP2~1)$coefficients[1]
conf_intervals1 <- confint(lmrob(group1$a_SSP2~1), level = 0.99)
conf_intervals1[conf_intervals1<0] <- 0

# Group 2
# Compute medians using lmrob, handling missing values
median_group2 <- lmrob(group2$a_SSP2~1)$coefficients[1]
conf_intervals2 <-  confint(lmrob(group2$a_SSP2~1), level = 0.99)
conf_intervals2[conf_intervals2<0] <- 0

# Create a summary dataframe
summary_df_rich <- data.frame(
  IG = c("LIC-LMIC","UIC-UMIC"),
  median = c(median_group1, median_group2),
  lower_ci = c(conf_intervals1[1], conf_intervals2[1]),
  upper_ci = c(conf_intervals1[2], conf_intervals2[2]),
  dimension =  colnames(group1[2])
)

# Assign colors to dimensions
dimension_colors <- c("#67AB4F", "#BF5841", "#D9751E", "#3A88B5", "#5D9671", "#D9981E")

# Create the plot with medians for all regions and dimensions
plot_rich_IG <- ggplot(summary_df, aes(x = factor(IG), y = median, color = IG)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = position_dodge(width = 0.8), width = 0.2, size = 1.4, alpha = 0.8) +
  geom_point(position = position_dodge(width = 0.8), size = 2) +
  labs(x = "Dimension", y = "Proportions", color = "Dimension") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.4) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) 


plot_rich_IG
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/poorest_IG.png", plot = plot_rich_IG, width = 4, height = 4, dpi = 300)

# ---- Plot and rich, all together -----
summary_df_poor$group <- "poor"
# Merge the two columns into a new column
summary_df_poor$fact <- paste(summary_df_poor$IG, summary_df_poor$group)
summary_df_rich$group <- "rich"
summary_df_rich$fact <- paste(summary_df_rich$IG, summary_df_rich$group)

# Merge the data frames
merged_df <- rbind(summary_df_poor, summary_df_rich)

# Set color palette for IG groups
ig_colors <- c("LIC-LMIC" = "blue", "UIC-UMIC" = "green")

# Create the plot
ggplot(merged_df, aes(x = factor(fact), y = median, color = IG, fill = group)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.2) +
  geom_point(size = 3, shape = 21) +
  scale_color_manual(values = ig_colors) +
  scale_fill_manual(values = c("poor" = ig_colors, "rich" = "white"))  +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.4) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  ) 

group_colors <-c("#D9751E","#D9981E") 

combined_plot <- ggplot(merged_df, aes(x = fact, y = median, ymin = lower_ci, ymax = upper_ci)) +
  geom_errorbar(aes(color = IG), width = 0.4, size=1.8, alpha=0.8,position = position_dodge(0.9)) +
  geom_point(aes(fill = group, shape = group, color = IG), size = 4, position = position_dodge(0.9)) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("rich" = "black", "poor" = "white")) +
  scale_shape_manual(values = c("rich" = 16, "poor" = 21)) +
  scale_color_manual(values = group_colors) +
  labs(x = "Group", y = "Exposure") +
  ylim(0, 0.4) +
  coord_cartesian(ylim = c(0, 0.4)) +
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
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/poorest_richest_IG.png", plot = combined_plot, width = 6, height = 4, dpi = 300)



# -------- POCs --------
# Group by 'country' and calculate the sum of 'ref' within each group
df_sum <- aggregate(ref ~ country, data = df, FUN = sum)
# Filter the groups where the sum of 'ref' is greater than or equal to 5
c_poc <- subset(df_sum, ref >= 15)$country
# Filter the original dataframe based on the selected groups
df_ref <- subset(df, country %in% c_poc)


# Compute the sum of population where heat_proj30 > 0, grouped by country
sum_population <- df_ref %>%
  filter(heat_50_245 > 0) %>%
  group_by(country) %>%
  summarise(sum_population = sum(pop_50_245))

# Compute the sum of total population, grouped by country
total_population <- df_ref %>%
  group_by(country) %>%
  summarise(total_population = sum(pop_50_245))

# Merge the two dataframes by country
result <- merge(sum_population, total_population, by = "country")

# Compute the ratio by dividing sum_population by total_population
result$population_ratio <- result$sum_population / result$total_population

# Filter countries that have above 50% of population suffering from hs
df_poc_filtered <- df_ref %>%
  filter(country %in% result$country)

df_poc_filtered <- na.omit(df_poc_filtered)

df_poc_prop <- df_poc_filtered %>%
  left_join(df_heat, by = "country") %>%
  filter(ref == 1) %>%
  group_by(country) %>%
  summarize(
    a_SSP2 = sum(pop_50_245[heat_50_245 > heat_245_80th_perc]) / sum(pop_50_245),
    b_SSP5 = sum(pop_50_585[heat_50_585 > heat_585_80th_perc]) / sum(pop_50_585)
    
  )

df_poc_prop <- subset(df_poc_prop, !apply(df_poc_prop == 0, 1, any))
df_poc_prop <- merge(df_poc_prop, filtered_df[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_poc_prop <- df_poc_prop[!duplicated(df_poc_prop), ]
df_poc_prop <- na.omit(df_poc_prop)

# POC delta
df_poc_prop_D <- df_poc_filtered %>%
  left_join(df_heat, by = "country") %>%
  filter(ref == 1) %>%
  group_by(country) %>%
  summarize(
    a_SSP2 = sum(pop_50_245[delta_50_16_2 > heat_D_245_80th_perc]) / sum(pop_50_245),
    b_SSP5 = sum(pop_50_585[delta_50_16_5 > heat_D_585_80th_perc]) / sum(pop_50_585)
    
  )

df_poc_prop_D <- subset(df_poc_prop_D, !apply(df_poc_prop_D == 0, 1, any))
df_poc_prop_D <- merge(df_poc_prop_D, filtered_df[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_poc_prop_D <- df_poc_prop_D[!duplicated(df_poc_prop_D), ]
df_poc_prop_D <- na.omit(df_poc_prop_D)

# ----- POCs by INCOME GROUPS ----
# Add column of interest
# Create new column "IG" based on "income group" values
df_poc_prop_D$IG <- ifelse(df_poc_prop_D$`income group` %in% c("Low income", "Lower middle income"), "LIC-LMIC",
                           ifelse(df_poc_prop_D$`income group` %in% c("Upper middle income", "High income"), "UIC-UMIC", NA))

# Compute medians and confidence intervals per region and dimension
# Subset the dataframe for the two groups in "IG"
group1 <- subset(df_poc_prop_D, IG == "LIC-LMIC")
group2 <- subset(df_poc_prop_D, IG == "UIC-UMIC")

# Group 1
median_group1 <- lmrob(group1$a_SSP2~1)$coefficients[1]
conf_intervals1 <- confint(lmrob(group1$a_SSP2~1), level = 0.99)
conf_intervals1[conf_intervals1<0] <- 0

# Group 2
# Compute medians using lmrob, handling missing values
median_group2 <- lmrob(group2$a_SSP2~1)$coefficients[1]
conf_intervals2 <-  confint(lmrob(group2$a_SSP2~1), level = 0.99)
conf_intervals2[conf_intervals2<0] <- 0

# Create a summary dataframe
summary_df_POC <- data.frame(
  IG = c("LIC-LMIC","UIC-UMIC"),
  median = c(median_group1[[1]], median_group2[[1]]),
  lower_ci = c(conf_intervals1[1], conf_intervals2[1]),
  upper_ci = c(conf_intervals1[2], conf_intervals2[2])
)


# Assign colors to dimensions
IG_colors <- c("#D9751E","#D9981E")

# Create the plot with medians for all regions and dimensions
plot_POC_IG <- ggplot(summary_df_POC, aes(x = factor(IG), y = median, color = IG)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = position_dodge(width = 0.8), width = 0.4, size = 1.8, alpha = 0.8, color=IG_colors) +
  geom_point(position = position_dodge(width = 0.8), size = 4, color = IG_colors) +
  labs(x = "Dimension", y = "Proportions", color = "Dimension") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.4) +
  coord_cartesian(ylim = c(0, 0.4)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )


plot_POC_IG
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/POC_IG.png", plot = plot_POC_IG, width = 3, height = 4, dpi = 300)



# ------------- INDIGENOUS LANDS ------------

# Only consider countries with at least 10% of IL AND 10% of non-IL
df_IL <- data.frame(country=sort(unique(df$country)))

fun_IL <- function(x,df) {
  all <- sum(df[df$country==x, "pop_50_245"])
  IL <- sum(df[df$country==x&df$ind_com==1, "pop_50_245"])
  fraction <- IL/all * 100
  return(fraction)
}

df_IL$prop_IL <- apply(df_IL,1,fun_IL,df)

df_IL <- na.omit(df_IL)

c_threshold <- df_IL[df_IL$prop_IL>7&df_IL$prop_IL<95, "country"]

c_threshold <- na.omit(c(c_threshold))


df_ind <- filtered_df %>%
  filter(country %in% c_threshold)

# Group the data by country and calculate the proportion for each country
df_ind_prop <- df_ind %>%
  group_by(country) %>%
  summarize(
    threshold_SSP2 = df_heat[df_heat$country == first(country), "heat_245_80th_perc"][[1]],
    a_SSP2 = sum(pop_50_245[ind_com == 1 & heat_50_245 > threshold_SSP2], na.rm = TRUE) /
      sum(pop_50_245[ind_com == 1], na.rm = TRUE),
    threshold_SSP5 = df_heat[df_heat$country == first(country), "heat_585_80th_perc"][[1]],
    b_SSP5 = sum(pop_50_585[ind_com == 1 & heat_50_585 > threshold_SSP5], na.rm = TRUE) /
      sum(pop_50_585[ind_com == 1], na.rm = TRUE)
  ) %>%
  ungroup()



df_ind_prop <- subset(df_ind_prop, !apply(df_ind_prop == 0, 1, any))
df_ind_prop <- merge(df_ind_prop, filtered_df[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_ind_prop <- df_ind_prop[!duplicated(df_ind_prop), ]
df_ind_prop <- na.omit(df_ind_prop)

# --------- INDIGENOUS LANDS DELTAS --------
df_ind_prop_D <- df_ind %>%
  left_join(df_heat, by = "country") %>%
  filter(ind_com == 1) %>%
  group_by(country) %>%
  summarize(
    a_SSP2 = sum(pop_50_245[delta_50_16_2 > heat_D_245_80th_perc]) / sum(pop_50_245),
    b_SSP5 = sum(pop_50_585[delta_50_16_5 > heat_D_585_80th_perc]) / sum(pop_50_585)
    
  )

df_ind_prop_D <- df_ind %>%
  group_by(country) %>%
  summarize(
    threshold_SSP2 = df_heat[df_heat$country == first(country), "heat_D_245_80th_perc"][[1]],
    a_SSP2 = sum(pop_50_245[ind_com == 1 & delta_50_16_2 > threshold_SSP2], na.rm = TRUE) /
      sum(pop_50_245[ind_com == 1], na.rm = TRUE),
    threshold_SSP5 = df_heat[df_heat$country == first(country), "heat_D_585_80th_perc"][[1]],
    b_SSP5 = sum(pop_50_585[ind_com == 1 & delta_50_16_5 > threshold_SSP5], na.rm = TRUE) /
      sum(pop_50_585[ind_com == 1], na.rm = TRUE)
  ) %>%
  ungroup()

df_ind_prop_D <- subset(df_ind_prop_D, !apply(df_ind_prop_D == 0, 1, any))
df_ind_prop_D <- merge(df_ind_prop_D, filtered_df[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_ind_prop_D <- df_ind_prop_D[!duplicated(df_ind_prop_D), ]
df_ind_prop_D <- na.omit(df_ind_prop_D)


# ----- IL by INCOME GROUPS ----
# Add column of interest
# Create new column "IG" based on "income group" values
df_ind_prop_D$IG <- ifelse(df_ind_prop_D$`income group` %in% c("Low income", "Lower middle income"), "LIC-LMIC",
                          ifelse(df_ind_prop_D$`income group` %in% c("Upper middle income", "High income"), "UIC-UMIC", NA))

# Compute medians and confidence intervals per region and dimension
# Subset the dataframe for the two groups in "IG"
group1 <- subset(df_ind_prop_D, IG == "LIC-LMIC")
group2 <- subset(df_ind_prop_D, IG == "UIC-UMIC")

# group1 <- subset(df_ind_prop, `income group`=="Low income")
# group2 <- subset(df_ind_prop, `income group`=="Lower middle income")

# Group 1
median_group1 <- lmrob(group1$a_SSP2~1)$coefficients[1]
conf_intervals1 <- confint(lmrob(group1$a_SSP2~1), level = 0.99)
conf_intervals1[conf_intervals1<0] <- 0

# Group 2
# Compute medians using lmrob, handling missing values
median_group2 <- lmrob(group2$a_SSP2~1)$coefficients[1]
conf_intervals2 <-  confint(lmrob(group2$a_SSP2~1), level = 0.99)
conf_intervals2[conf_intervals2<0] <- 0

# Create a summary dataframe
summary_df_IL <- data.frame(
  IG = c("LIC-LMIC","UIC-UMIC"),
  median = c(median_group1[[1]], median_group2[[1]]),
  lower_ci = c(conf_intervals1[1], conf_intervals2[1]),
  upper_ci = c(conf_intervals1[2], conf_intervals2[2])
)


# Assign colors to dimensions
IG_colors <- c("#D9751E","#D9981E")

# Create the plot with medians for all regions and dimensions
plot_IL_IG <- ggplot(summary_df_IL, aes(x = factor(IG), y = median, color = IG)) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = position_dodge(width = 0.8), width = 0.4, size = 1.8, alpha = 0.8, color=IG_colors) +
  geom_point(position = position_dodge(width = 0.8), size = 4, color = IG_colors) +
  labs(x = "Dimension", y = "Proportions", color = "Dimension") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.4) +
  coord_cartesian(ylim = c(0, 0.4)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )


plot_IL_IG
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/IL_IG.png", plot = plot_IL_IG, width = 3, height = 4, dpi = 300)





# ------------ PLOT ABSOLUTES ------------
# Compute medians and confidence intervals for a_SSP2 and b_SSP5 in each dataframe
confidence_level <- 0.95

# Function to compute median using lmrob
compute_median <- function(x) lmrob(x ~ 1)$coefficients[1]

# Function to compute confidence intervals using lmrob
compute_confidence_intervals <- function(x) {
  fit <- lmrob(x ~ 1)
  ci_low <- confint(fit, level = confidence_level)[1]
  ci_high <- confint(fit, level = confidence_level)[2]
  return(c(ci_low, ci_high))
}

# Compute medians and confidence intervals for df_poorest
poorest_a_SSP2_median <- compute_median(df_poorest$a_SSP2)
poorest_a_SSP2_ci <- compute_confidence_intervals(df_poorest$a_SSP2)
poorest_b_SSP5_median <- compute_median(df_poorest$b_SSP5)
poorest_b_SSP5_ci <- compute_confidence_intervals(df_poorest$b_SSP5)

# Compute medians and confidence intervals for df_rich
rich_a_SSP2_median <- compute_median(df_richest$a_SSP2)
rich_a_SSP2_ci <- compute_confidence_intervals(df_richest$a_SSP2)
rich_b_SSP5_median <- compute_median(df_richest$b_SSP5)
rich_b_SSP5_ci <- compute_confidence_intervals(df_richest$b_SSP5)

# Compute medians and confidence intervals for df_ind_prop
indigenous_a_SSP2_median <- compute_median(df_ind_prop$a_SSP2)
indigenous_a_SSP2_ci <- compute_confidence_intervals(df_ind_prop$a_SSP2)
indigenous_b_SSP5_median <- compute_median(df_ind_prop$b_SSP5)
indigenous_b_SSP5_ci <- compute_confidence_intervals(df_ind_prop$b_SSP5)

# Compute medians and confidence intervals for df_poc_prop
poc_a_SSP2_median <- compute_median(df_poc_prop$a_SSP2)
poc_a_SSP2_ci <- compute_confidence_intervals(df_poc_prop$a_SSP2)
poc_b_SSP5_median <- compute_median(df_poc_prop$b_SSP5)
poc_b_SSP5_ci <- compute_confidence_intervals(df_poc_prop$b_SSP5)

# Create a combined dataframe for plotting
combined_df <- data.frame(
  Group = c("e_Poorest", "g_Richest", "a_Indigenous", "c_POC","f_Poorest", "h_Richest", "b_Indigenous", "d_POC"),
  SSP = c("SSP2","SSP2","SSP2","SSP2","SSP5","SSP5","SSP5","SSP5"),
  a_SSP2_median = c(poorest_a_SSP2_median, rich_a_SSP2_median, indigenous_a_SSP2_median, poc_a_SSP2_median,
                    poorest_b_SSP5_median, rich_b_SSP5_median, indigenous_b_SSP5_median, poc_b_SSP5_median),
  a_SSP2_ci_low = c(poorest_a_SSP2_ci[1], rich_a_SSP2_ci[1], indigenous_a_SSP2_ci[1], poc_a_SSP2_ci[1],
                    poorest_b_SSP5_ci[1], rich_b_SSP5_ci[1], indigenous_b_SSP5_ci[1], poc_b_SSP5_ci[1]),
  a_SSP2_ci_high = c(poorest_a_SSP2_ci[2], rich_a_SSP2_ci[2], indigenous_a_SSP2_ci[2], poc_a_SSP2_ci[2],
                     poorest_b_SSP5_ci[2], rich_b_SSP5_ci[2], indigenous_b_SSP5_ci[2], poc_b_SSP5_ci[2])
)

# Create the plot
# Define the colors for each group
library(wesanderson)
wes_colors <- append(wes_palettes$GrandBudapest1,wes_palettes$GrandBudapest2)
group_colors <- c(wes_colors[1],wes_colors[1],wes_colors[2],wes_colors[2],
                  wes_colors[3],wes_colors[3],wes_colors[4],wes_colors[4])

# Plotting
all_together <- ggplot(combined_df, aes(x = Group, y = a_SSP2_median, ymin = a_SSP2_ci_low, ymax = a_SSP2_ci_high)) +
  geom_errorbar(aes(color = Group), width = 0.2, size=1, position = position_dodge(0.9)) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  geom_point(aes(fill = SSP, shape = SSP, color = Group), size = 4, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("SSP2" = "black", "SSP5" = "white")) +
  scale_shape_manual(values = c("SSP2" = 16, "SSP5" = 21)) +
  scale_color_manual(values = group_colors) +
  labs(x = "Group", y = "a_SSP2_median") +
  ylim(0, 0.6) +
  coord_cartesian(ylim = c(0, 0.4)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

all_together
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/all_together.png", plot = all_together, width = 6, height = 6, dpi = 300)

# ------------ PLOT DELTAS ------------
# Compute medians and confidence intervals for df_poorest
poorest_a_SSP2_median <- compute_median(df_poorest_D$a_SSP2)
poorest_a_SSP2_ci <- compute_confidence_intervals(df_poorest_D$a_SSP2)
poorest_b_SSP5_median <- compute_median(df_poorest_D$b_SSP5)
poorest_b_SSP5_ci <- compute_confidence_intervals(df_poorest_D$b_SSP5)

# Compute medians and confidence intervals for df_rich
rich_a_SSP2_median <- compute_median(df_richest_D$a_SSP2)
rich_a_SSP2_ci <- compute_confidence_intervals(df_richest_D$a_SSP2)
rich_b_SSP5_median <- compute_median(df_richest_D$b_SSP5)
rich_b_SSP5_ci <- compute_confidence_intervals(df_richest_D$b_SSP5)

# Compute medians and confidence intervals for df_ind_prop
indigenous_a_SSP2_median <- compute_median(df_ind_prop_D$a_SSP2)
indigenous_a_SSP2_ci <- compute_confidence_intervals(df_ind_prop_D$a_SSP2)
indigenous_b_SSP5_median <- compute_median(df_ind_prop_D$b_SSP5)
indigenous_b_SSP5_ci <- compute_confidence_intervals(df_ind_prop_D$b_SSP5)

# Compute medians and confidence intervals for df_poc_prop
poc_a_SSP2_median <- compute_median(df_poc_prop_D$a_SSP2)
poc_a_SSP2_ci <- compute_confidence_intervals(df_poc_prop_D$a_SSP2)
poc_b_SSP5_median <- compute_median(df_poc_prop_D$b_SSP5)
poc_b_SSP5_ci <- compute_confidence_intervals(df_poc_prop_D$b_SSP5)

# Create a combined dataframe for plotting
combined_df_D <- data.frame(
  Group = c("e_Poorest", "g_Richest", "a_Indigenous", "c_POC","f_Poorest", "h_Richest", "b_Indigenous", "d_POC"),
  SSP = c("SSP2","SSP2","SSP2","SSP2","SSP5","SSP5","SSP5","SSP5"),
  a_SSP2_median = c(poorest_a_SSP2_median, rich_a_SSP2_median, indigenous_a_SSP2_median, poc_a_SSP2_median,
                    poorest_b_SSP5_median, rich_b_SSP5_median, indigenous_b_SSP5_median, poc_b_SSP5_median),
  a_SSP2_ci_low = c(poorest_a_SSP2_ci[1], rich_a_SSP2_ci[1], indigenous_a_SSP2_ci[1], poc_a_SSP2_ci[1],
                    poorest_b_SSP5_ci[1], rich_b_SSP5_ci[1], indigenous_b_SSP5_ci[1], poc_b_SSP5_ci[1]),
  a_SSP2_ci_high = c(poorest_a_SSP2_ci[2], rich_a_SSP2_ci[2], indigenous_a_SSP2_ci[2], poc_a_SSP2_ci[2],
                     poorest_b_SSP5_ci[2], rich_b_SSP5_ci[2], indigenous_b_SSP5_ci[2], poc_b_SSP5_ci[2])
)

# Create the plot
# Plotting
all_together_D <- ggplot(combined_df_D, aes(x = Group, y = a_SSP2_median, ymin = a_SSP2_ci_low, ymax = a_SSP2_ci_high)) +
  geom_errorbar(aes(color = Group), width = 0.2, size=1, position = position_dodge(0.9)) +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  geom_point(aes(fill = SSP, shape = SSP, color = Group), size = 4, position = position_dodge(0.9)) +
  scale_fill_manual(values = c("SSP2" = "black", "SSP5" = "white")) +
  scale_shape_manual(values = c("SSP2" = 16, "SSP5" = 21)) +
  scale_color_manual(values = group_colors) +
  labs(x = "Group", y = "a_SSP2_median") +
  ylim(0, 0.6) +
  coord_cartesian(ylim = c(0, 0.4)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

all_together_D
ggsave(filename = "/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/all_together_D.png", plot = all_together_D, width = 6, height = 6, dpi = 300)


# -------------------
# ------------------- INEQUALITIES ----------------------
# ---------------- supplementary plots ------------------
# -------------------
# Set ssp
ssp <- "585"
# Group by country and calculate the percentiles
df_heat <- df_heat_filtered %>%
  group_by(country) %>%
  summarize(GDP_245_20th_perc = fun_ineq1(gdp_50_245, norm_pop_245, 0.2),
            GDP_245_80th_perc = fun_ineq1(gdp_50_245, norm_pop_245, 0.8),
            GDP_585_20th_perc = fun_ineq1(gdp_50_585, norm_pop_585, 0.2),
            GDP_585_80th_perc = fun_ineq1(gdp_50_585, norm_pop_585, 0.8),
            heat_245_20th_perc = fun_ineq1(heat_50_245, norm_pop_245, 0.2),
            heat_245_80th_perc = fun_ineq1(heat_50_245, norm_pop_245, 0.8),
            heat_585_20th_perc = fun_ineq1(heat_50_585, norm_pop_585, 0.2),
            heat_585_80th_perc = fun_ineq1(heat_50_585, norm_pop_585, 0.8),
            heat_D_245_20th_perc = fun_ineq1(delta_50_16_2, norm_pop_245, 0.2),
            heat_D_245_80th_perc = fun_ineq1(delta_50_16_2, norm_pop_245, 0.8),
            heat_D_585_20th_perc = fun_ineq1(delta_50_16_5, norm_pop_585, 0.2),
            heat_D_585_80th_perc = fun_ineq1(delta_50_16_5, norm_pop_585, 0.8)) 


df_heat_m <- merge(df_heat, df_heat_filtered[c("country", "income group", "region")], by = "country", all.x = TRUE)
df_heat_m <- df_heat_m[!duplicated(df_heat_m), ]

# -------------- POOREST: plots -----------------
# Compute robust medians using lmrob by "income group"
medians_income_5 <- aggregate(b_SSP5 ~ `income group`, data = df_poorest_D, FUN = function(x) lmrob(x ~ 1)$coefficients[1])
medians_income_5 <- medians_income_5[order(medians_income_5$`income group`), ]

# Compute 95% confidence intervals using lmrob by "income group"
conf_intervals_income_5 <- aggregate(b_SSP5 ~ `income group`, data = df_poorest_D, FUN = function(x) confint(lmrob(x ~ 1), level = 0.95))
conf_intervals_income_5 <- conf_intervals_income_5[order(conf_intervals_income_5$`income group`), ]

# Compute robust medians using lmrob by "region"
medians_region_5 <- aggregate(b_SSP5 ~ region, data = df_poorest_D, FUN = function(x) lmrob(x ~ 1)$coefficients[1])
medians_region_5 <- medians_region_5[order(medians_region_5$region), ]

# Compute 95% confidence intervals using lmrob by "region"
conf_intervals_region_5 <- aggregate(b_SSP5 ~ region, data = df_poorest_D, FUN = function(x) confint(lmrob(x ~ 1), level = 0.95))
conf_intervals_region_5 <- conf_intervals_region_5[order(conf_intervals_region_5$region), ]

# Extract lower and upper confidence interval values for "income group"
medians_income_5$lower <- as.numeric(apply(conf_intervals_income_5, 1, function(x) x[2]))
medians_income_5$upper <- as.numeric(apply(conf_intervals_income_5, 1, function(x) x[3]))

# Extract lower and upper confidence interval values for "region"
medians_region_5$lower <- as.numeric(apply(conf_intervals_region_5, 1, function(x) x[2]))
medians_region_5$upper <- as.numeric(apply(conf_intervals_region_5, 1, function(x) x[3]))
medians_region_5$lower <- ifelse(medians_region_5$lower<0, 0, medians_region_5$lower)


# ------- Plots for SSP585 POOREST DELTAS ------
# Define a palette of muted colors
library(wesanderson)
wes_colors <- append(wes_palettes$GrandBudapest1,wes_palettes$GrandBudapest2)

# Assign colors to each region
region_colors <- c(
  "East Asia & Pacific" = wes_colors[1],
  "Europe & Central Asia" = wes_colors[2],
  "Latin America & Caribbean" = wes_colors[3],
  "Middle East & North Africa" = wes_colors[4],
  "North America" = wes_colors[5],
  "South Asia" = wes_colors[6],
  "Sub-Saharan Africa" = wes_colors[7]
)

plot_reg_5 <- ggplot(medians_region_5, aes(x = region, y = b_SSP5, fill = region)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = region_colors) +
  geom_point(aes(y = b_SSP5, color = region), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.6) +
  scale_fill_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  scale_color_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_reg_5
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","regions_poor_SSP585_D",".png", sep="")
ggsave(filename = out_path, plot = plot_reg_5, width = 5, height = 3, dpi = 300)

# Assign colors to each region
income_colors <- c(
  "High income" = wes_colors[1],
  "Low income" = wes_colors[2],
  "Lower middle income" = wes_colors[3],
  "Upper middle income" = wes_colors[4]
)

plot_income_5 <- ggplot(medians_income_5, aes(x = `income group`, y = b_SSP5, fill = `income group`)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = income_colors) +
  geom_point(aes(y = b_SSP5, color = `income group`), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.6) +
  scale_fill_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  scale_color_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_income_5
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","income_poor_SSP585_D", ".png", sep="")
ggsave(filename = out_path, plot = plot_income_5, width = 5, height = 3, dpi = 300)



# ------- Plots for 2050 ------
# Compute robust medians using lmrob by "income group"
medians_income50 <- aggregate(b_h50 ~ `income group`, data = df_poorest, FUN = function(x) lmrob(x ~ 1)$coefficients[1])
medians_income50 <- medians_income50[order(medians_income50$`income group`), ]

# Compute 95% confidence intervals using lmrob by "income group"
conf_intervals_income50 <- aggregate(b_h50 ~ `income group`, data = df_poorest, FUN = function(x) confint(lmrob(x ~ 1), level = 0.95))
conf_intervals_income50 <- conf_intervals_income50[order(conf_intervals_income50$`income group`), ]

# Compute robust medians using lmrob by "region"
medians_region50 <- aggregate(b_h50 ~ region, data = df_poorest, FUN = function(x) lmrob(x ~ 1)$coefficients[1])
medians_region50 <- medians_region50[order(medians_region50$region), ]

# Compute 95% confidence intervals using lmrob by "region"
conf_intervals_region50 <- aggregate(b_h50 ~ region, data = df_poorest, FUN = function(x) confint(lmrob(x ~ 1), level = 0.95))
conf_intervals_region50 <- conf_intervals_region50[order(conf_intervals_region50$region), ]

# Extract lower and upper confidence interval values for "income group"
medians_income50$lower <- as.numeric(apply(conf_intervals_income50, 1, function(x) x[2]))
medians_income50$upper <- as.numeric(apply(conf_intervals_income50, 1, function(x) x[3]))

# Extract lower and upper confidence interval values for "region"
medians_region50$lower <- as.numeric(apply(conf_intervals_region50, 1, function(x) x[2]))
medians_region50$upper <- as.numeric(apply(conf_intervals_region50, 1, function(x) x[3]))
medians_region50$lower <- ifelse(medians_region50$lower<0, 0, medians_region50$lower)

# Define a palette of muted colors
library(wesanderson)

plot_reg50 <- ggplot(medians_region50, aes(x = region, y = b_h50, fill = region)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = region_colors) +
  geom_point(aes(y = b_h50, color = region), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.6) +
  scale_fill_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  scale_color_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_reg50
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","regions50_SSP", ssp,".png", sep="")
ggsave(filename = out_path, plot = plot_reg50, width = 5, height = 3, dpi = 300)


plot_income50 <- ggplot(medians_income50, aes(x = `income group`, y = b_h50, fill = `income group`)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = income_colors) +
  geom_point(aes(y = b_h50, color = `income group`), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.6) +
  scale_fill_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  scale_color_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_income50
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","income50_SSP", ssp,".png", sep="")
ggsave(filename = out_path, plot = plot_income50, width = 5, height = 3, dpi = 300)






# -------------- RICHEST DELTAS SSP585 -----------------

# --- plots

# Compute robust medians using lmrob by "income group"
medians_income_5 <- aggregate(b_SSP5 ~ `income group`, data = df_richest_D, FUN = function(x) lmrob(x ~ 1)$coefficients[1])
medians_income_5 <- medians_income_5[order(medians_income_5$`income group`), ]

# Compute 95% confidence intervals using lmrob by "income group"
conf_intervals_income_5 <- aggregate(b_SSP5 ~ `income group`, data = df_richest_D, FUN = function(x) confint(lmrob(x ~ 1), level = 0.95))
conf_intervals_income_5 <- conf_intervals_income_5[order(conf_intervals_income_5$`income group`), ]

# Compute robust medians using lmrob by "region"
medians_region_5 <- aggregate(b_SSP5 ~ region, data = df_richest_D, FUN = function(x) lmrob(x ~ 1)$coefficients[1])
medians_region_5 <- medians_region_5[order(medians_region_5$region), ]

# Compute 95% confidence intervals using lmrob by "region"
conf_intervals_region_5 <- aggregate(b_SSP5 ~ region, data = df_richest_D, FUN = function(x) confint(lmrob(x ~ 1), level = 0.95))
conf_intervals_region_5 <- conf_intervals_region_5[order(conf_intervals_region_5$region), ]

# Extract lower and upper confidence interval values for "income group"
medians_income_5$lower <- as.numeric(apply(conf_intervals_income_5, 1, function(x) x[2]))
medians_income_5$upper <- as.numeric(apply(conf_intervals_income_5, 1, function(x) x[3]))
medians_income_5$lower <- ifelse(medians_income_5$lower<0, 0, medians_income_5$lower)

# Extract lower and upper confidence interval values for "region"
medians_region_5$lower <- as.numeric(apply(conf_intervals_region_5, 1, function(x) x[2]))
medians_region_5$upper <- as.numeric(apply(conf_intervals_region_5, 1, function(x) x[3]))
medians_region_5$lower <- ifelse(medians_region_5$lower<0, 0, medians_region_5$lower)


# ---- Plot 2030 -----
plot_reg_rich_5 <- ggplot(medians_region_5, aes(x = region, y = b_SSP5, fill = region)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = region_colors) +
  geom_point(aes(y = b_SSP5, color = region), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.6) +
  scale_fill_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  scale_color_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_reg_rich_5
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","regions_rich_SSP585_D",".png", sep="")
ggsave(filename = out_path, plot = plot_reg_rich_5, width = 5, height = 3, dpi = 300)


plot_income_5 <- ggplot(medians_income_5, aes(x = `income group`, y = b_SSP5, fill = `income group`)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = income_colors) +
  geom_point(aes(y = b_SSP5, color = `income group`), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.6) +
  scale_fill_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  scale_color_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_income_5
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","income_rich_SSP585_D",".png", sep="")
ggsave(filename = out_path, plot = plot_income_5, width = 5, height = 3, dpi = 300)


# ------- Plots for 2050 ------
# Compute robust medians using lmrob by "income group"
medians_income50 <- aggregate(b_h50 ~ `income group`, data = df_richest, FUN = function(x) lmrob(x ~ 1)$coefficients[1])
medians_income50 <- medians_income50[order(medians_income50$`income group`), ]

# Compute 95% confidence intervals using lmrob by "income group"
conf_intervals_income50 <- aggregate(b_h50 ~ `income group`, data = df_richest, FUN = function(x) confint(lmrob(x ~ 1), level = 0.95))
conf_intervals_income50 <- conf_intervals_income50[order(conf_intervals_income50$`income group`), ]

# Compute robust medians using lmrob by "region"
medians_region50 <- aggregate(b_h50 ~ region, data = df_richest, FUN = function(x) lmrob(x ~ 1)$coefficients[1])
medians_region50 <- medians_region50[order(medians_region50$region), ]

# Compute 95% confidence intervals using lmrob by "region"
conf_intervals_region50 <- aggregate(b_h50 ~ region, data = df_richest, FUN = function(x) confint(lmrob(x ~ 1), level = 0.95))
conf_intervals_region50 <- conf_intervals_region50[order(conf_intervals_region50$region), ]

# Extract lower and upper confidence interval values for "income group"
medians_income50$lower <- as.numeric(apply(conf_intervals_income50, 1, function(x) x[2]))
medians_income50$upper <- as.numeric(apply(conf_intervals_income50, 1, function(x) x[3]))
medians_income50$lower <- ifelse(medians_income50$lower<0, 0, medians_income50$lower)

# Extract lower and upper confidence interval values for "region"
medians_region50$lower <- as.numeric(apply(conf_intervals_region50, 1, function(x) x[2]))
medians_region50$upper <- as.numeric(apply(conf_intervals_region50, 1, function(x) x[3]))
medians_region50$lower <- ifelse(medians_region50$lower<0, 0, medians_region50$lower)


plot_reg50 <- ggplot(medians_region50, aes(x = region, y = b_h50, fill = region)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = region_colors) +
  geom_point(aes(y = b_h50, color = region), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.6) +
  scale_fill_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  scale_color_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_reg50
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","richest_regions50_SSP", ssp,".png", sep="")
ggsave(filename = out_path, plot = plot_reg50, width = 5, height = 3, dpi = 300)


plot_income50 <- ggplot(medians_income50, aes(x = `income group`, y = b_h50, fill = `income group`)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = income_colors) +
  geom_point(aes(y = b_h50, color = `income group`), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 0.6) +
  scale_fill_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  scale_color_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_income50
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","richest_income50_SSP", ssp,".png", sep="")
ggsave(filename = out_path, plot = plot_income50, width = 5, height = 3, dpi = 300)





# -------- POCs --------


# ------------- INDIGENOUS LANDS ------------
# --- plots -------

# Compute robust medians using lmrob by "income group"
medians_income_5 <- aggregate(b_SSP5 ~ `income group`, data = df_ind_prop, FUN = function(x) lmrob(x ~ 1)$coefficients[1])
medians_income_5 <- medians_income_5[order(medians_income_5$`income group`), ]

# Compute 95% confidence intervals using lmrob by "income group"
conf_intervals_income_5 <- aggregate(b_SSP5 ~ `income group`, data = df_ind_prop, FUN = function(x) confint(lmrob(x ~ 1), level = 0.9))
conf_intervals_income_5 <- conf_intervals_income_5[order(conf_intervals_income_5$`income group`), ]

df_ind_prop <- df_ind_prop[!df_ind_prop$region%in%c("North America"),]
# Compute robust medians using lmrob by "region"
medians_region_5 <- aggregate(b_SSP5 ~ region, data = df_ind_prop, FUN = function(x) median(x))
medians_region_5 <- medians_region_5[order(medians_region_5$region), ]

library(boot)

# Function to compute the median
median_fun <- function(x, indices) {
  median(x[indices], na.rm = TRUE)
}

# Function to perform bootstrap resampling and compute confidence intervals
bootstrap_ci <- function(x, alpha = 0.95, nboot = 1000) {
  boot_obj <- boot(x, statistic = median_fun, R = nboot)
  ci <- boot.ci(boot_obj, type = "basic", conf = alpha)
  return(ci$basic)
}

# Compute bootstrap confidence intervals by region
conf_intervals_region_5 <- aggregate(b_SSP5 ~ region, data = df_ind_prop, FUN = bootstrap_ci)

conf_intervals_region_5 <- conf_intervals_region_5[order(conf_intervals_region_5$region), ]

# Extract lower and upper confidence interval values for "income group"
medians_income_5$lower <- as.numeric(apply(conf_intervals_income_5, 1, function(x) x[2]))
medians_income_5$upper <- as.numeric(apply(conf_intervals_income_5, 1, function(x) x[3]))
medians_income_5$lower <- ifelse(medians_income_5$lower<0, 0, medians_income_5$lower)
medians_income_5$upper <- ifelse(medians_income_5$upper>1, 1, medians_income_5$upper)

# Extract lower and upper confidence interval values for "region"
medians_region_5$lower <- as.numeric(apply(conf_intervals_region_5, 1, function(x) x[5]))
medians_region_5$upper <- as.numeric(apply(conf_intervals_region_5, 1, function(x) x[6]))




# ------- Plots for 2030 ------
# Define a palette of muted colors
library(wesanderson)
wes_colors <- append(wes_palettes$GrandBudapest1,wes_palettes$GrandBudapest2)

# Assign colors to each region
region_colors <- c(
  "East Asia & Pacific" = wes_colors[1],
  #"Europe & Central Asia" = wes_colors[2],
  "Latin America & Caribbean" = wes_colors[3],
  "Middle East & North Africa" = wes_colors[4],
  "South Asia" = wes_colors[6],
  "Sub-Saharan Africa" = wes_colors[7]
)

plot_reg_IL_5 <- ggplot(medians_region_5, aes(x = region, y = b_SSP5, fill = region)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = region_colors) +
  geom_point(aes(y = b_SSP5, color = region), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 1) +
  scale_fill_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  scale_color_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_reg_IL_5

out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","IL_regions_30_SSP", ssp,".png", sep="")
ggsave(filename = out_path, plot = plot_reg_IL_5, width = 5, height = 3, dpi = 300)

# Assign colors to each region
income_colors <- c(
  "High income" = wes_colors[1],
  "Low income" = wes_colors[2],
  "Lower middle income" = wes_colors[3],
  "Upper middle income" = wes_colors[4]
)

plot_income_5 <- ggplot(medians_income_5, aes(x = `income group`, y = b_SSP5, fill = `income group`)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = income_colors) +
  geom_point(aes(y = b_SSP5, color = `income group`), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 1) +
  scale_fill_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  scale_color_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_income_5
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","IL_income_30_SSP", ssp,".png", sep="")
ggsave(filename = out_path, plot = plot_income30, width = 5, height = 3, dpi = 300)



# ------ plots for 2050 -------

# Compute robust medians using lmrob by "income group"
medians_income50 <- aggregate(b_h50 ~ `income group`, data = df_ind_prop, FUN = function(x) median(x))
medians_income50 <- medians_income50[order(medians_income50$`income group`), ]

# Compute 95% confidence intervals using lmrob by "income group"
conf_intervals_income50 <- aggregate(b_h50 ~ `income group`, data = df_ind_prop, FUN = bootstrap_ci)
conf_intervals_income50 <- conf_intervals_income50[order(conf_intervals_income50$`income group`), ]
# Compute robust medians using lmrob by "region"
medians_region50 <- aggregate(b_h50 ~ region, data = df_ind_prop, FUN = function(x) median(x))
medians_region50 <- medians_region50[order(medians_region50$region), ]

# Compute bootstrap confidence intervals by region
conf_intervals_region50 <- aggregate(b_h50 ~ region, data = df_ind_prop, FUN = bootstrap_ci)
conf_intervals_region50 <- conf_intervals_region50[order(conf_intervals_region50$region), ]

# Extract lower and upper confidence interval values for "income group"
medians_income50$lower <- as.numeric(apply(conf_intervals_income50, 1, function(x) x[5]))
medians_income50$upper <- as.numeric(apply(conf_intervals_income50, 1, function(x) x[6]))
medians_income50$lower <- ifelse(medians_income50$lower<0, 0, medians_income50$lower)
medians_income50$upper <- ifelse(medians_income50$upper>1, 1, medians_income50$upper)

# Extract lower and upper confidence interval values for "region"
medians_region50$lower <- as.numeric(apply(conf_intervals_region50, 1, function(x) x[5]))
medians_region50$upper <- as.numeric(apply(conf_intervals_region50, 1, function(x) x[6]))
medians_region50$lower <- ifelse(medians_region50$lower<0, 0, medians_region50$lower)
medians_region50$upper <- ifelse(medians_region50$upper>1, 1, medians_region50$upper)



# ------- Plots for 2050 ------

plot_reg50 <- ggplot(medians_region50, aes(x = region, y = b_h50, fill = region)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = region_colors) +
  geom_point(aes(y = b_h50, color = region), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 1) +
  scale_fill_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  scale_color_manual(values = region_colors, guide = guide_legend(title = "Region")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_reg50
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","IL_regions_50_SSP", ssp,".png", sep="")
ggsave(filename = out_path, plot = plot_reg50, width = 5, height = 3, dpi = 300)



plot_income50 <- ggplot(medians_income50, aes(x = `income group`, y = b_h50, fill = `income group`)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.4, size = 1.4, alpha = 0.8, color = income_colors) +
  geom_point(aes(y = b_h50, color = `income group`), size = 2) +
  labs(x = NULL, y = "Proportions") +
  geom_hline(yintercept = 0.2, linetype = "dashed", color = "black") +
  ylim(0, 1) +
  scale_fill_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  scale_color_manual(values = income_colors, guide = guide_legend(title = "Income group")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 16),
    plot.title = element_text(size = 22),
    axis.title.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "right"
  )

plot_income50
out_path <- paste("/Users/naiacasina/Documents/ENVS/Results/Heat Stress Projections/CHIRTS projections/","IL_income_50_SSP", ssp,".png", sep="")
ggsave(filename = out_path, plot = plot_income50, width = 5, height = 3, dpi = 300)



