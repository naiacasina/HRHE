# Descriptive stats plots

library(ggplot2)


custom_colors <- c("#77A95A", "#869758", "#938256", "#AA7A48", "#9F5154")

# Example data
region_names <- c("NA", "ECA", "SA", "MENA", "SSA", "LAC", "EAP")
rights_violations <- c("One", "Two", "Three", "Four", "Five")
# percentages_min <- matrix(c(2, 1, 0, 0, 0,
#                             4, 2, 1, 0, 0,
#                             28, 40, 50, 42, 45,
#                             6, 7, 5, 4, 1,
#                             18, 13, 9, 3, 1,
#                             7, 2, 0, 0, 0,
#                             35, 35, 35, 51, 53), ncol = 5, byrow = TRUE)
# percentages_max <- matrix(c(4, 5, 4, 2, 1,
#                             11, 11, 8, 3, 0,
#                             25, 25, 29, 36, 29,
#                             5, 6, 6, 5, 3,
#                             17, 14, 13, 10, 6,
#                             8, 9, 8, 5, 2,
#                             30, 30, 32, 39, 59), ncol = 5, byrow = TRUE)

percentages_min <- matrix(c(2, 1, 0, 0, 0,
                            4, 2, 1, 0, 0,
                            28, 40, 50, 42, 45,
                            6, 7, 5, 4, 1,
                            18, 13, 9, 3, 1,
                            7, 2, 0, 0, 0,
                            35, 35, 35, 51, 53), ncol = 5, byrow = TRUE)
percentages_max <- matrix(c(4, 5, 4, 2, 1,
                            11, 11, 8, 3, 0,
                            25, 25, 29, 36, 29,
                            5, 6, 6, 5, 3,
                            17, 14, 13, 10, 6,
                            8, 9, 8, 5, 2,
                            30, 30, 32, 39, 59), ncol = 5, byrow = TRUE)

# Create data frame
data <- data.frame(region = rep(region_names, each = length(rights_violations)),
                   rights_violation = factor(rep(rights_violations, times = length(region_names)), levels = rights_violations),
                   min_percentage = as.vector(t(percentages_min)),
                   max_percentage = as.vector(t(percentages_max)))

# Create the plot
ggplot(data, aes(x = region, y = max_percentage - min_percentage, fill = rights_violation)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "World Region", y = "Percentage Range", fill = expression(atop("Rights", "Violations"))) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 14),  # Change x-axis label size
        axis.text.y = element_text(size = 14),  # Change y-axis label size
        axis.title = element_text(size = 16),   # Change axis title size
        legend.text = element_text(size = 14),   # Change legend text size
        legend.title = element_text(size = 16))  # Change legend title size

ggsave("/Users/naiacasina/Documents/ENVS/Results/Descriptive statistics/rights_violations_plot.png", plot = last_plot(), width = 8, height = 6, dpi = 600)


# Example data
rights_violations <- c("One", "Two", "Three", "Four", "Five")
people_min <- c(5.4, 3.5, 1.9, 0.23, 0.0089)
people_max <- c(7.6, 7.3, 5.9, 3.5, 0.82)
additional_data <- c(7.6, 6.8, 4.4, 2, 0.47)

# Create data frame
data <- data.frame(rights_violation = factor(rights_violations, levels = rights_violations),
                   min_people = people_min,
                   max_people = people_max,
                   additional_data = additional_data)


# Create the plot
ggplot(data, aes(x = rights_violation, y = min_people, fill = rights_violation)) +
  geom_bar(aes(y = additional_data), stat = "identity", position = position_dodge(), alpha=1) +
  geom_errorbar(aes(ymin = min_people, ymax = max_people), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_point(aes(y = additional_data), shape = 16, size = 3, color = "black", position = position_dodge(width = 0.9)) +
  labs(x = "Rights Violation", y = "Number of People (in B)", fill = expression(atop("Rights", "Violations"))) +
  scale_fill_manual(values = custom_colors) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16),  # Change x-axis label size
        axis.text.y = element_text(size = 16),  # Change y-axis label size
        axis.title = element_text(size = 16),   # Change axis title size
        legend.text = element_text(size = 16),   # Change legend text size
        legend.title = element_text(size = 16))  # Change legend title size

ggsave("/Users/naiacasina/Documents/ENVS/Results/Descriptive statistics/rights_violations_ranges.png", plot = last_plot(), width = 6, height = 9, dpi = 600)



# ------------ PLOTTING FOR THE SENSITIVITY ANALYSIS -----------------
library(ggplot2)

# 1. Reading the CSV file
file_path <- "/Users/naiacasina/Documents/ENVS/Sensitivity/sens_numbers.csv"
df <- read.csv(file_path)

# 2. Computing the error bars
rights_violations <- c("a_One", "b_Two", "c_Three", "d_Four", "e_Five")
df$min_people <- apply(df[2:ncol(df)], 1, min)/1e9 # Assuming the sensitivity columns are from 2 to the end
df$max_people <- apply(df[2:ncol(df)], 1, max)/1e9

# Create the plot
ggplot(df, aes(x = rights_violations, y = df[,2]/1e9, fill = rights_violations)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha=1) +
  geom_errorbar(aes(ymin = min_people, ymax = max_people), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_point(shape = 16, size = 3, color = "black", position = position_dodge(width = 0.9)) +
  labs(x = "Rights Violation", y = "Number of People (in B)", fill = expression(atop("Rights", "Violations"))) +
  scale_fill_manual(values = custom_colors) + # Make sure custom_colors is defined
  theme_minimal() +
  theme(axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16))

ggsave("/Users/naiacasina/Documents/ENVS/Results/Descriptive statistics/rights_violations_sensitivity.png", plot = last_plot(), width = 6, height = 9, dpi = 600)

