# First plot of the second panel

# --------------- Clear the environment and load packages ---------------
rm(list=ls()) 

# Load libraries
packages <- c("terra", "raster", "tidyverse", "rasterVis", "ncdf4", 
              "lattice", "foreign", "rworldmap", "data.table",
              "classInt", "patchwork", "matrixStats", "dplyr",
              "xtable", "BMisc", "latex2exp", "readxl", "sf", "networkD3",
              'car', "khroma", "gapminder", "modi", "boot")
lapply(packages, require, character=TRUE)

# Boot function
boot_lm <- function(original_data, original_model,
                    type = c('ordinary', 'param'),
                    B = 1000L, seed = 1) {
  set.seed(seed)
  betas_original_model <- coef(original_model)
  len_coef <- length(betas_original_model)
  mat <- matrix(rep(0L, B * len_coef), ncol = len_coef)
  if (type %in% 'ordinary') {
    n_rows <- length(residuals(original_model))
    for (i in seq_len(B)) {
      boot_dat <- original_data[sample(seq_len(n_rows), replace = TRUE), ]
      mat[i, ] <- coef(lm(terms(original_model), data = boot_dat))
    }
  }
  if (type %in% 'param') {
    X <- model.matrix(delete.response(terms(original_model)),
                      data = original_data)[, -1L]
    for (i in seq_len(B)) {
      mat[i, ] <- coef(lm(unlist(simulate(original_model)) ~ X,
                          data = original_data))
    }
  }
  confints <- matrix(rep(0L, 2L * len_coef), ncol = 2L)
  pvals <- numeric(len_coef)
  for (i in seq_len(len_coef)) {
    pvals[i] <- mean(abs(mat[, i] - mean(mat[, i])) > abs(betas_original_model[i]))
    confints[i, ] <- quantile(mat[, i], c(.01, 0.99))
  }
  names(pvals) <- names(betas_original_model)
  out <- data.frame(estimate = betas_original_model,
                    'lwr' = confints[, 1], 'upr' = confints[, 2],
                    p_value = pvals)
  return(out)
}

# ----------------------- Plot ------------------------- 
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq30000_sens_all.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq30000_sens_IPC.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq30000_sens_wa.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_panel1_ineq30000_sens_heat.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/c_food_IPC.R")
load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/wb.R")
load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainHRV.R")
countries_hm <- c.food
# 
# # Edits per dimension
df_ineq_m <- df_ineq_m[!df_ineq_m$country%in%c("Bermuda", "Cape Verde",
                                                "Isle of Man", "Greenland",
                                                "Dominica", "Barbados",
                                                "Cyprus", "French Guiana",
                                                "Hong Kong S.A.R."),]
df_ineq_m_pm <- df_ineq_m[!df_ineq_m$country%in%c("Greenland", "Bermuda"),]
df_ineq_m_wa <- df_ineq_m.wa
df_ineq_m_heat <- df_ineq_m.heat[!df_ineq_m.heat$country%in%c("Djibouti"),]
df_ineq_m_heat <- df_ineq_m_heat[df_ineq_m_heat$q_80_heat>0,]
df_ineq_m_bii <- df_ineq_m[df_ineq_m$median_bii>0,]
df_ineq_m_hm <- df_ineq_m.food



# ---- Bootstrap ----

# --- Air Quality ----
fm0 <- lm(diff_pm25 ~ median_pm25, data = df_ineq_m_pm)

# non-parametric bootstrap
ordinary <- boot_lm(original_data = df_ineq_m_pm, original_model = fm0,
                    type = 'ordinary', B = 1e4)

df_ineq_m_pm$med_y <- ordinary$estimate[1] + ordinary$estimate[2]*df_ineq_m_pm$median_pm25
df_ineq_m_pm$low <- ordinary$lwr[1] + ordinary$lwr[2]*df_ineq_m_pm$median_pm25
df_ineq_m_pm$high <- ordinary$upr[1] + ordinary$upr[2]*df_ineq_m_pm$median_pm25
# -------------------

# --- BII ----
fm0 <- lm(diff_bii ~ median_bii, data = df_ineq_m_bii)

# non-parametric bootstrap
ordinary <- boot_lm(original_data = df_ineq_m_bii, original_model = fm0,
                    type = 'ordinary', B = 1e4)

df_ineq_m_bii$med_y <- ordinary$estimate[1] + ordinary$estimate[2]*df_ineq_m_bii$median_bii
df_ineq_m_bii$low <- ordinary$lwr[1] + ordinary$lwr[2]*df_ineq_m_bii$median_bii
df_ineq_m_bii$high <- ordinary$upr[1] + ordinary$upr[2]*df_ineq_m_bii$median_bii
# -------------------

# --- WAccess ----
fm0 <- lm(diff_waccess ~ median_waccess, data = df_ineq_m_wa)

# non-parametric bootstrap
ordinary <- boot_lm(original_data = df_ineq_m_wa, original_model = fm0,
                    type = 'ordinary', B = 1e4)

df_ineq_m_wa$med_y <- ordinary$estimate[1] + ordinary$estimate[2]*df_ineq_m_wa$median_waccess
df_ineq_m_wa$low <- ordinary$lwr[1] + ordinary$lwr[2]*df_ineq_m_wa$median_waccess
df_ineq_m_wa$high <- ordinary$upr[1] + ordinary$upr[2]*df_ineq_m_wa$median_waccess
# -------------------

# --- WSuf ----
fm0 <- lm(diff_wsuf ~ median_wsuf, data = df_ineq_m)

# non-parametric bootstrap
ordinary <- boot_lm(original_data = df_ineq_m, original_model = fm0,
                    type = 'ordinary', B = 1e4)

df_ineq_m$med_y_wsuf <- ordinary$estimate[1] + ordinary$estimate[2]*df_ineq_m$median_wsuf
df_ineq_m$low_wsuf <- ordinary$lwr[1] + ordinary$lwr[2]*df_ineq_m$median_wsuf
df_ineq_m$high_wsuf <- ordinary$upr[1] + ordinary$upr[2]*df_ineq_m$median_wsuf
# -------------------


# --- Heat ----
fm0 <- lm(diff_heat ~ median_heat, data = df_ineq_m_heat)

# non-parametric bootstrap
ordinary <- boot_lm(original_data = df_ineq_m_heat, original_model = fm0,
                    type = 'ordinary', B = 1e4)

df_ineq_m_heat$med_y_heat <- ordinary$estimate[1] + ordinary$estimate[2]*df_ineq_m_heat$median_heat
df_ineq_m_heat$low_heat <- ordinary$lwr[1] + ordinary$lwr[2]*df_ineq_m_heat$median_heat
df_ineq_m_heat$high_heat <- ordinary$upr[1] + ordinary$upr[2]*df_ineq_m_heat$median_heat
# -------------------


# --- Pest ----
fm0 <- lm(diff_pest ~ median_pest, data = df_ineq_m)

# non-parametric bootstrap
ordinary <- boot_lm(original_data = df_ineq_m, original_model = fm0,
                    type = 'ordinary', B = 1e4)

df_ineq_m$med_y_pest <- ordinary$estimate[1] + ordinary$estimate[2]*df_ineq_m$median_pest
df_ineq_m$low_pest <- ordinary$lwr[1] + ordinary$lwr[2]*df_ineq_m$median_pest
df_ineq_m$high_pest <- ordinary$upr[1] + ordinary$upr[2]*df_ineq_m$median_pest
# -------------------


# --- IPC ----
fm0 <- lm(diff_food ~ median_food, data = df_ineq_m_hm)

# non-parametric bootstrap
ordinary <- boot_lm(original_data = df_ineq_m_hm, original_model = fm0,
                    type = 'ordinary', B = 1e4)

df_ineq_m_hm$med_y <- ordinary$estimate[1] + ordinary$estimate[2]*df_ineq_m_hm$median_food
df_ineq_m_hm$low <- ordinary$lwr[1] + ordinary$lwr[2]*df_ineq_m_hm$median_food
df_ineq_m_hm$high <- ordinary$upr[1] + ordinary$upr[2]*df_ineq_m_hm$median_food
# -------------------


df_medians <- c(df_ineq_m_bii$median_bii, df_ineq_m_heat$median_heat,
                df_ineq_m$median_pest,
                df_ineq_m$median_pm25, df_ineq_m_wa$median_waccess,
                df_ineq_m$median_wsuf, df_ineq_m_hm$median_food)

df_diffs <- c(df_ineq_m_bii$med_y, df_ineq_m_heat$med_y_heat,
              df_ineq_m$med_y_pest, 
              df_ineq_m_pm$med_y, df_ineq_m_wa$med_y,
              df_ineq_m$med_y_wsuf, df_ineq_m_hm$med_y)

df_CIL <- c(df_ineq_m_bii$low, df_ineq_m_heat$low_heat,
             df_ineq_m$low_pest, 
            df_ineq_m_pm$low, df_ineq_m_wa$low,
            df_ineq_m$low_wsuf, df_ineq_m_hm$low)

df_CIH <- c(df_ineq_m_bii$high, df_ineq_m_heat$high_heat,
            df_ineq_m$high_pest, 
            df_ineq_m_pm$high, df_ineq_m_wa$high,
            df_ineq_m$high_wsuf, df_ineq_m_hm$high)

df_dims <- c(rep("bii",dim(df_ineq_m_bii)[1]), rep("heat",dim(df_ineq_m_heat)[1]),
             rep("pest",dim(df_ineq_m)[1]),
             rep("pm2.5",dim(df_ineq_m_pm)[1]), rep("waccess",dim(df_ineq_m_wa)[1]), 
             rep("wsuf",dim(df_ineq_m)[1]), rep("food",dim(df_ineq_m_hm)[1]) )

df_plots <- data.frame(medians=df_medians, diffs=df_diffs, lower=df_CIL, upper=df_CIH, dim=df_dims)


save(df_plots, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_plots_bootstrapped1.R")


# ----------- LOAD DF TO PLOT -----------
load("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_plots_bootstrapped1.R")


ggplot(df_plots, aes(x=medians,y=diffs, color=dim)) + 
  #geom_point()+
  geom_quantile(quantiles = 0.5, size = 1, alpha = 0.7)+
  #geom_line(col=dim) +
  geom_ribbon(df_plots, aes(ymin = lower, ymax = upper), alpha = 0.1) +
  xlab("Median") +
  ylab("Inequality") +
  ylim(-0.2,1) +
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  theme_minimal()
  #scale_color_brewer(palette="Set2")

ggplot(df_plots, aes(x=medians,y=diffs, color=dim)) + 
  #geom_point()+
  geom_quantile(quantiles = 0.5, size = 1, alpha = 0.7)+
  #geom_quantile(quantiles = 0.5, colour = "grey", method = "rq", size = 1, alpha = 0.8)+
  #stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour = "red") + 
  #stat_summary(fun.y = mean, geom = "point", colour = "red") +
  xlab("Median") +
  ylab("Inequality") +
  ylim(0,1) +
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  theme_minimal()+
  scale_color_brewer(palette="Set2")


# Main plot
ggplot(df_ineq_m, aes(x=median_pest, y=med_y_pest)) + 
  geom_line(col='red') + 
  geom_ribbon(aes(ymin = low_pest, ymax = high_pest), alpha = 0.1, fill='red') +
  xlab("Median") +
  ylab("Inequality") +
  ylim(-0.25,1) +
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  theme_minimal()+
  scale_color_brewer(palette="Set2")


# MAIN PLOTS
# Set colour palette
#light <- colour("light")

light <-  c("#67AB4F","#BF5841", "#D9751E","#3A88B5", "#5D9671","#D9981E",  "#864975")


p1 <- ggplot(df_ineq_m_bii, aes(x=median_bii,y=med_y)) + 
  #geom_point()+
  geom_quantile(quantiles = 0.5, size = 1, alpha = 0.7, colour = light[1])+
  #geom_line(col=dim) +
  geom_ribbon(aes(ymin = low, ymax = high), 
              size=0.01, alpha = 0.2, fill = light[1]) +
  xlab("Median") +
  ylab("Inequality") +
  ylim(-0.1,1) +
  ggtitle("Biodiversity") +
  theme_minimal()


p2 <- ggplot(df_ineq_m_heat, aes(x=median_heat,y=med_y_heat)) + 
  #geom_point()+
  geom_quantile(quantiles = 0.5, size = 1, alpha = 0.7, colour = light[2])+
  #geom_line(col=dim) +
  geom_ribbon(aes(ymin = low_heat, ymax = high_heat), 
              size=0.01, alpha = 0.2, fill = light[2]) +
  xlab("Median") +
  ylab("Inequality") +
  ggtitle("Heat stress") +
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  theme_minimal() +
  coord_cartesian(ylim = c(-0.1, 1)) # Set the visible y-limits with coord_cartesian


p6 <- ggplot(df_ineq_m, aes(x=median_pest,y=med_y_pest)) + 
  #geom_point()+
  geom_quantile(quantiles = 0.5, size = 1, alpha = 0.7, colour = light[6])+
  #geom_line(col=dim) +
  geom_ribbon(aes(ymin = low_pest, ymax = high_pest), 
              size=0.01, alpha = 0.2, fill = light[6]) +
  xlab("Median") +
  ylab("Inequality") +
  ylim(-0.1,1) +
  ggtitle("Banned pesticides") +
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  theme_minimal()

p7 <- ggplot(df_ineq_m_hm, aes(x=median_food,y=med_y)) + 
  #geom_point()+
  geom_quantile(quantiles = 0.5, size = 1, alpha = 0.7, colour = light[7])+
  #geom_line(col=dim) +
  geom_ribbon(aes(ymin = low, ymax = high), 
              size=0.01, alpha = 0.2, fill = light[7]) +
  xlab("Median") +
  ylab("Inequality") +
  ylim(-0.1,1) +
  ggtitle("Access to food") +
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  theme_minimal()


p3 <- ggplot(df_ineq_m_pm, aes(x=median_pm25,y=med_y)) + 
  #geom_point()+
  geom_quantile(quantiles = 0.5, size = 1, alpha = 0.7, colour = light[3])+
  #geom_line(col=dim) +
  geom_ribbon(aes(ymin = low, ymax = high), 
              size=0.01, alpha = 0.2, fill = light[3]) +
  xlab("Median") +
  ylab("Inequality") +
  ggtitle("Air quality") +
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  theme_minimal()+
  coord_cartesian(ylim = c(-0.1, 1))

p4 <- ggplot(df_ineq_m_wa, aes(x=median_waccess,y=med_y)) + 
  #geom_point()+
  geom_quantile(quantiles = 0.5, size = 1, alpha = 0.7, colour = light[4])+
  #geom_line(col=dim) +
  geom_ribbon(aes(ymin = low, ymax = high), 
              size=0.01, alpha = 0.2, fill = light[4]) +
  xlab("Median") +
  ylab("Inequality") +
  ylim(-0.1,1) +
  ggtitle("Access to sanitation") +
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  theme_minimal()

p5 <- ggplot(df_ineq_m, aes(x=median_wsuf,y=med_y_wsuf)) + 
  #geom_point()+
  geom_quantile(quantiles = 0.5, size = 1, alpha = 0.7, colour = light[5])+
  #geom_line(col=dim) +
  geom_ribbon(aes(ymin = low_wsuf, ymax = high_wsuf), 
              size=0.01, alpha = 0.2, fill = light[5]) +
  xlab("Median") +
  ylab("Inequality") +
  ggtitle("Water scarcity") +
  theme(legend.position = "none",
        axis.title.y = element_blank())+
  theme_minimal()+
  coord_cartesian(ylim = c(-0.1, 1))

p_tot <- p1 + p2 + p3 + p4 + p5 + p6 + p7

p_tot
# Save
ggsave(plot = p_tot,
       filename = "ineq_vs_medianALL_wFood.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Second panel/First figure/",
       width = 10, height = 7, units = "in",
       dpi = 320)

# If RWI limited area:
ggsave(plot = p_tot,
       filename = "ineq_vs_medianALL_wFood_rwi_limited_area.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Second panel/First figure/",
       width = 10, height = 7, units = "in",
       dpi = 320)
