# Clear the environment
rm(list=ls()) 
# Load libraries
packages <- c("sp", "rworldmap", "ggplot2", "dplyr",
              "tidyr", "stringr", "RColorBrewer", "readxl",
              "xtable", "camiller", "caret", "modi")
lapply(packages, require, character=TRUE)


# -----------------------------------------------------------------------------

pop_number <- 1

if (pop_number==1){
  pop_layer <- "ppp_2020_1km_Aggregated"
  pop_path <- "pop1"
}else{
  pop_layer <- "gpw_v4_population_count_rev11_2020_2pt5_min"
  pop_path <- "pop2"
}


load(file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/df_mainHRV.R")

# ------ Descriptive stats for INCOME REGIONS ------
i <- 'Total Pop'
lic_v <- sum(df_m[(df_m$`income group`=='Low income'), pop_layer], na.rm=TRUE)
lmic_v <- sum(df_m[(df_m$`income group`=='Lower middle income'), pop_layer], na.rm=TRUE)
umic_v <- sum(df_m[(df_m$`income group`=='Upper middle income'), pop_layer], na.rm=TRUE)
hic_v <- sum(df_m[(df_m$`income group`=='High income'), pop_layer], na.rm=TRUE)

df_stats_n <- data.frame(HRV=i,
                       LIC=big_number(lic_v, digits=2),
                       LMIC=big_number(lmic_v),
                       UMIC=big_number(umic_v),
                       HIC=big_number(hic_v))

s_all_pop <- 0
hrv <- c("HRV1" ,"HRV2", "HRV3", "HRV4", "HRV5")
# Loop it until HRV=4 (max)
for(i in 1:length(hrv)) {                                     
  lic_v <- sum(df_m[(df_m$`income group`=='Low income'), hrv[i]], na.rm=TRUE)
  lmic_v <- sum(df_m[(df_m$`income group`=='Lower middle income'), hrv[i]], na.rm=TRUE)
  umic_v <- sum(df_m[(df_m$`income group`=='Upper middle income'), hrv[i]], na.rm=TRUE)
  hic_v <- sum(df_m[(df_m$`income group`=='High income'), hrv[i]], na.rm=TRUE)
  
  s_all <- lic_v + lmic_v + umic_v + hic_v
  
  lic_v <- round(lic_v/s_all*100)
  lmic_v <- round(lmic_v/s_all*100)
  umic_v <- round(umic_v/s_all*100)
  hic_v <- round(hic_v/s_all*100)
  
  all_v <- lic_v + lmic_v + umic_v + hic_v
  
  hic_v <- hic_v - (all_v-100)
  df_stats <- data.frame(HRV=i,
                         LIC=paste(lic_v, "%", sep=""),
                         LMIC=paste(lmic_v, "%", sep=""),
                         UMIC=paste(umic_v, "%", sep=""),
                         HIC=paste(hic_v, "%", sep=""))
  
  df_stats_n <- rbind(df_stats_n, df_stats)
  s_all_pop <- append(s_all_pop, s_all)
}

x <- c("1", "2", "3", "4", "5", "Total Pop")

df_stats_n <- df_stats_n %>% slice(match(x, HRV))
df_stats_n <- df_stats_n[df_stats_n$HRV!="Total Pop",]
df_stats_n$Tot_pop <- c(big_number(s_all_pop[2:6]))

path_table <- paste("/Users/naiacasina/Documents/ENVS/Results/Descriptive stats/WB_IncomeRegions","_",pop_path,".tex",sep="")
print(xtable(df_stats_n, type = "latex", align="lcccccc", caption = 'Percentages of total population experiencing up to five HRV across income groups.',digits=c(0,0,0,0,0,0,0)), caption.placement = 'top', include.rownames=FALSE,file = path_table)
path <- paste("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/descriptive_stats1","_",pop_path,".R",sep="")
save(df_stats_n, file=path)


# ------ Descriptive stats for REGIONS ------
i <- 'Total Pop'
sa_p <- sum(df_m[(df_m$`region`=='South Asia'), pop_layer], na.rm=TRUE)
eca_p <- sum(df_m[(df_m$`region`=='Europe & Central Asia'), pop_layer], na.rm=TRUE)
mena_p <- sum(df_m[(df_m$`region`=='Middle East & North Africa'), pop_layer], na.rm=TRUE)
eap_p <- sum(df_m[(df_m$`region`=='East Asia & Pacific'), pop_layer], na.rm=TRUE)
ssa_p <- sum(df_m[(df_m$`region`=='Sub-Saharan Africa'), pop_layer], na.rm=TRUE)
lac_p <- sum(df_m[(df_m$`region`=='Latin America & Caribbean'), pop_layer], na.rm=TRUE)
na_p <- sum(df_m[(df_m$`region`=='North America'), pop_layer], na.rm=TRUE)

df_stats_n2 <- data.frame(HRV=i,
                         SA=big_number(sa_p),
                         ECA=big_number(eca_p),
                         MENA=big_number(mena_p),
                         EAP=big_number(eap_p),
                         SSA=big_number(ssa_p),
                         LAC=big_number(lac_p),
                         NA_=big_number(na_p))
s_all_pop <- 0
# Loop it until HRV=4 (max)
for(i in 1:length(hrv)) {                                     
  sa_v <- sum(df_m[(df_m$`region`=='South Asia'), hrv[i]], na.rm=TRUE)
  eca_v <- sum(df_m[(df_m$`region`=='Europe & Central Asia'), hrv[i]], na.rm=TRUE)
  mena_v <- sum(df_m[(df_m$`region`=='Middle East & North Africa'), hrv[i]], na.rm=TRUE)
  eap_v <- sum(df_m[(df_m$`region`=='East Asia & Pacific'), hrv[i]], na.rm=TRUE)
  ssa_v <- sum(df_m[(df_m$`region`=='Sub-Saharan Africa'), hrv[i]], na.rm=TRUE)
  lac_v <- sum(df_m[(df_m$`region`=='Latin America & Caribbean'), hrv[i]], na.rm=TRUE)
  na_v <- sum(df_m[(df_m$`region`=='North America'), hrv[i]], na.rm=TRUE)
  
  s_all <- sa_v + eca_v + mena_v + eap_v + ssa_v + lac_v + na_v
  
  sa_v <- round(sa_v/s_all*100)
  eca_v <- round(eca_v/s_all*100)
  mena_v <- round(mena_v/s_all*100)
  eap_v <- round(eap_v/s_all*100)
  ssa_v <- round(ssa_v/s_all*100)
  lac_v <- round(lac_v/s_all*100)
  na_v <- round(na_v/s_all*100)
  
  all_v <- sa_v + eca_v + mena_v + eap_v + ssa_v + lac_v + na_v
  
  sa_v <- sa_v + (100-all_v)
  
  df_stats <- data.frame(HRV=round(i),
                         SA=paste(sa_v, "%", sep=""),
                         ECA=paste(eca_v, "%", sep=""),
                         MENA=paste(mena_v, "%", sep=""),
                         EAP=paste(eap_v, "%", sep=""),
                         SSA=paste(ssa_v, "%", sep=""),
                         LAC=paste(lac_v, "%", sep=""),
                         NA_=paste(na_v, "%", sep=""))
  
  df_stats_n2 <- rbind(df_stats_n2, df_stats)
  s_all_pop  <- append(s_all_pop, s_all)
}

df_stats_n2 <- df_stats_n2 %>% slice(match(x, HRV))
df_stats_n2 <- df_stats_n2[df_stats_n2$HRV!="Total Pop",]
df_stats_n2$Tot_pop <- c(big_number(s_all_pop[2:6]))

path_table <- paste("/Users/naiacasina/Documents/ENVS/Results/Descriptive stats/WB_Regions","_",pop_path,".tex",sep="")
print(xtable(df_stats_n2, type = "latex", align="lccccccccc",caption = 'Percentages of total population experiencing up to 5 HRV across wrold regions.', digits=c(0,0,0,0,0,0,0,0,0,0)), caption.placement = 'top', include.rownames=FALSE,file = path_table)
path <- paste("/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/descriptive_stats2","_",pop_path,".R")
save(df_stats_n2, file=path)



# ----------------------------------------------------------------------------
# ----------------------- Sensitivity check with 6 HRV -----------------------
# ----------------------------------------------------------------------------
df <- df_m
df <- na.omit(df)
# Sens check with 6 HRV -- including toxic environments
df$HRV1 <- pmax(df$pop_air, df$pop_biodiv, df$pop_safecl, df$pop_water, df$pop_food_l, df$pop_nontox)
df$HRV2 <- apply(df[, c("pop_air", "pop_biodiv", "pop_safecl", "pop_water", "pop_food_l", "pop_nontox")], 1, function(row) {
  sorted_pop <- sort(row, decreasing = TRUE)
  if (sorted_pop[1] > 0) sorted_pop[2] else 0
})
df$HRV3 <- apply(df[, c("pop_air", "pop_biodiv", "pop_safecl", "pop_water", "pop_food_l", "pop_nontox")], 1, function(row) {
  sorted_pop <- sort(row, decreasing = TRUE)
  if (sorted_pop[1] > 0) sorted_pop[3] else 0
})
df$HRV4 <- apply(df[, c("pop_air", "pop_biodiv", "pop_safecl", "pop_water", "pop_food_l", "pop_nontox")], 1, function(row) {
  sorted_pop <- sort(row, decreasing = TRUE)
  if (sorted_pop[1] > 0) sorted_pop[4] else 0
})
df$HRV5 <- apply(df[, c("pop_air", "pop_biodiv", "pop_safecl", "pop_water", "pop_food_l", "pop_nontox")], 1, function(row) {
  sorted_pop <- sort(row, decreasing = TRUE)
  if (sorted_pop[1] > 0) sorted_pop[5] else 0
})
df$HRV6 <- apply(df[, c("pop_air", "pop_biodiv", "pop_safecl", "pop_water", "pop_food_l", "pop_nontox")], 1, function(row) {
  sorted_pop <- sort(row, decreasing = TRUE)
  if (sorted_pop[1] > 0) sorted_pop[6] else 0
})



# ------ Descriptive stats for INCOME REGIONS ------
df_m <- df
i <- 'Total Pop'
lic_v <- sum(df_m[(df_m$`income group`=='Low income'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)
lmic_v <- sum(df_m[(df_m$`income group`=='Lower middle income'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)
umic_v <- sum(df_m[(df_m$`income group`=='Upper middle income'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)
hic_v <- sum(df_m[(df_m$`income group`=='High income'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)

df_stats_n <- data.frame(HRV=i,
                         LIC=big_number(lic_v, digits=2),
                         LMIC=big_number(lmic_v),
                         UMIC=big_number(umic_v),
                         HIC=big_number(hic_v))

s_all_pop <- 0
hrv <- c("HRV1" ,"HRV2", "HRV3", "HRV4", "HRV5", "HRV6")
# Loop it until HRV=4 (max)
for(i in 1:length(hrv)) {                                     
  lic_v <- sum(df_m[(df_m$`income group`=='Low income'), hrv[i]], na.rm=TRUE)
  lmic_v <- sum(df_m[(df_m$`income group`=='Lower middle income'), hrv[i]], na.rm=TRUE)
  umic_v <- sum(df_m[(df_m$`income group`=='Upper middle income'), hrv[i]], na.rm=TRUE)
  hic_v <- sum(df_m[(df_m$`income group`=='High income'), hrv[i]], na.rm=TRUE)
  
  s_all <- lic_v + lmic_v + umic_v + hic_v
  
  lic_v <- round(lic_v/s_all*100)
  lmic_v <- round(lmic_v/s_all*100)
  umic_v <- round(umic_v/s_all*100)
  hic_v <- round(hic_v/s_all*100)
  
  all_v <- lic_v + lmic_v + umic_v + hic_v
  
  hic_v <- hic_v - (all_v-100)
  df_stats <- data.frame(HRV=i,
                         LIC=paste(lic_v, "%", sep=""),
                         LMIC=paste(lmic_v, "%", sep=""),
                         UMIC=paste(umic_v, "%", sep=""),
                         HIC=paste(hic_v, "%", sep=""))
  
  df_stats_n <- rbind(df_stats_n, df_stats)
  s_all_pop <- append(s_all_pop, s_all)
}

x <- c("1", "2", "3", "4", "5", "6", "Total Pop")

df_stats_n <- df_stats_n %>% slice(match(x, HRV))
df_stats_n <- df_stats_n[df_stats_n$HRV!="Total Pop",]
df_stats_n$Tot_pop <- c(big_number(s_all_pop[2:7]))

print(xtable(df_stats_n, type = "latex", align="lcccccc", caption = 'Percentages of total population experiencing up to five HRV across income groups -- non-toxic environments included.',digits=c(0,0,0,0,0,0,0)), caption.placement = 'top', include.rownames=FALSE,file = "/Users/naiacasina/Documents/ENVS/Results/Descriptive stats/WB_IncomeRegions_6_HRV.tex")
save(df_stats_n, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/descriptive_stats_6_HRV.R")


# ------ Descriptive stats for REGIONS ------
i <- 'Total Pop'
sa_p <- sum(df_m[(df_m$`region`=='South Asia'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)
eca_p <- sum(df_m[(df_m$`region`=='Europe & Central Asia'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)
mena_p <- sum(df_m[(df_m$`region`=='Middle East & North Africa'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)
eap_p <- sum(df_m[(df_m$`region`=='East Asia & Pacific'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)
ssa_p <- sum(df_m[(df_m$`region`=='Sub-Saharan Africa'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)
lac_p <- sum(df_m[(df_m$`region`=='Latin America & Caribbean'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)
na_p <- sum(df_m[(df_m$`region`=='North America'), "ppp_2020_1km_Aggregated"], na.rm=TRUE)

df_stats_n2 <- data.frame(HRV=i,
                          SA=big_number(sa_p),
                          ECA=big_number(eca_p),
                          MENA=big_number(mena_p),
                          EAP=big_number(eap_p),
                          SSA=big_number(ssa_p),
                          LAC=big_number(lac_p),
                          NA_=big_number(na_p))
s_all_pop <- 0
# Loop it until HRV=4 (max)
for(i in 1:length(hrv)) {                                     
  sa_v <- sum(df_m[(df_m$`region`=='South Asia'), hrv[i]], na.rm=TRUE)
  eca_v <- sum(df_m[(df_m$`region`=='Europe & Central Asia'), hrv[i]], na.rm=TRUE)
  mena_v <- sum(df_m[(df_m$`region`=='Middle East & North Africa'), hrv[i]], na.rm=TRUE)
  eap_v <- sum(df_m[(df_m$`region`=='East Asia & Pacific'), hrv[i]], na.rm=TRUE)
  ssa_v <- sum(df_m[(df_m$`region`=='Sub-Saharan Africa'), hrv[i]], na.rm=TRUE)
  lac_v <- sum(df_m[(df_m$`region`=='Latin America & Caribbean'), hrv[i]], na.rm=TRUE)
  na_v <- sum(df_m[(df_m$`region`=='North America'), hrv[i]], na.rm=TRUE)
  
  s_all <- sa_v + eca_v + mena_v + eap_v + ssa_v + lac_v + na_v
  
  sa_v <- round(sa_v/s_all*100)
  eca_v <- round(eca_v/s_all*100)
  mena_v <- round(mena_v/s_all*100)
  eap_v <- round(eap_v/s_all*100)
  ssa_v <- round(ssa_v/s_all*100)
  lac_v <- round(lac_v/s_all*100)
  na_v <- round(na_v/s_all*100)
  
  all_v <- sa_v + eca_v + mena_v + eap_v + ssa_v + lac_v + na_v
  
  sa_v <- sa_v + (100-all_v)
  
  df_stats <- data.frame(HRV=round(i),
                         SA=paste(sa_v, "%", sep=""),
                         ECA=paste(eca_v, "%", sep=""),
                         MENA=paste(mena_v, "%", sep=""),
                         EAP=paste(eap_v, "%", sep=""),
                         SSA=paste(ssa_v, "%", sep=""),
                         LAC=paste(lac_v, "%", sep=""),
                         NA_=paste(na_v, "%", sep=""))
  
  df_stats_n2 <- rbind(df_stats_n2, df_stats)
  s_all_pop  <- append(s_all_pop, s_all)
}

df_stats_n2 <- df_stats_n2 %>% slice(match(x, HRV))
df_stats_n2 <- df_stats_n2[df_stats_n2$HRV!="Total Pop",]
df_stats_n2$Tot_pop <- c(big_number(s_all_pop[2:7]))


print(xtable(df_stats_n2, type = "latex", align="lccccccccc",caption = 'Percentages of total population experiencing up to six HRV across world regions -- non-toxic environments included.', digits=c(0,0,0,0,0,0,0,0,0,0)), caption.placement = 'top', include.rownames=FALSE,file = "/Users/naiacasina/Documents/ENVS/Results/Descriptive stats/WB_Regions_6_HRV.tex")
save(df_stats_n2, file="/Users/naiacasina/Documents/ENVS/Codes and Data/Saved R Data/descriptive_stats_regions_6_HRV.R")

















# ----------- Inequality stats by country------------



# Drop islands and Antartctica
df <- df[!(df$country=="Cook Islands" | df$country=="Federated States of Micronesia"
           | df$country=="Niue" | df$country=="Northern Mariana Islands"
           | df$country=="Tonga" | df$country=="Siachen Glacier"
           | df$country=="American Samoa" | df$country=="Kiribati"
           | df$country=="Norfolk Island" | df$country=="Samoa"
           | df$country=="Wallis and Futuna" | df$country=="French Polynesia"
           | df$country=="Bahrain" | df$country=="East Timor"
           | df$country=="Falkland Islands" | df$country=="Guernsey"
           | df$country=="Guinea Bissau" | df$country=="Jersey"
           | df$country=="Saint Helena" | df$country=="Seychelles"
           | df$country=="Vanuatu" | df$country=="Solomon Islands"
           | df$country=="New Caledonia" | df$country=="Fiji"
           | df$country=="Faroe Islands" | df$country=="The Bahamas"
           | df$country=="Trinidad and Tobago" | df$country=="Marshall Islands"
           | df$country=="Antigua and Barbuda" | df$country=="Aruba"
           | df$country=="British Virgin Islands" | df$country=="Grenada"
           | df$country=="Cayman Islands" | df$country=="Curacao"
           | df$country=="Guam" | df$country=="Malta"
           | df$country=="Turks and Caicos Islands"
           | df$country%in%c("Aland","Saint Pierre and Miquelon" ,"United States Virgin Islands",
                             "Anguilla","Sint Maarten","Saint Lucia","Saint Kitts and Nevis"
                             ,"French Southern and Antarctic Lands","British Indian Ocean Territory"
                             ,"Saint Vincent and the Grenadines","Heard Island and McDonald Islands",
                             "South Georgia and South Sandwich Islands", "Sao Tome and Principe",
                             "Comoros", "Mauritius", "Antarctica") ),]
# Delete NAs
df <- na.omit(df)

countries <- unique(df$country)

# Normalize
normalized<-function(y) {
  x<-y[!is.na(y)]
  x<-(x - min(x)) / (max(x) - min(x))
  y[!is.na(y)]<-x
  return(y)
}

# Normalize population
df$pop_norm <- normalized(df[, "ppp_2020_1km_Aggregated"])

# Create ineq df
df_ineq <- data.frame(country=countries)
df_ineq <- na.omit(df_ineq)

# Add weighted quantile data
fun_ineq1 <- function(x,qu,df) {
  quant <- weighted.quantile(x=df[df$country==x,"HRV"], 
                             w=df[df$country==x,c("pop_norm")], 
                             prob = qu)
  return(quant)
}


df_ineq_tot$median_HRV <- apply(df_ineq,1,fun_ineq1,0.5,df)
df_ineq_tot$q_10_HRV <- apply(df_ineq,1,fun_ineq1,0.1,df)
df_ineq_tot$q_90_HRV <- apply(df_ineq,1,fun_ineq1,0.9,df)
df_ineq_tot$dif_HRV <- df_ineq_tot$q_90_HRV-df_ineq_tot$q_10_HRV

ggplot(df_ineq_tot, aes(x=median_HRV,y=dif_HRV, color=country)) +
  geom_point()+
  #geom_smooth(se=F)+
  #geom_smooth(method="lm", se=F, formula = y~poly(x,2)) +
  #geom_smooth(aes(logGDP, pm2.5), method="lm", se=F, formula = y~poly(x,2), show.legend = FALSE) +
  #facet_wrap(~quantile) +
  ggtitle("Relationship between PM2.5 estimates and national GDP through 1998-2018 in Latin American Countries")

# Basic scatter plot
ggplot(df_ineq_tot, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point() + 
  theme(legend.position = "none")+
  xlim(0, 5) +
  ylim(0, 5)



# ----------- Inequality stats by INCOME GROUPS------------
df_ineq_m <- merge(wb_ir, df_ineq_tot, by = 'country')
df_li <- df_ineq_m[df_ineq_m$`income group`=="Low income", ]
df_umi <- df_ineq_m[df_ineq_m$`income group`=="Upper middle income", ]
df_lmi <- df_ineq_m[df_ineq_m$`income group`=="Lower middle income", ]
df_hi <- df_ineq_m[df_ineq_m$`income group`=="High income", ]

# Low income
ggplot(df_li, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point() + 
  theme(legend.position = "none")+
  xlim(0, 5) +
  ylim(0, 5)

# Lower middle income
ggplot(df_lmi, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point() + 
  theme(legend.position = "none")+
  xlim(0, 5) +
  ylim(0, 5)

# Upper middle income
ggplot(df_umi, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point() + 
  theme(legend.position = "none")+
  xlim(0, 5) +
  ylim(0, 5)

# High income
ggplot(df_hi, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point() + 
  theme(legend.position = "none")+
  xlim(0, 5) +
  ylim(0, 5)


df_ineq_m <- merge(wb_ir, df_ineq_tot, by = 'country')

# ----------- Inequality stats by REGIONS ------------
df_sa <- df_ineq_m[df_ineq_m$region=="South Asia", ]
df_eca <- df_ineq_m[df_ineq_m$region=="Europe & Central Asia", ]
df_mena <- df_ineq_m[df_ineq_m$region=="Middle East & North Africa", ]
df_ssa <- df_ineq_m[df_ineq_m$region=="Sub-Saharan Africa", ]
df_lac <- df_ineq_m[df_ineq_m$region=="Latin America & Caribbean", ]
df_eap <- df_ineq_m[df_ineq_m$region=="East Asia & Pacific", ]
df_na <- df_ineq_m[df_ineq_m$region=="North America", ]

# South Asia
fig_comp <- ggplot(df_sa, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point(size=1.5, position=position_jitter(h=0.15,w=0.15))+ 
  xlim(0, 5) + xlab("Median HRV") +
  ylim(0, 5) + ylab("HRV_q90 - HRV_q10") +
  ggtitle("Inequality in South Asia")

ggsave(plot = fig_comp,
       filename = "sa.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Inequalities/",
       width = 10, height = 7, units = "in",
       dpi = 320)

# ECA
ggplot(df_eca, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point(size=1.5, position=position_jitter(h=0.15,w=0.15))+ 
  xlim(0, 5) + xlab("Median HRV") +
  ylim(0, 5) + ylab("HRV_q90 - HRV_q10") +
  ggtitle("Inequality in Europe and Central Asia")

# MENA
fig_comp <- ggplot(df_mena, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point(size=1.5, position=position_jitter(h=0.15,w=0.15))+ 
  xlim(0, 5) + xlab("Median HRV") +
  ylim(0, 5) + ylab("HRV_q90 - HRV_q10") +
  ggtitle("Inequality in Europe and Middle East and North Africa")

ggsave(plot = fig_comp,
       filename = "mena.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Inequalities/",
       width = 10, height = 7, units = "in",
       dpi = 320)

# SSA
ggplot(df_ssa, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point(size=1.5, position=position_jitter(h=0.15,w=0.15))+ 
  xlim(0, 5) + xlab("Median HRV") +
  ylim(0, 5) + ylab("HRV_q90 - HRV_q10") +
  ggtitle("Inequality in Sub-Saharan Africa")

# LAC
ggplot(df_lac, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point(size=1.5, position=position_jitter(h=0.15,w=0.15))+ 
  xlim(0, 5) + xlab("Median HRV") +
  ylim(0, 5) + ylab("HRV_q90 - HRV_q10") +
  ggtitle("Inequality in Latin America and the Caribbean")

# EAP
fig_comp <- ggplot(df_eap, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point(size=1.5, position=position_jitter(h=0.15,w=0.15))+ 
  xlim(0, 5) + xlab("Median HRV") +
  ylim(0, 5) + ylab("HRV_q90 - HRV_q10") +
  ggtitle("Inequality in East Asia and the Pacific")

ggsave(plot = fig_comp,
       filename = "eap.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Inequalities/",
       width = 10, height = 7, units = "in",
       dpi = 320)

# NA
ggplot(df_na, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point(size=1.5, position=position_jitter(h=0.15,w=0.15))+ 
  xlim(0, 5) + xlab("Median HRV") +
  ylim(0, 5) + ylab("HRV_q90 - HRV_q10") +
  ggtitle("Inequality in North America")


# Comparison
comp <- c("United States of America", "Afghanistan", "China", "Argentina", "Finland", "Iceland")

df_comp <- df_ineq_m[df_ineq_m$country%in%comp, ]

fig_comp <-ggplot(df_comp, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point() + 
  geom_jitter()+
  xlim(0, 5) +
  ylim(0, 5)

ggsave(plot = fig_comp,
       filename = "comparison.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Inequalities/",
       width = 10, height = 7, units = "in",
       dpi = 320)


# EAC plus HI
df_eca_hi <- df_ineq_m[df_ineq_m$region=="Europe & Central Asia"&df_ineq_m$`income group`=="High income", ]
# EAC plus HI
fig_comp <- ggplot(df_eca_hi, aes(x=median_HRV,y=dif_HRV, color=country)) + 
  geom_point(size=1.5, position=position_jitter(h=0.15,w=0.15))+ 
  xlim(0, 5) + xlab("Median HRV") +
  ylim(0, 5) + ylab("HRV_q90 - HRV_q10") +
  ggtitle("Inequality in Europe and Central Asian High Income countries")

ggsave(plot = fig_comp,
       filename = "eca_hi.png",
       device = "png", path = "/Users/naiacasina/Documents/ENVS/Results/Inequalities/",
       width = 10, height = 7, units = "in",
       dpi = 320)
