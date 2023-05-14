# ============================ #
# TDK - Layoff Event study
# Industry Robustness Analysis
# ============================ #

# ====================== #
# TDK 2023 - Event Study #
# ====================== #

#clear memory
rm(list = ls())

# SETUP =====
library(readxl)
library(tidyverse)
library(ggplot2)
library(stargazer)
library(corrplot)
library(dplyr)
library(lmtest)
library(scales)

# set working directory
setwd("D:\\OneDrive\\Tanulás\\BCE\\TDK\\")
output <- "D:\\OneDrive\\Tanulás\\BCE\\TDK\\output\\"
dir_df <- "D:\\OneDrive\\Tanulás\\BCE\\TDK\\Databases\\"
# load theme
source("theme_bg.R")

# DATA IMPORT ====================
# Stock closing price data
data <- read_excel(paste0(dir_df, "CorpLayoffStockPrices_clean.xlsx"), sheet = "Binded")
# company names
Asset_long <- c("Meta", "Amazon", "Netflix", "Microsoft", "Disney", "Redfin", "Carvana", "Snap", "Lyft", "Peloton")
IT_Asset_long <- c("Meta", "Amazon", "Netflix", "Microsoft", "Snap")
NIT_Asset_long <- c("Disney", "Redfin", "Carvana", "Lyft", "Peloton")
# announcement data importing and filtering
announce_data <- read_excel(paste0(dir_df, "CorpLayoffAnnouncements.xlsx"), sheet = "Sheet1")
announce_data$LayoffAnnouncementDate <- as.Date(announce_data$LayoffAnnouncementDate)
announce_data <- announce_data %>% 
  spread(key = "Company", value = "LayoffAnnouncementDate") %>% 
  dplyr::select(-GM, -WB, -Twitter, -HP)

# date conversion
date <- as.Date(data$Date)

# Market index import
SP500 <- read_excel(paste0(dir_df, "MarketIndices5Y.xlsx"), sheet = "SP500", range = "A1:B1275")
SP500 <- SP500 %>% dplyr::arrange(Date) %>% dplyr::select(-Date)
SP5IT <- read_excel(paste0(dir_df, "MarketIndices5Y.xlsx"), sheet = "S5INFT Index", range = "A1:B1275")
SP5IT <- SP5IT %>% dplyr::arrange(Date) %>% dplyr::select(-Date)

# DATA PREPARATION =====

# filtering data according to IT and Not IT stocks
price <- data.frame(data[,-1]) %>% dplyr::select(-GM, -HP, -Twitter, -WB)
IT_price <- data %>% dplyr::select(all_of(IT_Asset_long))
NIT_price <- data %>% dplyr::select(all_of(NIT_Asset_long))

# log return calculation
returns <- price %>% as.matrix() %>% log() %>% diff() %>% data.frame()
IT_returns <- IT_price %>% as.matrix() %>% log() %>% diff() %>% data.frame()
NIT_returns <- NIT_price %>% as.matrix() %>% log() %>% diff() %>% data.frame()

SP500_returns <- SP500 %>% as.matrix() %>% log() %>% diff() %>% data.frame()
SP5IT_returns <- SP5IT %>% as.matrix() %>% log() %>% diff() %>% data.frame()

# t_Window sizes
regression_window_size <- 60
event_window <- 5
indice <- c()

# CALCULATION LOOP / market model =====
# each stock separately --> due to the difference in announcement dates

# ALL FIRMS ---
alpha <- c()
beta <- c()
var <- c()

abnormal_returns <- data.frame(matrix(0, nrow = (2 * event_window + 1), ncol = length(Asset_long))) # dummy data frame
colnames(abnormal_returns) <- Asset_long

event_data <- data.frame(matrix(0, nrow = (2 * event_window + 1), ncol = length(Asset_long)))
colnames(event_data) <- Asset_long

for (n in 1:length(Asset_long)){
  
  # find announcement date
  indice <- which(date[2:length(date)] == announce_data[, Asset_long[n]])
  
  # filter the original data set
  regression_data <- returns[(indice - event_window - regression_window_size) : (indice - event_window - 1), n]
  market1 <- SP500_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1),]
  event_data[,n] <- returns[(indice - event_window - 1):(indice + event_window - 1), n]
  market2 <- SP500_returns[(indice - event_window - 1) : (indice + event_window - 1),]
  
  # MARKET MODELL ESTIMATION ========== #
  df_model <- data.frame(regression_data, market1)
  colnames(df_model) <- c("regression_data","market1")
  
  #create formula for model estimation
  model_formula <- as.formula("regression_data ~ market1")
  
  model <- lm(data = df_model , model_formula)
  alpha[n] <- model$coefficients[1]
  beta[n] <- model$coefficients[2]
  var[n] <- sum(model$residuals ^2)/(regression_window_size - 2)
  
  # AR
  abnormal_returns[,n] <- event_data[,n] - (alpha[n] + beta[n]*market2)
  
}

# IT abnormal returns ---
alpha <- c()
beta <- c()
IT_var <- c()

IT_abnormal_returns <- data.frame(matrix(0, nrow = (2 * event_window + 1), ncol = length(IT_Asset_long))) #dummy data frame for abnormal return
colnames(IT_abnormal_returns) <- IT_Asset_long

event_data <- data.frame(matrix(0, nrow = 2 * event_window + 1, ncol = length(IT_Asset_long)))
colnames(event_data) <- IT_Asset_long

for (n in 1:length(IT_Asset_long)){
  
  # find announcement date
  indice <- which(date[2:length(date)] == announce_data[, IT_Asset_long[n]])
  
  # filter the original data set
  regression_data <- IT_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1), n]
  market1 <- SP5IT_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1),]
  event_data[,n] <- IT_returns[(indice - event_window - 1):(indice + event_window - 1), n]
  market2 <- SP5IT_returns[(indice - event_window - 1) : (indice + event_window - 1),]
  
  # MARKET MODELL ESTIMATION ========== #
  df_model <- data.frame(regression_data, market1)
  colnames(df_model) <- c("regression_data","market1")
  
  #create formula for model estimation
  model_formula <- as.formula("regression_data ~ market1")
  
  model <- lm(data = df_model , model_formula)
  alpha[n] <- model$coefficients[1]
  beta[n] <- model$coefficients[2]
  IT_var[n] <- sum(model$residuals ^2)/(regression_window_size - 2)
  
  # AR
  IT_abnormal_returns[,n] <- event_data[,n] - (alpha[n] + beta[n]*market2)
  
}

# NIT abnormal returns ---
alpha <- c()
beta <- c()
NIT_var <- c()

NIT_abnormal_returns <- data.frame(matrix(0, nrow = (2 * event_window + 1), ncol = length(NIT_Asset_long)))
colnames(NIT_abnormal_returns) <- NIT_Asset_long

event_data <- data.frame(matrix(0, nrow = 2 * event_window + 1, ncol = length(NIT_Asset_long)))
colnames(event_data) <- NIT_Asset_long

for (n in 1:length(NIT_Asset_long)){
  
  # find announcement date
  indice <- which(date[2:length(date)] == announce_data[, NIT_Asset_long[n]])
  
  # filter the original data set
  regression_data <- NIT_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1), n]
  market1 <- SP500_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1),]
  event_data[,n] <- NIT_returns[(indice - event_window - 1):(indice + event_window - 1), n]
  market2 <- SP500_returns[(indice - event_window - 1) : (indice + event_window - 1),]
  
  # MARKET MODELL ESTIMATION ========== #
  df_model <- data.frame(regression_data, market1)
  colnames(df_model) <- c("regression_data","market1")
  
  #create formula for model estimation
  model_formula <- as.formula("regression_data ~ market1")
  
  model <- lm(data = df_model , model_formula)
  alpha[n] <- model$coefficients[1]
  beta[n] <- model$coefficients[2]
  NIT_var[n] <- sum(model$residuals ^2)/(regression_window_size - 2)
  
  # AR
  NIT_abnormal_returns[,n] <- event_data[,n] - (alpha[n] + beta[n]*market2)
  
}


# CAR, var calculations =====
CAR <- cumsum(abnormal_returns)
IT_CAR <- cumsum(IT_abnormal_returns)
NIT_CAR <- cumsum(NIT_abnormal_returns)

#average of AR and CAR
av_AR <- rowSums(abnormal_returns)/length(Asset_long)
IT_av_AR <- rowSums(IT_abnormal_returns)/length(IT_Asset_long)
NIT_av_AR <- rowSums(NIT_abnormal_returns)/length(NIT_Asset_long)

av_CAR <- rowSums(CAR)/length(Asset_long)
IT_av_CAR <- rowSums(IT_CAR)/length(IT_Asset_long)
NIT_av_CAR <- rowSums(NIT_CAR)/length(NIT_Asset_long)

#variance of the abnormal return and CAR
var_AR <- sum(var) / (length(Asset_long))^2
var_CAR <- var_AR * (event_window * 2 + 1)

IT_var_AR <- sum(IT_var) / (length(IT_Asset_long))^2
IT_var_CAR <- IT_var_AR * (event_window * 2 + 1)

NIT_var_AR <- sum(NIT_var) / (length(NIT_Asset_long))^2
NIT_var_CAR <- NIT_var_AR * (event_window * 2 + 1)


# PLOT PREPARATION =====
xcoord_eventw <- seq(-event_window, event_window, by = 1)
# All firms CAR data frame for plot 1.
df_av_CAR <- data.frame(xcoord_eventw,
                        av_CAR,
                        upperconf = av_CAR + 1.96 * var_CAR,
                        lowerconf = av_CAR - 1.96 * var_CAR)
# IT CAR data frame for plot
df_IT_av_CAR <- data.frame(xcoord_eventw,
                           IT_av_CAR,
                           IT_upperconf = IT_av_CAR + 1.96 * IT_var_CAR,
                           IT_lowerconf = IT_av_CAR - 1.96 * IT_var_CAR)
# Not IT CAR data frame for plot
df_NIT_av_CAR <- data.frame(xcoord_eventw,
                            NIT_av_CAR,
                            NIT_upperconf = NIT_av_CAR + 1.96 * NIT_var_CAR,
                            NIT_lowerconf = NIT_av_CAR - 1.96 * NIT_var_CAR)

# IT & NIT combined data frame for simultaneous plotting
df_av_CAR_combo <- data.frame(df_IT_av_CAR, df_NIT_av_CAR)

# PLOTS ======================

### PLOT 1. - all firms
cols <- c("All observed firms" = "black")

ggplot(data = df_av_CAR, aes(x = xcoord_eventw)) +
  geom_ribbon(aes(ymin = lowerconf, ymax = upperconf), color = "grey80", alpha = 0.2) +
  geom_point(aes(y = av_CAR, color = "All observed firms"), shape = 15) +
  geom_line(aes(y = av_CAR, group = 1, color = "All observed firms")) +
  geom_segment(x = 0, y = min(df_av_CAR$lowerconf)*1.5, xend = 0, yend = max(df_av_CAR$upperconf)*1.5,
               col = "grey20", size = 1, linetype = "dashed") +
  geom_segment(x = xcoord_eventw[1] - 1, y = 0, xend = xcoord_eventw[length(xcoord_eventw)] + 1, yend = 0, size = 1) +
  labs(x = "Event Window",
       y = "Average CAR",
       title = "Average Cumulative Abnormal Returns in the event period",
       subtitle = "All observed stocks") +
  scale_x_continuous(limits = c(-event_window, event_window), 
                     breaks = xcoord_eventw) +
  scale_y_continuous(limits = c(min(df_av_CAR$lowerconf)*1.2, 
                                max(df_av_CAR$upperconf)*1.2), 
                     breaks = c(seq(min(df_av_CAR$lowerconf)*1.2, max(df_av_CAR$upperconf)*1.2, length.out = 6),
                                0),
                     labels = percent(c(round(seq(min(df_av_CAR$lowerconf)*1.2, max(df_av_CAR$upperconf)*1.2, length.out = 6), digits = 4),
                                        0))) +
  scale_color_manual(name = "", values = cols) +
  theme_bg() + theme(legend.text = element_text(size = 10), legend.position = "bottom",
                     plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 11))


### PLOT 2. IT and non-IT firms combined plot
cols <- c("IT stocks" = theme_colors[1], "non-IT stocks" = theme_colors[4]) #this is for the legend
shapes <- c("IT stocks" = 15, "non-IT stocks" = 17) #this is for the legend

ggplot(data = df_av_CAR_combo, aes(x = xcoord_eventw)) +
  geom_ribbon(aes(ymin = IT_lowerconf, ymax = IT_upperconf), fill = theme_colors[1], alpha = 0.2) +
  geom_ribbon(aes(ymin = NIT_lowerconf, ymax = NIT_upperconf), fill = theme_colors[4], alpha = 0.2) +
  geom_line(aes(y = IT_av_CAR, group = 1, color = "IT stocks"), size = 1.3) +
  geom_point(aes(y = IT_av_CAR, color = "IT stocks", shape = "IT stocks"), size = 2) + #
  geom_line(aes(y = NIT_av_CAR, group = 1, color = "non-IT stocks"), size = 1.3) +
  geom_point(aes(y = NIT_av_CAR, color = "non-IT stocks", shape = "non-IT stocks"), size = 2.3) + #
  geom_line(aes(y = IT_upperconf), color = theme_colors[1], size = 0.3) +
  geom_line(aes(y = IT_lowerconf), color = theme_colors[1], size = 0.3) +
  geom_line(aes(y = NIT_upperconf), color = theme_colors[4], size = 0.3) +
  geom_line(aes(y = NIT_lowerconf), color = theme_colors[4], size = 0.3) +
  geom_segment(x = 0, y = min(df_NIT_av_CAR$lowerconf)*1.5, xend = 0, yend = max(df_IT_av_CAR$upperconf)*1.5, 
               col = "grey20", size = 1.1, linetype = "dashed") +
  geom_segment(x = xcoord_eventw[1] - 1, y = 0, xend = xcoord_eventw[length(xcoord_eventw)] + 1, yend = 0, size = 1) +
  labs(x = "Event Window",
       y = "Average CAR",
       title = "Average Cumulative Abnormal Returns in the event period",
       subtitle = "IT and non-IT stocks") +
  scale_x_continuous(limits = c(-event_window, event_window), breaks = xcoord_eventw) +
  scale_y_continuous(limits = c(min(df_NIT_av_CAR$NIT_lowerconf)*1.2, max(df_IT_av_CAR$IT_upperconf)*1.2, 0), 
                     breaks = c(seq(min(df_NIT_av_CAR$NIT_lowerconf)*1.2, max(df_IT_av_CAR$IT_upperconf)*1.2, length.out = 5), 0),
                     labels = percent(c(round(seq(min(df_NIT_av_CAR$NIT_lowerconf)*1.2, max(df_IT_av_CAR$IT_upperconf)*1.2, length.out = 5),
                                              digits = 4), 0))) +
  scale_color_manual(name = "", values = cols) +
  scale_shape_manual(name = "", values = shapes) +
  theme_bg() + theme(legend.text = element_text(size = 10), legend.position = "bottom",
                     plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 11))

# INDUSTRY ROBUSTNESS ==========================

# Data prep ---
comp_assets <- c("Alibaba", "Alphabet", "AMCX", "Autonation Inc", "Brunswick Corp", 
                 "Buzzfeed", "CBRE", "Comcast", "Compass Inc", "CRMT", "Doordash", 
                 "Ebay", "Etsy", "EXP World Holding", "Joyy", "Kingsoft", "Nextdoor Holding", "Opendoor", 
                 "Oracle", "Paltalk", "Paramount Global", "Phoenix New", "Pinterest", 
                 "Sirius XM Holding", "Snowflake", "Spotify", "Topgolf Callaway", 
                 "Uber", "Vista Outdoor", "VROOM Inc", "Warner Bros", "Weibo", "Yelp")

IT_comp_assets <- c("Alibaba", "Alphabet", "Buzzfeed", "Ebay", "Etsy", "Joyy", "Kingsoft", 
                    "Nextdoor Holding", "Oracle", "Paltalk", "Phoenix New", "Pinterest", 
                    "Sirius XM Holding", "Snowflake", "Spotify", "Weibo", "Yelp")
NIT_comp_assets <- c("AMCX", "Autonation Inc", "Brunswick Corp", "CBRE", "Comcast", 
                     "Compass Inc", "CRMT", "Doordash", "EXP World Holding", "Opendoor", 
                     "Paramount Global", "Topgolf Callaway", "Uber", "Vista Outdoor", 
                     "VROOM Inc", "Warner Bros")


comp_all <- read.csv(paste0(dir_df, "IndustryCompetitors_clean.csv"))

### Calculating returns
comp_all <- comp_all %>% dplyr::arrange(Date) %>% dplyr::select(-Date)
comp_returns <- comp_all %>% as.matrix() %>% log() %>% diff() %>% data.frame()
colnames(comp_returns) <- comp_assets

IT_comp_returns <- comp_returns %>% dplyr::select(all_of(IT_comp_assets))
NIT_comp_returns <- comp_returns %>% dplyr::select(all_of(NIT_comp_assets))

### Removing unused companies
comp_returns <- comp_returns %>% dplyr::select(-Alphabet, -Buzzfeed, -`Warner Bros`)
IT_comp_returns <- IT_comp_returns %>% dplyr::select(-Alphabet, -Buzzfeed)
NIT_comp_returns <- NIT_comp_returns %>% dplyr::select(-`Warner Bros`)
comp_assets <- comp_assets[! comp_assets %in% c("Alphabet", "Buzzfeed", "Warner Bros")]
IT_comp_assets <- IT_comp_assets[! IT_comp_assets %in% c("Alphabet", "Buzzfeed")]
NIT_comp_assets <- NIT_comp_assets[! NIT_comp_assets %in% c("Warner Bros")]

### assigning announcement dates to industry competitors
comp_announce_data <- matrix(0, nrow = length(comp_assets), ncol = 2)
colnames(comp_announce_data) <- c("Company", "AnnouncementDate")
comp_announce_data <- comp_announce_data %>% data.frame()
comp_announce_data[,1] <- comp_assets
comp_announce_data[,2] <- as.Date(comp_announce_data[,2])
comp_announce_data <- comp_announce_data %>% 
  spread(key = "Company", value = "AnnouncementDate")


comp_announce_data[,c("Autonation Inc", "CRMT", "VROOM Inc")] <- announce_data[, "Carvana"]
comp_announce_data[,c("Paramount Global", "AMCX", "Comcast")] <- announce_data[, "Disney"]
comp_announce_data[,c("Uber", "Opendoor", "Doordash")] <- announce_data[, "Lyft"]
comp_announce_data[,c("Brunswick Corp", "Topgolf Callaway", "Vista Outdoor")] <- announce_data[, "Peloton"]
comp_announce_data[,c("CBRE", "Compass Inc", "EXP World Holding")] <- announce_data[, "Redfin"]
comp_announce_data[,c("Alibaba", "Ebay", "Etsy")] <- announce_data[, "Amazon"]
comp_announce_data[,c("Weibo", "Pinterest", "Joyy")] <- announce_data[, "Meta"]
comp_announce_data[,c("Oracle", "Kingsoft", "Snowflake")] <- announce_data[, "Microsoft"]
comp_announce_data[,c("Spotify", "Sirius XM Holding", "Phoenix New")] <- announce_data[, "Netflix"]
comp_announce_data[,c("Yelp", "Paltalk", "Nextdoor Holding")] <- announce_data[, "Snap"]



# Market model ==== #

## All firms ---
alpha <- c()
beta <- c()
comp_var <- c()

comp_abnormal_returns <- data.frame(matrix(0, nrow = (2 * event_window + 1), ncol = length(comp_assets))) # dummy data frame
colnames(comp_abnormal_returns) <- comp_assets

event_data <- data.frame(matrix(0, nrow = (2 * event_window + 1), ncol = length(comp_assets)))
colnames(event_data) <- comp_assets

for (n in 1:length(comp_assets)){
  
  # find announcement date
  indice <- which(date[2:length(date)] == comp_announce_data[, comp_assets[n]])
  
  # filter the original data set
  regression_data <- comp_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1), n]
  market1 <- SP500_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1),]
  event_data[,n] <- comp_returns[(indice - event_window - 1):(indice + event_window - 1), n]
  market2 <- SP500_returns[(indice - event_window - 1) : (indice + event_window - 1),]
  
  # MARKET MODELL ESTIMATION ========== #
  df_model <- data.frame(regression_data, market1)
  colnames(df_model) <- c("regression_data","market1")
  
  #create formula for model estimation
  model_formula <- as.formula("regression_data ~ market1")
  
  model <- lm(data = df_model , model_formula)
  alpha[n] <- model$coefficients[1]
  beta[n] <- model$coefficients[2]
  comp_var[n] <- sum(model$residuals ^2)/(regression_window_size - 2)
  
  # AR
  comp_abnormal_returns[,n] <- event_data[,n] - (alpha[n] + beta[n]*market2)
  
}

# IT abnormal returns ---
alpha <- c()
beta <- c()
IT_comp_var <- c()

IT_comp_abnormal_returns <- data.frame(matrix(0, nrow = (2 * event_window + 1), ncol = length(IT_comp_assets))) #dummy data frame for abnormal return
colnames(IT_comp_abnormal_returns) <- IT_comp_assets

event_data <- data.frame(matrix(0, nrow = 2 * event_window + 1, ncol = length(IT_comp_assets)))
colnames(event_data) <- IT_comp_assets

for (n in 1:length(IT_comp_assets)){
  
  # find announcement date
  indice <- which(date[2:length(date)] == comp_announce_data[, IT_comp_assets[n]])
  
  # filter the original data set
  regression_data <- IT_comp_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1), n]
  market1 <- SP5IT_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1),]
  event_data[,n] <- IT_comp_returns[(indice - event_window - 1):(indice + event_window - 1), n]
  market2 <- SP5IT_returns[(indice - event_window - 1) : (indice + event_window - 1),]
  
  # MARKET MODELL ESTIMATION ========== #
  df_model <- data.frame(regression_data, market1)
  colnames(df_model) <- c("regression_data","market1")
  
  #create formula for model estimation
  model_formula <- as.formula("regression_data ~ market1")
  
  model <- lm(data = df_model , model_formula)
  alpha[n] <- model$coefficients[1]
  beta[n] <- model$coefficients[2]
  IT_comp_var[n] <- sum(model$residuals ^2)/(regression_window_size - 2)
  
  # AR
  IT_comp_abnormal_returns[,n] <- event_data[,n] - (alpha[n] + beta[n]*market2)
  
}

# NIT abnormal returns ---
alpha <- c()
beta <- c()
NIT_comp_var <- c()

NIT_comp_abnormal_returns <- data.frame(matrix(0, nrow = (2 * event_window + 1), ncol = length(NIT_comp_assets)))
colnames(NIT_comp_abnormal_returns) <- NIT_comp_assets

event_data <- data.frame(matrix(0, nrow = 2 * event_window + 1, ncol = length(NIT_comp_assets)))
colnames(event_data) <- NIT_comp_assets

for (n in 1:length(NIT_comp_assets)){
  
  # find announcement date
  indice <- which(date[2:length(date)] == comp_announce_data[, NIT_comp_assets[n]])
  
  # filter the original data set
  regression_data <- NIT_comp_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1), n]
  market1 <- SP500_returns[(indice - event_window - regression_window_size) : (indice - event_window - 1),]
  event_data[,n] <- NIT_comp_returns[(indice - event_window - 1):(indice + event_window - 1), n]
  market2 <- SP500_returns[(indice - event_window - 1) : (indice + event_window - 1),]
  
  # MARKET MODELL ESTIMATION ========== #
  df_model <- data.frame(regression_data, market1)
  colnames(df_model) <- c("regression_data","market1")
  
  #create formula for model estimation
  model_formula <- as.formula("regression_data ~ market1")
  
  model <- lm(data = df_model , model_formula)
  alpha[n] <- model$coefficients[1]
  beta[n] <- model$coefficients[2]
  NIT_comp_var[n] <- sum(model$residuals ^2)/(regression_window_size - 2)
  
  # AR
  NIT_comp_abnormal_returns[,n] <- event_data[,n] - (alpha[n] + beta[n]*market2)
  
}


# CAR, var calculations for industry competirors ===== #
comp_CAR <- cumsum(comp_abnormal_returns)
IT_comp_CAR <- cumsum(IT_comp_abnormal_returns)
NIT_comp_CAR <- cumsum(NIT_comp_abnormal_returns)

#average of AR and CAR
comp_av_AR <- rowSums(comp_abnormal_returns)/length(comp_assets)
IT_comp_av_AR <- rowSums(IT_comp_abnormal_returns)/length(IT_comp_assets)
NIT_comp_av_AR <- rowSums(NIT_comp_abnormal_returns)/length(NIT_comp_assets)

comp_av_CAR <- rowSums(comp_CAR)/length(comp_assets)
IT_comp_av_CAR <- rowSums(IT_comp_CAR)/length(IT_comp_assets)
NIT_comp_av_CAR <- rowSums(NIT_comp_CAR)/length(NIT_comp_assets)

#variance of the abnormal return and CAR
comp_var_AR <- sum(comp_var) / (length(comp_assets))^2
comp_var_CAR <- comp_var_AR * (event_window * 2 + 1)

IT_comp_var_AR <- sum(IT_comp_var) / (length(IT_comp_assets))^2
IT_comp_var_CAR <- IT_comp_var_AR * (event_window * 2 + 1)

NIT_comp_var_AR <- sum(NIT_comp_var) / (length(NIT_comp_assets))^2
NIT_comp_var_CAR <- NIT_comp_var_AR * (event_window * 2 + 1)

# PLOT PREPARATION ===== #
# All firms + Industry competitors' CAR data frame for plot 1.
df_comp_av_CAR <- data.frame(xcoord_eventw,
                             av_CAR,
                             upperconf = av_CAR + 1.96 * var_CAR,
                             lowerconf = av_CAR - 1.96 * var_CAR,
                             comp_av_CAR,
                             comp_upperconf = comp_av_CAR + 1.96 * comp_var_CAR,
                             comp_lowerconf = comp_av_CAR - 1.96 * comp_var_CAR)
# IT CAR data frame for plot
df_IT_comp_av_CAR <- data.frame(xcoord_eventw,
                                IT_av_CAR,
                                IT_upperconf = IT_av_CAR + 1.96 * IT_var_CAR,
                                IT_lowerconf = IT_av_CAR - 1.96 * IT_var_CAR,
                                IT_comp_av_CAR,
                                IT_comp_upperconf = IT_comp_av_CAR + 1.96 * IT_var_CAR,
                                IT_comp_lowerconf = IT_comp_av_CAR - 1.96 * IT_var_CAR)
# Not IT CAR data frame for plot
df_NIT_comp_av_CAR <- data.frame(xcoord_eventw,
                                 NIT_av_CAR,
                                 NIT_upperconf = NIT_av_CAR + 1.96 * NIT_var_CAR,
                                 NIT_lowerconf = NIT_av_CAR - 1.96 * NIT_var_CAR,
                                 NIT_comp_av_CAR,
                                 NIT_comp_upperconf = NIT_comp_av_CAR + 1.96 * NIT_comp_var_CAR,
                                 NIT_comp_lowerconf = NIT_comp_av_CAR - 1.96 * NIT_comp_var_CAR)

# IT & NIT firms + competitors combined data frame for simultaneous plotting
df_comp_av_CAR_combo <- data.frame(df_IT_comp_av_CAR, df_NIT_comp_av_CAR)


# INDUSTRY PLOTS ======================

# 1. PLOT - All firms + industry
### PLOT 1. - all firms
cols <- c("All observed firms" = "black", "Industry competitors" = "grey40")
shapes <- c("All observed firms" = 15, "Industry competitors" = 21)

ggplot(data = df_comp_av_CAR, aes(x = xcoord_eventw)) +
  # observed firms
  geom_ribbon(aes(ymin = lowerconf, ymax = upperconf), color = "grey80", alpha = 0.2) +
  geom_point(aes(y = av_CAR, color = "All observed firms", shape = "All observed firms")) +
  geom_line(aes(y = av_CAR, group = 1, color = "All observed firms")) +
  # industry competitors
  geom_ribbon(aes(ymin = comp_lowerconf, ymax = comp_upperconf), color = "grey90", alpha = 0.2) +
  geom_point(aes(y = comp_av_CAR, color = "Industry competitors", shape = "Industry competitors"), size = 2) +
  geom_line(aes(y = comp_av_CAR, group = 1, color = "Industry competitors")) +
  # Additional plot elements
  geom_segment(x = 0, y = min(df_av_CAR$lowerconf)*1.5, xend = 0, yend = max(df_av_CAR$upperconf)*1.5,
               col = "grey20", size = 1, linetype = "dashed") +
  geom_segment(x = xcoord_eventw[1] - 1, y = 0, xend = xcoord_eventw[length(xcoord_eventw)] + 1, yend = 0, size = 1) +
  labs(x = "Event Window",
       y = "Average CAR",
       title = "Average Cumulative Abnormal Returns in the event period",
       subtitle = "All observed stocks and their industry competitors") +
  scale_x_continuous(limits = c(-event_window, event_window), 
                     breaks = xcoord_eventw) +
  scale_y_continuous(limits = c(min(df_av_CAR$lowerconf)*1.2, 
                                max(df_av_CAR$upperconf)*1.2), 
                     breaks = c(seq(min(df_av_CAR$lowerconf)*1.2, max(df_av_CAR$upperconf)*1.2, length.out = 6),
                                0),
                     labels = percent(c(round(seq(min(df_av_CAR$lowerconf)*1.2, max(df_av_CAR$upperconf)*1.2, length.out = 6), digits = 4),
                                        0))) +
  scale_color_manual(name = "", values = cols) +
  scale_shape_manual(name = "", values = shapes) +
  theme_bg() + theme(legend.text = element_text(size = 10), legend.position = "bottom",
                     plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 11))

save_fig("plot_industry_all_v2", output, "verylarge")


### PLOT 2. - Combined
cols <- c("IT stocks" = theme_colors[1], "non-IT stocks" = theme_colors[4],
          "IT competitors" = theme_colors[2], "non-IT competitors" = theme_colors[3]) #this is for the legend
shapes <- c("IT stocks" = 15, "non-IT stocks" = 17,
            "IT competitors" = 0, "non-IT competitors" = 2) #this is for the legend

ggplot(data = df_comp_av_CAR_combo, aes(x = xcoord_eventw)) +
  #IT firms
  geom_ribbon(aes(ymin = IT_lowerconf, ymax = IT_upperconf), fill = theme_colors[1], alpha = 0.2) +
  geom_line(aes(y = IT_av_CAR, group = 1, color = "IT stocks"), size = 1.3) +
  geom_point(aes(y = IT_av_CAR, color = "IT stocks", shape = "IT stocks"), size = 2) +
  geom_line(aes(y = IT_upperconf), color = theme_colors[1], size = 0.3) +
  geom_line(aes(y = IT_lowerconf), color = theme_colors[1], size = 0.3) +
  #NIT firms
  geom_ribbon(aes(ymin = NIT_lowerconf, ymax = NIT_upperconf), fill = theme_colors[4], alpha = 0.2) +
  geom_line(aes(y = NIT_av_CAR, group = 1, color = "non-IT stocks"), size = 1.3) +
  geom_point(aes(y = NIT_av_CAR, color = "non-IT stocks", shape = "non-IT stocks"), size = 2.3) +
  geom_line(aes(y = NIT_upperconf), color = theme_colors[4], size = 0.3) +
  geom_line(aes(y = NIT_lowerconf), color = theme_colors[4], size = 0.3) +
  #IT competitors
  geom_ribbon(aes(ymin = IT_comp_lowerconf, ymax = IT_comp_upperconf), fill = theme_colors[2], alpha = 0.2) +
  geom_line(aes(y = IT_comp_av_CAR, group = 1, color = "IT competitors"), size = 1) +
  geom_point(aes(y = IT_comp_av_CAR, color = "IT competitors", shape = "IT competitors"), size = 2) +
  geom_line(aes(y = IT_comp_lowerconf), color = theme_colors[2], size = 0.3) +
  geom_line(aes(y = IT_comp_upperconf), color = theme_colors[2], size = 0.3) +
  #NIT competitors
  geom_ribbon(aes(ymin = NIT_comp_lowerconf, ymax = NIT_comp_upperconf), fill = theme_colors[3], alpha = 0.2) +
  geom_line(aes(y = NIT_comp_av_CAR, group = 1, color = "non-IT competitors"), size = 0.5) +
  geom_point(aes(y = NIT_comp_av_CAR, color = "non-IT competitors", shape = "non-IT competitors"), size = 2) +
  geom_line(aes(y = NIT_comp_lowerconf), color = theme_colors[3], size = 0.3) +
  geom_line(aes(y = NIT_comp_upperconf), color = theme_colors[3], size = 0.3) +
  
  # Additional plot elements
  geom_segment(x = 0, y = min(df_NIT_av_CAR$lowerconf)*1.5, xend = 0, yend = max(df_IT_av_CAR$upperconf)*1.5, 
               col = "grey20", size = 1.1, linetype = "dashed") +
  geom_segment(x = xcoord_eventw[1] - 1, y = 0, xend = xcoord_eventw[length(xcoord_eventw)] + 1, yend = 0, size = 1) +
  labs(x = "Event Window",
       y = "Average CAR",
       title = "Average Cumulative Abnormal Returns in the event period",
       subtitle = "IT and non-IT stocks and their industry competitors") +
  scale_x_continuous(limits = c(-event_window, event_window), breaks = xcoord_eventw) +
  scale_y_continuous(limits = c(min(df_NIT_av_CAR$NIT_lowerconf)*1.2, max(df_IT_av_CAR$IT_upperconf)*1.2, 0), 
                     breaks = c(seq(min(df_NIT_av_CAR$NIT_lowerconf)*1.2, max(df_IT_av_CAR$IT_upperconf)*1.2, length.out = 5), 0),
                     labels = percent(c(round(seq(min(df_NIT_av_CAR$NIT_lowerconf)*1.2, max(df_IT_av_CAR$IT_upperconf)*1.2, length.out = 5),
                                              digits = 4), 0))) +
  scale_color_manual(name = "", values = cols) +
  scale_shape_manual(name = "", values = shapes) +
  theme_bg() + theme(legend.text = element_text(size = 10), legend.position = "bottom",
                     plot.subtitle = element_text(face = "italic", hjust = 0.5, size = 11))

save_fig("plot_industry_combined_v2", output, "verylarge")

# INDUSTRY T-TESTS ===================

print("ALL COMPETITORS AV_CAR")
t.test(comp_av_CAR)

print("All firms vs All competitors avCAR")
t.test(av_CAR, comp_av_CAR)

print("IT competitors av_CAR")
t.test(IT_comp_av_CAR)

print("Non-IT competitors av_CAR")
t.test(NIT_comp_av_CAR)

print("IT --><-- Non-IT competitors av_CAR")
t.test(IT_comp_av_CAR, NIT_comp_av_CAR)

print("IT vs. competitor avCAR")
t.test(IT_av_CAR, IT_comp_av_CAR)

print("NIT vs. competitor avCAR")
t.test(NIT_av_CAR, NIT_comp_av_CAR)

print("IT vs. competitor avCAR AFTER the event")
t.test(IT_av_CAR[6:11], IT_comp_av_CAR[6:11])

# #Saving test results into a data frame
df_industry_tests <- data.frame(matrix(0, nrow = 7, ncol = 3))
colnames(df_industry_tests) <- c("Test", "T-test", "P-value")

df_industry_tests[1,] <- c("All competitors", t.test(comp_av_CAR)$statistic, t.test(comp_av_CAR)$p.value)
df_industry_tests[2,] <- c("All firms vs All competitors", t.test(av_CAR, comp_av_CAR)$statistic, t.test(av_CAR, comp_av_CAR)$p.value)
df_industry_tests[3,] <- c("IT competitors", t.test(IT_comp_av_CAR)$statistic, t.test(IT_comp_av_CAR)$p.value)
df_industry_tests[4,] <- c("Non-IT competitors", t.test(NIT_comp_av_CAR)$statistic, t.test(NIT_comp_av_CAR)$p.value)
df_industry_tests[5,] <- c("IT competitors vs. Non-IT competitors", t.test(IT_comp_av_CAR, NIT_comp_av_CAR)$statistic, t.test(IT_comp_av_CAR, NIT_comp_av_CAR)$p.value)
df_industry_tests[6,] <- c("IT vs IT competitors", t.test(IT_comp_av_CAR, IT_av_CAR)$statistic, t.test(IT_comp_av_CAR, IT_av_CAR)$p.value)
df_industry_tests[7,] <- c("Non-IT vs. non-IT competitors", t.test(NIT_comp_av_CAR, NIT_av_CAR)$statistic, t.test(NIT_comp_av_CAR, NIT_av_CAR)$p.value)

write.csv(df_industry_tests, paste0(output, "table_industry-tests_v2.csv"), row.names = FALSE)
