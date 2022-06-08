#' **************************************************************************************
#' **************************************************************************************
#' @author Ali Raisolsadat
#' @date 18/05/2022
#' @details University of Prince Edward Island - Research 2022 
#' School of Climate Change and Adaptation / School of Mathematical and Computational Sciences.
#' This code is not to be shared, published or used without permission by the author. 

#' **************************************************************************************
#' **************************************************************************************

#' Required libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(scales)
library(ggpubr)

require(grid)
require(gridExtra)

#' Constants. Please apply changes here. 
YEAR_THRESHOLD <- 1995
YEAR_SEQUENCE <- 1995:2022
NEW_YEAR_SEQ <- 2000:2020
SDG_INDICATORS_PATH <- "clean_sdg_indicators"
COUNTRY_NAMES <- c("Canada", "France", "Germany", "Japan", "Italy", "United Kingdom", "United States")

#' **************************************************************************************
#' **************************************************************************************

#' This function normalizes entire data-frame of indicator values for group of countries.
#' @param x is the data-frame to be normalized.
#' @return a normalized data-frame.
normalize_fun <- function(x) {
  norm_x <- as.matrix(x)
  return(data.frame(rescale(x, c(0,1))))
}

#' This function cleans the indicator data. The processes is as follows:
#' Recursively: 
#' 1. Filter the countries by name and extract their data. 
#' 2. Add their data to a new data frame.
#' 3. Add the new data frame to a list 
#' @param sdg_ind_unclean_list is a list of unclean data
#' @return a list of cleaned data-frames with each column representing each country
#' and each row an observation of a specific indicator value for each country.
clean_indicator_data_fun <- function(sdg_ind_unclean_list) {
  #' find indicators that have more than one attribute.
  num_more_than_one_att <- c()
  for(i in 1:length(sdg_ind_unclean_list)) {
    if (ncol(sdg_ind_unclean_list[[i]]) > 4) {
      num_more_than_one_att <- rbind(num_more_than_one_att, i)
      print(i)
    }
  }
  if (length(num_more_than_one_att) != 0) {
    sdg_ind_unclean_list[[num_more_than_one_att]] <- NULL #remove them
  }
  
  #' The unclean data has four columns. 
  #' The fourth column is the data, associated to the country in the second column,
  #' for the observation date in first column. 
  #' Recursively: 
  #' 1. Filter the countries by name and extract their data. 
  #' 2. Add their data to a new data frame.
  #' 3. Add the new data frame to a list 
  sdg_indicator_selected_for_countries <- list()
  for (j in 1:length(sdg_ind_unclean_list)) {
    temp_ind_df <- data.frame()
    for (i in 1:length(COUNTRY_NAMES)) {
      temp_ind_data <- ((sdg_ind_unclean_list[[j]] %>% 
                           filter(Entity == COUNTRY_NAMES[i] & Year >= YEAR_THRESHOLD) %>%
                           complete(Year = YEAR_SEQUENCE))[,4] %>% data.frame())[,1]
      temp_ind_df <- rbind(temp_ind_df, temp_ind_data)
    }
    temp_ind_df <- (t(temp_ind_df))
    temp_ind_df <- normalize_fun(temp_ind_df)
    colnames(temp_ind_df) <- gsub(pattern = " ", replacement = "_", tolower(COUNTRY_NAMES))
    rownames(temp_ind_df) <- YEAR_SEQUENCE
    sdg_indicator_selected_for_countries[[j]] <- temp_ind_df
  }
  
  names(sdg_indicator_selected_for_countries) <- names(sdg_ind_unclean_list)
  print(names(sdg_indicator_selected_for_countries))
  return(sdg_indicator_selected_for_countries)
}

#' This function uses the cleaned data-frames to evaluate impact for a group of
#' countries provided by the user.
#' @param sdg_ind_clean_list is he list of cleaned data-frames
#' @return an impact matrix with each row representing the observation date, and
#' each column the impact of an indicator.
evaluate_dist_group_fun <- function(sdg_ind_clean_list) {
  cc <- data.frame()
  for (i in 1:length(sdg_ind_clean_list)) {
    dd <- sdg_ind_clean_list[[i]]
    one_dist_indicator <- sqrt(rowSums(diff(as.matrix(dd))^2, na.rm = TRUE))
    cc <- rbind(cc, one_dist_indicator)
  }
  cc <- data.frame(t(cc))
  colnames(cc) <- names(sdg_ind_clean_list)
  rownames(cc) <- as.Date(ISOdate(YEAR_SEQUENCE[-1], 1, 1))  # beginning of year
  cc$year <- YEAR_SEQUENCE[-1]
  cc <- cc %>% filter(year <= 2020 & year >= 2000)
  cc$year <- NULL
  return(cc)
}

#' This function uses the cleaned data-frames to evaluate impact for a single country
#' from the list of countries provided by the user.
#' @param sdg_ind_clean_list is he list of cleaned data-frames
#' @return an impact matrix with each row representing the observation date, and
#' each column the impact of an indicator.
evaluate_dist_individual_fun <- function(sdg_ind_clean_list) {
  country_list <- gsub(pattern = " ", replacement = "_", tolower(COUNTRY_NAMES))
  country_dist_mat_list <- list()
  for(j in 1:length(country_list)) {
    cc <- data.frame()
    for (i in 1:length(sdg_ind_clean_list)) {
      dd <- sdg_ind_clean_list[[i]][country_list[j]]
      one_dist_indicator <- sqrt(rowSums(diff(as.matrix(dd))^2, na.rm = TRUE))
      cc <- rbind(cc, one_dist_indicator)
    }
    cc <- data.frame(t(cc))
    colnames(cc) <- names(sdg_ind_clean_list)
    rownames(cc) <- as.Date(ISOdate(YEAR_SEQUENCE[-1], 1, 1))  # beginning of year
    cc$year <- YEAR_SEQUENCE[-1]
    cc <- cc %>% filter(year <= 2020 & year >= 2000)
    cc$year <- NULL
    country_dist_mat_list[[j]] <- cc
  }
  names(country_dist_mat_list) <- country_list
  return(country_dist_mat_list)
}

#' This function uses the cleaned data-frames to evaluate difference matrix for a group of
#' countries provided by the user.
#' @param sdg_ind_clean_list is he list of cleaned data-frames
#' @return an difference matrix with each row representing the observation date, and
#' each column the difference (historical change) of an indicator.
evaluate_grads_group_fun <- function(sdg_ind_clean_list) {
  cc <- data.frame()
  for (i in 1:length(sdg_ind_clean_list)) {
    dd <- sdg_ind_clean_list[[i]]
    one_grads_indicator <- rowMeans(diff(as.matrix(dd)), na.rm = TRUE)
    one_grads_indicator[one_grads_indicator < 0] <- -1
    one_grads_indicator[one_grads_indicator > 0] <- 1
    cc <- rbind(cc, one_grads_indicator)
  }
  cc <- data.frame(t(cc))
  colnames(cc) <- names(sdg_ind_clean_list)
  rownames(cc) <- as.Date(ISOdate(YEAR_SEQUENCE[-1], 1, 1))  # beginning of year
  cc$year <- YEAR_SEQUENCE[-1]
  cc <- cc %>% filter(year <= 2020 & year >= 2000)
  cc$year <- NULL
  return(cc)
}

#' This function uses the cleaned data-frames to evaluate difference matrix for a single 
#' country from the list of countries provided by the user.
#' @param sdg_ind_clean_list is he list of cleaned data-frames
#' @return an difference matrix with each row representing the observation date, and
#' each column the difference (historical change) of an indicator.
evaluate_grad_individual_fun <- function(sdg_ind_clean_list) {
  country_list <- gsub(pattern = " ", replacement = "_", tolower(COUNTRY_NAMES))
  country_grad_mat_list <- list()
  for(j in 1:length(country_list)) {
    cc <- data.frame()
    for (i in 1:length(sdg_ind_clean_list)) {
      dd <- sdg_ind_clean_list[[i]][country_list[j]]
      one_grads_indicator <- rowMeans(diff(as.matrix(dd)), na.rm = TRUE)
      one_grads_indicator[one_grads_indicator < 0] <- -1
      one_grads_indicator[one_grads_indicator > 0] <- 1
      cc <- rbind(cc, one_grads_indicator)
    }
    cc <- data.frame(t(cc))
    colnames(cc) <- names(sdg_ind_clean_list)
    rownames(cc) <- as.Date(ISOdate(YEAR_SEQUENCE[-1], 1, 1))  # beginning of year
    cc$year <- YEAR_SEQUENCE[-1]
    cc <- cc %>% filter(year <= 2020 & year >= 2000)
    cc$year <- NULL
    country_grad_mat_list[[j]] <- cc
  }
  names(country_grad_mat_list) <- country_list
  return(country_grad_mat_list)
}

#' This function evaluates the distribution of positive contributions for each indicator by
#' the group of countries. 
#' @param contribution_list is a list of country contributions to indicators
#' @param indicator_names is a vector of indicator names
#' @return a data frame that has each country as column, each indicator name as row, and 
#' each observation is the distribution of a single country to the indicator through out the 
#' given years
cont_dist_fun <- function(contribution_list, indicator_names) {
  contribution_j_count <- data.frame()
  for (j in 1:length(COUNTRY_NAMES)) {
    contribution_j_data <- contribution_list[[j]]
    contribution_j_data <- contribution_j_data[indicator_names]
    Pos_Count <- rowSums(contribution_j_data[,1:ncol(contribution_j_data)] > 0, na.rm = TRUE)
    Neg_Count <- rowSums(contribution_j_data[,1:ncol(contribution_j_data)] < 0, na.rm = TRUE)
    contribution_j_count <- rbind(contribution_j_count, (Pos_Count / (Pos_Count + Neg_Count)))
  }
  contribution_j_count <- data.frame(t(contribution_j_count))
  colnames(contribution_j_count) <- COUNTRY_NAMES
  rownames(contribution_j_count) <- as.Date(ISOdate(NEW_YEAR_SEQ, 1, 1))  # beginning of year
  return(contribution_j_count)
}

#' This function evaluates the distribution of positive contributions for each indicators each country
#' through out the years.
#' @param contribution_list is a list of country contributions to indicators
#' @param indicator_names is a vector of indicator names
#' @return a data frame that has each country as column, each given year as the row, and each
#' observation is the distribution of positive contribution to all indicators during the given year
cont_yearly_dist_fun <- function(contribution_list, indicator_names) {
  contribution_j_count <- data.frame()
  for (j in 1:length(COUNTRY_NAMES)) {
    contribution_j_data <- contribution_list[[j]]
    contribution_j_data <- contribution_j_data[indicator_names]
    
    Pos_Count <- colSums(contribution_j_data[,1:ncol(contribution_j_data)] > 0, na.rm = TRUE)
    Neg_Count <- colSums(contribution_j_data[,1:ncol(contribution_j_data)] < 0, na.rm = TRUE)
    contribution_j_count <- rbind(contribution_j_count, (Pos_Count / (Pos_Count + Neg_Count)))
  }
  contribution_j_count <- data.frame(t(contribution_j_count))
  colnames(contribution_j_count) <- COUNTRY_NAMES
  rownames(contribution_j_count) <- indicator_names  # beginning of year
  return(contribution_j_count)
}

#' **************************************************************************************
#' **************************************************************************************

econ_ind_names <- tolower(c("ind1.1.1", "ind1.A.1.1", "ind1.A.2.1", "ind2.5.2", 
                    "ind3.1.1", "ind3.2.1", "ind3.2.2", "ind3.3.2", "ind3.4.1", "ind3.4.2", "ind3.6.1.1", 
                    "ind3.B.1.1", "ind3.B.1.3", "ind6.1.1.1", "ind6.2.1.1", "ind7.1.1", "ind7.1.2", 
                    "ind7.2.1", "ind7.3.1", "ind8.1.1", "ind8.2.1", "ind8.4.2.1", "ind8.4.2.2","ind8.5.2.1",
                    "ind9.1.2.1", "ind9.1.2.2", "ind9.1.2.3", "ind9.2.1", "ind9.2.2.1", "ind9.2.2.2", 
                    "ind9.4.1", "ind9.5.1", "ind9.5.2", "ind9.C.1.1", "ind9.C.1.2"))

envir_ind_names <- tolower(c("ind12.2.2.1", "ind12.2.2.2", "ind13.1.1.1", "ind14.1.1.2", 
                     "ind15.1.1", "ind15.1.2.2", "ind15.1.2.3", "ind15.4.1", "ind15.A.1.2", "ind15.B.1.2"))
indicaotrs_econ_envir_names <- c(econ_ind_names, envir_ind_names)

#' Read all the SDG indicator files into a list
filenames = list.files(path = SDG_INDICATORS_PATH, pattern="*.csv", full.names=TRUE)
sdg_indicator_selected_clean <- lapply(filenames, read.csv, row.names = 1)
indicatornames = gsub(pattern = ".csv", replacement = "", list.files(path = SDG_INDICATORS_PATH, pattern="*.csv"))
names(sdg_indicator_selected_clean) <- indicatornames

#' **************************************************************************************
#' **************************************************************************************

#' Compute the impact for the group of countries.
indicator_impact_group <- evaluate_dist_group_fun(sdg_indicator_selected_clean)
write.csv(x = indicator_impact_group, file = "impact_results\\impact_g7.csv")

#' Compute the impact for individual countries in the group.
indicator_impact_countries_list <- evaluate_dist_individual_fun(sdg_indicator_selected_clean)
#' Write the impact results.
for (i in 1:length(indicator_impact_countries_list)) {
  write.csv(x = indicator_impact_countries_list[[i]], file = paste0("impact_results\\", tolower(COUNTRY_NAMES[i]), "_impact.csv"))
}

#' **************************************************************************************
#' **************************************************************************************
#' Construct average impact for G7 countries - Economic indicators
econ_impact_ind_df <- data.frame()
for (i in 1:length(indicator_impact_countries_list)) {
  econ_impact_ind_data <- rowMeans(indicator_impact_countries_list[[i]][econ_ind_names], na.rm = TRUE)
  econ_impact_ind_df <- rbind(econ_impact_ind_df, econ_impact_ind_data)
}
econ_impact_ind_df <- rbind(econ_impact_ind_df, rowMeans(indicator_impact_group[econ_ind_names], na.rm = TRUE))
econ_impact_ind_df <- data.frame(t(econ_impact_ind_df))
colnames(econ_impact_ind_df) <- c(COUNTRY_NAMES, "G7")
write.csv(x = econ_impact_ind_df, file = paste0("impact_results\\impact_average_economic_indicators.csv"))

#' Construct average impact for G7 countries - Economic indicators
env_impact_ind_df <- data.frame()
for (i in 1:length(indicator_impact_countries_list)) {
  env_impact_ind_data <- rowMeans(indicator_impact_countries_list[[i]][envir_ind_names], na.rm = TRUE)
  env_impact_ind_df <- rbind(env_impact_ind_df, econ_impact_ind_data)
}
env_impact_ind_df <- rbind(env_impact_ind_df, rowMeans(indicator_impact_group[envir_ind_names], na.rm = TRUE))
env_impact_ind_df <- data.frame(t(env_impact_ind_df))
colnames(env_impact_ind_df) <- c(COUNTRY_NAMES, "G7")
write.csv(x = env_impact_ind_df, file = paste0("impact_results\\impact_average_environment_indicators.csv"))

#' **************************************************************************************
#' **************************************************************************************
#' Compute the difference for the group of countries.
indicator_gradient_group <- evaluate_grads_group_fun(sdg_indicator_selected_clean)

#' Compute the difference for individual countries in the group.
indicator_gradient_countries_list <- evaluate_grad_individual_fun(sdg_indicator_selected_clean)

#' This section reads the goal direction of the SDG indicators.
true_direction_df <- read.csv("sdg_conditions_true_directions.csv")
true_direction_df <- true_direction_df %>% filter(included == 1)
true_direction_df_copy <- true_direction_df %>% filter(tolower(indicator) %in% indicaotrs_econ_envir_names)

#' Create a data-frame that has rows either +1 or -1 to show the increasing and decreasing of 
#' SDG indicators. 
target_indicator_df <- data.frame()
for(i in 1:nrow(true_direction_df_copy)) {
  target_indicator_df <- rbind(target_indicator_df, rep(true_direction_df_copy$direction[i], length(YEAR_SEQUENCE[-1])))
}
target_indicator_df <- data.frame(t(target_indicator_df))
colnames(target_indicator_df) <- colnames(indicator_gradient_group)
rownames(target_indicator_df) <- as.Date(ISOdate(YEAR_SEQUENCE[-1], 1, 1))  # beginning of year
target_indicator_df$year <- as.Date(ISOdate(YEAR_SEQUENCE[-1], 1, 1))  # beginning of year
target_indicator_df <- target_indicator_df %>% filter(year <= "2020-01-01" & year >= "2000-01-01")
target_indicator_df$year <- NULL

synergy_direction_df <- indicator_gradient_group * target_indicator_df
synergy_direction_df[is.na(synergy_direction_df)] <- NA
#' **************************************************************************************
#' **************************************************************************************
#' Compute synergy and synergy total based on the definition. 
#' Start by creating an empty data-frame.
sum_indicator_impact_countries <- data.frame(matrix(0, ncol = ncol(indicator_impact_group), nrow = nrow(indicator_impact_group)))

#' Loop through the country impact list and add them to each other
for (i in 1:(length(COUNTRY_NAMES))) {
  sum_indicator_impact_countries <- sum_indicator_impact_countries + indicator_impact_countries_list[[i]]
}

#' Apply the synergy formula
synergy <- sum_indicator_impact_countries - indicator_impact_group
colnames(synergy) <- names(sdg_indicator_selected_clean)
rownames(synergy) <- as.Date(ISOdate(NEW_YEAR_SEQ, 1, 1))  # beginning of year

#' **************************************************************************************
#' **************************************************************************************
#' Compute synergy total using the definition: synergy x Ic
gross_synergy <- (synergy * synergy_direction_df) / length(COUNTRY_NAMES)
write.csv(x = gross_synergy[econ_ind_names], file = "synergy_results\\economic_gross_synergy_g7.csv")
write.csv(x = gross_synergy[envir_ind_names], file = "synergy_results\\environment_gross_synergy_g7.csv")

#' **************************************************************************************
#' **************************************************************************************
#' Construct average Gross Synergy for G7 countries - Economic and Environment indicators
econ_gross_synergy_ind_df <- gross_synergy[econ_ind_names]
econ_gross_synergy_ind_df <- data.frame(rowMeans(econ_gross_synergy_ind_df, na.rm = TRUE))

envir_gross_synergy_ind_df <- gross_synergy[envir_ind_names]
envir_gross_synergy_ind_df <- data.frame(rowMeans(envir_gross_synergy_ind_df, na.rm = TRUE))

econ_envir_gross_synergy_avg_df <- data.frame(econ_gross_synergy_ind_df, envir_gross_synergy_ind_df)
colnames(econ_envir_gross_synergy_avg_df) <- c("Economy", "Environment")
write.csv(x = econ_envir_gross_synergy_avg_df, file = paste0("synergy_results\\average_gross_synergy.csv"))

#' **************************************************************************************
#' **************************************************************************************
#' Compute Contribution of each country to the overall synergy. 
country_synergy_contribution_list <- list()
for (i in 1:length(COUNTRY_NAMES)) {
  contribution_j <- (indicator_impact_countries_list[[i]] - (indicator_impact_group/length(COUNTRY_NAMES))) / gross_synergy * 100
  contribution_j[contribution_j == -Inf] <- 100
  contribution_j[contribution_j == Inf] <- 100
  contribution_j <- contribution_j / length(COUNTRY_NAMES)
  
  write.csv(x = contribution_j, file = paste0("contribution_results\\", tolower(COUNTRY_NAMES[i]), "_contribution_of_synergy.csv"))
  country_synergy_contribution_list[[i]] <- contribution_j
}
names(country_synergy_contribution_list) <- names(indicator_impact_countries_list)

#' **************************************************************************************
#' **************************************************************************************
#' Check if the synergies add up.
sum_syn_count <- data.frame(matrix(0, ncol = ncol(indicator_impact_group), nrow = nrow(indicator_impact_group)))
for (i in 1:(length(COUNTRY_NAMES))) {
  sum_syn_count <- sum_syn_count + country_synergy_contribution_list[[i]]
}

#' **************************************************************************************
#' **************************************************************************************
#' Positive proportion of contributions to gross synergy of SDG indicators. 
economic_cont_j_dist_per_ind <- cont_yearly_dist_fun(country_synergy_contribution_list, econ_ind_names)
write.csv(x = economic_cont_j_dist_per_ind, file = paste0("contribution_results\\economic_contribuitons_per_indicator_g7.csv"))

environment_cont_j_dist_per_ind <- cont_yearly_dist_fun(country_synergy_contribution_list, envir_ind_names)
write.csv(x = economic_cont_j_dist_per_ind, file = paste0("contribution_results\\environment_contribuitons_per_indicator_g7.csv"))

#' yearly positive proportion of contributions to gross synergy of economic and environmental SDG indicators by G7 countries.
yearly_economic_cont_j_dist <- cont_dist_fun(country_synergy_contribution_list, econ_ind_names)
write.csv(x = yearly_economic_cont_j_dist, file = paste0("contribution_results\\yearly_economic_contributions_by_g7.csv"))

yearly_environment_cont_j_dist <- cont_dist_fun(country_synergy_contribution_list, envir_ind_names)
write.csv(x = yearly_economic_cont_j_dist, file = paste0("contribution_results\\yearly_environment_contributions_by_g7.csv"))