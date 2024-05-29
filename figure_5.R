#' ==============================================================================
#' Script Name: figure_5.R
#' Description: This script generates figure 5.
#' @Author: [Ali Raisolsadat (sraisolsadat@upei.ca) & Quan Dau (vdau@upei.ca)]
#' @Date: [28 May 2024]
#' ==============================================================================

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' Load necessary libraries
library(ggplot2)       # For creating plots
library(patchwork)     # For combining multiple plots
library(tidyverse)     # For data manipulation and visualization
library(ggtext)        # For improved text and annotations in ggplot2
library(tidyr)         # For data tidying and reshaping
library(ggthemes)   # For additional themes in ggplot

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' 
#' Function 1
#' This function prepares data for plot a and b
#' @param file_name this csv file name (directory)
#' @param countries the name of the countries in the data file
#' @return gathered data frame for plotting
prepare_plot_a_c_data_func <- function(file_name, countries) {
  #' read the csv file with synergy contributions and rename the columns
  average_synergy_df <- read.csv(file_name)
  names(average_synergy_df) <- c("X", countries)
  #' widen the data using country as the key
  average_synergy_df_gather <- gather(average_synergy_df, key = "Country", value = "Contribution",  -X)
  #' factorize the country names
  average_synergy_df_gather$Country <-
    factor(average_synergy_df_gather$Country, levels = c(country_names))

  return(average_synergy_df_gather)
}

#' Function 2
#' This function creates data frames necessary for plotting plots b and d
#' @param file_name is the directory for the synergy contribution data frame
#' @return a list of data frame for plots b and d
prepare_plot_b_d_data_func <- function(file_name) {
  #' constants
  country_length <- 7
  bound <- 20

  #' start by finding the order of the countries based on the median
  average_synergy_df <-
    read.csv(file_name, row.names = 1) %>%  #' read csv file
    apply(2, median, na.rm = TRUE) %>% #' apply median to columns (can change to mean)
    data.frame() %>%  #' make it a data frame
    setNames("median") %>%  #' set column name to median
    arrange(median) #' sort median in ascending order

  #' replace dots with space in the row names
  graybar_counries_econ <- gsub("\\.", " ", row.names(average_synergy_df))

  #' order countries based on their median values and factorize
  #' create gray line using the bound value
  graybar1 <-
    data.frame(Country = factor(graybar_counries_econ,
                                levels = graybar_counries_econ),
               data = rep(-bound, country_length))
  graybar2 <-
    data.frame(Country = factor(graybar_counries_econ,
                                levels = graybar_counries_econ),
               data = rep(bound, country_length))

  #' assign color based on factor
  average_synergy_df$Color <- ifelse(average_synergy_df[,1] < 0, "#ff9039", "#cfe756")

  #' create the final data frame for plots b and d
  average_synergy_df <- cbind(graybar_counries_econ, average_synergy_df) %>%
    setNames(c("Country", "Contribution", "Color"))
  row.names(average_synergy_df) <- NULL

  return(list("graybar1" = graybar1, "graybar2" = graybar2, "average_synergy_df" = average_synergy_df))
}

#' Function 3
#' This function plots the plot a and c
#' @param contribution_df is the cleaned data frame for plots a and b
#' @param title is the title of the plot
#' @return a ggplot
plot_a_c_func <- function(contribution_df, title) {
  #' Constants
  upper_limit <- 60 #' upper limit for y-axis
  lower_limit <- -30 #' lower limit for y-axis

  #' Change the plot theme here
  #' Anything that affects the theme needs to be written here
  plot_theme <- theme(legend.position = "none", plot.title = element_text(face = "bold"))

  #' Plot the boxplots here
  plot <-
    #' select the data frame and the aesthetics
    ggplot(contribution_df, aes(Country,  Contribution)) +
    #' select the manual aesthetics for the boxplot
    geom_boxplot(aes(color = Country), lwd = 1.2) +
    #' add jitter if there are overlaying data
    geom_jitter(aes(color = Country), alpha = 0.5 , size = 7, position = position_jitter(width = 0.1)) +
    theme_few(base_size = 45) +
    #' add a horizontal line at the y-axis to indicate the center
    geom_hline(yintercept = 0, color = "darkgray") +
    #' add the plot theme
    plot_theme +
    #' change the scale of the y-axis
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(lower_limit, upper_limit)) +
    #' add labels
    labs(x = "", y = "Contribution", title = title)

  return(plot)
}

# Function 4
#' This function plots the plot b and d
#' @param contribution_df is the cleaned data frame for plots a and b
#' @param graybar1 is the left boundary for x-axis
#' @param graybar2 is the right boundary for x-axis
#' @param title is the title of the plot
#' @return a ggplot
plot_b_d_func <- function(contribution_df, graybar1, graybar2, title) {
  #' Change the plot theme here
  #' Anything that affects the theme needs to be written here
  plot_theme <-
    theme(
      axis.ticks.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(margin = margin(r = -70)),
      panel.border  = element_blank()
      # axis.line.x  = element_line(color = 'black')
    )

  #' Plot everything
  plot <-  ggplot(data=contribution_df, aes(x = Country, y = Contribution , fill = Contribution)) +
    ggthemes::theme_few(base_size = 45) +
    geom_bar(data =graybar1, aes(x =Country, y = data),stat="identity", width = 0.7,  fill = "#f5f3f3") +
    geom_bar(data =graybar2, aes(x =Country, y = data),stat="identity", width = 0.7,  fill = "#f5f3f3") +
    geom_bar(stat="identity",width = 0.7,  fill = contribution_df$Color) +
    labs(x="", y = "Contribution", title = title) +
    scale_y_continuous(labels = function(x) paste0(x, "%  "),limits=c(-20,20)) +
    geom_text(aes(label=paste0("  ",round(Contribution, 2), "%  ")), hjust="outward", size=13)+
    geom_rect(aes(xmin=0.65, xmax = 1.35, ymin = -20, ymax = -17), fill = "#d5ebf3") +
    geom_rect(aes(xmin=1.65, xmax = 2.35, ymin = -20, ymax = -17), fill = "#bfe3f0") +
    geom_rect(aes(xmin=2.65, xmax = 3.35, ymin = -20, ymax = -17), fill = "#a3daee") +
    geom_rect(aes(xmin=3.65, xmax = 4.35, ymin = -20, ymax = -17), fill = "#85d1ed") +
    geom_rect(aes(xmin=4.65, xmax = 5.35, ymin = -20, ymax = -17), fill = "#5cc9f1") +
    geom_rect(aes(xmin=5.65, xmax = 6.35, ymin = -20, ymax = -17), fill = "#28b9ef") +
    geom_rect(aes(xmin=6.65, xmax = 7.35, ymin = -20, ymax = -17), fill = "#0aafec") +
    geom_text(aes(label = 7:1 , y = -18.5), size = 23, color = "white", fontface = "bold")+
    scale_y_continuous(breaks=seq(-20,20,by=10), label = c("-20%","-10%","0%","10%","20%" ))+
    geom_hline(yintercept = 0, color = "#4d4d4d", lwd = 1.5) +
    plot_theme +
    coord_flip()

  return(plot)
}

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' 
#' SECTION 1: PLOT A,C
#' Constants
file_name_econ <- "results_datasets/plot5_years_median_for_econ_indicators.csv"
file_name_env <- "results_datasets/plot5_years_median_for_env_indicators.csv"
country_names <- c("Canada", "France","Germany","Italy","Japan","United \nKingdom", "United \nStates")

#' Plot a and c
boxplot_data_econ <- prepare_plot_a_c_data_func(file_name_econ, country_names)
p1 <- plot_a_c_func(boxplot_data_econ, "a")
boxplot_data_env <- prepare_plot_a_c_data_func(file_name_env, country_names)
p2 <- plot_a_c_func(boxplot_data_env, "c")


#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' 
#' SECTION 2: PLOT B
#' Constants
#' Create plotting list
plotting_list <- prepare_plot_b_d_data_func(file_name_econ)

#' create plotting data frame
graybar1 <- plotting_list$graybar1
graybar2 <- plotting_list$graybar2
average_synergy_df <- plotting_list$average_synergy_df

#' plot b
p3 <- plot_b_d_func(average_synergy_df, graybar1, graybar2, "b")

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' 
#' SECTION 3: PLOT D
#' Create plotting list
plotting_list <- prepare_plot_b_d_data_func(file_name_env)

#' create plotting data frame
graybar1 <- plotting_list$graybar1
graybar2 <- plotting_list$graybar2
average_synergy_df <- plotting_list$average_synergy_df

#' plot d
p4 <- plot_b_d_func(average_synergy_df, graybar1, graybar2, "d")

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' 
#' SECTION 4: Put everything together and save the figure
ggsave("main_manuscript_figures/figure_5.png",(p1 | p3) / (p2 | p4), 
       width = 6500, height = 3200, units = "px", limitsize = FALSE, dpi = 100)
