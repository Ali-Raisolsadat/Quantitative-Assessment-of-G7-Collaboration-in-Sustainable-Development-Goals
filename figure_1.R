#' ==============================================================================
#' Script Name: figure_1.R
#' Description: This script generates figure 1.
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
# Load Data
dc.eco <- read.csv("results_datasets/plot_1_a_distance_average_economic_indicators.csv")  # Load economic indicators data
dc.env <- read.csv("results_datasets/plot_1_b_distance_average_environment_indicators.csv")  # Load environment indicators data
dc.eco <- dc.eco %>% 
  select(-G7)
dc.env <- dc.env %>% 
  select(-G7)

# Define country names for columns
Country <- c("Year", "Canada", "France", "Germany","Italy", "Japan", "United Kingdom" , "United States")

# Rename columns in the data frames
names(dc.eco) <- Country  # Rename columns for economic data
names(dc.env) <- Country  # Rename columns for environmental data

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
# Transforming Data
# Reshape the data frames to long format for plotting
dc.eco.gather <- tidyr::gather(dc.eco, key ="Country", value = "Change", na.rm= FALSE, -Year)
dc.env.gather <- tidyr::gather(dc.env, key ="Country", value = "Change", na.rm= FALSE, -Year)

# Define color levels for countries
colLevel <- c("#177aeb","#576b04","#8a0404","#25124f","#0b5c36","#8a8a03","#9e0e43")

# Plot Domestic Economic Change
eco <- ggplot2:: ggplot(dc.eco.gather, aes(x= Year, y = Change, color = Country))+
  geom_line(aes(y= Change), lwd = 2)+  # Add line for each country
  geom_point(aes(shape = Country), size = 8) +  # Add points for each country
  ylim(0.00,0.20) +  # Set y-axis limits
  theme_classic(base_size = 45)+  # Use classic theme
  theme( plot.title = element_text(color="black", size=70, face="bold"),
         legend.position = c(.1,.8))+  # Customize plot title and legend position
  ylab("Domestic Economic Change")+  # Set y-axis label
  ggtitle("a")  # Set plot title

# Plot Domestic Environmental Change
env <- ggplot2:: ggplot(dc.env.gather, aes(x= Year, y = Change, color = Country))+
  geom_line(aes(y= Change), lwd = 2)+  # Add line for each country
  geom_point(aes(shape = Country), size = 8) +  # Add points for each country
  ylim(0.00,0.20) +  # Set y-axis limits
  theme_classic(base_size = 45)+  # Use classic theme
  theme( plot.title = element_text(color="black", size=70, face="bold"),
         legend.position = c(.1,.8))+  # Customize plot title and legend position
  ylab("Domestic Environmental Change")+  # Set y-axis label
  ggtitle("b")  # Set plot title

# Plot Economic Change Distribution
eco.dis<- ggplot2:: ggplot(dc.eco.gather, aes(x= Country, y = Change, color = Country))+
  geom_boxplot(aes(y= Change), lwd = 1)+  # Add boxplot for each country
  geom_violin(aes(fill = Country), color = "white", alpha = 0.8) +  # Add violin plot for each country
  ylim(0.00,0.20) +  # Set y-axis limits
  theme_classic(base_size = 45)+  # Use classic theme
  theme( plot.title = element_text(color="black", size=70, face="bold"),
         legend.position = "none")+  # Customize plot title and remove legend
  ylab("Domestic Economic Change")+  # Set y-axis label
  ggtitle("c")  # Set plot title

# Plot Environmental Change Distribution
env.dis<- ggplot2:: ggplot(dc.env.gather, aes(x= Country, y = Change, color = Country))+
  geom_boxplot(aes(y= Change), lwd = 1)+  # Add boxplot for each country
  geom_violin(aes(fill = Country), color = "white", alpha = 0.8) +  # Add violin plot for each country
  ylim(0.00,0.20) +  # Set y-axis limits
  theme_classic(base_size = 45)+  # Use classic theme
  theme( plot.title = element_text(color="black", size=70, face="bold"),
         legend.position = "none")+  # Customize plot title and remove legend
  ylab("Domestic Environmental Change")+  # Set y-axis label
  ggtitle("d")  # Set plot title


#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' Please Save image at least 6500 x 3200px to get high resolution
#'
# Put plots together
change <- eco + env
dis <- eco.dis + env.dis
ggsave("main_manuscript_figures/figure_1.png", change/dis, 
       width = 6500, height = 3200, units = "px", limitsize = FALSE, dpi = 100)



















