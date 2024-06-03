#' ==============================================================================
#' Script Name: figure_2.R
#' Description: This script generates figure 2 and supplementary figures S1-S6.
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
#' Function 1
#' Create a plot for a specific indicator
#' 
#' This function generates a plot for a specific indicator over the years,
#' showing the changes across different countries.
#' 
#' @param ind The data for the indicator, with years as row names and countries as column names.
#' @param yaxis The label for the y-axis.
#' @param title The title of the plot.
#' @return A ggplot object representing the indicator plot.
#' @export
indicator <- function(ind, yaxis, title) {
  # Combine year with the data and adjust column names
  ind <- cbind(Year = rownames(ind), ind)  # Add 'Year' column
  names(ind) <- c("Year", "Canada", "France", "Germany", "Italy", "Japan", "United Kingdom", "United States")  # Rename columns
  ind$Year <- year(ind$Year)  # Extract year from date
  
  # Reshape the data for plotting
  data.gather <- tidyr::gather(ind, key = "Country", value = "Change", na.rm = TRUE, -Year)  # Reshape data
  
  # Create the plot
  p <- ggplot(data.gather, aes(x = Year, y = Change, color = Country, group = Country)) +  # Set aesthetics
    geom_segment(aes(x = Year, xend = Year, y = -Inf, yend = Change), color = "gray") +  # Add gray reference lines
    geom_line(aes(y = Change), lwd = 2) +  # Add lines
    geom_point(aes(shape = Country), size = 8) +  # Add points
    scale_shape_manual(values = c(15, 17, 18, 20, 12, 4, 23)) +  # Set custom point shapes
    scale_x_continuous(breaks = 2000:2020) +  # Set x-axis breaks
    theme_classic(base_size = 38) +  # Set theme
    theme(
      plot.title = element_text(color = "black", size = 35, face = "bold", hjust = 0.5),  # Title formatting
      legend.position = "none",  # Remove legend
      legend.title = element_blank(),  # Remove legend title
      legend.text = element_text(size = 35),  # Set legend text size
      plot.margin = margin(t = 10, r = 10, b = 10, l = 10)  # Adjust plot margins
    ) +
    ylab(yaxis) +  # Set y-axis label
    xlab("") +  # Set x-axis label
    ggtitle(title)  # Set plot title
  
  return(p)
}

#' Function 2
#' Function to combine six plots into one and save as PNG
#' @plots_list: List containing six ggplot2 plots
#' @filename: Name of the PNG file to save
#' @width: Width of the PNG file in specified units (default is centimeters)
#' @height: Height of the PNG file in specified units (default is centimeters)
#' @units: Units for width and height (default is centimeters)
save_plots <- function(plots_list, filename, width, height, units) {
# Combine the first and fourth plots
  d7 <- plots_list[[1]] + plots_list[[4]]
  
  # Combine the second and fifth plots
  d8 <- plots_list[[2]] + plots_list[[5]]
  
  # Combine the third and sixth plots
  d9 <- plots_list[[3]] + plots_list[[6]] 
  d9 <- d9 + theme(
    legend.position = c(0, -0.2), 
    legend.direction = "horizontal", 
    strip.background = element_blank(),
    legend.text = element_text(size = 35)
  ) +
    guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))
  
  # Combine the three combined plots by division
  combined_plot <- (d7 / d8 / d9) + 
    plot_annotation(tag_levels = 'a') & 
    theme(
      plot.margin = margin(t = 20, r = 10, b = 50, l = 10),  # Adjust plot margins
      plot.tag = element_text(size = 55, face = "bold", vjust = -2),  # Adjust tag position and styling
      plot.tag.position = c(0.075, 0.85)  # Adjust tag position (left aligned)
    )
  # Save the combined plot as a PNG file with specified dimensions
  ggsave(filename, combined_plot, width = width, height = height, units = units, limitsize = FALSE, dpi = 100)
}


#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
# Load the data
load("results_datasets/plot_2_domestic_changes.RData")
data <- indicator_impact_countries_list

# Define a function to extract data for a specific year
extractData <- function(n) {
  ind <-  data.frame(Canada = data$canada[n],
                     France = data$france[n],
                     Germany = data$germany[n],
                     Italy = data$italy[n],
                     Japan = data$japan[n],
                     United_KingDom = data$united_kingdom[n],
                     United_States = data$united_states[n])
}

# Figure2
a <- indicator(extractData(which(colnames(data$canada) == "ind8.1.1")), "Domestic Change" , "SDG 8.1.1")
b <- indicator(extractData(which(colnames(data$canada) == "ind8.5.2(1)")), "Domestic Change" ,"SDG 8.5.2(1)")
c <- indicator(extractData(which(colnames(data$canada) == "ind9.2.1")), "Domestic Change" ,"SDG 9.2.1")
d <- indicator(extractData(which(colnames(data$canada) == "ind13.1.1(1)")), "Domestic Change", "SDG 13.1.1(1)")
e <- indicator(extractData(which(colnames(data$canada) == "ind14.1.1(1)")), "Domestic Change" ,"SDG 14.1.1(1)")
f <- indicator(extractData(which(colnames(data$canada) == "ind15.1.2(1)")), "Domestic Change" ,"SDG 15.1.2(1)") 
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "main_manuscript_figures/figure_2.png", width = 5000, height = 3000, units = "px")

# S1 figure
a <- indicator(extractData(which(colnames(data$canada) == "ind1.1.1")), "Domestic Change" ,"SDG 1.1.1")
b <- indicator(extractData(which(colnames(data$canada) == "ind1.a.1(1)")), "Domestic Change" ,"SDG 1.A.1(1)")
c <- indicator(extractData(which(colnames(data$canada) == "ind1.a.2(1)")), "Domestic Change" ,"SDG 1.A.2(1)")
d <- indicator(extractData(which(colnames(data$canada) == "ind2.5.2")), "Domestic Change" ,"SDG 2.5.2")
e <- indicator(extractData(which(colnames(data$canada) == "ind3.1.1")), "Domestic Change" ,"SDG 3.1.1")
f <- indicator(extractData(which(colnames(data$canada) == "ind3.2.1")), "Domestic Change" ,"SDG 3.2.1")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s1.png", width = 5000, height = 3000, units = "px")

# S2 figure
a<- indicator(extractData(which(colnames(data$canada) == "ind3.2.2")), "Domestic Change", "SDG 3.2.2")
b<- indicator(extractData(which(colnames(data$canada) == "ind3.3.2")), "Domestic Change", "SDG 3.3.2")
c<- indicator(extractData(which(colnames(data$canada) == "ind3.4.1")), "Domestic Change", "SDG 3.4.1")
d<- indicator(extractData(which(colnames(data$canada) == "ind3.4.2")), "Domestic Change", "SDG 3.4.2")
e<- indicator(extractData(which(colnames(data$canada) == "ind3.6.1(1)")), "Domestic Change", "SDG 3.6.1(1)") 
f<- indicator(extractData(which(colnames(data$canada) == "ind3.b.1(1)")), "Domestic Change", "SDG 3.B.1(1)")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s2.png", width = 5000, height = 3000, units = "px")

# S3 figure
a <- indicator(extractData(which(colnames(data$canada) == "ind6.1.1(1)")), "Domestic Change" ,"SDG 6.1.1(1)")
b <- indicator(extractData(which(colnames(data$canada) == "ind6.2.1(1)")), "Domestic Change", "SDG 6.2.1(1)")
c <- indicator(extractData(which(colnames(data$canada) == "ind7.1.1")), "Domestic Change" ,"SDG 7.1.1")
d <- indicator(extractData(which(colnames(data$canada) == "ind7.1.2")), "Domestic Change" ,"SDG 7.1.2")
e <- indicator(extractData(which(colnames(data$canada) == "ind7.2.1")), "Domestic Change", "SDG 7.2.1")
f <- indicator(extractData(which(colnames(data$canada) == "ind7.3.1")), "Domestic Change" ,"SDG 7.3.1")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s3.png", width = 5000, height = 3000, units = "px")

# S4 figure
a <- indicator(extractData(which(colnames(data$canada) == "ind8.4.2(1)")), "Domestic Change" ,"SDG 8.4.2(1)")
b <- indicator(extractData(which(colnames(data$canada) == "ind8.4.2(2)")), "Domestic Change" ,"SDG 8.4.2(2)")
c <- indicator(extractData(which(colnames(data$canada) == "ind9.1.2(1)")), "Domestic Change" ,"SDG 9.1.2(1)")
d <- indicator(extractData(which(colnames(data$canada) == "ind9.1.2(2)")), "Domestic Change" ,"SDG 9.1.2(2)")
e <- indicator(extractData(which(colnames(data$canada) == "ind9.1.2(3)")), "Domestic Change" ,"SDG 9.1.2(3)")
f <- indicator(extractData(which(colnames(data$canada) == "ind9.2.1")), "Domestic Change" ,"SDG 9.2.1")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s4.png", width = 5000, height = 3000, units = "px")

# S5 figure
a <- indicator(extractData(which(colnames(data$canada) == "ind9.2.2(1)")), "Domestic Change" ,"SDG 9.2.2(1)")
b <- indicator(extractData(which(colnames(data$canada) == "ind9.2.2(2)")), "Domestic Change" , "SDG 9.2.2(2)")
c <- indicator(extractData(which(colnames(data$canada) == "ind9.4.1")), "Domestic Change" ,"SDG 9.4.1")
d <- indicator(extractData(which(colnames(data$canada) == "ind9.5.1")), "Domestic Change" ,"SDG 9.5.1")
e <- indicator(extractData(which(colnames(data$canada) == "ind9.5.2")), "Domestic Change" ,"SDG 9.5.2")
f <- indicator(extractData(which(colnames(data$canada) == "ind9.c.1(1)")), "Domestic Change", "SDG 9.C.1(1)")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s5.png", width = 5000, height = 3000, units = "px")


# S6 figure
a <- indicator(extractData(which(colnames(data$canada) == "ind9.c.1(2)")), "Domestic Change" ,"SDG 9.C.1(2)")
b <- indicator(extractData(which(colnames(data$canada) == "ind12.2.2(1)")), "Domestic Change" , "SDG 12.2.2(1)")
c <- indicator(extractData(which(colnames(data$canada) == "ind12.2.2(2)")), "Domestic Change" , "SDG 12.2.2(2)")
d <- indicator(extractData(which(colnames(data$canada) == "ind15.1.1")), "Domestic Change" , "SDG 15.1.1")
e <- indicator(extractData(which(colnames(data$canada) == "ind15.4.1")), "Domestic Change" ,"SDG 15.4.1")
f <- indicator(extractData(which(colnames(data$canada) == "ind15.a.1(1)")), "Domestic Change" ,"SDG 15.A.1(1)")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s6.png", width = 5000, height = 3000, units = "px")