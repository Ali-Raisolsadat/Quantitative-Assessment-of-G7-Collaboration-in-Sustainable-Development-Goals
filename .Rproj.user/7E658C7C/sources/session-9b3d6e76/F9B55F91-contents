#' ==============================================================================
#' Script Name: figure_4.R
#' Description: This script generates the heatmaps for figure 4 and Supplementary Figures S7-S12.
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
#' Function to plot indicator contributions
#' @data: Data frame containing the contributions data
#' @ind.Name: Name of the indicator to be used as the plot title
ind.plot <- function(data, ind.Name) {
  # Calculate median for each column
  AVG <- apply(data, 2, median, na.rm = TRUE)
  
  # Append the median as a new row
  data <- rbind(data, AVG)
  
  # Add 'Year' column to the data
  data$Year <- c(format(2000:2020), "Median")
  
  # Define column names for countries
  heading <- c("Canada", "France", "Germany", "Italy", "Japan", "United \nKingdom", "United \nStates", "Year")
  names(data) <- heading  # Rename columns
  
  # Reshape the data to long format for plotting
  sdg_df_gather <- tidyr::gather(data, key = "Country", value = "Contribution", -Year)
  sdg_df_gather$Country <- factor(sdg_df_gather$Country, levels = heading[1:7])
  sdg_df_gather$Year <- factor(sdg_df_gather$Year, levels = c("Median", format(2020:2000)))
  
  # Assign colors based on the value of 'Contribution'
  color <- TRUE
  for (i in 1: nrow(sdg_df_gather)) {
    if (isTRUE(sdg_df_gather$Contribution[i] < 0)) {
      color[i] <- "#ff9039"  # Orange for negative contributions
    } else if (isTRUE(sdg_df_gather$Contribution[i] > 0)) {
      color[i] <- "#cfe756"  # Green for positive contributions
    } else if (isTRUE(sdg_df_gather$Contribution[i] == 0)) {
      color[i] <- "#339900"  # Dark green for zero contributions
    } else {
      color[i] <- "gray"  # Gray for NA or other values
    }
  }
  
  # Create the first plot (heatmap)
  p1 <- ggplot2::ggplot(sdg_df_gather, aes(Country, Year, fill = Contribution)) +
    geom_tile(
      stat = "identity",
      position = "identity",
      color = "white",
      fill = color,
      lwd = 1.5,
      linetype = 1
    ) +
    ggthemes::theme_few(base_size = 50) +
    labs(x = "", y = "") +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      legend.title = element_blank(),
      axis.text.x = element_text(size = 40, angle = 0, vjust = 1),
      axis.ticks.length.x = unit(0.8, "cm")
    ) +
    geom_text(
      aes(
        label = ifelse(
          is.na(Contribution), 
          "", 
          ifelse(
            round(Contribution, digits = 1) %in% c(100, -100),
            paste0(round(Contribution), "%"),
            ifelse(
              Contribution >= -0.099999999 & Contribution <= 0,
              "0%",
              sprintf("%.1f%%", round(Contribution, digits = 1))
            )
          )
        )
      ),
      size = 14,
      alpha = 0.7,
      hjust = "middle"
    )
  
  # Create the second plot (boxplot with jitter)
  p2 <- ggplot2::ggplot(sdg_df_gather, aes(Country, Contribution)) +
    geom_boxplot(aes(color = Country), lwd = 1.2) +
    ggthemes::theme_few(base_size = 50) +
    geom_jitter(
      aes(color = Country),
      alpha = 0.5,
      size = 5,
      position = position_jitter(width = 0.1)
    ) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(-100, 100)) +
    geom_hline(yintercept = 0, color = "darkgray") +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    ggtitle(ind.Name) +
    labs(x = "", y = "")
  
  # Combine plots using patchwork
  p2 / plot_spacer() / p1 + plot_layout(heights = unit(c(1.7, -0.4, 7), c('null', 'null', 'null')))
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
  
  p1 <-  plots_list[[1]] | plots_list[[2]] | plots_list[[3]]
  p2 <- plots_list[[4]] | plots_list[[5]] | plots_list[[6]]
  combined_plot <- p1 / p2
  
  
  # Save the combined plot as a PNG file with specified dimensions
  ggsave(filename, combined_plot, width = width, height = height, units = units, limitsize = FALSE, dpi = 100)
}
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
# Load data
load("results_datasets/plot_4_economic_indicator_contributions.RData")  # Load economic indicator contributions data
load("results_datasets/plot_4_environment_indicator_contributions.RData")  # Load environment indicator contributions data

# Note: Replace 'ind.Name' with the appropriate indicator name when calling the 'ind.plot' function.

# Figure 4
a <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind8.1.1`, "SDG 8.1.1")
b <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind8.5.2(1)`, "SDG 8.5.2(1)")
c <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind9.2.1`, "SDG 9.2.1")
d <- ind.plot( environment_indicator_contirbutions_by_country_copy$`ind13.1.1(1)`, "SDG 13.1.1(1)")
e <- ind.plot( environment_indicator_contirbutions_by_country_copy$`ind14.1.1`, "SDG 14.1.1(1)")
f <- ind.plot(environment_indicator_contirbutions_by_country_copy$`ind15.1.2(1)`, "SDG 15.1.2(1)")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "main_manuscript_figures/figure_4.png", width = 6200, height = 6500, units = "px")

# S7 figure
a <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind1.1.1`, "SDG 1.1.1")
b <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind1.a.1(1)`, "SDG 1.A.1(1)")
c <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind1.a.2(1)`, "SDG 1.A.2(1)")
d <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind2.5.2`, "SDG 2.5.2")
e <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind3.1.1`, "SDG 3.1.1")
f <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind3.2.1`, "SDG 3.2.1")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s7.png", width = 6200, height = 6500, units = "px")

# S8 figure
a <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind3.2.2`, "SDG 3.2.2")
b <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind3.3.2`, "SDG 3.3.2")
c <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind3.4.1`, "SDG 3.4.1")
d <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind3.4.2`, "SDG 3.4.2")
e <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind3.6.1(1)`, "SDG 3.6.1(1)")
f <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind3.b.1(1)`, "SDG 3.B.1(1)")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s8.png", width = 6200, height = 6500, units = "px")

# S9 figure
a <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind6.1.1(1)`, "SDG 6.1.1(1)")
b <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind6.2.1(1)`, "SDG 6.2.1(1)")
c <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind7.1.1`, "SDG 7.1.1")
d <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind7.1.2`, "SDG 7.1.2")
e <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind7.2.1`, "SDG 7.2.1")
f <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind7.3.1`, "SDG 7.3.1")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s9.png", width = 6200, height = 6500, units = "px")

# S10 figure
a <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind8.2.1`, "SDG 8.2.1")
b <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind8.4.2(1)`, "SDG 8.4.2(1)")
c <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind8.4.2(2)`, "SDG 8.4.2(2)")
d <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind9.1.2(1)`, "SDG 9.1.2(1)")
e <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind9.1.2(2)`, "SDG 9.1.2(2)")
f <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind9.1.2(3)`, "SDG 9.1.2(3)")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s10.png", width = 6200, height = 6500, units = "px")

# S11 figure
a <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind9.2.2(1)`, "SDG 9.2.2(1)")
b <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind9.2.2(2)`, "SDG 9.2.2(2)")
c <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind9.4.1`, "SDG 9.4.1")
d <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind9.5.1`, "SDG 9.5.1")
e <- ind.plot(economic_indicator_contirbutions_by_country_copy$ind9.5.2, "SDG 9.5.2")
f <- ind.plot(economic_indicator_contirbutions_by_country_copy$`ind9.c.1(1)`, "SDG 9.C.1(1)")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s11.png", width = 6200, height = 6500, units = "px")

# S12 figure
a <- ind.plot( economic_indicator_contirbutions_by_country_copy$`ind9.c.1(2)`, "SDG 9.C.1(2)")
b <- ind.plot( environment_indicator_contirbutions_by_country_copy$`ind12.2.2(1)`, "SDG 12.2.2(1)")
c <- ind.plot(environment_indicator_contirbutions_by_country_copy$`ind12.2.2(2)`, "SDG 12.2.2(2)")
d <- ind.plot(environment_indicator_contirbutions_by_country_copy$ind15.1.1, "SDG 15.1.1")
e <- ind.plot(environment_indicator_contirbutions_by_country_copy$`ind15.4.1`, "SDG 15.4.1")
f <- ind.plot(environment_indicator_contirbutions_by_country_copy$`ind15.a.1(1)`, "SDG 15.A.1(1)")
plots_list <- list(a, b, c, d, e, f)

# Call the function to save the plots
save_plots(plots_list, "s1_s12_supplementary_figures/combined_plots_s12.png", width = 6200, height = 6500, units = "px")