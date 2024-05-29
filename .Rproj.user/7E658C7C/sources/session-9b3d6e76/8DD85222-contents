#' ==============================================================================
#' Script Name: figure_3.R
#' Description: This script generates figure 3
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

# PLOT GROSS SYNERGY

# Load the necessary data from CSV files
synergy <- read.csv("results_datasets/plot_3_a_gross_synergy_g7.csv")
synergy.positive <- read.csv("results_datasets/plot_3_b_dist_of_positive_gross_synergy_g7.csv")

# Rename columns for better readability
name_indicator <- c("X", "SDG.1.1.1", "SDG.1.A.1(1)", "SDG.1.A.2(1)", "SDG.2.5.2", "SDG.3.1.1", 
                    "SDG.3.2.1", "SDG.3.2.2", "SDG.3.3.2", "SDG.3.4.1", "SDG.3.4.2", "SDG.3.6.1(1)", 
                    "SDG.3.B.1(1)", "SDG.6.1.1(1)", "SDG.6.2.1(1)", "SDG.7.1.1", "SDG.7.1.2", 
                    "SDG.7.2.1", "SDG.7.3.1", "SDG.8.1.1", "SDG.8.2.1", "SDG.8.4.2(1)", "SDG.8.4.2(2)", 
                    "SDG.8.5.2(1)", "SDG.9.1.2(1)", "SDG.9.1.2(2)", "SDG.9.1.2(3)", "SDG.9.2.1", 
                    "SDG.9.2.2(1)", "SDG.9.2.2(2)", "SDG.9.4.1", "SDG.9.5.1", "SDG.9.5.2", "SDG.9.C.1(1)", 
                    "SDG.9.C.1(2)", "SDG.12.2.2(1)", "SDG.12.2.2(2)", "SDG.13.1.1(1)", "SDG.14.1.1(1)", 
                    "SDG.15.1.1", "SDG.15.1.2(1)", "SDG.15.4.1", "SDG.15.A.1(1)")

# Assign the new column names to the synergy data
names(synergy) <- name_indicator

#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
# Transform data for plotting
synergy.gather <- tidyr::gather(synergy, key = "key", value = "Indicator", na.rm = F, -X)
synergy.gather$group <- c(rep('Economic', 714), rep('Environment', 168))

# Define functions to find the minimum and maximum values for each column
colMax <- function(data) sapply(data, max, na.rm = T)
colMin <- function(data) sapply(data, min, na.rm = T)

# Create labels for the min and max values of each indicator
labelName <- data.frame(idseq = seq(1, (ncol(synergy) - 1)), 
                        max = round(as.numeric(colMax(synergy)[-1]), 2), 
                        min = round(as.numeric(colMin(synergy)[-1]), 2), 
                        key = names(synergy)[-1])

# Set min and max labels to NA if they fall within a certain range
labelName$min <- ifelse(labelName$min >= 0.00, NA, labelName$min)
labelName$max <- ifelse(labelName$max <= 0.00, NA, labelName$max)

# Adjust label angles and positions for better readability
number_of_bar <- nrow(labelName)
angle <- 90 - 360 * (labelName$idseq - 0.5) / number_of_bar
labelName$hjust <- ifelse(angle < -90, 1, 0)
labelName$angle <- ifelse(angle < -90, angle + 180, angle)
labelName$group <- c(rep('Economic', 34), rep('Environment', 8))

# Create data for background bars
empty_bar <- 3
labelName$bg <- c(rep(0.33, 34), rep(0.33, 8))
labelName$bg_inter <- c(rep(-0.35, 34), rep(-0.35, 8))

# Create base data for the plot
base_data <- labelName %>%
  group_by(group) %>%
  summarize(start = min(idseq), end = max(idseq) - empty_bar) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end)))

# Create grid data for segmenting the plot
grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1, ]

# Re-order factor levels for correct display
synergy.gather$key <- factor(synergy.gather$key, levels = colnames(synergy[, -c(1)]))
labelName$key <- factor(labelName$key, levels = colnames(synergy[, -c(1)]))

# Create a manual sticks for labeling
stick <- data.frame(x = c(0.5, 15.5, 15.5, 15.5), y = c(-0.35, -0.20, 0, 0.20), label = c("-0.35", "-0.20", "0.00", "+0.20"))

# Create the plot using ggplot2
synergy <- ggplot2::ggplot() +
  # Draw backgrounds
  geom_bar(data = labelName, aes(x = as.factor(key), y = bg), width = 1, alpha = 0.15, fill = c(rep("#389dc7", 34), rep("#2bc516", 8)), stat = "identity", position = position_identity()) +
  geom_bar(data = labelName, aes(x = as.factor(key), y = bg_inter), width = 1, alpha = 0.1, fill = c(rep("#389dc7", 34), rep("#2bc516", 8)), stat = "identity", position = position_identity()) +
  # Draw data
  geom_col(data = synergy.gather, aes(x = as.factor(key), y = Indicator, fill = key), position = "dodge") +
  # Draw ring circle under labels
  geom_segment(data = base_data, aes(x = 0.5, y = 0.5, xend = nrow(labelName) + 0.5, yend = 0.5), colour = "white", alpha = 1, size = 159, inherit.aes = T) +
  # Draw outer lines
  geom_segment(data = base_data, aes(x = 0.5, y = 0.33, xend = 34.5, yend = 0.33), colour = "#389dc7", alpha = 1, size = 8, inherit.aes = F) +
  geom_segment(data = base_data, aes(x = 34.5, y = 0.33, xend = 42.5, yend = 0.33), colour = "#2bc516", alpha = 1, size = 8, inherit.aes = T) +
  # Draw white line at zeros
  geom_segment(data = base_data, aes(x = 0.5, y = 0, xend = nrow(labelName) + 0.5, yend = 0), colour = "white", alpha = 1, size = 2.5, inherit.aes = T) +
  # Create label sticks
  ggtext::geom_richtext(data = stick, aes(x = x, y = y, label = label), fill = "red", angle = 50, alpha = 0.7, color = "white", size = 10, fontface = "bold") +
  # Write max values at each bar
  geom_text(data = labelName, aes(x = idseq, y = max + 0.005, label = paste0(round(max, 2), " "), hjust = hjust, color = key), fontface = "bold", alpha = 0.9, size = 10, angle = labelName$angle, inherit.aes = FALSE) +
  # Write min values at each bar
  geom_text(data = labelName, aes(x = idseq, y = min - 0.065, label = paste0(round(min, 2), " "), hjust = hjust, color = key), fontface = "bold", alpha = 0.9, size = 9, angle = labelName$angle, inherit.aes = FALSE) +
  # Write label names for SDGs
  geom_text(data = labelName, aes(x = idseq, y = 0.35, label = key, hjust = hjust), color = c(rep("#389dc7", 34), rep("#2bc516", 8)), fontface = "bold", alpha = 0.8, size = 14, angle = labelName$angle, inherit.aes = FALSE) +
  ylim(-0.35, 0.55) +
  coord_polar() +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_line(colour = "gray64"),
    plot.title = element_text(color = "black", size = 100, face = "bold"),
    plot.margin = unit(rep(-1, 4), "cm")
  ) +
  ggtitle("a")


#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
# PLOT POSITIVE SYNERGY

# Rename columns for better readability
names(synergy.positive) <- name_indicator

# Transform data for plotting
synergy.gather <- tidyr::gather(synergy.positive, key = "key", value = "Indicator", na.rm = F, -X)
synergy.gather$group <- c(rep('Economic', 34), rep('Environment', 8))
synergy.gather$id <- seq(1, nrow(synergy.gather), by = 1)

# Making adjustment for label angles and positions
label_data <- synergy.gather
number_of_bar <- nrow(synergy.gather)
label_data$id <- seq(1, nrow(label_data))
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)
empty_bar <- 3

# Create base data for the plot
base_data <- synergy.gather %>%
  group_by(group) %>%
  summarize(start = min(id), end = max(id) - empty_bar) %>%
  rowwise() %>%
  mutate(title = mean(c(start, end)))

# Create grid data for segmenting the plot
grid_data <- base_data
grid_data$end <- grid_data$end[c(nrow(grid_data), 1:nrow(grid_data) - 1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Create data to draw background bars
label_data$bg <- c(rep(133, 34), rep(133, 8))
stick <- data.frame(x = c(0.5, 15.5, 15.5), y = c(0, 50, 100), label = c("0%", "50%", "100%"))

# Re-order factor levels for correct display
synergy.gather$key <- factor(synergy.gather$key, levels = colnames(synergy.positive[, -c(1)]))
label_data$key <- factor(label_data$key, levels = colnames(synergy.positive[, -c(1)]))

# Create the plot using ggplot2
synergy.positive <- ggplot2::ggplot() +
  # Draw background color
  geom_bar(data = label_data, aes(x = as.factor(id), y = bg), width = 1, alpha = 0.15, fill = c(rep("#389dc7", 34), rep("#2bc516", 8)), stat = "identity", position = position_identity()) +
  # Draw data
  geom_bar(data = synergy.gather, aes(x = as.factor(id), y = Indicator, fill = key), stat = "identity") +
  # Draw ring circle under labels
  geom_segment(data = base_data, aes(x = 0.5, y = 168, xend = nrow(labelName) + 0.5, yend = 168), colour = "white", alpha = 1, size = 166, inherit.aes = T) +
  # Draw outer lines
  geom_segment(data = base_data, aes(x = 0.5, y = 133, xend = 34.5, yend = 133), colour = "#389dc7", alpha = 1, size = 8, inherit.aes = F) +
  geom_segment(data = base_data, aes(x = 34.5, y = 133, xend = 42.5, yend = 133), colour = "#2bc516", alpha = 1, size = 8, inherit.aes = F) +
  # Draw white lines at 50% and 100%
  geom_segment(data = base_data, aes(x = 0.5, y = 50, xend = nrow(labelName) + 0.5, yend = 50), colour = "white", alpha = 1, size = 2.5, inherit.aes = T) +
  geom_segment(data = base_data, aes(x = 0.5, y = 100, xend = nrow(labelName) + 0.5, yend = 100), colour = "white", alpha = 1, size = 2.5, inherit.aes = T) +
  # Create label sticks
  ggtext::geom_richtext(data = stick, aes(x = x, y = y, label = label), fill = "red", angle = 50, alpha = 0.7, color = "white", size = 10, fontface = "bold") +
  # Write label names
  geom_text(data = label_data, aes(x = id, y = 138, label = key, hjust = hjust), colour = c(rep("#389dc7", 34), rep("#2bc516", 8)), fontface = "bold", alpha = 0.8, size = 14, angle = label_data$angle, inherit.aes = F) +
  # Write values at each bar
  geom_text(data = label_data, aes(x = id, y = Indicator + 2, label = paste0(round(Indicator, 0), "% "), hjust = hjust, color = key), fontface = "bold", alpha = 0.9, size = 10, angle = label_data$angle, check_overlap = TRUE, inherit.aes = F) +
  coord_polar() +
  ylim(0, 180) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_line(colour = "gray64"),
    plot.title = element_text(color = "black", size = 100, face = "bold"),
    plot.margin = unit(rep(-1, 4), "cm")
  ) +
  ggtitle("b")


#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' **********************************************************************************************************************************
#' Please Save image at 3200px x 6500  to get high resolution
ggsave("main_manuscript_figures/figure_3.png", synergy / synergy.positive, 
       width = 3200, height = 6500, units = "px", limitsize = FALSE, dpi = 100)
