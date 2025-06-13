################################################################################
# MODULE 4 - Leve-Voigt-Park Detour Index vs. Population Analysis
# AUTHOR: BENJAMIN LABOHM, BERLIN, 2022
################################################################################

# Load required libraries ----
library(ggplot2)
library(sf)
library(dplyr)
library(RColorBrewer)
library(ggspatial)
library(cowplot)

# Define working directories ----
DRIVE <- "D:/"
github_dir <- file.path(DRIVE, "MA")
working_dir <- file.path(DRIVE, "output/DE008")

# Define file paths ----
di_values_path <- file.path(working_dir, "scenarios", "di_values.gpkg")
output_plot_path <- file.path(github_dir, "plots", "3-2c_di_pop_lvp.pdf")

#' Prepare Population Cumulative Share by DI
#'
#' Loads DI values with associated population, computes cumulative
#' population percentage grouped by DI.
#'
#' @param di_values_path Character. Path to GPKG file with DI and population.
#' @return Data frame with `di` and relative cumulative population (`pop_rel`).
prepare_di_population_data <- function(di_values_path) {
  read_sf(di_values_path) %>%
    st_drop_geometry() %>%
    arrange(di) %>%
    transmute(
      pop_cum = cumsum(population),
      pop_sum = sum(population),
      pop_rel = (pop_cum / pop_sum) * 100,
      di = round(di, 2)
    ) %>%
    group_by(di) %>%
    summarise(pop_rel = mean(pop_rel), .groups = "drop")
}

#' Plot DI vs Population Cumulative Share
#'
#' @param data Data frame. Output from `prepare_di_population_data`.
#' @param highlight_di Numeric. DI value to highlight with red dashed lines.
#' @return ggplot object.
plot_di_vs_population <- function(data, highlight_di = 0.8) {
  yint <- data %>%
    filter(di == highlight_di) %>%
    pull(pop_rel) %>%
    max()

  ggplot(data, aes(x = di, y = pop_rel)) +
    geom_line() +
    geom_segment(aes(x = highlight_di, y = yint, xend = 0.4, yend = yint),
                 color = "red", linetype = "dashed") +
    geom_segment(aes(x = highlight_di, y = yint, xend = highlight_di, yend = 0),
                 color = "red", linetype = "dashed") +
    xlab("Detour Index (DI)") +
    ylab("Cumulative Population [%]") +
    xlim(0.4, 1)
}

#' Save Plot to File
#'
#' @param plot ggplot object.
#' @param output_path Character. Output file path.
#' @param width Numeric. Width of the saved plot in inches.
#' @param height Numeric. Height of the saved plot in inches.
save_di_plot <- function(plot, output_path, width = 11.69 / 2.5, height = 11.69 / 4.5) {
  ggsave(filename = output_path, plot = plot, width = width, height = height)
}

# Execute workflow ----
di_data <- prepare_di_population_data(di_values_path)
di_plot <- plot_di_vs_population(di_data, highlight_di = 0.8)
save_di_plot(di_plot, output_plot_path)
