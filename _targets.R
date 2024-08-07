# _targets.R file
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "lme4", "lmeresampler",
                            "quarto", "here",
                            "patchwork"))

list(
  # Load in data
  tar_target(file_1, "data/data_collection_s1.csv", format = "file"),
  tar_target(file_2, "data/data_collection_s2.csv", format = "file"),
  tar_target(data, prepare_data(file_1, file_2)),
  
  # Fit agreement and reliability models
  tar_target(agree_models, fit_agree_models(data)),
  tar_target(reli_models, fit_reli_models(data)),
  
  # Plots
  tar_target(agree_plot, make_agree_plot(data, agree_models)),
  tar_target(reli_plot, make_reli_plot(data, reli_models)),
  tar_target(agree_plot_tiff, make_plot_tiff(
    agree_plot, "plots/agree_plot.tiff", 12.5, 5, "tiff", 300
  )),
  tar_target(reli_plot_tiff, make_plot_tiff(
    reli_plot, "plots/reli_plot.tiff", 10, 10, "tiff", 300
  )),
  
  # Analysis and Results write up
  tar_quarto(report, path = "report.qmd"),
  tar_quarto(pre_print, path = "pre_print/pre_print.qmd")
  
)