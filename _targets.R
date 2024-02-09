# _targets.R file
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "lcc", "cccrm",
                            # "quarto", "kableExtra", 
                            "patchwork"))

list(
  # Load in data
  tar_target(file_1, "data/data_collection_s1.csv", format = "file"),
  tar_target(file_2, "data/data_collection_s2.csv", format = "file"),
  tar_target(data, prepare_data(file_1, file_2)),
  
  # Fit agreement and reliability models
  tar_target(agree_model, fit_agree_model(data)),
  tar_target(reli_models, fit_reli_models(data)),
  
  # Plots
  tar_target(agree_plot, make_agree_plot(data, agree_model)),
  tar_target(reli_plot, make_reli_plot(data, reli_models)),
  tar_target(agree_plot_tiff, make_plot_tiff(
    agree_plot, "plots/agree_plot.tiff", 10, 5, "tiff", 600
  )),
  tar_target(reli_plot_tiff, make_plot_tiff(
    reli_plot, "plots/reli_plot.tiff", 7.5, 7.5, "tiff", 600
  ))
)