# _targets.R file
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "lcc", "quarto",
                            "kableExtra", "patchwork"))
list(
  # Load in data
  tar_target(file_1, "data/data_collection_s1.csv", format = "file"),
  tar_target(file_2, "data/data_collection_s2.csv", format = "file"),
  tar_target(data, prepare_data(file_1,file_2)),
  
  # Fit agreement (lcc) and reliability models
  tar_target(lcc_model, fit_lcc_model(data))
  
)