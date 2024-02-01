# _targets.R file
library(targets)
library(tarchetypes)
source("R/functions.R")
tar_option_set(packages = c("tidyverse", "rstan", "brms", "base", "bayesplot",
                            "tidybayes", "broom.mixed", "quarto",
                            "kableExtra", "patchwork"))
list(
  ##### Repeated Sprint Trials
  # Load in data
  tar_target(rsa_file, "rsa_data.csv", format = "file"),
  tar_target(rsa_data, get_rsa_data(rsa_file)),
  
)