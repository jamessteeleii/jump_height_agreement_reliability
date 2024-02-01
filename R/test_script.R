library(tidyverse)

dat1 <- read_csv("data/data_collection_s1.csv") |>
  mutate(session_no = 1) |>
  rename_with(~str_remove(., 'sess1_'))

dat2 <- read_csv("data/data_collection_s2.csv") |>
  mutate(session_no = 2) |>
  rename_with(~str_remove(., 'sess2_'))

dat <- bind_rows(dat1, dat2) |>
  pivot_longer(2:16,
               names_to = "method",
               values_to = "jump_height") |>
  separate(method, c("method", "trial", "x")) |>
  select(-x) |>
  mutate(
         method = case_when(
           method == "fdi" ~ "M1",
           method == "jmc" ~ "M2",
           method == "mj" ~ "M3",
           method == "opto" ~ "M4"
         ),
         across(c(ID_num,method), factor)) |>
  filter(method != "jm")



# see https://peerj.com/articles/9850.pdf

dat_lcc <- lcc::lcc(
  data = dat,
  subject = "ID_num",
  resp = "jump_height",
  method = "method",
  time = "session_no",
  interaction = TRUE,
  REML = TRUE,
  ci = TRUE
)

lcc::lccPlot(dat_lcc)
