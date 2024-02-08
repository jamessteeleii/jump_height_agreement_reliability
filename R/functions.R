prepare_data <- function(file_1, file_2) {
  dat1 <- read_csv(file_1) |>
    mutate(session_no = 1) |>
    rename_with(~str_remove(., 'sess1_'))
  
  dat2 <- read_csv(file_2) |>
    mutate(session_no = 2) |>
    rename_with(~str_remove(., 'sess2_'))
  
  dat <- bind_rows(dat1, dat2) |>
    pivot_longer(2:16,
                 names_to = "method",
                 values_to = "jump_height") |>
    separate(method, c("method", "trial", "x")) |>
    select(-x) |>
    filter(method != "jm")
}

fit_lcc_model <- function(data) {
  lcc_model <-  data |>
    mutate(
      method = case_when(
        method == "fdi" ~ "M1",
        method == "jmc" ~ "M2",
        method == "mj" ~ "M3",
        method == "opto" ~ "M4"
      ),
      across(c(ID_num,method), factor)) |> 
    lcc(
      subject = "ID_num",
      resp = "jump_height",
      method = "method",
      time = "session_no",
      interaction = TRUE,
      REML = TRUE,
      ci = TRUE,
      components = TRUE
    )
}