##### Function to prepare data
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

##### Functions to fit agreement and reliability models
fit_agree_model <- function(data) {
  agree_model <-  data |>
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
      REML = TRUE,
      ci = TRUE,
      components = TRUE
    )
  
  agree_model_all <- unnest(tibble((agree_model$Summary.lcc$fitted)),
                            cols = c(`(agree_model$Summary.lcc$fitted)`))
  
  LCC <- unnest(agree_model_all[1:3,],
                cols = c(`(agree_model$Summary.lcc$fitted)`)) |>
    rename("session_no" = Time) |>
    mutate(facet_lab = "Session Number",
           Statistic = "LCC") |>
    rename(Estimate = "LCC")
  
  LPC <- unnest(agree_model_all[4:6,],
                cols = c(`(agree_model$Summary.lcc$fitted)`)) |>
    rename("session_no" = Time) |>
    mutate(facet_lab = "Session Number",
           Statistic = "LPC") |>
    rename(Estimate = "LPC")
  
  LA <- unnest(agree_model_all[7:9,],
               cols = c(`(agree_model$Summary.lcc$fitted)`)) |>
    rename("session_no" = Time) |>
    mutate(facet_lab = "Session Number",
           Statistic = "LA") |>
    rename(Estimate = "LA")
  
  agree_model_results <- bind_rows(LCC, LPC, LA)
  
  return(agree_model_results)
}


fit_reli_models <- function(data) {
  reli_models <- tibble(method = as.character(),
                        CCC = as.numeric(),
                        Lower = as.numeric(),
                        Upper = as.numeric(),
                        SEM = as.numeric())
  
  for(i in unique(data$method)) {
    
    reli_ccc <- data |>
      mutate(
        across(c(ID_num,method), factor)
      ) |> 
      filter(method == i) |>
      cccvc(
        rind = "ID_num",
        ry = "jump_height",
        rmet = "session_no",
      )
    
    reli_models <- rbind(reli_models,
                         tibble(method = i,
                                CCC = reli_ccc$ccc[1],
                                Lower = reli_ccc$ccc[2],
                                Upper = reli_ccc$ccc[3],
                                SEM = sqrt(nlme::intervals(reli_ccc$model)$sigma[2]),
                                SEM_lower = sqrt(nlme::intervals(reli_ccc$model)$sigma[1]),
                                SEM_upper = sqrt(nlme::intervals(reli_ccc$model)$sigma[3])))
  }
  
  return(reli_models)
}

##### Funtions for creating results plots

make_agree_plot <- function(data, agree_model) {
  fdi_jmc_plot <- data |>
    pivot_wider(id_cols = c(ID_num, trial, session_no),
                names_from = "method", 
                values_from = "jump_height") |>
    select(fdi,jmc,session_no) |>
    ggplot(aes(x=fdi, y=jmc)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed") +
    geom_text(data = filter(agree_model, Statistic == "LCC")[1:2,],
              aes(label = glue::glue("rho[CCC] == {round(Estimate,2)}")),
              x = 50,
              y = 35,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(agree_model, Statistic == "LCC")[1:2,],
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 30,
              size = 3
    ) +
    geom_text(data = filter(agree_model, Statistic == "LPC")[1:2,],
              aes(label = glue::glue("rho == {round(Estimate,2)}")),
              x = 50,
              y = 25,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(agree_model, Statistic == "LPC")[1:2,],
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 20,
              size = 3
    ) +
    geom_text(data = filter(agree_model, Statistic == "LA")[1:2,],
              aes(label = glue::glue("rho[C[b]] == {round(Estimate,2)}")),
              x = 50,
              y = 15,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(agree_model, Statistic == "LA")[1:2,],
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 10,
              size = 3
    ) +
    scale_x_continuous(limits = c(10,60)) +
    scale_y_continuous(limits = c(10,60)) +
    labs(
      x = "Force Decks Impulse-Momentum (cm)",
      y = "Jump Matt (cm)"
    ) +
    facet_grid(session_no~.) +
    theme_bw() +
    theme(strip.text = element_blank())
  
  fdi_opto_plot <- data |>
    pivot_wider(id_cols = c(ID_num, trial, session_no),
                names_from = "method", 
                values_from = "jump_height") |>
    select(fdi,opto,session_no) |>
    ggplot(aes(x=fdi, y=opto)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed") +
    geom_text(data = filter(agree_model, Statistic == "LCC")[3:4,],
              aes(label = glue::glue("rho[CCC] == {round(Estimate,2)}")),
              x = 50,
              y = 35,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(agree_model, Statistic == "LCC")[3:4,],
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 30,
              size = 3
    ) +
    geom_text(data = filter(agree_model, Statistic == "LPC")[3:4,],
              aes(label = glue::glue("rho == {round(Estimate,2)}")),
              x = 50,
              y = 25,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(agree_model, Statistic == "LPC")[3:4,],
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 20,
              size = 3
    ) +
    geom_text(data = filter(agree_model, Statistic == "LA")[3:4,],
              aes(label = glue::glue("rho[C[b]] == {round(Estimate,2)}")),
              x = 50,
              y = 15,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(agree_model, Statistic == "LA")[3:4,],
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 10,
              size = 3
    ) +
    scale_x_continuous(limits = c(10,60)) +
    scale_y_continuous(limits = c(10,60)) +
    labs(
      x = "Force Decks Impulse-Momentum (cm)",
      y = "Optojump (cm)"
    ) +
    facet_grid(session_no~.) +
    theme_bw() +
    theme(strip.text = element_blank())
  
  fdi_mj_plot <- data |>
    pivot_wider(id_cols = c(ID_num, trial, session_no),
                names_from = "method", 
                values_from = "jump_height") |>
    select(fdi,mj,session_no) |>
    mutate(facet_lab = "Session Number") |>
    ggplot(aes(x=fdi, y=mj)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed") +
    geom_text(data = filter(agree_model, Statistic == "LCC")[5:6,],
              aes(label = glue::glue("rho[CCC] == {round(Estimate,2)}")),
              x = 50,
              y = 35,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(agree_model, Statistic == "LCC")[5:6,],
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 30,
              size = 3
    ) +
    geom_text(data = filter(agree_model, Statistic == "LPC")[5:6,],
              aes(label = glue::glue("rho == {round(Estimate,2)}")),
              x = 50,
              y = 25,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(agree_model, Statistic == "LPC")[5:6,],
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 20,
              size = 3
    ) +
    geom_text(data = filter(agree_model, Statistic == "LA")[5:6,],
              aes(label = glue::glue("rho[C[b]] == {round(Estimate,2)}")),
              x = 50,
              y = 15,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(agree_model, Statistic == "LA")[5:6,],
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 10,
              size = 3
    ) +
    scale_x_continuous(limits = c(10,60)) +
    scale_y_continuous(limits = c(10,60)) +
    labs(
      x = "Force Decks Impulse-Momentum (cm)",
      y = "MyJump App (cm)"
    ) +
    ggh4x::facet_nested(facet_lab + session_no~.) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12))
  
  
  fdi_jmc_plot + fdi_opto_plot + fdi_mj_plot +
    plot_layout(axes = "collect") +
    plot_annotation(title = "Agreement Between Methods",
                    subtitle = expression(paste("Concordance Correlation Coefficients (", rho[CCC], "), Pearson's Correlation (", rho, "), and Bias Correction (", rho[C[b]], ") with Interval Estimates")),
                    caption = "Each compared to gold standard (Force Decks Impulse-Momentum)")
  
}

make_reli_plot <- function(data, reli_models) {
  fdi_reli_plot <- data |>
    filter(method == "fdi") |>
    pivot_wider(id_cols = c(ID_num, trial),
                names_from = "session_no", 
                values_from = "jump_height") |>
    ggplot(aes(x=`1`, y=`2`)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed") +
    geom_text(data = filter(reli_models, method == "fdi"),
              aes(label = glue::glue("SEM = {round(SEM,2)} cm")),
              x = 50,
              y = 25,
              size = 3
    ) +
    geom_text(data = filter(reli_models, method == "fdi"),
              aes(label = glue::glue("[95%CI: {round(SEM_lower,2)}, {round(SEM_upper,2)}]")),
              x = 50,
              y = 22.5,
              size = 3
    ) +
    geom_text(data = filter(reli_models, method == "fdi"),
              aes(label = glue::glue("rho[CCC] == {round(CCC,2)}")),
              x = 50,
              y = 20,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(reli_models, method == "fdi"),
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 17.5,
              size = 3
    ) +
    scale_x_continuous(limits = c(10,60)) +
    scale_y_continuous(limits = c(10,60)) +
    labs(
      x = "Session 1",
      y = "Session 2",
      title = "Force Decks Impulse Momementum"
    ) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12))
  
  jmc_reli_plot <- data |>
    filter(method == "jmc") |>
    pivot_wider(id_cols = c(ID_num, trial),
                names_from = "session_no", 
                values_from = "jump_height") |>
    ggplot(aes(x=`1`, y=`2`)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed") +
    geom_text(data = filter(reli_models, method == "jmc"),
              aes(label = glue::glue("SEM = {round(SEM,2)} cm")),
              x = 50,
              y = 25,
              size = 3
    ) +
    geom_text(data = filter(reli_models, method == "jmc"),
              aes(label = glue::glue("[95%CI: {round(SEM_lower,2)}, {round(SEM_upper,2)}]")),
              x = 50,
              y = 22.5,
              size = 3
    ) +
    geom_text(data = filter(reli_models, method == "jmc"),
              aes(label = glue::glue("rho[CCC] == {round(CCC,2)}")),
              x = 50,
              y = 20,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(reli_models, method == "jmc"),
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 17.5,
              size = 3
    ) +
    scale_x_continuous(limits = c(10,60)) +
    scale_y_continuous(limits = c(10,60)) +
    labs(
      x = "Session 1",
      y = "Session 2",
      title = "Jump Mat"
    ) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12))
  
  opto_reli_plot <- data |>
    filter(method == "opto") |>
    pivot_wider(id_cols = c(ID_num, trial),
                names_from = "session_no", 
                values_from = "jump_height") |>
    ggplot(aes(x=`1`, y=`2`)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed") +
    geom_text(data = filter(reli_models, method == "opto"),
              aes(label = glue::glue("SEM = {round(SEM,2)} cm")),
              x = 50,
              y = 25,
              size = 3
    ) +
    geom_text(data = filter(reli_models, method == "opto"),
              aes(label = glue::glue("[95%CI: {round(SEM_lower,2)}, {round(SEM_upper,2)}]")),
              x = 50,
              y = 22.5,
              size = 3
    ) +
    geom_text(data = filter(reli_models, method == "opto"),
              aes(label = glue::glue("rho[CCC] == {round(CCC,2)}")),
              x = 50,
              y = 20,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(reli_models, method == "opto"),
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 17.5,
              size = 3
    ) +
    scale_x_continuous(limits = c(10,60)) +
    scale_y_continuous(limits = c(10,60)) +
    labs(
      x = "Session 1",
      y = "Session 2",
      title = "Optojump"
    ) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12))
  
  mj_reli_plot <- data |>
    filter(method == "mj") |>
    pivot_wider(id_cols = c(ID_num, trial),
                names_from = "session_no", 
                values_from = "jump_height") |>
    ggplot(aes(x=`1`, y=`2`)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed") +
    geom_text(data = filter(reli_models, method == "mj"),
              aes(label = glue::glue("SEM = {round(SEM,2)} cm")),
              x = 50,
              y = 25,
              size = 3
    ) +
    geom_text(data = filter(reli_models, method == "mj"),
              aes(label = glue::glue("[95%CI: {round(SEM_lower,2)}, {round(SEM_upper,2)}]")),
              x = 50,
              y = 22.5,
              size = 3
    ) +
    geom_text(data = filter(reli_models, method == "mj"),
              aes(label = glue::glue("rho[CCC] == {round(CCC,2)}")),
              x = 50,
              y = 20,
              size = 3,
              parse = TRUE
    ) +
    geom_text(data = filter(reli_models, method == "mj"),
              aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
              x = 50,
              y = 17.5,
              size = 3
    ) +
    scale_x_continuous(limits = c(10,60)) +
    scale_y_continuous(limits = c(10,60)) +
    labs(
      x = "Session 1",
      y = "Session 2",
      title = "MyJump App"
    ) +
    theme_bw() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12))
  
  
  fdi_reli_plot + jmc_reli_plot + opto_reli_plot + mj_reli_plot +
    plot_layout(axes = "collect") +
    plot_annotation(title = "Reliability Across Sessions",
                    subtitle = expression(paste("Standard Error of Measurement (SEM) and Concordance Correlation Coefficients (", rho[CCC], ") with Interval Estimates")),
                    theme = theme(plot.subtitle = element_text(size = 10)))
    
  
  
} 

make_plot_tiff <- function(plot, path, width, height, device, dpi) {
  
  ggsave(filename = path, plot = plot, width = width, height = height, device = device, dpi = dpi)
  
}
