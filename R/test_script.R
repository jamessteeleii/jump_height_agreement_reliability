library(tidyverse)
library(patchwork)
library(glue)

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
  filter(method != "jm")




# see https://peerj.com/articles/9850.pdf

dat_lcc <-  dat |>
  mutate(
    method = case_when(
      method == "fdi" ~ "M1",
      method == "jmc" ~ "M2",
      method == "mj" ~ "M3",
      method == "opto" ~ "M4"
    ),
    across(c(ID_num,method), factor)
    ) |> 
  lcc::lcc(
  # data = dat,
  subject = "ID_num",
  resp = "jump_height",
  method = "method",
  time = "session_no",
  # interaction = TRUE,
  REML = TRUE,
  ci = TRUE
)

summary(dat_lcc, type = "lcc")

plot(dat_lcc)

df_lcc <- unnest(tibble((dat_lcc$Summary.lcc$fitted)),
                 cols = c(`(dat_lcc$Summary.lcc$fitted)`)) |>
  rename("session_no" = Time) |>
  mutate(facet_lab = "Session Number")


fdi_jmc_plot <- dat |>
  pivot_wider(id_cols = c(ID_num, trial, session_no),
              names_from = "method", 
              values_from = "jump_height") |>
  select(fdi,jmc,session_no) |>
  ggplot(aes(x=fdi, y=jmc)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed") +
  geom_text(data = df_lcc[1:2,],
            aes(label = glue::glue("rho[CCC] == {round(LCC,2)}")),
            x = 50,
            y = 20,
            size = 3,
            parse = TRUE
            ) +
  geom_text(data = df_lcc[1:2,],
            aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
            x = 50,
            y = 15,
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

fdi_opto_plot <- dat |>
  pivot_wider(id_cols = c(ID_num, trial, session_no),
              names_from = "method", 
              values_from = "jump_height") |>
  select(fdi,opto,session_no) |>
  ggplot(aes(x=fdi, y=opto)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed") +
  geom_text(data = df_lcc[3:4,],
            aes(label = glue::glue("rho[CCC] == {round(LCC,2)}")),
            x = 50,
            y = 20,
            size = 3,
            parse = TRUE
  ) +
  geom_text(data = df_lcc[3:4,],
            aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
            x = 50,
            y = 15,
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

fdi_mj_plot <- dat |>
  pivot_wider(id_cols = c(ID_num, trial, session_no),
              names_from = "method", 
              values_from = "jump_height") |>
  select(fdi,mj,session_no) |>
  mutate(facet_lab = "Session Number") |>
  ggplot(aes(x=fdi, y=mj)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed") +
  geom_text(data = df_lcc[5:6,],
            aes(label = glue::glue("rho[CCC] == {round(LCC,2)}")),
            x = 50,
            y = 20,
            size = 3,
            parse = TRUE
  ) +
  geom_text(data = df_lcc[5:6,],
            aes(label = glue::glue("[95%CI: {round(Lower,2)}, {round(Upper,2)}]")),
            x = 50,
            y = 15,
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
                  subtitle = "Concordance Correlation Coefficients with Interval Estimates",
                  caption = "Each compared to gold standard (Force Decks Impulse-Momentum)")




# reliability

# https://www.researchgate.net/publication/231860714_Estimation_of_the_concordance_correlation_coefficient_for_repeated_measures_using_SAS_and_R

reli_models <- tibble(method = as.character(),
                      CCC = as.numeric(),
                      Lower = as.numeric(),
                      Upper = as.numeric(),
                      SEM = as.numeric())

for(i in unique(dat$method)) {
  
  reli_ccc <- dat |>
    mutate(
      across(c(ID_num,method), factor)
    ) |> 
    filter(method == i) |>
    cccrm::cccvc(
      rind = "ID_num",
      ry = "jump_height",
      rmet = "session_no",
      # int = TRUE
    )
  
  reli_models <- rbind(reli_models,
                       tibble(method = i,
                              CCC = reli_ccc$ccc[1],
                              Lower = reli_ccc$ccc[2],
                              Upper = reli_ccc$ccc[3],
                              SEM = sqrt(reli_ccc$model$sigma)))
}


fdi_reli_plot <- dat |>
  filter(method == "fdi") |>
  pivot_wider(id_cols = c(ID_num, trial),
              names_from = "session_no", 
              values_from = "jump_height") |>
  ggplot(aes(x=`1`, y=`2`)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed") +
  geom_text(data = filter(reli_models, method == "fdi"),
            aes(label = glue::glue("SEM = {round(SEM,2)}")),
            x = 50,
            y = 25,
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
            y = 15,
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

jmc_reli_plot <- dat |>
  filter(method == "jmc") |>
  pivot_wider(id_cols = c(ID_num, trial),
              names_from = "session_no", 
              values_from = "jump_height") |>
  ggplot(aes(x=`1`, y=`2`)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed") +
  geom_text(data = filter(reli_models, method == "jmc"),
            aes(label = glue::glue("SEM = {round(SEM,2)}")),
            x = 50,
            y = 25,
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
            y = 15,
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

opto_reli_plot <- dat |>
  filter(method == "opto") |>
  pivot_wider(id_cols = c(ID_num, trial),
              names_from = "session_no", 
              values_from = "jump_height") |>
  ggplot(aes(x=`1`, y=`2`)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed") +
  geom_text(data = filter(reli_models, method == "opto"),
            aes(label = glue::glue("SEM = {round(SEM,2)}")),
            x = 50,
            y = 25,
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
            y = 15,
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

mj_reli_plot <- dat |>
  filter(method == "mj") |>
  pivot_wider(id_cols = c(ID_num, trial),
              names_from = "session_no", 
              values_from = "jump_height") |>
  ggplot(aes(x=`1`, y=`2`)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1,
              linetype = "dashed") +
  geom_text(data = filter(reli_models, method == "mj"),
            aes(label = glue::glue("SEM = {round(SEM,2)}")),
            x = 50,
            y = 25,
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
            y = 15,
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
                  subtitle = "Standard Error of Measurement and Concordance Correlation Coefficients with Interval Estimates")


