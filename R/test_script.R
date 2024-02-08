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
  interaction = TRUE,
  REML = TRUE,
  ci = TRUE,
  components = TRUE
)

summary(dat_lcc, type = "lcc")

plot(dat_lcc)

df_lcc <- unnest(as.tibble((dat_lcc$Summary.lcc$fitted))) |>
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
            aes(label = glue::glue("[95%CI: {round(Upper,2)}, {round(Lower,2)}]")),
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
            aes(label = glue::glue("[95%CI: {round(Upper,2)}, {round(Lower,2)}]")),
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
            aes(label = glue::glue("[95%CI: {round(Upper,2)}, {round(Lower,2)}]")),
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
  plot_annotation(title = "Observed Jump Heights",
                  subtitle = "Concordance Correlation Coefficients with Interval Estimates",
                  caption = "Each compared to gold standard (Force Decks Impulse-Momentum)")




# reliability

rel_fdi_lcc <-  dat |>
  mutate(
    across(c(ID_num,method), factor)
  ) |> 
  filter(method == "fdi") |>
  cccrm::cccvc(
    rind = "ID_num",
    ry = "jump_height",
    rmet = "session_no",
    int = TRUE
    )

sqrt(rel_fdi_lcc$model$sigma)




