library(tidyverse)
opt_e1 <- readRDS("app/empirical_data/e1_optimality.RDS")
opt_e2 <- readRDS("app/empirical_data/e2_optimality.RDS")
opt_e3 <- readRDS("app/empirical_data/e3_optimality.RDS")


full_data <- list()

# preprocess e1----
opt_e1 <- opt_e1 %>%
  rename(Rate = Dotdiff) %>%
  mutate(group = interaction(SwitchP, Delay, Placeh, Rate))

opt_e1_lines <- opt_e1 %>%
  unnest(temp) %>%
  group_by(ID, Rate, SwitchP, Delay, Placeh) %>%
  mutate(opt_check = earnings[earnings == max(earnings)],
         max_earnings = max(earnings),
         rel_reward = earnings/max_earnings) %>%
  ungroup()


# calculate densities for each condition
dens_fast <- density((opt_e1_lines %>% filter(rel_reward == 1, Rate == 0))$probabilities)
dens_slow <- density((opt_e1_lines %>% filter(rel_reward == 1, Rate == 1))$probabilities)


e1_dens_fast <- data.frame(x = dens_fast$x, 
                      ymin = 0.1 - (dens_fast$y / max(dens_fast$y) * 0.05),
                      ymax = 0.1 + (dens_fast$y / max(dens_fast$y) * 0.05),
                      Rate = "0")

e1_dens_slow <- data.frame(x = dens_slow$x, 
                      ymin = 0.2 - (dens_slow$y / max(dens_slow$y) * 0.05),
                      ymax = 0.2 + (dens_slow$y / max(dens_slow$y) * 0.05),
                      Rate = "1")


opt_e1_lines_agg <- opt_e1_lines %>%
  filter(rel_reward == 1) %>%
  group_by(ID, Rate) %>%
  summarize(mean_opt = mean(check_at_opt), .groups = "drop") %>%
  group_by(Rate) %>%
  summarize(mean_opt = mean(mean_opt), .groups = "drop")
 
full_data$e1 <- list(reg = opt_e1,
                     lines = opt_e1_lines,
                     dens = list(fast = e1_dens_fast,
                                 slow = e1_dens_slow),
                     agg = opt_e1_lines_agg)

# preprocess e2----
opt_e2 <- opt_e2 %>%
  rename(Rate = squarediff) %>%
  mutate(group = interaction(SwitchP, Placeh, Rate))

opt_e2_lines <- opt_e2 %>%
  unnest(temp) %>%
  group_by(ID, Rate, SwitchP, Placeh) %>%
  mutate(opt_check = earnings[earnings == max(earnings)],
         max_earnings = max(earnings),
         rel_reward = earnings/max_earnings) %>%
  ungroup()

# calculate densities for each condition
dens_fast <- density((opt_e2_lines %>% filter(rel_reward == 1, Rate == 0))$probabilities)
dens_slow <- density((opt_e2_lines %>% filter(rel_reward == 1, Rate == 1))$probabilities)


e2_dens_fast <- data.frame(x = dens_fast$x, 
                           ymin = 0.1 - (dens_fast$y / max(dens_fast$y) * 0.05),
                           ymax = 0.1 + (dens_fast$y / max(dens_fast$y) * 0.05),
                           Rate = "0")

e2_dens_slow <- data.frame(x = dens_slow$x, 
                           ymin = 0.2 - (dens_slow$y / max(dens_slow$y) * 0.05),
                           ymax = 0.2 + (dens_slow$y / max(dens_slow$y) * 0.05),
                           Rate = "1")


opt_e2_lines_agg <- opt_e2_lines %>%
  filter(rel_reward == 1) %>%
  group_by(ID, Rate) %>%
  summarize(mean_opt = mean(check_at_opt), .groups = "drop") %>%
  group_by(Rate) %>%
  summarize(mean_opt = mean(mean_opt), .groups = "drop")

full_data$e2 <- list(reg = opt_e2,
                     lines = opt_e2_lines,
                     dens = list(fast = e2_dens_fast,
                                 slow = e2_dens_slow),
                     agg = opt_e2_lines_agg)

# preprocess e3----
opt_e3 <- opt_e3 %>%
  rename(Rate = ITICond) %>%
  mutate(group = interaction(Placeh, Rate))

opt_e3_lines <- opt_e3 %>%
  unnest(temp) %>%
  group_by(ID, Rate, Placeh) %>%
  mutate(opt_check = earnings[earnings == max(earnings)],
         max_earnings = max(earnings),
         rel_reward = earnings/max_earnings) %>%
  ungroup()


# calculate densities for each condition
dens_fast <- density((opt_e3_lines %>% filter(rel_reward == 1, Rate == 0))$probabilities)
dens_slow <- density((opt_e3_lines %>% filter(rel_reward == 1, Rate == 1))$probabilities)


e3_dens_fast <- data.frame(x = dens_fast$x, 
                           ymin = 0.1 - (dens_fast$y / max(dens_fast$y) * 0.05),
                           ymax = 0.1 + (dens_fast$y / max(dens_fast$y) * 0.05),
                           Rate = "0")

e3_dens_slow <- data.frame(x = dens_slow$x, 
                           ymin = 0.2 - (dens_slow$y / max(dens_slow$y) * 0.05),
                           ymax = 0.2 + (dens_slow$y / max(dens_slow$y) * 0.05),
                           Rate = "1")

opt_e3_lines_agg <- opt_e3_lines %>%
  filter(rel_reward == 1) %>%
  group_by(ID, Rate) %>%
  summarize(mean_opt = mean(check_at_opt), .groups = "drop") %>%
  group_by(Rate) %>%
  summarize(mean_opt = mean(mean_opt), .groups = "drop")

full_data$e3 <- list(reg = opt_e3,
                     lines = opt_e3_lines,
                     dens = list(fast = e3_dens_fast,
                                 slow = e3_dens_slow),
                     agg = opt_e3_lines_agg)

saveRDS(full_data, "app/empirical_data/full_data.RDS")
