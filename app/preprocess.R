library(tidyverse)
library(plotly)
opt_e1 <- readRDS("app/empirical_data/e1_optimality.RDS")
opt_e2 <- readRDS("app/empirical_data/e2_optimality.RDS")
opt_e3 <- readRDS("app/empirical_data/e3_optimality.RDS")


full_data <- list()

# preprocess e1----
opt_e1 <- opt_e1 %>%
  rename(Rate = Dotdiff) %>%
  mutate(group = interaction(SwitchP, Delay, Placeh, Rate))

opt_e1 <- opt_e1 %>%
  group_by(ID) %>%
  mutate(intercept = lm(CC ~ check_at_opt)$coefficients[1],
         slope = lm(CC ~ check_at_opt)$coefficients[2],
         CC_pred = intercept+check_at_opt*slope)



opt_e1_lines <- opt_e1 %>%
  unnest(temp) %>%
  group_by(ID, Rate, SwitchP, Delay, Placeh) %>%
  mutate(opt_check = earnings[earnings == max(earnings)],
         max_earnings = max(earnings),
         rel_reward = earnings/max_earnings,
         RT_nCC = RT_nCC/1000,
         RT_CC = RT_CC/1000) %>%
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




e1_fig_curves <- ggplot(opt_e1_lines, aes(x = probabilities, y = rel_reward, group = interaction(ID, group), color = Rate, 
                                          text = paste0(
                                            "ID: ", ID, "\n",
                                            "RT not checking: ", round(RT_nCC, 2), "\n",
                                            "RT checking: ", round(RT_CC, 2), "\n",
                                            "Cost: ", round((RT_CC-RT_nCC)/RT_nCC, 2), "\n",
                                            "Optimum: ", round(check_at_opt, 2)
                                          )))+
  geom_line(alpha = 0.5, lwd = 0.2) +
  geom_ribbon(data = e1_dens_slow, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
  geom_ribbon(data = e1_dens_fast, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
  geom_point(data = opt_e1_lines_agg %>% filter(Rate == 0), aes(x = mean_opt, y = 0.1), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
  geom_point(data = opt_e1_lines_agg %>% filter(Rate == 1), aes(x = mean_opt, y = 0.2), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8,
                        labels = c("0" = "fast",
                                   "1" = "slow"), 
                        name = "Rate:") +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8, 
                       labels = c("0" = "fast",
                                  "1" = "slow"), 
                       name = "Rate:") +
  geom_text(data = NULL, aes(x = 0.8, y = max(e1_dens_slow$ymax, na.rm = TRUE) + 0.01), label = "slow", color = "#FE9F6D") +
  geom_text(data = NULL, aes(x = 0.7, y = max(e1_dens_fast$ymin, na.rm = TRUE) - 0.05), label = "fast", color = "#3B0F70") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Possible Check Rates", y = "Optimality") +
  theme_classic()  +
  theme(legend.position = "none")






e1_fig_slopes <- ggplot(opt_e1, aes(x = check_at_opt, y = CC_pred, group = ID, color = ID,
                                    text = paste0(
                                      "ID: ", ID, "\n",
                                      "slope: ", round(slope, 2)
                                    ))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_line() +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Model Optimal Rate", y = "Observed Rate") +
  theme_classic() +
  theme(legend.position = "none")


full_data$e1 <- list(
  curves_data = list(
    lines     = opt_e1_lines,
    dens_fast = e1_dens_fast,
    dens_slow = e1_dens_slow,
    agg       = opt_e1_lines_agg
  ),
  slopes_data = opt_e1
)


# preprocess e2----
opt_e2 <- opt_e2 %>%
  rename(Rate = squarediff) %>%
  mutate(group = interaction(SwitchP, Placeh, Rate))


opt_e2 <- opt_e2 %>%
  group_by(ID) %>%
  mutate(intercept = lm(CC ~ check_at_opt)$coefficients[1],
         slope = lm(CC ~ check_at_opt)$coefficients[2],
         CC_pred = intercept+check_at_opt*slope)


opt_e2_lines <- opt_e2 %>%
  unnest(temp) %>%
  group_by(ID, Rate, SwitchP, Placeh) %>%
  mutate(opt_check = earnings[earnings == max(earnings)],
         max_earnings = max(earnings),
         rel_reward = earnings/max_earnings,
         RT_nCC = RT_nCC/1000,
         RT_CC = RT_CC/1000) %>%
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





e2_fig_curves <- ggplot(opt_e2_lines, aes(x = probabilities, y = rel_reward, group = interaction(ID, group), color = Rate, 
                                          text = paste0(
                                            "ID: ", ID, "\n",
                                            "RT not checking: ", round(RT_nCC, 2), "\n",
                                            "RT checking: ", round(RT_CC, 2), "\n",
                                            "Cost: ", round((RT_CC-RT_nCC)/RT_nCC, 2), "\n",
                                            "Optimum: ", round(check_at_opt, 2)
                                          )))+
  geom_line(alpha = 0.5, lwd = 0.2) +
  geom_ribbon(data = e2_dens_slow, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
  geom_ribbon(data = e2_dens_fast, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
  geom_point(data = opt_e2_lines_agg %>% filter(Rate == 0), aes(x = mean_opt, y = 0.1), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
  geom_point(data = opt_e2_lines_agg %>% filter(Rate == 1), aes(x = mean_opt, y = 0.2), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8,
                        labels = c("0" = "fast",
                                   "1" = "slow"), 
                        name = "Rate:") +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8, 
                       labels = c("0" = "fast",
                                  "1" = "slow"), 
                       name = "Rate:") +
  geom_text(data = NULL, aes(x = 0.8, y = max(e2_dens_slow$ymax, na.rm = TRUE) + 0.01), label = "slow", color = "#FE9F6D") +
  geom_text(data = NULL, aes(x = 0.8, y = max(e2_dens_fast$ymin, na.rm = TRUE) - 0.05), label = "fast", color = "#3B0F70") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Possible Check Rates", y = "Optimality") +
  theme_classic() + 
  theme(legend.position = "none")


e2_fig_slopes <- ggplot(opt_e2, aes(x = check_at_opt, y = CC_pred, group = ID, color = ID,
                                    text = paste0(
                                      "ID: ", ID, "\n",
                                      "slope: ", round(slope, 2)
                                    ))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_line() +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Model Optimal Rate", y = "Observed Rate") +
  theme_classic() +
  theme(legend.position = "none")



full_data$e2 <- list(
  curves_data = list(
    lines     = opt_e2_lines,
    dens_fast = e2_dens_fast,
    dens_slow = e2_dens_slow,
    agg       = opt_e2_lines_agg
  ),
  slopes_data = opt_e2
)


# preprocess e3----
opt_e3 <- opt_e3 %>%
  rename(Rate = ITICond) %>%
  mutate(group = interaction(Placeh, Rate))

opt_e3 <- opt_e3 %>%
  group_by(ID) %>%
  mutate(intercept = lm(CC ~ check_at_opt)$coefficients[1],
         slope = lm(CC ~ check_at_opt)$coefficients[2],
         CC_pred = intercept+check_at_opt*slope)



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


e3_fig_curves <- ggplot(opt_e3_lines, aes(x = probabilities, y = rel_reward, group = interaction(ID, group), color = Rate, 
                                          text = paste0(
                                            "ID: ", ID, "\n",
                                            "RT not checking: ", round(RT_nCC, 2), "\n",
                                            "RT checking: ", round(RT_CC, 2), "\n",
                                            "Cost: ", round((RT_CC-RT_nCC)/RT_nCC, 2), "\n",
                                            "Optimum: ", round(check_at_opt, 2)
                                          )))+
  geom_line(alpha = 0.5, lwd = 0.2) +
  geom_ribbon(data = e3_dens_slow, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
  geom_ribbon(data = e3_dens_fast, aes(x = x, ymin = ymin, ymax = ymax, fill = Rate), inherit.aes = FALSE) +
  geom_point(data = opt_e3_lines_agg %>% filter(Rate == 0), aes(x = mean_opt, y = 0.1), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
  geom_point(data = opt_e3_lines_agg %>% filter(Rate == 1), aes(x = mean_opt, y = 0.2), inherit.aes = FALSE, pch = 23, fill = "white", size = 2) +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8,
                        labels = c("0" = "fast",
                                   "1" = "slow"), 
                        name = "Rate:") +
  scale_fill_viridis_d(option = "magma", begin = 0.2, end = 0.8, 
                       labels = c("0" = "fast",
                                  "1" = "slow"), 
                       name = "Rate:") +
  geom_text(data = NULL, aes(x = 0.7, y = max(e3_dens_slow$ymax, na.rm = TRUE) + 0.01), label = "slow", color = "#FE9F6D") +
  geom_text(data = NULL, aes(x = 0.6, y = max(e3_dens_fast$ymin, na.rm = TRUE) - 0.05), label = "fast", color = "#3B0F70") +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Possible Check Rates", y = "Optimality") +
  theme_classic()  +
  theme(legend.position = "none")


e3_fig_slopes <- ggplot(opt_e3_lines, aes(x = check_at_opt, y = CC_pred, group = ID, color = ID,
                                          text = paste0(
                                            "ID: ", ID, "\n",
                                            "slope: ", round(slope, 2)
                                          ))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  geom_line() +
  scale_color_viridis_d(option = "magma", begin = 0.2, end = 0.8) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Model Optimal Rate", y = "Observed Rate") +
  theme_classic()  +
  theme(legend.position = "none")

full_data$e3 <- list(
  curves_data = list(
    lines     = opt_e3_lines,
    dens_fast = e3_dens_fast,
    dens_slow = e3_dens_slow,
    agg       = opt_e3_lines_agg
  ),
  slopes_data = opt_e3
)

saveRDS(full_data, "app/empirical_data/full_data.RDS")
