analytic_long %>%
  select(covs_tv, time) %>%
  group_by(time) %>%
  summarise_all(mean, na.rm = T) %>%
  pivot_longer(-time) %>%
  pivot_wider(names_from = "time", names_prefix = "exam_")