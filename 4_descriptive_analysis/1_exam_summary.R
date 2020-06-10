analytic_long %>%
  select(covs_tv) %>%
  group_by(exam) %>%
  summarise_all(mean, na.rm = T) %>%
  pivot_longer(-exam) %>%
  pivot_wider(names_from = "exam", names_prefix = "exam_")