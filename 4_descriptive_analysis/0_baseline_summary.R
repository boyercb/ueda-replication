analytic_long %>%
  select(covs_fixed) %>%
  summarise_all(mean, na.rm = T) %>%
  pivot_longer(everything()) 
