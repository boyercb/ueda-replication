
# Create and clean fixed and time-varying covariates ----------------------

analytic_long <- analytic_long %>%
  mutate(
    # female sex (0/1)
    sex = sex - 1,
    
    # educational level (categorical)
    educ_1 = if_else(educ < 12, 1, 0),              # less than high school
    educ_2 = if_else(educ >= 12 & educ < 16, 1, 0), # high school/some college
    educ_3 = if_else(educ == 16, 1, 0),             # bachelor's degree
    educ_4 = if_else(educ > 16, 1, 0),              # postgraduate education
  
    # marital status (categorical)
    marital_1 = if_else(marital == 1, 1, 0),            # single
    marital_2 = if_else(marital == 2, 1, 0),            # married
    marital_3 = if_else(marital %in% c(3, 4, 5), 1, 0), # widowed, divorced, or separated
    
    # ever smoked (0/1)
    eversmk = as.numeric(currsmk1 + currsmk2 + currsmk3 > 0), 
      
    # baseline standard drinks per day (continuous)
    pre_dpd = (beer_week3 + wine_week + liquor_week) / 7,
    
    # baseline standard drinks per day (categorical)
    pre_dpd_1 = if_else(pre_dpd == 0, 1, 0),               # 0 drinks per day
    pre_dpd_2 = if_else(pre_dpd > 0 & pre_dpd < 2, 1, 0),  # 0 to 2 drinks per day
    pre_dpd_3 = if_else(pre_dpd >= 2 & pre_dpd < 4, 1, 0), # 2 to 4 drinks per day
    pre_dpd_4 = if_else(pre_dpd >= 4, 1, 0),               # >4 drinks per day
    
    # baseline cigarettes per day (categorical)
    pre_cpd_1 = if_else(cpd3 == 0, 1, 0),             # None
    pre_cpd_2 = if_else(cpd3 == 1, 1, 0),             # 1 or fewer
    pre_cpd_3 = if_else(cpd3 > 1 & cpd3 < 5, 1, 0),   # 2 to 4 
    pre_cpd_4 = if_else(cpd3 >= 5 & cpd3 < 25, 1, 0), # 5 to 24
    pre_cpd_5 = if_else(cpd3 > 25, 1, 0),             # 25 or more
    
    # standard drinks per day (continuous)
    dpd = case_when(
      exam %in% c(6, 7) ~ (beer_week + white_wine_week + red_wine_week + liquor_week) / 7,
      exam %in% c(4, 5) ~ (beer_week + wine_week + liquor_week) / 7
    ),
    
    # currently drinks (0/1)
    drk = if_else(dpd > 0, 1, 0),
    
    # standard drinks per day (categorical)
    dpd_1 = if_else(dpd == 0, 1, 0),           # 0 drinks per day
    dpd_2 = if_else(dpd > 0 & dpd < 2, 1, 0),  # 0 to 2 drinks per day
    dpd_3 = if_else(dpd >= 2 & dpd < 4, 1, 0), # 2 to 4 drinks per day
    dpd_4 = if_else(dpd >= 4, 1, 0),           # >4 drinks per day
    
    # cigarettes per day (categorical)
    cpd_1 = if_else(cpd == 0, 1, 0),             # None
    cpd_2 = if_else(cpd == 1, 1, 0),             # 1 or fewer
    cpd_3 = if_else(cpd > 1 & cpd < 5, 1, 0),    # 2 to 4 
    cpd_4 = if_else(cpd >= 5 & cpd < 25, 1, 0),  # 5 to 24
    cpd_5 = if_else(cpd > 25, 1, 0)              # 25 or more
  ) %>%
  # rename variables to final covariate names
  rename(
    pre_cpd = cpd3,
    pre_bmi = bmi3,
    pre_sbp = sbp3,
    pre_ldl = calc_ldl3,
    pre_hrx = hrx3,
    pre_liprx = liprx3,
    pre_dm = curr_diab3,
    ldl = calc_ldl,
    dm = curr_diab,
    smk = currsmk,
    agetv = age
  ) %>%
  # add age at 4th examination cycle as baseline age
  left_join(filter(analytic_long, exam == 4) %>% select(age, pid), by = "pid") %>%
  # drop intermediate variables
  select(
    -matches("wine"), 
    -starts_with("beer"), 
    -starts_with("liquor")
  )


# Create and clean outcomes -----------------------------------------------

analytic_long <- analytic_long %>%
  mutate(
    chd2 = if_else(chddate <= date, 1, 0)
  )


# Create censoring indicator ----------------------------------------------



# Handle missing data -----------------------------------------------------

# last observation carry forward
#analytic_long <- mutate_at()