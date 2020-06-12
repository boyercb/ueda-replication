
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
    pre_dpd = (beer_week3 + wine_week3 + liquor_week3) / 7,
    
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
    smk = currsmk
  ) %>%
  # drop intermediate variables
  select(
    -matches("wine"), 
    -starts_with("beer"), 
    -starts_with("liquor"),
    -matches(".*[a-z][1-3]"),
    -bdate
  )


# Fix all time variables to start of follow up ----------------------------

analytic_long <-
  analytic_long %>%
  mutate_at(date_vars, function(x) (x - analytic_long$date0))

analytic_long <-
  analytic_long %>%
  group_by(pid) %>%
  mutate(
    edate = case_when(
      exam %in% c(4,5,6) ~ lead(date) - 1,
      exam == 7 ~ date + 4 * 365.25
    ),
    datedth = if_else(is.na(datedth), lastcon, datedth)
  ) %>%
  ungroup()


# Censor individuals at the appropriate time ------------------------------

analytic_long <- 
  analytic_long %>%
  mutate(
    drop = case_when(
      # if you died or got CHD prior to interval then censor by dropping
      # subsequent exams
      datedth <= date | chddate <= date ~ 1, 
      
      # if you died or got CHD during this interval then keep 
      (date <= datedth & datedth <= edate) | (date <= chddate & chddate <= edate) ~ 0,
      
      # always keep exam 4
      exam == 4 ~ 0,
      
      # if you completed all exams then keep 
      fupat == "1_1_1_1" ~ 0,
      
      # if you have two or more consecutive misses drop after possibly carrying one forward
      fupat == "1_0_0_0" & exam > 5 ~ 1,
      fupat == "1_1_0_0" & exam == 7 ~ 1,
      fupat == "1_0_0_1" & exam > 5 ~ 1,
      
      # pattern: 1 0 0 0
      # if you died or got CHD after 4th exam, censor by dropping exams 5, 6, and 7
      fupat == "1_0_0_0" & (datedth <= 1461 | chddate <= 1461) & exam == 5 ~ 1,
      # if you didn't die or get CHD after 4th exam, carry forward one and then censor
      fupat == "1_0_0_0" & !(datedth <= 1461 | chddate <= 1461) & exam == 5 ~ 0,

      # pattern: 1 1 0 0
      # if you died or got CHD after 5th exam, censor by dropping exams 6 and 7
      fupat == "1_1_0_0" & (datedth <= 2922 | chddate <= 2922) & exam == 6 ~ 1, 
      fupat == "1_1_0_0" & (datedth <= 2922 | chddate <= 2922) & exam == 5 ~ 0, 
      # if you didn't die or get CHD after 5th exam, carry forward one and then censor
      fupat == "1_1_0_0" & !(datedth <= 2922 | chddate <= 2922) & exam == 6 ~ 0,
      fupat == "1_1_0_0" & !(datedth <= 2922 | chddate <= 2922) & exam == 5 ~ 0, 
      
      # pattern: 1 1 1 0
      fupat == "1_1_1_0" & exam == 5 ~ 1,
      # if you died or got CHD after 6th exam, censor by dropping exam 7
      fupat == "1_1_1_0" & (datedth <= 4383 | chddate <= 4383) & exam == 7 ~ 1, 
      fupat == "1_1_1_0" & (datedth <= 4383 | chddate <= 4383) & exam == 6 ~ 0, 
      # if you didn't die or get CHD after 6th exam, carry forward one and then censor
      fupat == "1_1_1_0" & !(datedth <= 4383 | chddate <= 4383) & exam == 7 ~ 0, 
      fupat == "1_1_1_0" & !(datedth <= 4383 | chddate <= 4383) & exam == 6 ~ 0, 
      
      # pattern: 1 0 0 1
      # if you died or got CHD after 4th exam, censor by dropping exams 5, 6, and 7
      fupat == "1_0_0_1" & (datedth <= 1461 | chddate <= 1461) & exam == 5 ~ 1,
      # if you didn't die or get CHD after 4th exam, carry forward one and then censor
      fupat == "1_0_0_1" & !(datedth <= 1461 | chddate <= 1461) & exam == 5 ~ 0,

      # pattern: 1 0 1 0
      # if you died or got CHD after 4th exam, censor by dropping exams 5, 6, and 7
      fupat == "1_0_1_0" & (datedth <= 1461 | chddate <= 1461) & exam > 4 ~ 1,
      fupat == "1_0_1_0" & !(datedth <= 1461 | chddate <= 1461) & exam == 5 ~ 0,
      # if you died or got CHD after 5th exam, censor by dropping exams 6 and 7
      fupat == "1_0_1_0" & (datedth <= 2922 | chddate <= 2922) & exam > 5 ~ 1, # should already be covered
      fupat == "1_0_1_0" & (datedth <= 2922 | chddate <= 2922) & exam == 5 ~ 0, 
      fupat == "1_0_1_0" & !(datedth <= 2922 | chddate <= 2922) & exam == 6 ~ 0, 
      # if you died or got CHD during 6th exam, drop 7th exam
      fupat == "1_0_1_0" &
        ((date <= datedth & datedth <= edate) | (date <= chddate & chddate <= edate)) & 
        exam == 7 ~ 1,
      fupat == "1_0_1_0" &
        ((date <= datedth & datedth <= edate) | (date <= chddate & chddate <= edate)) & 
        exam == 6 ~ 0,
      # otherwise leave it
      fupat == "1_0_1_0" &
        !((date <= datedth & datedth <= edate) | (date <= chddate & chddate <= edate)) & 
        exam == 7 ~ 0,
      fupat == "1_0_1_0" &
        !((date <= datedth & datedth <= edate) | (date <= chddate & chddate <= edate)) & 
        exam == 6 ~ 0,
      # if exam 7 still not assigned at this point means alive and CHD-free at exam 7
      fupat == "1_0_1_0" & exam == 7 ~ 0,
      
      # pattern: 1 0 1 1
      # if you died or got CHD after 4th exam, censor by dropping exam 5 (6 and 7 will already be taken care of)
      fupat == "1_0_1_1" & (datedth <= 1461 | chddate <= 1461) & exam == 5 ~ 1,
      fupat == "1_0_1_1" & !(datedth <= 1461 | chddate <= 1461) & exam == 5 ~ 0,
      fupat == "1_0_1_1" & exam %in% c(6, 7) ~ 0,
      
      # pattern: 1 1 0 1
      # if you died or got CHD after 5th exam, censor by dropping exam 6 (7 will already be taken care of)
      fupat == "1_1_0_1" & (datedth <= 2922 | chddate <= 2922) & exam == 6 ~ 1,
      fupat == "1_1_0_1" & !(datedth <= 2922 | chddate <= 2922) & exam == 6 ~ 0,
      fupat == "1_1_0_1" & exam %in% c(5, 6, 7) ~ 0,
    )
  )

# censor by dropping censored observations 
analytic_long <- filter(analytic_long, drop == 0)


# Handle missing data -----------------------------------------------------

# last observation carry forward
#analytic_long <- mutate_at()