
# Define interventions ----------------------------------------------------

intvars <- list("ldl", "ldl", "ldl", "ldl")

interventions <- list(
  list(c(threshold, -Inf, 130)),
  list(c(threshold, 130, 160)), 
  list(c(threshold, 160, 190)),
  list(c(threshold, 190, Inf))
)

int_descript <- c('low', 'moderate', 'high', 'very high')

# Define covariate models -------------------------------------------------

covtypes <- c(
  "binary",
  "normal",
  "binary",
  "normal",
  "normal",
  "absorbing",
  "normal",
  "normal",
  "binary"
)

restrictions <- list(
  c('cpd', 'smk == 1 & cpd > 0', simple_restriction, 0),
  c('dpd', 'drk == 1 & dpd > 0', simple_restriction, 0)
)

covparams <- list(
  covmodels = c(
    # logit model for probability of smoking 
    smk ~ sex + age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 + marital_1 + marital_2 +
      eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 + pre_bmi + pre_dm +
      pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 + pre_cpd_4 +
      pre_ldl + pre_hrx + pre_liprx + lag1_cpd + lag1_dpd + lag1_bmi + lag1_dm +
      lag1_sbp + lag1_ldl + lag1_hrx + as.factor(time), 
    
    # log-linear model for number of cigarettes per day among smokers
    cpd ~ age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 + marital_1 + marital_2 +
      eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 + pre_bmi + pre_dm +
      pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 + pre_cpd_4 +
      pre_ldl + pre_hrx + pre_liprx + lag1_cpd + lag1_dpd + lag1_bmi + lag1_dm +
      lag1_sbp + lag1_ldl + lag1_hrx + as.factor(time), 
    
    # logit model for probability of smoking
    drk ~ sex + age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 + marital_1 + marital_2 +
      eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 + pre_bmi + pre_dm +
      pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 + pre_cpd_4 +
      pre_ldl + pre_hrx + pre_liprx + lag1_cpd + lag1_dpd + lag1_bmi + lag1_dm +
      lag1_sbp + lag1_ldl + lag1_hrx + cpd + as.factor(time), 
    
    # log-linear model for number of drinks per day
    dpd ~ sex + age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 + marital_1 + marital_2 +
      eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 + pre_bmi + pre_dm +
      pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 + pre_cpd_4 +
      pre_ldl + pre_hrx + pre_liprx + lag1_cpd + lag1_dpd + lag1_bmi + lag1_dm +
      lag1_sbp + lag1_ldl + lag1_hrx + cpd + as.factor(time), 
    
    # linear model of BMI 
    bmi ~ sex + age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 + marital_1 + marital_2 +
      eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 + pre_bmi + pre_dm +
      pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 + pre_cpd_4 +
      pre_ldl + pre_hrx + pre_liprx + lag1_cpd + lag1_dpd + lag1_bmi + lag1_dm +
      lag1_sbp + lag1_ldl + lag1_hrx + cpd + dpd + as.factor(time),
    
    # logit model for diabetes (failure) 
    dm ~ sex + age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 + marital_1 + marital_2 +
      eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 + pre_bmi + pre_dm +
      pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 + pre_cpd_4 +
      pre_ldl + pre_hrx + pre_liprx + lag1_cpd + lag1_dpd + lag1_bmi + lag1_dm +
      lag1_sbp + lag1_ldl + lag1_hrx + cpd + dpd + bmi + as.factor(time),

    # linear model for systolic blood pressure 
    sbp ~ sex + age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 + marital_1 + marital_2 +
      eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 + pre_bmi + pre_dm +
      pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 + pre_cpd_4 +
      pre_ldl + pre_hrx + pre_liprx + lag1_cpd + lag1_dpd + lag1_bmi + lag1_dm +
      lag1_sbp + lag1_ldl + lag1_hrx + cpd + dpd + bmi + dm + as.factor(time),
    
    # linear model for LDL cholesterol
    ldl ~ sex + age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 + marital_1 + marital_2 +
      eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 + pre_bmi + pre_dm +
      pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 + pre_cpd_4 +
      pre_ldl + pre_hrx + pre_liprx + lag1_cpd + lag1_dpd + lag1_bmi + lag1_dm +
      lag1_sbp + lag1_ldl + lag1_hrx + cpd + dpd + bmi + dm + sbp + as.factor(time),
    
    # logit model for hypertension meds
    hrx ~ sex + age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 + marital_1 + marital_2 +
      eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 + pre_bmi + pre_dm +
      pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 + pre_cpd_4 +
      pre_ldl + pre_hrx + pre_liprx + lag1_cpd + lag1_dpd + lag1_bmi + lag1_dm +
      lag1_sbp + lag1_ldl + lag1_hrx + cpd + dpd + bmi + dm + sbp + ldl + as.factor(time)
  ),
  covlink = c(
    "logit",
    "log",
    "logit",
    "log",
    "identity",
    "logit",
    "identity",
    "identity",
    "logit"
  )
)


# Define outcome model ----------------------------------------------------

ymodel <- 
  event_chd ~ sex + age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 +
    marital_1 + marital_2 + eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 +
    pre_bmi + pre_dm + pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 +
    pre_cpd_4 + pre_ldl + pre_hrx + pre_liprx + cpd + dpd + bmi +
    dm + sbp + ldl + hrx + as.factor(time) 


# Define competing event model --------------------------------------------

compevent_model <- 
  event_dth ~ sex + age0 + I(age0 ^ 2) + educ_1 + educ_2 + educ_3 +
    marital_1 + marital_2 + eversmk + pre_dpd_1 + pre_dpd_2 + pre_dpd_3 +
    pre_bmi + pre_dm + pre_sbp + pre_cpd_1 + pre_cpd_2 + pre_cpd_3 +
    pre_cpd_4 + pre_ldl + pre_hrx + pre_liprx + cpd + dpd + bmi +
    dm + sbp + ldl + hrx + as.factor(time) 
  

# Run gcomputation algorithm ----------------------------------------------

fit <- 
  gformula_survival(
    obs_data = drop_na(analytic_long),
    id = "pid",
    time_name = "time",
    time_points = 4,
    covnames = covs_dvs, 
    covtypes = covtypes,
    covparams = covparams,
    outcome_name = "event_chd",
    ymodel = ymodel,
    compevent_name = "event_dth",
    compevent_model = compevent_model,
    intvars = intvars,
    interventions = interventions,
    int_descript = int_descript,
    restrictions = restrictions,
    ref_int = 1,
    basecovs = covs_fixed[!covs_fixed %in% covs_refs],
    histvars = list(covs_dvs),
    histories = c(lagged),
    nsamples = 10,
    nsimul = 10000,
    seed = 1234,
    show_progress = TRUE,
    parallel = TRUE,
    ncores = parallel::detectCores() - 1,
    model_fits = TRUE
  )
