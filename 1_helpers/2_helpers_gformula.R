# take fitted models of joint distribution and simulate data for a given
# baseline covariate profile

predict.gformula <- function(object, obs_data, newdata = NULL, id, covnames, covtypes, covparams,
                             covfits_custom = NA, covpredict_custom = NA,
                             histvars = NULL, histories = NA, basecovs = NA,
                             outcome_name, ymodel,
                             compevent_name = NULL, compevent_model = NA,
                             intvars = NULL, interventions = NULL,
                             int_times = NULL, int_descript = NULL, ref_int = 0, intcomp = NA,
                             visitprocess = NA, restrictions = NA,
                             yrestrictions = NA, compevent_restrictions = NA,
                             baselags = FALSE,
                             nsimul = NA, seed,
                             nsamples = 0, parallel = FALSE, ncores = NA,
                             ci_method = 'percentile', threads,
                             show_progress = TRUE, ...){
  if (object$time_points > 1){
    if (object$comprisk){
      fitD <- object$fits[[length(object$fits)]]
      fitY <- object$fits[[length(object$fits) - 1]]
      fitcov <- object$fits[1:(length(object$fits) - 2)]
    } else {
      fitY <- object$fits[[length(object$fits)]]
      fitcov <- object$fits[1:(length(object$fits) - 1)]
      fitD <- NA
    }
  } else {
    fitY <- unlist(object$fits)
    fitD <- NA
    fitcov <- NA
  }
  outcome_type <- "survival"
  max_visits <- NA
  
  lag_indicator <- lagavg_indicator <- cumavg_indicator <- c()
  lag_indicator <- gfoRmula:::update_lag_indicator(covparams$covmodels, lag_indicator)
  lagavg_indicator <- gfoRmula:::update_lagavg_indicator(covparams$covmodels, lagavg_indicator)
  cumavg_indicator <- gfoRmula:::update_cumavg_indicator(covparams$covmodels, cumavg_indicator)
  
  if (!missing(ymodel)){
    lag_indicator <- gfoRmula:::update_lag_indicator(ymodel, lag_indicator)
    lagavg_indicator <- gfoRmula:::update_lagavg_indicator(ymodel, lagavg_indicator)
    cumavg_indicator <- gfoRmula:::update_cumavg_indicator(ymodel, cumavg_indicator)
  }
  if (!(length(compevent_model) == 1 && is.na(compevent_model))){
    lag_indicator <- gfoRmula:::update_lag_indicator(compevent_model, lag_indicator)
    lagavg_indicator <- gfoRmula:::update_lagavg_indicator(compevent_model, lagavg_indicator)
    cumavg_indicator <- gfoRmula:::update_cumavg_indicator(compevent_model, cumavg_indicator)
  }
  histvals <- list(lag_indicator = lag_indicator, lagavg_indicator = lagavg_indicator,
                   cumavg_indicator = cumavg_indicator)
  
  comprisk <- object$comprisk
  
  if (!missing(threads)){
    data.table::setDTthreads(threads = threads)
  }
  else {
    threads <- data.table::getDTthreads()
  }
  
  min_time <- min(newdata[[object$time_name]])
  below_zero_indicator <- min_time < 0
  
  newdata <- data.table::copy(newdata)
  obs_data <- data.table::copy(obs_data)
  
  sample_size <- length(unique(newdata[[id]]))
  
  time_points <- object$time_points

  
  for (i in seq_along(covnames)){
    if (covtypes[i] == 'absorbing'){
      restrictions <- c(restrictions[!is.na(restrictions)],
                        list(c(covnames[i], paste("lag1_", covnames[i], "==0", sep = ""),
                               carry_forward, 1)))
      covtypes[i] <- 'binary'
    }
  }
  
  # Create 1-indexed numerical IDs for observed datasets
  ids <- data.table::as.data.table(sort(unique(newdata[[id]])))
  ids[, 'newid' := seq_len(.N)]
  data.table::setkeyv(newdata, id)
  newdata <- newdata[J(ids), allow.cartesian = TRUE]
  newdata_geq_0 <- newdata[newdata[[object$time_name]] >= 0]
  obs_data_geq_0 <- obs_data[obs_data[[object$time_name]] >= 0]
  
  # Set default number of simulated individuals to equal number of individuals in
  # observed dataset
  if (is.na(nsimul)){
    nsimul <- length(unique(newdata$newid))
  }
  
  
  # Generate seeds for simulations and bootstrapping
  set.seed(seed)
  newseeds <- sample.int(2^30, size = nsamples + 1)
  subseed <- newseeds[1]
  bootseeds <- newseeds[2:(nsamples + 1)]
  
  # Determine ranges of observed covariates and outcome
  ranges <- lapply(seq_along(covnames), FUN = function(i){
    if (covtypes[i] == 'normal' || covtypes[i] == 'bounded normal' ||
        covtypes[i] == 'truncated normal') {
      range(obs_data_geq_0[[covnames[i]]])
    } else if (covtypes[i] == 'zero-inflated normal'){
      range(obs_data_geq_0[obs_data_geq_0[[covnames[i]]] > 0][[covnames[i]]])
    } else {
      NA
    }
  })
  
  yrange <- range(obs_data_geq_0[[outcome_name]])

  if (comprisk){
    compevent_range <- range(obs_data_geq_0[[compevent_name]])
  } else {
    compevent_range <- NA
  }
  
  newdata_noresample <- data.table::copy(newdata)
  len <- length(unique(newdata$newid))
  # If the number of user desired simulations differs from the number of individuals in
  # the observed dataset, sample the desired number of observed IDs with replacement
  if (nsimul < len){
    ids <- data.table::as.data.table(sort(sample(unique(newdata$newid), nsimul, replace = TRUE)))
    colnames(ids) <- "newid"
    ids[, 'sid' := seq_len(.N)]
    newdata <- merge(ids, newdata, all.x = TRUE, by = "newid")
    newdata[, 'newid' := newdata$sid]
    newdata[, 'sid' := NULL]
  } else if (nsimul > len){
    ids <- data.table::as.data.table(sample(unique(newdata$newid), nsimul, replace = TRUE))
    ids[, 'newid' := 1:nsimul]
    colnames(ids) <- c("newid", "sid")
    data.table::setkeyv(newdata, "newid")
    newdata <- newdata[J(ids), allow.cartesian = TRUE]
    newdata[, 'newid' := newdata$sid]
    newdata[, 'sid' := NULL]
  }
  
  # Add natural course to list of interventions
  if (!is.null(interventions)){
    comb_interventions <- c(list(list(c(gfoRmula:::natural))), interventions)
    comb_intvars <- c(list('none'), intvars)
  } else {
    comb_interventions <- list(list(c(gfoRmula:::natural)))
    comb_intvars <- list('none')
  }
  
  if (is.null(int_times)){
    comb_int_times <- list()
    for (i in seq_along(comb_interventions)){
      comb_int_times[[i]] <- lapply(seq_along(comb_interventions[[i]]),
                                    FUN = function(i) {0:(time_points - 1)})
    }
  } else {
    comb_int_times <- c(list(list(0:(time_points - 1))), int_times)
  }
  
  if (parallel){
    cl <- gfoRmula:::prep_cluster(ncores = ncores, threads = threads, covtypes = covtypes)
    pools <- parallel::parLapply(cl, seq_along(comb_interventions), gfoRmula:::simulate,
                                 fitcov = fitcov, fitY = fitY, fitD = fitD,
                                 yrestrictions = yrestrictions,
                                 compevent_restrictions = compevent_restrictions,
                                 restrictions = restrictions,
                                 outcome_name = outcome_name, compevent_name = compevent_name,
                                 time_name = object$time_name,
                                 intvars = comb_intvars, interventions = comb_interventions,
                                 int_times = comb_int_times, histvars = histvars,
                                 histvals = histvals, histories = histories,
                                 covparams = covparams, covnames = covnames, covtypes = covtypes,
                                 covpredict_custom = covpredict_custom, basecovs = basecovs,
                                 comprisk = comprisk, ranges = ranges,
                                 yrange = yrange, compevent_range = compevent_range,
                                 outcome_type = outcome_type,
                                 subseed = subseed, time_points = time_points,
                                 obs_data = newdata, parallel = parallel, max_visits = max_visits,
                                 baselags = baselags, below_zero_indicator = below_zero_indicator,
                                 min_time = min_time, show_progress = FALSE, ...)
    parallel::stopCluster(cl)
  } else {
    pools <- lapply(seq_along(comb_interventions), FUN = function(i){
      gfoRmula:::simulate(fitcov = fitcov, fitY = fitY, fitD = fitD,
               yrestrictions = yrestrictions,
               compevent_restrictions = compevent_restrictions,
               restrictions = restrictions,
               outcome_name = outcome_name, compevent_name = compevent_name,
               time_name = object$time_name,
               intvars = comb_intvars[[i]], interventions = comb_interventions[[i]],
               int_times = comb_int_times[[i]], histvars = histvars, histvals = histvals,
               histories = histories, covparams = covparams,
               covnames = covnames, covtypes = covtypes,
               covpredict_custom = covpredict_custom, basecovs = basecovs, comprisk = comprisk,
               ranges = ranges, yrange = yrange, compevent_range = compevent_range,
               outcome_type = outcome_type,
               subseed = subseed, time_points = time_points,
               obs_data = newdata, parallel = parallel, max_visits = max_visits,
               baselags = baselags, below_zero_indicator = below_zero_indicator,
               min_time = min_time, show_progress = FALSE, ...)
    })
  }
  nat_pool <- pools[[1]] # Natural course data
  pools <- pools[-1] # List of intervention datasets
  
  # Calculate mean risk over all subjects at each time for natural course
  nat_result <- tapply(nat_pool$poprisk, nat_pool[[object$time_name]], FUN = mean)
  return(nat_result)
}

if (FALSE) {
  newdata <- fit_ldl$sim_data$`Natural course`[1:4]
  newdata$pid <- 1
  
  predict.gformula(
    object = fit_ldl,
    obs_data = data.table::as.data.table(drop_na(analytic_long)),
    newdata = data.table::as.data.table(newdata),
    id = "pid",
    covnames = covs_dvs[covs_dvs != "liprx"], 
    covtypes = covtypes,
    covparams = covparams,
    outcome_name = "event_chd",
    ymodel = ymodel,
    compevent_name = "event_dth",
    compevent_model = compevent_model,
    restrictions = restrictions,
    basecovs = covs_fixed[!covs_fixed %in% covs_refs],
    histvars = list(covs_dvs[covs_dvs != "liprx"]),
    histories = c(lagged),
    nsamples = 0,
    nsimul = GFORM_SIM,
    seed = 1234,
    show_progress = TRUE,
    # parallel = TRUE,
    # ncores = parallel::detectCores() - 1,
    model_fits = TRUE
  )
}