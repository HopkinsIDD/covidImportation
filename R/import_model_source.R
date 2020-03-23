

if(!require('data.table')) install.packages('data.table')
if(!require('reshape2')) install.packages('reshape2')






##' 
##' This is the base model for estimating importations. We will use this within the larger framework 
##' to get importations per simulation
##' 
##' @param input_data Full, combined input data that includes incident cases, travel volume, and population 
##' for all sources/destination pairs
##' @param tr_inf_redux Default=1; Travel reduction given infected 
##' @param meanD Vector of length nrow(input_data); mean duration of time during which an infected individual
##'                  can travel, in days.
##' @param u_origin Vector of length nrow(input_data); underreporting rate in the infection source location.
##' @param allow_travel_variance Logical, whether to sample the travel
##' 
est_imports_base <- function(input_data, 
                             tr_inf_redux = 0, 
                             meanD,
                             u_origin, 
                             allow_travel_variance=FALSE){
  
  
  cases <- input_data_sim$cases_incid
  this.sim <- rep(0, length(cases))
  
  
  # Get p_s,d,t  (probability of infected individual traveling from d to s during time t
  if (allow_travel_variance){  # if allowing variance in travel, using travelers SE
    Travelers_over_Population_and_days <- MCMCglmm::rtnorm(dim(input_data_sim)[1],
                                                           mean = input_data_sim$travelers,
                                                           sd = input_data_sim$travelers_SE,
                                                           lower = 0) / input_data_sim$days_per_t / input_data_sim$population
  } else {
    Travelers_over_Population_and_days <- input_data_sim$travelers / input_data_sim$days_per_t / input_data_sim$population
  }
  
  # adjust probability by travel probability reduction
  prob_travel_n_detection <- (1-tr_inf_redux) * Travelers_over_Population_and_days
  
  
  # Run simulations by day, in case travel likelihood is affected by symptoms on a day to day basis
  for (c in 1:length(cases)){
    this.sim[c] <- sum(rbinom(ceiling(meanD[c]), 
                              prob = prob_travel_n_detection[c], 
                              size = ceiling(cases[c]/u_origin[c])))
  }
  this.sim[is.na(this.sim)] <- 0

  return(this.sim)
  
}
  





##'
##' Estimate the detection dates of importations
##' 
##' @param sim_res Vector of importation simulation results
##' 
est_import_detect_dates <- function(sim_res){
  
  # Detection of Cases
  import_dates <- rep(as.Date(this.sim_$t), times=this.sim_$this.sim)
  detect_sources <- rep(this.sim_$source, times=this.sim_$this.sim)
  detect_dests <- rep(this.sim_$destination, times=this.sim_$this.sim)
  
  # Add detection times to importation dates
  time_dat <- data.frame(time_inftodetect, time_inftotravel) %>%
    filter(time_inftodetect>=time_inftotravel & time_inftodetect<20)
  samp <- sample(1:nrow(time_dat), length(import_dates), replace = TRUE)
  inf_dates <- import_dates - time_dat$time_inftotravel[samp] # calculate the date of infection
  detect_dates <- inf_dates + time_dat$time_inftodetect[samp] # calculate the date of detection
  
  return(data.frame(detect_sources, detect_dests, inf_dates, import_dates, detect_dates))
  
}







##'
##' Run the full simulation of daily importations
##' 
##' @param n_sim number of simulations to run 
##' @param input_data full importation input data, including case, travel, and population data
##' @param travel_data_monthly monthly travel data between sources and destinations
##' @param travel_dispersion how evenly the monthly travel should be distributed across days
##' @param travel_restrictions data.frame of travel restrictions
##' @param allow_travel_variance whether to sample from the travel variance
##' @param meanD_mat matrix of mean duration during which infected individuals can travel
##' @param tr_inf_redux
##' @param u_origin
##' @param time_inftotravel
##' @param 
##' @param 

run_daily_import_model_par <- function(n_sim=10000,
                                       input_data,
                                       travel_data_monthly=travel_data_monthly, 
                                       travel_dispersion=3, 
                                       travel_restrictions=data.frame(loc="Hubei", min="2020-01-25", max="2020-04-01", p_travel=0), #hubei_shutdown="2020-01-25",
                                       allow_travel_variance=FALSE,
                                       meanD_mat, 
                                       tr_inf_redux, 
                                       u_origin, 
                                       get_detection_time=FALSE,
                                       time_inftotravel, time_inftodetect,
                                       incub_mean_log, incub_sd_log,
                                       inf_period_hosp_shape, inf_period_hosp_scale,
                                       inf_period_nohosp_mean, inf_period_nohosp_sd,
                                       project_name, batch, version, 
                                       print_progress=TRUE,
                                       cores=4){
  
  library(doParallel)
  library(abind)
  
  sources_ <- sort(unique(input_data$source))
  dests_ <- sort(unique(input_data$destination))
  t_ <- sort(unique(input_data$t))
  t_detect_ <- seq(as.Date(min(t_)), as.Date(max(t_))+30, by="days") # this might need to increased past 15 days, not sure
  
  # Sims in longform
  sim <- input_data %>% select(source, destination, t) %>% data.table::as.data.table()
  
  # Sims as multidimensional arrays
  importation_sim <- array(0, dim = c(length(sources_), length(dests_), length(t_), n_sim),
                           dimnames = list(sources_, dests_, as.character(t_), 1:n_sim))
  importation_detect <- array(0, dim = c(length(sources_), length(dests_), length(t_detect_), n_sim),
                              dimnames = list(sources_, dests_, as.character(t_detect_), 1:n_sim))

  # Set up the cluster for parallelization
  print(paste0("Making cluster of ", cores," for parallelization."))
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  
  
  # start the timer
  t.start <- proc.time() 
  
  # make a base set of daily travel, from which we will add to each time
  travel_data_daily <- make_daily_travel(travel_data=travel_data_monthly, travel_dispersion) 
  # Make travel restrictions into long, expanded format for easy merging
  travel_restrictions_long <- expand_travel_restrict(travel_restrictions)
    

  # make the function to bind each simulation array in the foreach loop
  acomb <- function(...) abind::abind(..., along=4)
  # Run the foreach loop to estimate importations for n simulations
  importation_sim <- foreach(n=1:n_sim, .combine = acomb,
                             .export=c("make_daily_travel_faster", "apply_travel_restrictions",
                                              "est_imports_base"), 
                         .packages=c("dplyr","tidyr")) %dopar% {
    
    if (print_progress){
      if (n %% 10 == 0) print(paste('sim', n, 'of', n_sim, sep = ' '))
    }
                           
    importation_sim_tmp <- array(0, dim = c(length(sources_), length(dests_), length(t_)),
                                   dimnames = list(sources_, dests_, as.character(t_)))
    
    # Simulate the daily travel from monthly
    travel_data_daily <- make_daily_travel_faster(travel_data=travel_data_monthly, 
                                                  travel_data_daily=travel_data_daily,
                                                  travel_dispersion=travel_dispersion) 

    # Apply travel restrictions
    travel_data_daily_ <- apply_travel_restrictions(travel_data_daily, travel_restrictions_long)

    # Join sampled travel data back into input data
    input_data_sim <- left_join(input_data %>% dplyr::select(-travelers), 
                                travel_data_daily_ %>% dplyr::select(source,destination,t,travelers), 
                                by=c("source"="source", "destination"="destination", "t"="t"))

    
    # Run base model to estimate the number of importations during this simulation
    this.sim <- est_imports_base(input_data = input_data_sim, 
                                 tr_inf_redux = tr_inf_redux[n], 
                                 meanD = meanD_mat[n,],
                                 u_origin = u_origin[n,], 
                                 allow_travel_variance=allow_travel_variance)

    
    ## Estimate dates of importation and detection of the simulated importations
    this.sim_ <- data.frame(sim, this.sim)

    # Importations
    importation_sim_tmp <- reshape2::acast(this.sim_ %>% group_by(source, destination, t),
                                  source ~ destination ~ t, value.var = "this.sim")
    
    importation_sim_tmp 
    
  }
    
  # Give the sim dimension dimnames
  dimnames(importation_sim)[[4]] <- 1:n_sim
  # Replace NAs with 0. These are all pairs that did not have travel or cases
  importation_sim[is.na(importation_sim)] <- 0

  
  
  
  # Now lets get detections ........................................

  # If we want detected time of the importations, we generate a 4D array of that here
  if (get_detection_time){
    
    importation_detect <- foreach(n=1:n_sim, .combine = acomb,
                               .export=c("est_import_detect_dates"), 
                               .packages=c("dplyr","tidyr")) %dopar% {
      
      importation_detect_tmp <- importation_detect[,,,n]
                                                   
      # get a single simulation
      this.sim_ <- arrayhelpers::array2df(importation_sim[,,,n])
      colnames(this.sim_) <- c("this.sim", "source","destination","t")
      this.sim <- this.sim_$this.sim
      
      # Detected Importations
      if (sum(this.sim) == 0 ) next    # - if no importations, skip to next
      
      import_dates_ <- est_import_detect_dates(this.sim_)
      
      tmp <- data.frame(source=import_dates_$detect_sources, 
                        destination=import_dates_$detect_dests, 
                        t = as.character(as.Date(import_dates_$detect_dates))) %>%
        group_by(source, destination, t) %>% summarise(this.sim = n())
  
      detect_array_ <- reshape2::acast(tmp, source ~ destination ~ t, value.var = "this.sim")
      importation_detect_tmp[dimnames(detect_array_)[[1]], dimnames(detect_array_)[[2]], dimnames(detect_array_)[[3]]] <- detect_array_
      
      importation_detect_tmp    
    }
  }
  
  stopCluster(cl)
  
  # Give the sim dimension dimnames
  dimnames(importation_detect)[[4]] <- 1:n_sim
  # Replace NAs with 0. These are all pairs that did not have travel or cases
  importation_detect[is.na(importation_detect)] <- 0
  

  
  # Save Sims
  dir.create(file.path("output",project_name), recursive = TRUE)
  save(importation_sim, file = file.path("output",project_name, sprintf("nCoV_importation_sim_%s_batch_v%s.RData", batch, version)))
  save(importation_detect, file = file.path("output",project_name, sprintf("nCoV_importation_detect_%s_batch_v%s.RData", batch, version)))
  
  print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))
  return(list(importation_sim=importation_sim, importation_detect=importation_detect))
  
}









##' 
##' Get negative binomial estimates for each time and destination
##' 
##' @param importation_sim 4D array outputted from the importation model
##' @param project_name
##' @param batch
##' @param version
##' 
calc_nb_import_pars <- function(importation_sim, project_name, batch, version){
  
  # aggregate to just destination
  import_sim_dests <- apply(importation_sim, 2:4, sum, na.rm=TRUE)
  
  dests <- dimnames(import_sim_dests)[[1]]
  t <- dimnames(import_sim_dests)[[2]]
  n_t <- length(t)
  n_dest <- length(dests)
  
  # Make the blank parameter data.frame
  import_pars_df <- tidyr::expand_grid(detestination=dests, t=t, size=1, mu=0)
  
  for (d_ in 1:length(dests)){
    for (t_ in 1:length(t)){
      
        pars_ <- tryCatch ( {
          fitdistrplus::fitdist(import_sim_dests[d_, t_, ], distr="nbinom", method="mle")$estimate
          },
          #warning = function(w) { },
          error = function(err) {
            c(1,0)
          })
        row_ind <- (d_-1)*n_t + t_
        #import_pars_df_[[t_]] <- data.frame(airport = dests[d_], date=t[t_], size=pars_[1], mu=pars_[2] ))
        import_pars_df[row_ind, 3:4] <- pars_
    }
  }
  
  write_csv(import_pars_df, file.path("output",project_name, sprintf("nCoV_importation_nb_params_%s_batch_v%s.csv", batch, version)))
  return(import_pars_df)
}








