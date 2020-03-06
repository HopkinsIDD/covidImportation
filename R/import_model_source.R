
# SETUP -------------------------------------------------------------------

if(!require('data.table')) install.packages('data.table')
if(!require('reshape2')) install.packages('reshape2')




# DAILY IMPORTATION MODEL -------------------------------------------------

run_daily_import_model <- function(input_data, n_sim=10000, allow_travel_variance=FALSE,
                                   meanD, t_red, u_origin,  
                                   project_name, batch, version){

    sources_ <- sort(unique(input_data$source))
    dests_ <- sort(unique(input_data$destination))
    t_ <- sort(unique(input_data$t))
    
    # Sims in longform
    cases <- input_data$cases_incid
    sim <- input_data %>% select(source, destination, t) %>% data.table::as.data.table()
    
    # Sims as multidimensional arrays
    importation_sim <- array(0, dim = c(length(sources_), length(dests_), length(t_), n_sim),
                             dimnames = list(sources_, dests_, as.character(t_), 1:n_sim))

    # start the timer
    t.start <- proc.time() 
    
    for (n in 1:n_sim){
      if (n %% 100 == 0) print(paste('sim', n, 'of', n_sim, sep = ' '))
    
      this.sim <- rep(0, length(cases))
    
      # Get p_s,d,t  (probability of infected individual traveling from d to s during time t
      if (allow_travel_variance){  # if allowing variance in travel, using travelers SE
        Travelers_over_Population_and_days <- MCMCglmm::rtnorm(dim(input_data)[1],
                                                    mean = input_data$travelers,
                                                    sd = input_data$travelers_SE,
                                                    lower = 0) / input_data$days_per_t / input_data$population
      } else {
        Travelers_over_Population_and_days <- input_data$travelers / input_data$days_per_t / input_data$population
      }
    
      # adjust probability by travel probability reduction
      prob_travel_n_detection <- t_red[n]*Travelers_over_Population_and_days
    
      # Run simulations by day, in case travel likelihood is affected by symptoms on a day to day basis
      for (day in 1:meanD[n]){
        this.sim <- this.sim + rbinom(length(cases),
                                      prob = prob_travel_n_detection, size = round( cases/u_origin[n]))
      }
    
      sim.wide <- reshape2::acast(cbind(sim, this.sim) %>%
                          group_by(source, destination, t) %>%
                          summarize(this.sim = sum(this.sim)),
                        source ~ destination ~ t, value.var = "this.sim")
      importation_sim[dimnames(sim.wide)[[1]], dimnames(sim.wide)[[2]], dimnames(sim.wide)[[3]], n] <- sim.wide
    
    }
    
    # Saving Sims
    dir.create(file.path("output",project_name), recursive = TRUE)
    save(importation_sim, file = file.path("output",project_name, sprintf("nCoV_importation_sim_%s_batch_v%s.RData", batch, version)))
    
    print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))
    return(importation_sim)
}





# DAILY IMPORTATION MODEL - TIME AND LOCATION  -------------------------------------------------

run_daily_import_model_timeloc <- function(input_data, n_sim=10000, allow_travel_variance=FALSE,
                                   meanD_mat, t_red, u_origin, time_inftotravel, time_inftodetect,
                                   project_name, batch, version, print_progress=TRUE){
  
  sources_ <- sort(unique(input_data$source))
  dests_ <- sort(unique(input_data$destination))
  t_ <- sort(unique(input_data$t))
  t_detect_ <- seq(as.Date(min(t_)), as.Date(max(t_))+30, by="days") # this might need to increased past 15 days, not sure
  
  # Sims in longform
  cases <- input_data$cases_incid
  sim <- input_data %>% select(source, destination, t) %>% data.table::as.data.table()
  
  # Sims as multidimensional arrays
  importation_sim <- array(0, dim = c(length(sources_), length(dests_), length(t_), n_sim),
                           dimnames = list(sources_, dests_, as.character(t_), 1:n_sim))
  importation_detect <- array(0, dim = c(length(sources_), length(dests_), length(t_detect_), n_sim),
                              dimnames = list(sources_, dests_, as.character(t_detect_), 1:n_sim))
  
  
  # start the timer
  t.start <- proc.time() 
  
  for (n in 1:n_sim){
    if (print_progress){
      if (n %% 100 == 0) print(paste('sim', n, 'of', n_sim, sep = ' '))
    }
    
    this.sim <- rep(0, length(cases))
    
    # Get p_s,d,t  (probability of infected individual traveling from d to s during time t
    if (allow_travel_variance){  # if allowing variance in travel, using travelers SE
      Travelers_over_Population_and_days <- MCMCglmm::rtnorm(dim(input_data)[1],
                                                             mean = input_data$travelers,
                                                             sd = input_data$travelers_SE,
                                                             lower = 0) / input_data$days_per_t / input_data$population
    } else {
      Travelers_over_Population_and_days <- input_data$travelers / input_data$days_per_t / input_data$population
    }
    
    # adjust probability by travel probability reduction
    prob_travel_n_detection <- t_red[n]*Travelers_over_Population_and_days
    
    # Run simulations by day, in case travel likelihood is affected by symptoms on a day to day basis
    for (c in 1:length(cases)){
      this.sim[c] <- sum(rbinom(ceiling(meanD_mat[n,c]), prob = prob_travel_n_detection[c], size = ceiling(cases[c]/u_origin[n,c])))
    }
    
    # Detection of Cases  
    this.sim_ <- cbind(sim, this.sim)
    import_dates <- as.Date(integer(0), origin = "1970-01-01")
    detect_sources <- detect_dests <- NULL
    for (r in 1:nrow(this.sim_)){
      import_dates <- c(import_dates, rep(as.Date(this.sim_$t[r]), times=this.sim_$this.sim[r]))
      detect_sources <- c(detect_sources, rep(this.sim_$source[r], times=this.sim_$this.sim[r]))
      detect_dests <- c(detect_dests, rep(this.sim_$destination[r], times=this.sim_$this.sim[r]))
    }
    
    # Add detection times to importation dates
    time_dat <- data.frame(time_inftodetect, time_inftotravel) %>% 
      filter(time_inftodetect>=time_inftotravel & time_inftodetect<20)
    samp <- sample(1:nrow(time_dat), length(import_dates), replace = TRUE)
    inf_dates <- import_dates - time_dat$time_inftotravel[samp] # calculate the date of infection
    detect_dates <- inf_dates + time_dat$time_inftodetect[samp] # calculate the date of detection
    #View(data.frame(as.Date(inf_dates), as.Date(import_dates), as.Date(detect_dates)))
    tmp <- data.frame(source=detect_sources, destination=detect_dests, t = as.character(as.Date(detect_dates))) %>% group_by(source, destination, t) %>% summarise(this.sim = n())
    #tmp <- tmp %>% mutate(conf_dates = as.Date(t) + 2)
    
    # Importations
    sim.wide <- reshape2::acast(cbind(sim, this.sim) %>%
                        group_by(source, destination, t) %>%
                        summarize(this.sim = sum(this.sim)),
                      source ~ destination ~ t, value.var = "this.sim")
    importation_sim[dimnames(sim.wide)[[1]], dimnames(sim.wide)[[2]], dimnames(sim.wide)[[3]], n] <- sim.wide
    
    
    # Detected Importations
    detect.wide <- reshape2::acast(tmp, source ~ destination ~ t, value.var = "this.sim")
    detect.wide[is.na(detect.wide)] <- 0
    importation_detect[dimnames(detect.wide)[[1]], dimnames(detect.wide)[[2]], dimnames(detect.wide)[[3]], n] <- detect.wide
    
  }
  
  # Saving Sims
  dir.create(file.path("output",project_name), recursive = TRUE)
  save(importation_sim, file = file.path("output",project_name, sprintf("nCoV_importation_sim_%s_batch_v%s.RData", batch, version)))
  save(importation_detect, file = file.path("output",project_name, sprintf("nCoV_importation_detect_%s_batch_v%s.RData", batch, version)))
  
  print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))
  return(list(importation_sim=importation_sim, importation_detect=importation_detect))
}




# DAILY IMPORTATION MODEL - TIME AND LOCATION  -------------------------------------------------

run_daily_import_model_timeloc_withdailyest <- function(input_data, travel_data_monthly=travel_data_monthly, 
                                                        travel_dispersion=10, 
                                                        travel_restrictions=data.frame(loc="Hubei", min="2020-01-25", max="2020-04-01", p_travel=0), #hubei_shutdown="2020-01-25",
                                                        n_sim=10000, allow_travel_variance=FALSE,
                                                         meanD_mat, t_red, u_origin, time_inftotravel, time_inftodetect,
                                                         project_name, batch, version, print_progress=TRUE){
                
  # start the timer
  t.start <- proc.time() 
  
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
  
  
  for (n in 1:n_sim){
    if (print_progress){
      if (n %% 100 == 0) print(paste('sim', n, 'of', n_sim, sep = ' '))
    }
    
    # Simulate the daily travel from monthly
    travel_data_daily <- make_daily_travel(travel_data=travel_data_monthly, travel_dispersion)
    input_data_sim <- left_join(input_data %>% dplyr::select(-travelers), 
                            travel_data_daily %>% dplyr::select(source,destination,t,travelers), 
                            by=c("source", "destination", "t"))
    
    # Set up any travel restrictions  
    for (r in 1:nrow(travel_restrictions)){
      input_data_sim <- input_data_sim %>% 
                            mutate(travelers = ifelse(
                                      source==travel_restrictions$loc[r] & 
                                      t >=as.Date(travel_restrictions$min[r]) & 
                                      t < as.Date(travel_restrictions$max[r]), 
                                      travelers*travel_restrictions$p_travel[r], travelers))
    }
  
    
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
    prob_travel_n_detection <- t_red[n]*Travelers_over_Population_and_days
    
    # Run simulations by day, in case travel likelihood is affected by symptoms on a day to day basis
    for (c in 1:length(cases)){
      this.sim[c] <- sum(rbinom(ceiling(meanD_mat[n,c]), 
                                prob = prob_travel_n_detection[c], 
                                size = ceiling(cases[c]/u_origin[n,c])))
    }

    
    # Detection of Cases  
    this.sim_ <- cbind(sim, this.sim)
    import_dates <- as.Date(integer(0), origin = "1970-01-01")
    detect_sources <- detect_dests <- NULL
    for (r in 1:nrow(this.sim_)){
      import_dates <- c(import_dates, rep(as.Date(this.sim_$t[r]), times=this.sim_$this.sim[r]))
      detect_sources <- c(detect_sources, rep(this.sim_$source[r], times=this.sim_$this.sim[r]))
      detect_dests <- c(detect_dests, rep(this.sim_$destination[r], times=this.sim_$this.sim[r]))
    }
    
    # Add detection times to importation dates
    time_dat <- data.frame(time_inftodetect, time_inftotravel) %>% 
      filter(time_inftodetect>=time_inftotravel & time_inftodetect<20)
    samp <- sample(1:nrow(time_dat), length(import_dates), replace = TRUE)
    inf_dates <- import_dates - time_dat$time_inftotravel[samp] # calculate the date of infection
    detect_dates <- inf_dates + time_dat$time_inftodetect[samp] # calculate the date of detection
    #View(data.frame(as.Date(inf_dates), as.Date(import_dates), as.Date(detect_dates)))
    
    
    # Importations
    sim.wide <- reshape2::acast(cbind(sim, this.sim) %>%
                        group_by(source, destination, t) %>%
                        summarize(this.sim = sum(this.sim)),
                      source ~ destination ~ t, value.var = "this.sim")
    importation_sim[dimnames(sim.wide)[[1]], dimnames(sim.wide)[[2]], dimnames(sim.wide)[[3]], n] <- sim.wide
    
    
    # Detected Importations
    if (sum(this.sim)== 0 ) next    # - if no importations, skip to next
    tmp <- data.frame(source=detect_sources, destination=detect_dests, t = as.character(as.Date(detect_dates))) %>% 
                    group_by(source, destination, t) %>% summarise(this.sim = n())
    #tmp <- tmp %>% mutate(conf_dates = as.Date(t) + 2)
    detect.wide <- reshape2::acast(tmp, source ~ destination ~ t, value.var = "this.sim")
    detect.wide[is.na(detect.wide)] <- 0
    importation_detect[dimnames(detect.wide)[[1]], dimnames(detect.wide)[[2]], dimnames(detect.wide)[[3]], n] <- detect.wide
    
  }
  
  # Saving Sims
  dir.create(file.path("output",project_name), recursive = TRUE)
  save(importation_sim, file = file.path("output",project_name, sprintf("nCoV_importation_sim_%s_batch_v%s.RData", batch, version)))
  save(importation_detect, file = file.path("output",project_name, sprintf("nCoV_importation_detect_%s_batch_v%s.RData", batch, version)))
  
  print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))
  return(list(importation_sim=importation_sim, importation_detect=importation_detect))
}






