##' Estimate imports base
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
                                       time_inftotravel,
                                       time_inftodetect,
                                       incub_mean_log, incub_sd_log,
                                       inf_period_hosp_shape,
                                       inf_period_hosp_scale,
                                       inf_period_nohosp_mean,
                                       inf_period_nohosp_sd,
                                       project_name, batch, version,
                                       print_progress=TRUE,
                                       cores=4){
  require(doParallel)
  require(abind)

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
  save(importation_sim, file = file.path("output",project_name, sprintf("covid_importation_sim_%s_batch_v%s.RData", batch, version)))
  save(importation_detect, file = file.path("output",project_name, sprintf("covid_importation_detect_%s_batch_v%s.RData", batch, version)))

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

  write_csv(import_pars_df, file.path("output",project_name, sprintf("covid_importation_nb_params_%s_batch_v%s.csv", batch, version)))
  return(import_pars_df)
}


#' @param name_start character string, first letters in file name
#' @param path character string, path to folder of interest, end with "/"
#' @param exclude character string, patterns to exclude from the file names of interest
#'
#' @return character string, path to most recent file
#'
#' @examples
find_recent_file <- function(name_start, path, exclude=NULL){
    if(substring(path, nchar(path))!="/")
        warning("Path does not end with a '/', problems may ensue.")
    ## view all files of that name at that path
    file_list <- list.files(path=path,
                            pattern=paste0(name_start, "*"))
    ## remove files with unwanted patterns
    if(!is.null(exclude)){
        for(i in 1:length(exclude))
            file_list <- file_list[!grepl(pattern = exclude[i], file_list)]
    }
    if(length(file_list)==0){
        warning('File not found')
        return(NA)
    }
    ## view file info
    file_info <- file.info(paste0(path, file_list))
    ## find most recent file
    most_recent_file <- paste0(path,
                               file_list[which.max(file_info$mtime)])
    cat(sprintf("Loaded file: \n %s last updated on \n %s \n",most_recent_file,file_info$mtime[which.max(file_info$mtime)]))
    return(most_recent_file)
}

#' Set up and run importation sims
#'
#' @param dest character string, name of destination to simulate importations for
#' @param dest_type character string, options: "airport", "city", "state", "country"
#' @param dest_0 optional character string, specify a higher level destination (i.e. dest_0="USA"), default NULL
#' @param dest_0_type optional character string, must specify if specifying a `dest_0` option; default=NULL
#' @param dest_aggr_level character string, level to which travel will be aggregated for destination. Includes "airport", "city", "state", "country", "metro" (only available for CA currently)
#' @param project_name character string
#' @param version character string
#' @param batch character string
#' @param end_date Date, last import date
#' @param n_sim numeric, number of simulations
#' @param cores numeric, number of cores to run in parallel
#' @param get_detection_time logical
#' @param travel_dispersion numeric
#' @param allow_travel_variance logical
#' @param print_progress logical
#' @param param_list list, with the following elements
#' \itemize{
#'   \item \code{incub_mean_log} numeric, the mean_log parameter for the lnorm distribution for the incubation period
#'   \item \code{incub_sd_log} numeric, the sd_log parameter for the lnorm distribution for the incubation period
#'   \item \code{inf_period_nohosp_mean} numeric, the mean parameter of a truncated normal distribution for the infectious period for non-hospitalized infections
#'   \item \code{inf_period_nohosp_sd} numeric, the sd parameter of a truncated normal distribution for the infectious period for non-hospitalized infections
#'   \item \code{inf_period_hosp_shape} numeric, the shape parameter of a Gamma distribution for the infectious period for time to hospitalization
#'   \item \code{inf_period_hosp_scale} numeric, the scale parameter of a Gamma distribution for the infectious period for time to hospitalization
#'   \item \code{p_report_source} numeric vector of length 2, currently the probability of reporting by source with the first indicating Hubei reporting and second indicating everywhere else (UPDATE WITH THESE https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html)
#'   \item \code{shift_incid_days} lag from infection to report
#'   \item \code{delta} days per time period
#' }
#'
#' @return
#'
#' @examples
setup_and_run_importations <- function(dest="UT",
                                       dest_type=c("state", "city","airport", "country"),
                                       dest_0=NULL,
                                       dest_0_type=NULL,
                                       dest_aggr_level=c("airport", "city", "state", "country", "metro"),
                                       project_name="Utah_import",
                                       version="global",
                                       batch="1st",
                                       pull_github_data=TRUE,
                                       get_travel,
                                       end_date=Sys.Date(),
                                       n_sim=100,
                                       cores=4,
                                       get_detection_time=FALSE,
                                       travel_dispersion=3,
                                       allow_travel_variance=FALSE,
                                       print_progress=TRUE,
                                       param_list=list(incub_mean_log=log(5.89),
                                                       incub_sd_log=log(1.74),
                                                       inf_period_nohosp_mean=15,
                                                       inf_period_nohosp_sd=5,
                                                       inf_period_hosp_shape=0.75,
                                                       inf_period_hosp_scale=5.367,
                                                       p_report_source=c(0.05, 0.25),
                                                       shift_incid_days=-10,
                                                       delta=1)){
    dest_type <- match.arg(dest_type)
    dest_aggr_level <- match.arg(dest_aggr_level)
    require(tidyverse)
    ## GENERAL SETUP -----------------------------------------------------------
    ## Create needed directories
    dir.create(file.path("output",project_name), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path("data",project_name), recursive = TRUE, showWarnings = FALSE)
    dir.create(file.path("figures",project_name), recursive = TRUE, showWarnings = FALSE)

    ## DATA --------------------------------------------------------------------
    ## ~ Incidence data --------------------------------------------------------
    incid_data_list <- get_incidence_data(first_date = ISOdate(2019,12,1),
                                          last_date = Sys.time(),
                                          pull_github_data=pull_github_data)
    incid_data <- incid_data_list$incid_data %>%
        filter(source != "USA")
    jhucsse <- incid_data_list$jhucsse

    ## ~ Travel Data  ----------------------------------------------------------
    ## if travel data exists load it, otherwise download it
    if(is.missing(get_travel)){
        get_travel <- paste0(paste(dest, collapse = "+"), "-", dest_aggr_level,
                             "_oag_20172019.csv") %>%
            find_recent_file(path="data/") %>%
            is.na()
    }
    if(get_travel) {
        travel_data_monthly <- get_oag_travel(destination=dest,
                                              destination_type=dest_type,
                                              dest_aggr_level=dest_aggr_level,
                                              dest_0=dest_0,
                                              dest_0_type=dest_0_type) %>%
            mutate(travelers=travelers_mean, t_year=2020) %>%
            mutate(travelers=ifelse(t_month == "01" & dep_country=="CHN",
                                    # Increase travel for Chinese New Year
                                    travelers*1.6, travelers)) %>%
            rename(source = dep_loc_aggr)
        travel_data_monthly$destination <- travel_data_monthly[,paste0("arr_", dest_aggr_level),drop=T]
    } else{
        travel_data_monthly <- paste0("data/", paste(dest, collapse = "+"), "-",
                                      dest_aggr_level, "_oag_20172019.csv") %>%
            read_csv(na=c(""," ","NA"))%>%
            mutate(travelers=travelers_mean,
                   t_year=2020) %>%
            mutate(travelers=ifelse(t_month == "01" & dep_country=="CHN",
                                    # Increase travel for Chinese New Year
                                    travelers*1.6, travelers)) %>%
            rename(source = dep_loc_aggr)
        travel_data_monthly$destination <- travel_data_monthly[,paste0("arr_", dest_aggr_level),drop=T]
    }
    ## monthly average totals into destinations
    travel_mean <- travel_data_monthly %>%
        group_by(destination, t_month) %>%
        summarise(travelers = sum(travelers_mean,na.rm=TRUE)) %>%
        group_by(destination) %>%
        summarise(travelers = mean(travelers))

    write_csv(travel_mean, paste0("data/", paste(dest, collapse = "+"), "-", dest_aggr_level, "_monthymeantravelers.csv"))

    ## Travel data
    ##  - Get daily for merging purposes
    travel_data <- make_daily_travel(travel_data_monthly, travel_dispersion=3)

    ## ~ Population Data -------------------------------------------------------
    data(pop_data)

    ## ~~ First Check that the variables match up
    # Check that incidence data does not have duplicates
    incid_dups <- sum(incid_data %>%
                          mutate(source_t = paste(source, t)) %>%
                          mutate(dup_entry=duplicated(source_t)) %>%
                          pull(dup_entry))
    if(sum(incid_dups)>0){
        dup_entry <- incid_data %>%
            mutate(source_t = paste(source, t)) %>%
            mutate(dup_entry=duplicated(source_t)) %>%
            filter(dup_entry) %>% pull(source_t)
        warning("There are duplicate entries in the incidence data.")
    }
    ## Check travel data
    travel_dups <- travel_data %>%
        mutate(source_dest_t = paste(source, destination, t),
               dup_entry=duplicated(source_dest_t)) %>%
        pull(dup_entry) %>%
        sum()
    if(sum(travel_dups)>0){
        warning("There are duplicate entries in the travel data.")
    }
    ## Check Population data
    pop_dups <- pop_data %>%
        mutate(dup_entry=duplicated(source)) %>%
        pull(dup_entry) %>%
        sum()
    if(sum(pop_dups)>0){
        warning("There are duplicate entries in the population data.")
    }
    # we really just need to make sure there are travel data and pop data for all source locations with incidence
    # incid_sources <- sort(unique(incid_data$source))
    # travel_sources <- sort(unique(travel_data$source))
    # pop_sources <- sort(unique(pop_data$source))
    # incid_sources[!(incid_sources %in% travel_sources)]
    # incid_sources[!(incid_sources %in% pop_sources)]

    ## ~~ Merge it all ---------------------------------------------------------
    input_data <- make_input_data(incid_data, travel_data, pop_data,
                                  shift_incid_days=param_list$shift_incid_days,
                                  dest_aggr_level=dest_aggr_level) %>%
        mutate(p_report_source=ifelse(source=="Hubei",
                                      param_list$p_report_source[1],
                                      param_list$p_report_source[2]),
               # For first pass, reporting rate is just Hubei/not Hubei
               days_per_t=param_list$delta # ~ delta: days per time period
        ) %>%
        filter(t<=as.Date(end_date))

    ## save final input data
    write_csv(input_data,
              file.path("data", project_name,
                        sprintf("input_data_%s_batch_v%s.RData", batch, version)))

    ## ~ Time to detect importations -------------------------------------------
    ## -- If we assume people generally depart at some point during their incubation period,
    ##     or very early in the symptomatic phase,
    ##     we can generate a distribution of time from travel to detection.
    ## -- because we are only worried about those who are detected, we can ignore time to recover
    time_inftodetect <- rlnorm(10000, mean = param_list$incub_mean_log,
                               sd = param_list$incub_sd_log) +
        rgamma(10000, shape=param_list$inf_period_hosp_shape,
               scale=param_list$inf_period_hosp_scale)

    ## We assume people can and do travel during their incubation period and
    ##  during that period during which symptoms are still minor.
    ##  There are reports of travelers taking fever-reducers and a portion dont show fever
    ## We assume this is uniform
    time_inftotravel <- sapply(time_inftodetect, runif, n=1, min=0)
    time_traveltodetect <- time_inftodetect - time_inftotravel
    ## ~ Travel reductions -----------------------------------------------------
    tr_inf_redux <- rep(0, n_sim)
    ## ~ Origin reporting rate -------------------------------------------------
    u_origin <- matrix(rep(input_data$p_report_source, n_sim),
                       nrow=n_sim, byrow = TRUE)

    ## ~ Travel restrictions -----------------------------------------------------
    data("travel-restrictions")

    ## Run Model -------------------------------------------------------------
    input_data <- input_data %>%
        mutate(source = as.character(source),
               destination = as.character(dest))

    ## Filter to sources with cases -- to speed it up
    source_w_cases <- input_data %>%
        filter(!duplicated(paste0(source, t))) %>%
        group_by(source) %>%
        summarise(cum_cases = sum(cases_incid, na.rm=TRUE)) %>%
        filter(cum_cases>0)
    input_data_cases <- input_data %>%
        filter(source %in% source_w_cases$source)
    travel_data_monthly_cases <- travel_data_monthly %>%
        filter(source %in% source_w_cases$source)

    ## SAVE ALL THE DATA NEEDED
    dir.create(file.path("data", project_name))
    write_csv(input_data_cases, file.path("data", project_name, "input_data_cases.csv"))
    write_csv(travel_data_monthly_cases, file.path("data", project_name, "travel_data_monthly.csv"))

    ## The "meanD_mat" here is the distribution of time during which an infected individual could
    ##   potentially travel from a source to a sink/destination. This distribution includes the time
    ##   from infection to isolation/quarantine for detected cases (typically hospitalized/reported),
    ##   travel restriction or a decision not to travel, or for cases with asymptomatic or very mild illness, until recovery.
    ##   This value is drawn from a combination of the other distributions show here.
    meanD_mat <- make_meanD(input_data_cases, n_sim,
                            param_list$incub_mean_log,
                            param_list$incub_sd_log,
                            param_list$inf_period_hosp_shape,
                            param_list$inf_period_hosp_scale,
                            param_list$inf_period_nohosp_mean,
                            param_list$inf_period_nohosp_sd)

    t.start <- proc.time() # start timer to measure this

    ## Run the model
    importation_sim <- run_daily_import_model_par(
        n_sim=n_sim,
        input_data = input_data_cases,
        travel_data_monthly = travel_data_monthly,
        travel_dispersion=travel_dispersion,
        travel_restrictions=travel_restrictions,
        allow_travel_variance=allow_travel_variance,
        meanD_mat=meanD_mat,
        tr_inf_redux=tr_inf_redux,
        u_origin=u_origin,
        get_detection_time=get_detection_time,
        time_inftotravel=time_inftotravel,
        time_inftodetect=time_inftodetect,
        incub_mean_log=param_list$incub_mean_log,
        incub_sd_log=param_list$incub_sd_log,
        inf_period_hosp_shape=param_list$inf_period_hosp_shape,
        inf_period_hosp_scale=param_list$inf_period_hosp_scale,
        inf_period_nohosp_mean=param_list$inf_period_nohosp_mean,
        inf_period_nohosp_sd=param_list$inf_period_nohosp_sd,
        project_name=project_name, batch=batch, version=version,
        print_progress=print_progress,
        cores=cores)

    ## print time required
    print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))

    ## get parameters from sims
    import_pars_df <- calc_nb_import_pars(importation_sim$importation_sim,
                                          project_name,
                                          batch,
                                          version)
    return(list(importation_sim=importation_sim,
                importation_pars=import_pars_df))
}
