
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
##' @return
##'
##' @importFrom truncnorm rtruncnorm
##'
est_imports_base <- function(input_data,
                             tr_inf_redux = 0,
                             meanD,
                             u_origin,
                             allow_travel_variance=FALSE){

    cases <- input_data$cases_incid

    # Get p_s,d,t  (probability of infected individual traveling from d to s during time t
    if (allow_travel_variance){  # if allowing variance in travel, using travelers SE
        Travelers_over_Population_and_days <- truncnorm::rtruncnorm(dim(input_data)[1],
                                                                    mean = input_data$travelers,
                                                                    sd = input_data$travelers_SE,
                                                                    a = 0) / input_data$days_per_t / input_data$population
    } else {
        Travelers_over_Population_and_days <- input_data$travelers / input_data$days_per_t / input_data$population
    }

    # adjust probability by travel probability reduction
    prob_travel_n_detection <- (1-tr_inf_redux) * Travelers_over_Population_and_days

    # Run simulations by day, in case travel likelihood is affected by symptoms on a day to day basis
    this.sim <- rbinom(length(meanD), size = ceiling(meanD) * ceiling(cases / u_origin), prob = prob_travel_n_detection)
    this.sim[is.na(this.sim)] <- 0

    return(this.sim)

}

##'
##' Estimate the detection dates of importations
##'
##' @param sim_res Vector of importation simulation results
##' @param time_inftodetect
##' @param time_inftotravel
##'
##' @import dplyr
##'
est_import_detect_dates <- function(sim_res, time_inftodetect, time_inftotravel){

    # Detection of Cases
    import_dates <- rep(as.Date(sim_res$t), times=sim_res$this.sim)
    detect_sources <- rep(sim_res$source, times=sim_res$this.sim)
    detect_dests <- rep(sim_res$destination, times=sim_res$this.sim)

    # Add detection times to importation dates
    time_dat <- data.frame(time_inftodetect, time_inftotravel) %>%
        dplyr::filter(time_inftodetect>=time_inftotravel & time_inftodetect<20)
    samp <- sample(seq_len(nrow(time_dat)), length(import_dates), replace = TRUE)
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
##' @param tr_inf_redux proportion reduction in travel when individuals are infected
##' @param u_origin reporting rate, origin
##' @param get_detection_time logical; return importation detection or not
##' @param time_inftodetect Time from infection to detection
##' @param project_name project name, if saving in the function
##' @param batch run batch, if saving in the function
##' @param version run version, if saving in the function
##' @param print_progress logical, whether to print the progress of the simulations
##' @param cores number of cores for parallel processing
##' @param time_inftotravel time from infection to traveling
##'
##' @return list consisting of two objects: 1) an array of importations by date, location, and simulation, 2) a dataframe with negative binomial parameters for each location and date
##'
##' @import doParallel dplyr parallel foreach
##' @importFrom abind abind
##' @importFrom reshape2 acast
##' @importFrom arrayhelpers array2df
##'
##' @export
##'
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
                                       project_name=NULL, batch=NULL, version=NULL,
                                       print_progress=TRUE,
                                       cores=4, save_sims=FALSE){

    library(doParallel)
    #library(abind)

    sources_ <- sort(unique(input_data$source))
    dests_ <- sort(unique(input_data$destination))
    t_ <- sort(unique(input_data$t))
    t_detect_ <- seq(as.Date(min(t_)), as.Date(max(t_))+30, by="days") # this might need to increased past 15 days, not sure

    # Sims in longform
    sim <- input_data %>% dplyr::select(source, destination, t) %>% data.table::as.data.table()

    # start the timer
    t.start <- proc.time()

    # make a base set of daily travel, from which we will add to each time
    travel_data_daily <- make_daily_travel(travel_data=travel_data_monthly, travel_dispersion)
    # Make travel restrictions into long, expanded format for easy merging
    travel_restrictions_long <- expand_travel_restrict(travel_restrictions)

    # Sims as multidimensional arrays
    importation_sim <- array(0, dim = c(length(sources_), length(dests_), length(t_), n_sim),
                             dimnames = list(sources_, dests_, as.character(t_), seq_len(n_sim)))

    # make the function to bind each simulation array in the foreach loop
    acomb <- function(...) abind::abind(..., along=4)

    # Set up the cluster for parallelization
    print(paste0("Making a cluster of ", cores," for parallelization."))
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)


    # Run the foreach loop to estimate importations for n simulations
    importation_sim <-
        foreach(n=seq_len(n_sim), .combine = acomb,
                .export=c("make_daily_travel_faster", "apply_travel_restrictions", "est_imports_base"),
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
                    input_data_sim <- input_data %>%
                        dplyr::select(-travelers) %>%
                        left_join(travel_data_daily_ %>%
                                      dplyr::select(source,destination,t,travelers),
                                  by=c("source", "destination", "t"))


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
    dimnames(importation_sim)[[4]] <- seq_len(n_sim)
    # Replace NAs with 0. These are all pairs that did not have travel or cases
    importation_sim[is.na(importation_sim)] <- 0



    importation_detect <- NULL

    # Now lets get detections ........................................

    # If we want detected time of the importations, we generate a 4D array of that here
    if (get_detection_time){

        importation_detect <- array(0, dim = c(length(sources_), length(dests_), length(t_detect_), n_sim),
                                    dimnames = list(sources_, dests_, as.character(t_detect_), seq_len(n_sim)))


        importation_detect <- foreach(n=seq_len(n_sim), .combine = acomb,
                                      .export=c("est_import_detect_dates"),
                                      .packages=c("dplyr","tidyr")) %dopar% {

                                          importation_detect_tmp <- importation_detect[,,,n]

                                          # get a single simulation
                                          this.sim_ <- arrayhelpers::array2df(importation_sim[,,,n])
                                          colnames(this.sim_) <- c("this.sim", "source","destination","t")
                                          this.sim <- this.sim_$this.sim

                                          # Detected Importations
                                          if (sum(this.sim) == 0 ){
                                              NULL    # - if no importations, skip to next
                                          } else {

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

        # Give the sim dimension dimnames
        dimnames(importation_detect)[[4]] <- seq_len(n_sim)
        # Replace NAs with 0. These are all pairs that did not have travel or cases
        importation_detect[is.na(importation_detect)] <- 0
    }

    parallel::stopCluster(cl)



    # Save Sims
    if (save_sims){
        dir.create(file.path("output",project_name), recursive = TRUE, showWarnings = FALSE)
        save(importation_sim, file = file.path("output",project_name, sprintf("covid_importation_sim_%s_batch_v%s.RData", batch, version)))
        if (get_detection_time){
            save(importation_detect, file = file.path("output",project_name, sprintf("covid_importation_detect_%s_batch_v%s.RData", batch, version)))
        }
    }

    print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))
    return(list(importation_sim=importation_sim, importation_detect=importation_detect))

}















##'
##' Get negative binomial estimates for each time and destination
##'
##' @param importation_sim 4D array outputted from the importation model
##' @param cores number of cores to use in parallel. if not parallel, specify 1.
##'
##' @import doParallel parallel
##' @importFrom fitdistrplus fitdist
##'
##' @export
##'
calc_nb_import_pars <- function(importation_sim, cores=4){

    # aggregate to just destination
    import_sim_dests <- apply(importation_sim, 2:4, sum, na.rm=TRUE)

    dests <- dimnames(import_sim_dests)[[1]]
    t <- dimnames(import_sim_dests)[[2]]
    n_t <- length(t)
    n_dest <- length(dests)

    # Make the blank parameter data.frame
    #import_pars_df <- tidyr::expand_grid(destination=dests, t=t, size=1, mu=0)


    # Set up the cluster for parallelization
    print(paste0("Making a cluster of ", cores," for parallelization."))
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)

    # Suppress errors for this loop
    options(show.error.messages = FALSE)

    import_pars_df_all <- foreach(t_=seq_len(length(t)),
                                  .packages = "fitdistrplus",
                                  .combine = "rbind") %dopar% {

                                      import_pars_df <- data.frame(destination=dests, t=t_, size=1, mu=0)

                                      for (d_ in seq_len(length(dests))){

                                          pars_ <- tryCatch ( {
                                              fitdistrplus::fitdist(import_sim_dests[d_, t_, ], distr="nbinom", method="mle")$estimate
                                          },
                                          #warning = function(w) { },
                                          error = function(e) {
                                              c(1,0)
                                          })

                                          import_pars_df[d_, 3:4] <- pars_

                                      }
                                      import_pars_df
                                  }

    # Un-suppress errors
    options(show.error.messages = TRUE)

    parallel::stopCluster(cl)


    # write_csv(import_pars_df, file.path("output",project_name, sprintf("covid_importation_nb_params_%s_batch_v%s.csv", batch, version)))
    return(import_pars_df_all)
}








#' Set up and run importation sims
#'
#' @param dest character string, name of destination to simulate importations for
#' @param dest_type character string, options: "airport", "city", "state", "country"
#' @param dest_country optional character string, specify a higher level destination (i.e. dest_0="USA"), default NULL
#' @param dest_aggr_level character string, level to which travel will be aggregated for destination. Includes "airport", "city", "state", "country", "metro" (only available for CA currently)
#' @param project_name character string
#' @param version character string
#' @param batch character string
#' @param end_date Date, last import date
#' @param n_sim numeric, number of simulations
#' @param cores numeric, number of cores to run in parallel
#' @param n_top_dests Number of destinations to include, ranked by volume; default (Inf) is all.
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
#'   \item \code{inf_period_hosp_mean_log} numeric, the log-mean parameter of a log-normal distribution for the infectious period for time to hospitalization
#'   \item \code{inf_period_hosp_sd_log} numeric, the log-sd parameter of a log-normal distribution for the infectious period for time to hospitalization
#'   \item \code{p_report_source} numeric vector of length 2, currently the probability of reporting by source with the first indicating Hubei reporting and second indicating everywhere else (UPDATE WITH THESE https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html)
#'   \item \code{shift_incid_days} lag from infection to report
#'   \item \code{delta} days per time period
#' }
#'
#' @return
#'
#' @import dplyr
#'
#' @export
#'
setup_and_run_importations <- function(dest="UT",
                                       dest_type=c("state"), #,"city","airport", "country"),
                                       dest_country="USA",
                                       dest_aggr_level=c("airport"), #, "city", "state", "country", "metro"),
                                       project_name="Utah_import",
                                       version="global",
                                       batch="1st",
                                       first_date = ISOdate(2019,12,1),
                                       last_date = Sys.time(),
                                       update_case_data=TRUE,
                                       case_data_dir = "data/case_data",
                                       check_saved_data=TRUE,
                                       save_case_data=TRUE,
                                       get_travel=TRUE,
                                       n_sim=100,
                                       cores=4,
                                       n_top_dests=Inf,
                                       get_detection_time=FALSE,
                                       travel_dispersion=3,
                                       allow_travel_variance=FALSE,
                                       print_progress=TRUE,
                                       param_list=list(incub_mean_log=log(5.89),
                                                       incub_sd_log=log(1.74),
                                                       inf_period_nohosp_mean=15,
                                                       inf_period_nohosp_sd=5,
                                                       inf_period_hosp_mean_log=1.23,
                                                       inf_period_hosp_sd_log=0.79,
                                                       p_report_source=c(0.05, 0.25),
                                                       shift_incid_days=-10,
                                                       delta=1)){
    ## >>> NOT SURE WHAT THESE ARE HERE FOR
    # dest_type <- match.arg(dest_type)
    # dest_aggr_level <- match.arg(dest_aggr_level)

    # ## Create needed directories
    # dir.create(file.path("output",project_name), recursive = TRUE, showWarnings = FALSE)
    # dir.create(file.path("data",project_name), recursive = TRUE, showWarnings = FALSE)
    # dir.create(file.path("figures",project_name), recursive = TRUE, showWarnings = FALSE)

    ## DATA
    ## ~ Incidence data
    # OLD VERSION
    # incid_data_list <- get_incidence_data(first_date = first_date,
    #                                       last_date = last_date,
    #                                       update_case_data = update_case_data,
    #                                       case_data_dir = case_data_dir,
    #                                       check_saved_data = check_saved_data,
    #                                       save_data = save_case_data)
    # 
    # incid_data <- incid_data_list$incid_data %>% dplyr::filter(source != "USA")
    # incid_data <- incid_data %>% rename(incid_est = cases_incid)
    # jhucsse <- incid_data_list$jhucsse_case_data
    # jhucsse_state <- incid_data_list$jhucsse_case_data_state
    
    incid_data <- get_incidence_fits(aggr_level = "source",
                                     first_date = first_date,
                                     last_date = last_date,
                                     case_data_dir = case_data_dir,
                                     save_raw_data = save_case_data,
                                     us_data_only=FALSE)
    
    incid_data <- incid_data %>% dplyr::filter(source != "USA") %>%
      rename(incid_est = cases_incid)
    readr::write_csv(incid_data, file.path(case_data_dir, "incidence_fits.csv"))
    
    ## ~ Travel Data
    ## if travel data exists load it, otherwise download it
    if(get_travel) {
        travel_data_monthly <- get_oag_travel(destination=dest,
                                              destination_type=dest_type,
                                              dest_country=dest_country,
                                              dest_aggr_level=dest_aggr_level) %>% as.data.frame()
        travel_data_monthly <- travel_data_monthly %>%
            dplyr::mutate(t_year=2020) %>%
            dplyr::rename(source = dep_loc_aggr)
        travel_data_monthly$destination <- travel_data_monthly[,paste0("arr_", dest_aggr_level),drop=T]

    } else{
        travel_data_monthly <- paste0("data/", paste(dest, collapse = "+"), "-", dest_aggr_level, "_oag_20172019.csv") %>%
            readr::read_csv(na=c(""," ","NA"))%>%
            dplyr::mutate(t_year=2020) %>%
            dplyr::mutate(travelers=ifelse(t_month == "01" & dep_country=="CHN",
                                           # Increase travel for Chinese New Year
                                           travelers*1.6, travelers)) %>%
            dplyr::rename(source = dep_loc_aggr)
        travel_data_monthly$destination <- travel_data_monthly[,paste0("arr_", dest_aggr_level),drop=T]
    }

    ## monthly average totals into destinations
    travel_mean <- travel_data_monthly %>%
        dplyr::group_by(destination, t_month) %>%
        dplyr::summarise(travelers = sum(travelers_mean,na.rm=TRUE)) %>%
        dplyr::group_by(destination) %>%
        dplyr::summarise(travelers = mean(travelers)) %>%
        dplyr::arrange(desc(travelers))

    # Destinations to keep
    dests_keep <- travel_mean$destination[seq_len(min(c(nrow(travel_mean), n_top_dests)))]
    travel_data_monthly <- travel_data_monthly %>% dplyr::filter(destination %in% dests_keep)

    ## Travel data
    ##  - Get daily for merging purposes
    travel_data <- make_daily_travel(travel_data_monthly, travel_dispersion=3)

    ## ~ Population Data
    data(pop_data, package="covidImportation")

    ## ~~ First Check that the variables match up
    # Check that incidence data does not have duplicates
    incid_dups <- sum(incid_data %>%
                          dplyr::mutate(source_t = paste(source, t)) %>%
                          dplyr::mutate(dup_entry=duplicated(source_t)) %>%
                          dplyr::pull(dup_entry))
    if(sum(incid_dups)>0){
        dup_entry <- incid_data %>%
            dplyr::mutate(source_t = paste(source, t)) %>%
            dplyr::mutate(dup_entry = duplicated(source_t)) %>%
            dplyr::filter(dup_entry) %>% dplyr::pull(source_t)
        warning("There are duplicate entries in the incidence data.")
    }
    ## Check travel data
    travel_dups <- travel_data %>%
        dplyr::mutate(source_dest_t = paste(source, destination, t),
                      dup_entry = duplicated(source_dest_t)) %>%
        dplyr::pull(dup_entry) %>%
        sum()
    if(sum(travel_dups)>0){
        warning("There are duplicate entries in the travel data.")
    }
    ## Check Population data
    pop_dups <- pop_data %>%
        dplyr::mutate(dup_entry = duplicated(source)) %>%
        dplyr::pull(dup_entry) %>%
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

    ## ~~ Merge it all
    input_data <- make_input_data(incid_data, travel_data, pop_data,
                                  shift_incid_days=param_list$shift_incid_days,
                                  dest_aggr_level=dest_aggr_level) %>%
        dplyr::mutate(p_report_source=ifelse(source=="Hubei",
                                             param_list$p_report_source[1],
                                             param_list$p_report_source[2]),
                      # For first pass, reporting rate is just Hubei/not Hubei
                      days_per_t=param_list$delta # ~ delta: days per time period
        ) %>%
        dplyr::filter(t<=as.Date(last_date))

    ## save final input data
    # write_csv(input_data,
    #           file.path("data", project_name,
    #                     sprintf("input_data_%s_batch_v%s.RData", batch, version)))

    ## ~ Time to detect importations
    ## -- If we assume people generally depart at some point during their incubation period,
    ##     or very early in the symptomatic phase,
    ##     we can generate a distribution of time from travel to detection.
    ## -- because we are only worried about those who are detected, we can ignore time to recover
    time_inftodetect <- rlnorm(10000, mean = param_list$incub_mean_log,
                               sd = param_list$incub_sd_log) +
        rlnorm(10000, meanlog=param_list$inf_period_hosp_mean_log,
               sdlog=param_list$inf_period_hosp_sd_log)

    ## We assume people can and do travel during their incubation period and
    ##  during that period during which symptoms are still minor.
    ##  There are reports of travelers taking fever-reducers and a portion dont show fever
    ## We assume this is uniform
    time_inftotravel <- sapply(time_inftodetect, runif, n=1, min=0)
    #time_traveltodetect <- time_inftodetect - time_inftotravel

    ## ~ Travel restrictions
    data("travel_restrictions")

    input_data <- input_data %>%
        dplyr::mutate(source = as.character(source),
                      destination = as.character(destination))

    ## Filter to sources with cases -- to speed it up
    source_w_cases <- input_data %>%
        dplyr::filter(!duplicated(paste0(source, t))) %>%
        dplyr::group_by(source) %>%
        dplyr::summarise(cum_cases = sum(cases_incid, na.rm=TRUE)) %>%
        dplyr::filter(cum_cases>0)
    input_data <- input_data %>%
        dplyr::filter(source %in% source_w_cases$source)
    travel_data_monthly <- travel_data_monthly %>%
        dplyr::filter(source %in% source_w_cases$source)


    ## ~ Travel reductions
    tr_inf_redux <- rep(0, n_sim)

    ## ~ Origin reporting rate
    u_origin <- matrix(rep(input_data$p_report_source, n_sim),
                       nrow=n_sim, byrow = TRUE)

    ## SAVE ALL THE DATA NEEDED
    # dir.create(file.path("data", project_name))
    # write_csv(input_data, file.path("data", project_name, "input_data.csv"))
    # write_csv(travel_data_monthly, file.path("data", project_name, "travel_data_monthly.csv"))

    ## The "meanD_mat" here is the distribution of time during which an infected individual could
    ##   potentially travel from a source to a sink/destination. This distribution includes the time
    ##   from infection to isolation/quarantine for detected cases (typically hospitalized/reported),
    ##   travel restriction or a decision not to travel, or for cases with asymptomatic or very mild illness, until recovery.
    ##   This value is drawn from a combination of the other distributions show here.
    meanD_mat <- make_meanD(input_data, n_sim,
                            param_list$incub_mean_log,
                            param_list$incub_sd_log,
                            param_list$inf_period_hosp_mean_log,
                            param_list$inf_period_hosp_sd_log,
                            param_list$inf_period_nohosp_mean,
                            param_list$inf_period_nohosp_sd)

    #t.start <- proc.time() # start timer to measure this

    ## Run the model
    importation_sim <- run_daily_import_model_par(
        n_sim=n_sim,
        input_data = input_data,
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
        project_name=project_name, batch=batch, version=version,
        print_progress=print_progress,
        cores=cores, save_sims=FALSE)


    # ## print time required
    # print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))

    return(list(input_data=input_data_cases,
                #travel_data_monthly=travel_data_monthly,
                importation_sims=importation_sim,
                airport_monthly_mean_travelers=travel_mean))

}













#' Set up importation sims
#'
#' @param dest character string, name of destination to simulate importations for
#' @param dest_type character string, options: "airport", "city", "state", "country"
#' @param dest_country optional character string, specify a higher level destination (i.e. dest_0="USA"), default NULL
#' @param dest_aggr_level character string, level to which travel will be aggregated for destination. Includes "airport", "city", "state", "country", "metro" (only available for CA currently)
#' @param first_date Date, first import date
#' @param last_date Date, last import date
#' @param output_dir where output files are saved
#' @param save_case_data Whether to save the JHUCSSE raw data
#' @param get_travel whether to load or pull travel data
#' @param n_top_dests Number of destinations to include, ranked by volume; default (Inf) is all.
#' @param travel_dispersion numeric
#' @param param_list list, with the following elements
#' \itemize{
#'   \item \code{p_report_source} numeric vector of length 2, currently the probability of reporting by source with the first indicating Hubei reporting and second indicating everywhere else (UPDATE WITH THESE https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html)
#'   \item \code{shift_incid_days} lag from infection to report
#'   \item \code{delta} days per time period
#' }
#' @param check_errors logical
#'
#' @return
#'
#' @import dplyr
#'
#' @export
#'
setup_importations <- function(dest="UT",
                               dest_type=c("state"), #,"city","airport", "country"),
                               dest_country="USA",
                               dest_aggr_level=c("airport"), #, "city", "state", "country", "metro"),
                               first_date = ISOdate(2019,12,1),
                               last_date = Sys.time(),
                               output_dir = file.path("output", paste0(paste(dest, collapse="+"),"_", as.Date(Sys.Date()))),
                               save_case_data=TRUE,
                               get_travel=TRUE,
                               n_top_dests=Inf,
                               travel_dispersion=3,
                               param_list=list(p_report_source=c(0.05, 0.25),
                                               shift_incid_days=-10,
                                               delta=1),
                               check_errors = TRUE){

    ## Create needed directories
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    ## DATA
    ## ~ Incidence data
  
  # OLD VERSION
    # incid_data_list <- get_incidence_data(first_date = first_date,
    #                                       last_date = last_date,
    #                                       update_case_data = update_case_data,
    #                                       case_data_dir = case_data_dir,
    #                                       check_saved_data = check_saved_data,
    #                                       save_data = save_case_data)
    # 
    # incid_data <- incid_data_list$incid_data %>% dplyr::filter(source != "USA")
    # incid_data <- incid_data %>% rename(incid_est = cases_incid)
    # jhucsse <- incid_data_list$jhucsse_case_data
    # jhucsse_state <- incid_data_list$jhucsse_case_data_state
    
    incid_data <- get_incidence_fits(aggr_level = "source",
                                     first_date = first_date,
                                     last_date = last_date,
                                     case_data_dir = output_dir,
                                     save_raw_data = save_case_data,
                                     us_data_only=FALSE)

    incid_data <- incid_data %>% dplyr::filter(source != "USA") %>% 
      rename(incid_est = cases_incid)
    readr::write_csv(incid_data, file.path(output_dir, "incidence_fits.csv"))

    print("Successfully pulled and cleaned case data.")

    ## ~ Travel Data
    ## if travel data exists load it, otherwise download it
    if(get_travel) {
        travel_data_monthly <- get_oag_travel(destination=dest,
                                              destination_type=dest_type,
                                              dest_country=dest_country,
                                              dest_aggr_level=dest_aggr_level) %>% as.data.frame()
        travel_data_monthly <- travel_data_monthly %>%
            dplyr::mutate(t_year=2020) %>%
            dplyr::rename(source = dep_loc_aggr)
        travel_data_monthly$destination <- travel_data_monthly[,paste0("arr_", dest_aggr_level),drop=T]
    } else{
        travel_data_monthly <- paste0("data/", paste(dest, collapse = "+"), "-",
                                      dest_aggr_level, "_oag_20172019.csv") %>%
            readr::read_csv(na=c(""," ","NA"))%>%
            dplyr::mutate(t_year=2020) %>%
            dplyr::mutate(travelers=ifelse(t_month == "01" & dep_country=="CHN",
                                           # Increase travel for Chinese New Year
                                           travelers*1.6, travelers)) %>%
            dplyr::rename(source = dep_loc_aggr)
        travel_data_monthly$destination <- travel_data_monthly[,paste0("arr_", dest_aggr_level),drop=T]
    }

    ## monthly average totals into destinations
    travel_mean <- travel_data_monthly %>%
        dplyr::group_by(destination, t_month) %>%
        dplyr::summarise(travelers = sum(travelers_mean,na.rm=TRUE)) %>%
        dplyr::group_by(destination) %>%
        dplyr::summarise(travelers = mean(travelers)) %>%
        dplyr::arrange(desc(travelers))

    print("Successfully set up travel data.")
    

    # Destinations to keep
    if (!is.infinite(n_top_dests)){
      dests_keep <- travel_mean$destination[seq_len(min(c(nrow(travel_mean), n_top_dests)))]
      travel_data_monthly <- travel_data_monthly %>% dplyr::filter(destination %in% dests_keep)
    }
    
    ## Travel data
    ##  - Get daily for merging purposes
    travel_data_daily <- covidImportation:::make_daily_travel(travel_data_monthly, travel_dispersion=travel_dispersion)

    ## ~ Population Data
    data(pop_data, package="covidImportation")

    ## ~~ First Check that the variables match up
    if(check_errors){
      # Check that incidence data does not have duplicates
      incid_dups <- sum(incid_data %>%
                            dplyr::mutate(source_t = paste(source, t)) %>%
                            dplyr::mutate(dup_entry=duplicated(source_t)) %>%
                            dplyr::pull(dup_entry))
      if(sum(incid_dups)>0){
          dup_entry <- incid_data %>%
              dplyr::mutate(source_t = paste(source, t)) %>%
              dplyr::mutate(dup_entry=duplicated(source_t)) %>%
              dplyr::filter(dup_entry) %>% dplyr::pull(source_t)
          warning("There are duplicate entries in the incidence data.")
      }
      ## Check travel data
      travel_dups <- travel_data_daily %>%
          dplyr::mutate(source_dest_t = paste(source, destination, t),
                        dup_entry=duplicated(source_dest_t)) %>%
          dplyr::pull(dup_entry) %>%
          sum()
      if(sum(travel_dups)>0){
          warning("There are duplicate entries in the travel data.")
      }
      ## Check Population data
      pop_dups <- pop_data %>%
          dplyr::mutate(dup_entry=duplicated(source)) %>%
          dplyr::pull(dup_entry) %>%
          sum()
      if(sum(pop_dups)>0){
          warning("There are duplicate entries in the population data.")
      }
    }
    # we really just need to make sure there are travel data and pop data for all source locations with incidence
    # incid_sources <- sort(unique(incid_data$source))
    # travel_sources <- sort(unique(travel_data_daily$source))
    # pop_sources <- sort(unique(pop_data$source))
    # incid_sources[!(incid_sources %in% travel_sources)]
    # incid_sources[!(incid_sources %in% pop_sources)]

    ## ~~ Merge it all
    input_data <- covidImportation:::make_input_data(incid_data, travel_data_daily, pop_data,
                                                     shift_incid_days=param_list$shift_incid_days,
                                                     dest_aggr_level=dest_aggr_level) %>%
        dplyr::mutate(p_report_source=ifelse(source=="Hubei",
                                             param_list$p_report_source[1],
                                             param_list$p_report_source[2]),
                      # For first pass, reporting rate is just Hubei/not Hubei
                      days_per_t=param_list$delta # ~ delta: days per time period
        ) %>%
        dplyr::filter(t<=as.Date(last_date)) %>%
        dplyr::mutate(source = as.character(source),
                      destination = as.character(destination))

    print("Successfully set up combined input data.")
    
    
    ## Filter to sources with cases -- to speed it up
    source_w_cases <- input_data %>%
        dplyr::filter(!duplicated(paste0(source, t))) %>%
        dplyr::group_by(source) %>%
        dplyr::summarise(cum_cases = sum(cases_incid, na.rm=TRUE)) %>%
        dplyr::filter(cum_cases>0)
    input_data <- input_data %>%
        dplyr::filter(source %in% source_w_cases$source)
    travel_data_monthly <- travel_data_monthly %>%
        dplyr::filter(source %in% source_w_cases$source)
    travel_data_daily <- travel_data_daily %>%
        dplyr::filter(source %in% source_w_cases$source)


    # save the data that we will pass to the model
    data.table::fwrite(input_data, file.path(output_dir, "input_data.csv"))
    data.table::fwrite(travel_data_monthly, file.path(output_dir, "travel_data_monthly.csv"))
    data.table::fwrite(travel_mean, file.path(output_dir, "travel_mean.csv"))
    data.table::fwrite(travel_data_daily, file.path(output_dir, "travel_data_daily.csv"))

    print(paste0("Input and Travel data setup successfully and saved in ", output_dir, "."))
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
##' @param tr_inf_redux proportion reduction in travel when individuals are infected
##' @param u_origin reporting rate, origin
##' @param get_detection_time logical; return importation detection or not
##' @param time_inftodetect Time from infection to detection
##' @param incub_mean_log log mean incubation period
##' @param incub_sd_log log sd of incubation period
##' @param inf_period_hosp_mean_log infectious period of hospitalized, log-mean
##' @param inf_period_hosp_sd_log infectious period of hospitalized, log-sd
##' @param inf_period_nohosp_mean infectious period of non-hospitalized, shape
##' @param inf_period_nohosp_sd infectious period of non-hospitalized, scale
##' @param project_name project name, if saving in the function
##' @param batch run batch, if saving in the function
##' @param version run version, if saving in the function
##' @param print_progress logical, whether to print the progress of the simulations
##' @param cores number of cores for parallel processing
##' @param time_inftotravel time from infection to traveling
##'
##' @return list consisting of two objects: 1) an array of importations by date, location, and simulation, 2) a dataframe with negative binomial parameters for each location and date
##'
##' @import dplyr
##'
##' @export
##'
run_daily_import_model <- function(input_data,
                                   travel_data_monthly,
                                   travel_data_daily,
                                   travel_dispersion=3,
                                   travel_restrictions=NULL,
                                   allow_travel_variance=FALSE,
                                   tr_inf_redux=0,
                                   get_detection_time=FALSE,
                                   param_list=list(incub_mean_log=log(5.89),
                                                   incub_sd_log=log(1.74),
                                                   inf_period_nohosp_mean=15,
                                                   inf_period_nohosp_sd=5,
                                                   inf_period_hosp_mean_log=1.23,
                                                   inf_period_hosp_sd_log=0.79)
){

    # start the timer
    t.start <- proc.time()

    ## ~ Origin reporting rate
    u_origin <- matrix(rep(input_data$p_report_source, 1),
                       nrow=1, byrow = TRUE)

    ## The "meanD_mat" here is the distribution of time during which an infected individual could
    ##   potentially travel from a source to a sink/destination. This distribution includes the time
    ##   from infection to isolation/quarantine for detected cases (typically hospitalized/reported),
    ##   travel restriction or a decision not to travel, or for cases with asymptomatic or very mild illness, until recovery.
    ##   This value is drawn from a combination of the other distributions show here.
    meanD_mat <- make_meanD(input_data, 1,
                            param_list$incub_mean_log,
                            param_list$incub_sd_log,
                            param_list$inf_period_hosp_mean_log,
                            param_list$inf_period_hosp_sd_log,
                            param_list$inf_period_nohosp_mean,
                            param_list$inf_period_nohosp_sd)


    # Make travel restrictions into long, expanded format for easy merging
    if(is.null(travel_restrictions)){
        data("travel_restrictions")
    }
    travel_restrictions_long <- expand_travel_restrict(travel_restrictions)

    # Simulate the daily travel from monthly
    travel_data_daily <- make_daily_travel_faster(travel_data=travel_data_monthly,
                                                  travel_data_daily=travel_data_daily,
                                                  travel_dispersion=travel_dispersion)

    # Apply travel restrictions
    travel_data_daily <- apply_travel_restrictions(travel_data=travel_data_daily,
                                                   travel_restrictions_long=travel_restrictions_long)

    # Join sampled travel data back into input data
    small_data_daily <- travel_data_daily %>%
                            dplyr::select(source, destination, t, travelers) %>%
                            dplyr::filter(destination %in% unique(input_data$destination) &
                                          source %in% unique(input_data$source) &
                                          t %in% unique(input_data$t))
    # TODO: arrange to make sure small_data_daily and input_data are in order
    input_data_sim <- input_data %>% dplyr::select(-travelers)
    input_data_sim$travelers = small_data_daily$travelers


    # Run base model to estimate the number of importations during this simulation
    this.sim <- est_imports_base(input_data = input_data_sim,
                                 tr_inf_redux = tr_inf_redux,
                                 meanD = meanD_mat,
                                 u_origin = u_origin,
                                 allow_travel_variance=allow_travel_variance)

    ## Estimate dates of importation and detection of the simulated importations
    importation_sim <- data.frame(input_data %>% dplyr::select(source, destination, t), this.sim)


    # Now lets get detections ........................................

    # If we want detected time of the importations, we generate a 4D array of that here
    if (get_detection_time){

        ## ~ Time to detect importations
        ## -- If we assume people generally depart at some point during their incubation period,
        ##     or very early in the symptomatic phase,
        ##     we can generate a distribution of time from travel to detection.
        ## -- because we are only worried about those who are detected, we can ignore time to recover
        time_inftodetect <- rlnorm(10000, mean = param_list$incub_mean_log,
                                   sd = param_list$incub_sd_log) +
            rlnorm(10000, meanlog=param_list$inf_period_hosp_mean_log,
                   sdlog=param_list$inf_period_hosp_sd_log)

        ## We assume people can and do travel during their incubation period and
        ##  during that period during which symptoms are still minor.
        ##  There are reports of travelers taking fever-reducers and a portion dont show fever
        ## We assume this is uniform
        time_inftotravel <- sapply(time_inftodetect, runif, n=1, min=0)
        #time_traveltodetect <- time_inftodetect - time_inftotravel


        # - if no importations, skip to next
        if (sum(this.sim) == 0 ){
            importation_detect <- NULL
        } else{

            import_dates_ <- est_import_detect_dates(importation_sim, time_inftodetect, time_inftotravel)

            importation_detect <- data.frame(source=import_dates_$detect_sources,
                                             destination=import_dates_$detect_dests,
                                             t = as.character(as.Date(import_dates_$detect_dates))) %>%
                group_by(source, destination, t) %>% summarise(this.sim = n())
        }

        return(list(importation_sim=importation_sim,
                    importation_detect=importation_detect))

    } else {

        return(importation_sim)
    }

    # # Save Sims
    # if (save_sims){
    #   dir.create(file.path("output",project_name), recursive = TRUE, showWarnings = FALSE)
    #   write_csv(importation_sim, file = file.path("output",project_name, sprintf("covid_importation_sim_%s_batch_v%s.RData", batch, version)))
    #   if (get_detection_time){
    #     save(importation_detect, file = file.path("output",project_name, sprintf("covid_importation_detect_%s_batch_v%s.RData", batch, version)))
    #   }
    # }
    # print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))

}








#' Run importation sims
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
#' @param n_top_dests Number of destinations to include, ranked by volume; default (Inf) is all.
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
#'   \item \code{inf_period_hosp_mean_log} numeric, the log-mean parameter of a log-normal distribution for the infectious period for time to hospitalization
#'   \item \code{inf_period_hosp_sd_log} numeric, the log-sd parameter of a log-normal distribution for the infectious period for time to hospitalization
#'   \item \code{p_report_source} numeric vector of length 2, currently the probability of reporting by source with the first indicating Hubei reporting and second indicating everywhere else (UPDATE WITH THESE https://cmmid.github.io/topics/covid19/severity/global_cfr_estimates.html)
#'   \item \code{shift_incid_days} lag from infection to report
#'   \item \code{delta} days per time period
#' }
#' @param drop_zeros Whether to drop zeros from results to speed up and reduce file sizes
#' @param file_nums File numbers to input if the simualation got interrupted
#'
#' @return
#'
#' @import doParallel dplyr parallel foreach
#' @importFrom readr read_csv write_csv
#'
#' @export
#'
run_importations <- function(n_sim=100,
                             cores=5,
                             get_detection_time=FALSE,
                             travel_dispersion=3,
                             allow_travel_variance=FALSE,
                             print_progress=TRUE,
                             output_dir = file.path("output", paste0(paste(dest, collapse="+"),"_", as.Date(Sys.Date()))),
                             param_list=list(incub_mean_log=log(5.89),
                                             incub_sd_log=log(1.74),
                                             inf_period_nohosp_mean=15,
                                             inf_period_nohosp_sd=5,
                                             inf_period_hosp_mean_log=1.23,
                                             inf_period_hosp_sd_log=0.79,
                                             p_report_source=c(0.05, 0.25)),
                             drop_zeros = FALSE,
                             file_nums = NA){

    t.start <- proc.time() # start timer to measure this

    # Set up the cluster for parallelization
    print(paste0("Making a cluster of ", cores," for parallelization."))
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    # setup file numbers
    if (is.na(file_nums)){
      file_nums <- seq_len(n_sim)
    }  
    # Run the foreach loop to estimate importations for n simulations
    foreach(n=file_nums, 
            .export=c("make_daily_travel_faster", 
                      "apply_travel_restrictions", 
                      "est_imports_base", 
                      "run_daily_import_model"), 
            .packages=c("dplyr","tidyr")) %dopar% {

      if (print_progress){
          if (n %% 10 == 0) print(paste('sim', n, 'of', n_sim, sep = ' '))
      }

      if (!exists("input_data")) {
        print("Reading input data")
	      input_data <<- readr::read_csv(file.path(output_dir, "input_data.csv"))
        travel_data_monthly <<- readr::read_csv(file.path(output_dir, "travel_data_monthly.csv"))
        travel_data_daily <<- readr::read_csv(file.path(output_dir, "travel_data_daily.csv"))
	    }

      ## ~ Travel restrictions
      data("travel_restrictions")

      import_est_run <- run_daily_import_model(
          input_data,
          travel_data_monthly,
          travel_data_daily,
          travel_dispersion=travel_dispersion,
          travel_restrictions=travel_restrictions,
          allow_travel_variance=allow_travel_variance,
          tr_inf_redux=0,
          get_detection_time=get_detection_time,
          param_list=param_list)
      
      if(drop_zeros){
        import_est_run <- import_est_run %>% dplyr::filter(this.sim>0)
      }

      n_str = stringr::str_pad(as.character(n), width=9, pad="0")
      if(get_detection_time){
          data.table::fwrite(import_est_run$importation_sim, file.path(output_dir, paste0(n_str,".imps.csv")))
          data.table::fwrite(import_est_run$importation_detect, file.path(output_dir, paste0(n_str,".impd.csv")))
      } else {
          data.table::fwrite(import_est_run, file.path(output_dir, paste0(n_str,".imps.csv")))
      }
      #clear the garbage
      gc()
    	# Null return value here
    	NULL
    }

    parallel::stopCluster(cl)

    ## print time required
    print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))

}







