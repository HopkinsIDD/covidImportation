


# BUILD THE INFECTED POPULATION FROM REPORTED CASES -----------------------

# get cases by day
# - X cases are reported today, that means these cases have gone through 
#     (1) full infectious periods
#     (2) some time from onset to detection, which likely mirrors onset to hospitalization
# - These X cases also represent some number X_real, which depends on the detection and reporting rate
# - If we assume the daily cohort of reported cases represents individuals who follow the above distributions, 
#    we can randomly assign them time to reporting, thus time previously at risk of travel
# - Additionally, X_real are able to travel for even longer time going forward, as they are not detected/quarantined


##' 
##' Build the infecteds population
##' 
##' @param incid_data Incidence data 
##' @param incub_mean_log 
##' 
##' 
build_infecteds <- function(incid_data, 
                            incub_mean_log, incub_sd_log, 
                            inf_period_hosp_shape, inf_period_hosp_scale){

    sources_ <- sort(unique(incid_data$source))
    case_dat_all <- NULL
    
    for (s in 1:length(sources_)){
        data_ <- incid_data %>% filter(source==sources_[s])
        
        # First -- Detected case time at risk (this is only looking backwards)
        
        data_ <- data_ %>% arrange(desc(t))
        cases_ <- data_$cases_incid
        #case_reconst <- data.frame(days_prior = integer(0), cases=integer(0), date_inf=as.Date(integer(0), origin = "1970-01-01"))
        case_reconst <- data.frame(date = as.Date(integer(0), origin = "1970-01-01"), 
                                   cases_inf = integer(0), 
                                   cases_curr = integer(0))
        
        for (d in 1:nrow(data_)){
            day_detect <- data_$t[d]
            time_to_report_detected <- ceiling(exp(rnorm(cases_[d], mean = incub_mean_log, sd = incub_sd_log)) +
                                                   rgamma(cases_[d], inf_period_hosp_shape, inf_period_hosp_scale))
            #MCMCglmm::rtnorm(cases_[d], inf_period_nohosp_mean, inf_period_nohosp_sd, lower=0))
            tmp_ <- table(time_to_report_detected)
            if(length(tmp_)==0) next
            tmp_cases <- data.frame(date_detect = day_detect,
                                    days_prior = as.integer(names(tmp_)), 
                                    cases = as.integer(tmp_), 
                                    date_inf = day_detect-as.integer(names(tmp_)))
            
            # need to add back day of data
            tmp_currcases <- data.frame(date = rev(seq(as.Date(min(tmp_cases$date_inf)), as.Date(day_detect), "days")))
            tmp_currcases <- full_join(tmp_currcases, tmp_cases %>% select(date=date_inf, cases), by="date") %>%
                mutate(cases = ifelse(is.na(cases), 0, cases))
            tmp_currcases <- tmp_currcases %>% arrange(date) %>% mutate(cases_curr = cumsum(cases))
            
            # Merge them
            tmp_cases <- full_join(tmp_cases %>% select(date=date_inf, cases_inf=cases),
                                   tmp_currcases %>% select(date, cases_curr), by="date")
            case_reconst <- bind_rows(case_reconst, tmp_cases)
        }
        
        case_reconst <- case_reconst %>% group_by(date) %>% 
            summarise(cases_inf = sum(cases_inf, na.rm = TRUE),
                      cases_curr = sum(cases_curr, na.rm = TRUE))
        
        case_dat <- full_join(data_ %>% rename(cases_rep = cases_incid), 
                              case_reconst, by=c("t"="date")) %>%
            mutate(source=data_$source[1], country=data_$country[1])
        
        case_dat <- case_dat %>% arrange(source, desc(t))
        
        
        # ~ Construct missing cases -----------------------------------------------
        # Now use the infections and reporting rate to fill in the missed cases moving forward
        
        # sample from the reporting rate distribution for each day --> this could be made to be time varying easily
        #p_report_ <- MCMCglmm::rtnorm(nrow(case_dat), data_$p_report_source[1], .1, lower=0.025, upper=.95)
        p_report_ <- rep(data_$p_report_source[1], nrow(case_dat))
        hist(p_report_, breaks=20)
        
        # calc total cases that were infected on that day, and cases infected that will not be detected
        case_dat$cases_inf_tot = round(case_dat$cases_inf / p_report_)
        case_dat$cases_inf_no_detect = round(case_dat$cases_inf_tot - case_dat$cases_inf)
        
        case_inf_ <- case_dat$cases_inf_no_detect
        case_curr_no_hosp <- data.frame(date = as.Date(integer(0), origin = "1970-01-01"), 
                                        cases_curr = integer(0))
        
        # Generate a set of days during which infected, unreported cases remain infected (during which they can travel)
        for (d in 1:nrow(case_dat)){
            if (is.na(case_inf_[d]) | case_inf_[d]==0) next
            
            tmp_ <- round(exp(rnorm(case_inf_[d], mean = incub_mean_log, sd = incub_sd_log)) + 
                              MCMCglmm::rtnorm(case_inf_[d], inf_period_nohosp_mean, inf_period_nohosp_sd, lower=0)) + case_dat$t[d]
            tmp2 <- table(tmp_)
            
            tmpdat <- data.frame(date = as.Date(names(tmp2)), cases_curr = as.integer(tmp2)) %>% arrange(desc(date))
            tmpdat <- full_join(tmpdat, data.frame(date = seq(as.Date(case_dat$t[d]), as.Date(max(tmpdat$date)), by="days")), by="date") %>% arrange(date)
            tmpdat <- tmpdat %>% mutate(cases_curr = ifelse(is.na(cases_curr), 0, cases_curr)) %>% mutate(cases_curr_cum = rev(cumsum(rev(cases_curr))))
            tmpdat <- tmpdat %>% mutate(cases_curr = cases_curr_cum - cases_curr) %>% select(-cases_curr_cum)
            case_curr_no_hosp <- bind_rows(case_curr_no_hosp, tmpdat)    
        }
        
        case_curr_no_hosp <- case_curr_no_hosp %>% group_by(date) %>% 
            summarise(cases_curr = sum(cases_curr, na.rm=TRUE)) %>% arrange(desc(date))
        
        
        # ~ Combine All Projected Cases -------------------------------------------
        
        case_dat_total <- full_join(case_dat %>% rename(cases_inf_rep = cases_inf, cases_curr_rep = cases_curr),
                                    case_curr_no_hosp %>% rename(cases_curr_norep = cases_curr) %>%
                                        mutate(source=case_dat$source[1]),
                                    by = c("t"="date", "source")) 
        
        case_dat_total <- case_dat_total %>% mutate(t = as.Date(t)) %>% arrange(source, desc(t))
        case_dat_total$epiweek <- lubridate::epiweek(case_dat_total$t)
        case_dat_total <- case_dat_total %>% mutate(cases_curr_total = cases_curr_norep + cases_curr_rep)
        case_dat_total <- case_dat_total %>% mutate(projection_type = ifelse(t > max(as.Date(case_dat$t)), "future", "current"))
        
        
        # ~ Plot the contructed cases ---------------------------------------------
        
        # last_inf <- max(as.Date(case_dat_total$t)[!is.na(case_dat_total$cases_inf_tot) & case_dat_total$cases_inf_tot>0])
        # ggplot(case_dat_total, aes(x=t, alpha=projection_type)) +
        #     geom_line(aes(y=cases_curr_norep), col="red") +
        #     geom_line(aes(y=cases_curr_rep), col="blue") +
        #     geom_vline(xintercept = last_inf, color="grey") + # last infection date
        #     coord_cartesian(xlim=c(as.Date("2020-01-01"), as.Date("2020-02-15"))) +
        #     scale_alpha_manual(values=c(1, 0.5))
        
        case_dat_all <- bind_rows(case_dat_all, case_dat_total)
    }
    return(case_dat_all)
}







# DAILY IMPORTATION MODEL - TIME AND LOCATION  -------------------------------------------------


#TESTING
# 
# input_data
# travel_data_monthly
# travel_dispersion=3
# travel_restrictions=data.frame(loc="Hubei", min=hubei_shutdown, max="2020-04-01")
# allow_travel_variance=FALSE


##' 
##' Run the daily importation model using model 2
##' 
##' 
##' 
##' 
##' @param reconst_inf logical, whether or not to reconstruct the infected population each simulation.
##' 
run_daily_import_timeloc_model2 <- function(input_data, travel_data_monthly=travel_data_monthly, 
                                            travel_dispersion=10, travel_restrictions=data.frame(loc="Hubei", min="2020-01-25", max="2020-04-01"), #hubei_shutdown="2020-01-25",
                                            n_sim=10000, allow_travel_variance=FALSE,
                                            t_red, u_origin, time_inftotravel, time_inftodetect,
                                            project_name, batch, version, print_progress=TRUE,
                                            reconst_inf=TRUE){
    
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
    
    # start the timer
    t.start <- proc.time() 
    
    for (n in 1:n_sim){
        if (print_progress){
            if (n %% 100 == 0) print(paste('sim', n, 'of', n_sim, sep = ' '))
        }
        
        # Reconstruct infected cases, if desired
        if (reconst_inf){
            incid_data_ <- build_infecteds(input_data %>% dplyr::select(source, t, p_report_source, cases_incid=cases_rep, country) %>%
                                               filter(!is.na(cases_incid)), 
                                           incub_mean_log, incub_sd_log, 
                                           inf_period_hosp_shape, inf_period_hosp_scale)
            input_data <- full_join(input_data %>% dplyr::select(-cases_curr_total), 
                                incid_data_ %>% dplyr::select(t, source, destination, cases_curr_total))
        }
        
        # Simulate the daily travel from monthly
        travel_data_daily <- make_daily_travel(travel_data=travel_data_monthly, travel_dispersion)
        input_data_sim <- left_join(input_data %>% dplyr::select(-travelers), 
                                    travel_data_daily %>% dplyr::select(source,destination,t,travelers), 
                                    by=c("source","destination", "t"))
        
        # Set up any travel restrictions  ***** THIS IS NOT DONE ******
        input_data_sim <- input_data_sim %>% 
            mutate(travelers = ifelse(source=="Hubei" & 
                                          t>=as.Date(hubei_shutdown) & t<as.Date("2020-04-01"), 0, travelers))
        
        ## EVENTUALLY WE WILL RE SAMPLE EACH SIMULATION   ***** THIS IS NOT DONE ******
            cases <- input_data$cases_curr_total  
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
        
        # This is the key part!!!!
        this.sim <- rbinom(n = length(cases), 
                           size = round(cases),#/u_origin[n]), 
                           prob = prob_travel_n_detection)
        this.sim[is.na(this.sim)] <- 0 
        
        # Detection of Cases ------------------------------
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



