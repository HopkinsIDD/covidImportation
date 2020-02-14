

#rm(list = ls())

#..........................................................................
# SETUP -------------------------------------------------------------------

# Analysis Options
project_name <- "shenzhen_import"
version <- "Model2"
batch <- "1st"
n_sim <- 10000
allow_travel_variance <- FALSE
options(scipen = 999)
# incid_data_path <- file.path("data","ncov_incid_rep.csv")
# travel_data_file   <- file.path("data","travel_data.csv")
# pop_data_file   <- file.path("data","worldpop_data.csv")

# Shenzen Model
source("source/setup_travel_data.R")
source("source/setup_pop_data.R")
source("source/ncov_incidence_data_processing.R")
incid_data_path <- file.path("data","china_incid_data_report.csv")
pop_data_file   <- file.path("data","china_province_pop_data.csv")

#travel_data_file   <- file.path("data","shenzhen_travel_data.csv")
#travel_data_file   <- file.path("data","shenzhen_data","flow.csv")
travel_data_file   <- file.path("data","shenzhen_travel_data.csv") # currently Baidu data


dir.create(file.path("output",project_name), recursive = TRUE)



# Packages
if(!require('data.table')) install.packages('data.table'); library(data.table)
if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('reshape2')) install.packages('reshape2'); library(reshape2)
if(!require('data.table')) install.packages('data.table'); library(data.table)
if(!require('stringr')) install.packages('stringr'); library(stringr)
if(!require('lettercase')) install.packages('lettercase'); library(lettercase)
if(!require('fields')) install.packages('fields'); library(fields)
if(!require('pracma')) install.packages('pracma'); library(pracma)
if(!require('msm')) install.packages('msm'); library(msm)
if(!require('countrycode')) install.packages('countrycode'); library(countrycode)
if(!require('tools')) install.packages('tools'); library(tools)
select <- dplyr::select





#..........................................................................
# DATA --------------------------------------------------------------------


# ~ Incidence Data ------------------------------------------------

# Using China's reported
# incid_data <- readr::read_csv(incid_data_path) %>% rename(source=loc) %>% 
#   mutate(source = ifelse(source=="Ningxia Hui", "Ningxia", source))

# Using Lauren's
incid_data <- readr::read_csv(incid_data_path) %>% rename(source=prov_state) %>% 
    mutate(source = ifelse(source=="Ningxia Hui", "Ningxia", source)) %>% 
    filter(China_source==TRUE)  %>% as.data.frame()

start_date <- min((incid_data %>% filter(cases_incid>0))$t)


# Remove Shenzhen cases from incid_data so we are not including imports that happened in the model
# --> We will use the linelist data for this for now
shen_cases <- read_csv("data/shenzhen_data/shenzhen_case_counts.csv")
inds_guangdong <- which(incid_data$source=="Guangdong")
matched_dates <- match(shen_cases$date, incid_data$t[inds_guangdong])
incid_data$cases_incid[inds_guangdong][matched_dates] <- incid_data$cases_incid[inds_guangdong][matched_dates] - shen_cases$count
incid_data <- incid_data %>% mutate(cases_incid = ifelse(cases_incid<0, 0, cases_incid)) # some mismatched data. for now just make sure no negatives


# ~ Add Reporting Rate to Incidence ---------------------------------------

# For first pass, reporting rate is just Hubei/not Hubei
incid_data <- incid_data %>% mutate(p_report_source = ifelse(source=="Hubei", p_report_source[1], p_report_source[2]))





# ~ Travel Data -------------------------------------------------------------

source("source/setup_travel_data.R")
travel_data <- read_csv(travel_data_file) %>%
    mutate(t = lubridate::ymd(t))  %>% as.data.frame()



# ~ Population Data ---------------------------------------------------------

# source("source/setup_pop_data.R")
pop_data <- readr::read_csv(pop_data_file) %>% 
    rename(source = loc, population=pop) %>% as.data.frame()



# ~ Merge all data ----------------------------------------------------------

# make sure the location names match
source("source/match_names_func.R")

provinces_ <- as.character(pop_data$source)
travel_sources_ <- as.character(travel_data$source)
travel_provs <- sapply(travel_sources_, match_names,
                       names_b=provinces_,
                       return_match_scores=FALSE)
travel_data <- travel_data %>%
    mutate(source_orig = source) %>%
    mutate(source = travel_provs) %>%
    rowwise() %>% mutate(source = ifelse(is.na(source), source_orig, source))


incid_sources_ <- as.character(incid_data$source)
incid_provs <- sapply(incid_sources_, match_names,
                      names_b=provinces_,
                      return_match_scores=FALSE)
incid_data <- incid_data %>%
    mutate(source_orig = source) %>%
    mutate(source = incid_provs) %>% 
    rowwise() %>% mutate(source = ifelse(is.na(source), source_orig, source))

incid_data <- incid_data %>% mutate(source = ifelse(source=="Inner Mongolia", "Nei Mongol", source))


# Check that we have the same locations and time values between data
# sort(unique(travel_data$source))
# sort(unique(incid_data$source))
# sort(unique(pop_data$source))

# sort(unique(travel_data$t))
# sort(unique(incid_data$t))
# 
# unique(incid_data$source)[!(sort(unique(incid_data$source)) %in% sort(unique(travel_data$source)))]
# unique(incid_data$source)[!(sort(unique(incid_data$source)) %in% sort(unique(pop_data$source)))]



# merge data (delimit it by travel data)
pop_data <- pop_data %>% mutate(source = as.character(source))
travel_data <- travel_data %>% mutate(source = as.character(source))
incid_data <- incid_data %>% mutate(source = as.character(source))

input_data <- left_join(right_join(pop_data, 
                                   travel_data %>% select(-source_orig), by=c("source")), 
                        incid_data %>% select(-source_orig, -country, -China_source), by=c("source"="source", "t"="t"))

# filter data by time and location
input_data <- input_data %>%
    #filter(t > as.Date("2019-12-31")) %>% 
    filter(t >= as.Date(start_date)) %>% 
    mutate(cases_incid=ifelse(is.na(cases_incid), 0, cases_incid),
           epiweek = lubridate::epiweek(t))

# Make all negatives 0
input_data$travelers[input_data$travelers<0] <- 0



#..........................................................................
# SUMMARIZE DATA ----------------------------------------------------------

summ_dat <- input_data %>% group_by(destination, t) %>% summarise(travel_sum=sum(travelers, na.rm = TRUE),
                                                                  cases_incid_sum=sum(cases_incid, na.rm = TRUE))
summ_dat



#..........................................................................
# PARAMETERS -------------------------------------------------------------

# Parameters:
# meanD: duration of infection (may include symptomatic and asymptomatic periods)
# t_red: reduction of travel likelihood due to illness
# u_origin: a number [0,1] that measures the fraction of cases reported at the country of origin
# u_destination: a number [0,1] that measures the fraction of cases reported by destination sentinel system


# ~ delta (days per time period) -----------------------------------------
delta <- 1

# Add time unit to input data
input_data$days_per_t <- delta


# ~ Proportion reported at the source  ----------------------------------------

p_report_source <- c(0.05, 0.50)


# ~ D (incub + dur infectious) --------------------------------------------

# Incubation
# mean: 5.2 days (95% CI 2.5-10.5)      # Lauer et al. 2020 (https://www.medrxiv.org/content/10.1101/2020.02.02.20020016v1)
incub_mean_log <- log(5.2)
incub_sd_log   <- log(1.43)

samp <- exp(rnorm(10000, incub_mean_log, incub_sd_log))
plot(density(samp), type="l", main="Incubation Period", xlab="Days")
quantile(samp, probs=c(0.025, 0.975))
quantile(samp, probs=c(0.25, 0.75))
quantile(samp, probs=c(.5))

# Infectious period - not hospitalized
inf_period_nohosp_mean <- 7  # needs revision
inf_period_nohosp_sd   <- 2

samp <- exp(MCMCglmm::rtnorm(10000, log(inf_period_nohosp_mean), log(inf_period_nohosp_sd), lower=0))
plot(density(samp), type="l")
quantile(samp, probs=c(0.025, 0.975))
quantile(samp, probs=c(0.25, 0.75))
quantile(samp, probs=c(.5))

# Infectious period - hospitalized (time to hospitalization)
inf_period_hosp_shape <- 0.46  # needs revision
inf_period_hosp_scale   <- 5.367

samp <- rgamma(1000, shape=inf_period_hosp_shape, scale=inf_period_hosp_scale)
plot(density(samp), type="l")
quantile(samp, probs=c(0.025, .5,  0.975))
mean(samp)





#..........................................................................
# PARAMETER SAMPLING ------------------------------------------------------

# Mean D -- This is a combination of incubation period, time to hospitalization, and time to recovery
meanD <- round(
    exp(rnorm(n_sim, mean = incub_mean_log, sd = incub_sd_log)) +
        p_report_source[1]*rgamma(n_sim, shape=inf_period_hosp_shape, scale=inf_period_hosp_scale) +
        (1-p_report_source[1])*MCMCglmm::rtnorm(n_sim, inf_period_nohosp_mean, inf_period_nohosp_sd, lower=0))


# Sample the components of meanD -- will apply the p_report_source to these
meanD_mat_ <- cbind(
    exp(rnorm(n_sim, mean = incub_mean_log, sd = incub_sd_log)),
    rgamma(n_sim, shape=inf_period_hosp_shape, scale=inf_period_hosp_scale),
    MCMCglmm::rtnorm(n_sim, inf_period_nohosp_mean, inf_period_nohosp_sd, lower=0))

# Apply p_report_source by location and time to get the meanD matrix, where each simulation run has a pre-sampled set of D for each time/location combination
meanD_mat <- meanD_mat_[,1] + 
    meanD_mat_[,2] %*% matrix(input_data$p_report_source, nrow=1) + 
    meanD_mat_[,3] %*% matrix((1-input_data$p_report_source), nrow=1)
#rm(meanD_mat_)

#hist(meanD, breaks=length(unique(meanD))-1)
hist(meanD_mat, breaks=length(unique(meanD))-1)
rm(meanD_mat_)


# ~ Travel reductions -----------------------------------------------------
#t_red <- rep(1, n_sim)
t_red <- rep(1, n_sim)

# t_red <- runif(n_sim, 0, 0.12)

# ~ Destination reporting rate --------------------------------------------
u_destination <- rep(1, n_sim)
# u_destination <- rbeta(n_sim, pars$alpha, pars$beta)
# u_destination <- rbeta(n_sim, 12, 12/4)

# ~ Origin reporting rate -------------------------------------------------
u_origin <- matrix(rep(input_data$p_report_source, n_sim), nrow=n_sim, byrow = TRUE)
# u_origin <- rep(1, n_sim)
# u_origin <- rbeta(n_sim, 12, 12/4)
# u_origin <- rbeta(n_sim, pars$alpha, pars$beta)










# BUILD THE INFECTED POPULATION FROM REPORTED CASES -----------------------

# get cases by day
# - X cases are reported today, that means these cases have gone through 
#     (1) full infectious periods
#     (2) some time from onset to detection, which likely mirrors onset to hospitalization
# - These X cases also represent some number X_real, which depends on the detection and reporting rate
# - If we assume the daily cohort of reported cases represents individuals who follow the above distributions, 
#    we can randomly assign them time to reporting, thus time previously at risk of travel
# - Additionally, X_real are able to travel for even longer time going forward, as they are not detected/quarantined


# have to do by location

sources_ <- sort(unique(incid_data$source))
case_dat_all <- NULL

for (s in 1:length(sources_)){
    data_ <- incid_data %>% filter(source==sources_[s])
    #plot(data_$t, data_$cases_incid)
    
    
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
                                date_inf = day_detect-as.integer(names(tmp_))
                                )
        
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
    
    case_dat <- full_join(data_ %>% select(-China_source, cases_rep = cases_incid), 
                          case_reconst, by=c("t"="date")) %>%
        mutate(source=data_$source[1], country=data_$country[1], source_orig=data_$source_orig[1])
    case_dat <- case_dat %>% arrange(source, desc(t))
    
        
    
    
    # ~ Construct missing cases -----------------------------------------------
    # Now use the infections and reporting rate to fill in the missed cases moving forward
    
    # sample from the reporting rate distribution for each day --> this could be made to be time varying easily
    p_report_ <- MCMCglmm::rtnorm(nrow(case_dat), data_$p_report_source[1], .1, lower=0.025, upper=.95)
    #hist(p_report_, breaks=20)
    
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
    
    case_dat_total <- full_join(case_dat %>% rename(cum_cases_rep = cum_cases, cases_inf_rep = cases_inf, cases_curr_rep = cases_curr),
                          case_curr_no_hosp %>% rename(cases_curr_norep = cases_curr) %>%
                              mutate(source=case_dat$source[1], country=case_dat$country[1], source_orig=case_dat$source_orig[1]),
                          by = c("t"="date", "source", "country", "source_orig")) 
    case_dat_total <- case_dat_total %>% mutate(t = as.Date(t)) %>% 
        arrange(source, desc(t))
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



# ~ Combine with other data -----------------------------------------------

# merge data (delimit it by travel data)
pop_data <- pop_data %>% mutate(source = as.character(source))
travel_data <- travel_data %>% mutate(source = as.character(source))
case_dat_all <- case_dat_all %>% mutate(source = as.character(source))

input_data <- left_join(right_join(pop_data, 
                                   travel_data %>% select(-source_orig), by=c("source")), 
                        case_dat_all %>% select(-source_orig, -country), by=c("source"="source", "t"="t"))

# filter data by time and location
input_data <- input_data %>%
    #filter(t > as.Date("2019-12-31")) %>% 
    filter(t >= as.Date(start_date)) %>% 
    mutate(cases_curr_total=ifelse(is.na(cases_curr_total), 0, cases_curr_total),
           epiweek = lubridate::epiweek(t))

# Make all negatives 0
input_data$travelers[input_data$travelers<0] <- 0

























#.....................................................................................................
# SIMULATION --------------------------------------------------------------

# save final input data
readr::write_csv(input_data, sprintf("data/input_data_%s_batch_v%s.RData", batch, version))


# Exclude Guangdong
# --> need to figure out how to appropriately include it

input_data_guangdong <- input_data %>% filter(source=="Guangdong")
input_data_noguangdong <- input_data %>% filter(source!="Guangdong")

# ~ Run the simulation ----------------------------------------------------
source("source/import_model_source.R")

# # All Sources
# importation_sim <- run_daily_import_model(input_data, n_sim=10000, allow_travel_variance=FALSE,
#                                           meanD, t_red, u_origin,  
#                                           project_name, batch, version)
# # No Guangdong
# importation_sim <- run_daily_import_model(input_data_noguangdong, n_sim=10000, allow_travel_variance=FALSE,
#                                           meanD, t_red, u_origin,  
#                                           project_name, batch, version="NoGuangdong")
# # Guangdong Only
# importation_sim <- run_daily_import_model(input_data_noguangdong, n_sim=10000, allow_travel_variance=FALSE,
#                                           meanD, t_red, u_origin, 
#                                           project_name, batch, version="OnlyGuangdong")




# TIME & LOCATION VARYING -------------------------------------------------


# All
importation_sim <- run_daily_import_model_timeloc(input_data, n_sim=10000, allow_travel_variance=FALSE,
                                                  meanD_mat, t_red, u_origin,  
                                                  project_name, batch, version="All_timevar")

# No Guangdong
importation_sim <- run_daily_import_model_timeloc(input_data_noguangdong, n_sim=10000, allow_travel_variance=FALSE,
                                                  meanD_mat, t_red, u_origin,  
                                                  project_name, batch, version="NoGuangdong_timevar")



### *** THIS HAS NOW BEEN PUT INTO A FUNCTION FOR ORGANIZATION AND SENARIO RUNNING PURPOSES ****

# sources_ <- sort(unique(input_data$source))
# dests_ <- sort(unique(input_data$destination))
# t_ <- sort(unique(input_data$t))
#
# # Sims in longform
# cases <- input_data$cases_incid
# sim <- input_data %>% select(source, destination, t) %>% as.data.table()
# 
# 
# # Sims as multidimensional arrays
# importation_sim <- array(0, dim = c(length(sources_), length(dests_), length(t_), n_sim),
#                          dimnames = list(sources_, dests_, as.character(t_), 1:n_sim))
# 
# {t.start <- proc.time() # start the timer
# for (n in 1:n_sim){
#   if (n %% 100 == 0) print(paste('sim', n, 'of', n_sim, sep = ' '))
#   
#   this.sim <- rep(0, length(cases))
#   
#   # Get p_s,d,t  (probability of infected individual traveling from d to s during time t
#   if (allow_travel_variance){  # if allowing variance in travel, using travelers SE
#     Travelers_over_Population_and_days <- MCMCglmm::rtnorm(dim(input_data)[1], 
#                                                 mean = input_data$travelers,
#                                                 sd = input_data$travelers_SE,
#                                                 lower = 0) / input_data$days_per_t / input_data$population
#   } else {
#     Travelers_over_Population_and_days <- input_data$travelers / input_data$days_per_t / input_data$population
#   }
#   
#   # adjust probability by detection and travel probability reduction
#   prob_travel_n_detection <- t_red[n]*u_destination[n]*Travelers_over_Population_and_days
#   
#   # Run simulations by day, in case travel likelihood is affected by symptoms on a day to day basis
#   for (day in 1:meanD[n]){
#     this.sim <- this.sim + rbinom(length(cases),
#                                   prob = prob_travel_n_detection, size = round( cases/u_origin[n]))
#   }
# 
#   # sim.airport <- acast(cbind(select(sim, -State), this.sim), Country.Code ~ Destination.airport ~ MonthYear, value.var = "this.sim")
#   # Measles_importation_airport_sim[dimnames(sim.airport)[[1]], dimnames(sim.airport)[[2]], dimnames(sim.airport)[[3]], n] <- sim.airport
#   
#   sim.wide <- acast(cbind(sim, this.sim) %>% 
#                       group_by(source, destination, t) %>% 
#                       summarize(this.sim = sum(this.sim)), 
#                     source ~ destination ~ t, value.var = "this.sim")
#   importation_sim[dimnames(sim.wide)[[1]], dimnames(sim.wide)[[2]], dimnames(sim.wide)[[3]], n] <- sim.wide
#   
# }
# print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))}



