
rm(list = ls())

#..........................................................................
# SETUP -------------------------------------------------------------------

# Analysis Options
project_name <- "shenzhen_import"
version <- "1"
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
travel_data_file   <- file.path("data","shenzhen_data","flow.csv")


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

# countries_ <- unique(incid_data$country) %>% sort()
# country_codes_ <- unique(incid_data$country_code) %>% sort()
# monthyear_ <- unique(incid_data$monthyear) %>% sort()
# years_ <- as.numeric(unique(str_extract(monthyear_, "[0-9]{4}")))


# ~ Travel Data -------------------------------------------------------------

# source("source/setup_travel_data.R")
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
sort(unique(travel_data$source))
sort(unique(incid_data$source))
sort(unique(pop_data$source))

sort(unique(travel_data$t))
sort(unique(incid_data$t))

unique(incid_data$source)[!(sort(unique(incid_data$source)) %in% sort(unique(travel_data$source)))]
unique(incid_data$source)[!(sort(unique(incid_data$source)) %in% sort(unique(pop_data$source)))]



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



# Add time_unit
input_data$days_per_t <- delta

# Make all negatives 0
input_data$travelers[input_data$travelers<0] <- 0


# # Intersecting intervals and countries
# {MonthYear_sim <- intersect(MonthYear_measles, MonthYear_travel)
# Country.Codes_sim <- intersect(Country.Codes_measles, Country.Codes_travel)
# MonthYear_sim.imports <- intersect(MonthYear_sim, MonthYear_imports)
# Country.Codes_sim.imports <- intersect(Country.Codes_sim, Origin.Codes_imports)}




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


# ~ Proportion symptomatic/severe  ----------------------------------------

p_sympt <- c(0.10, .5)


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
quantile(samp, probs=c(0.025, 0.975))




# # MAKING D MORE CORRECT ---------------------------------------------------
# 
# incid_data <- read_csv(incid_data_path)
# 
# data_ <- incid_data %>% filter(prov_state == "Hubei") %>% select(-China_source, -epiweek)
# #data_ <- data_ %>% select(prov_state, date, cases_incid) %>% spread(key = date, value = cases_incid)
# 
# # First add all following cases to previous days
# data_ <- data_ %>% rowwise() %>% mutate(infected_curr = sum(lag(cases_incid, n=1L), na.rm = TRUE))
# 
# cases <- as.integer(data_$cases_incid)
# dates <- data_$t
# 
# 
# days_incub_n_not_hosp <- 5 + 2.5
# incub_cases <- sapply(1:days_incub_n_not_hosp, FUN=function(x) lead(cases, x))
# incub_cases_ <- rowSums(incub_cases, na.rm=TRUE)
# 
# plot(dates, incub_cases_, type="b", col="blue", ylim = c(0, max(incub_cases_)))
# lines(dates, cases, type="b")
# abline(v=rev(dates)[days_incub_n_not_hosp], col="grey") # horizonal line for when incubation has "completed"
# abline(v=as.Date("2020-01-23"), col="red")
# 





#..........................................................................
# PARAMETER SAMPLING ------------------------------------------------------


# Mean D -- This is a combination of incubation period, time to hospitalization, and time to recovery
meanD <- round(
  exp(rnorm(n_sim, mean = incub_mean_log, sd = incub_sd_log)) + 
  p_sympt*rgamma(n_sim, shape=inf_period_hosp_shape, scale=inf_period_hosp_scale) +
  (1-p_sympt)*MCMCglmm::rtnorm(n_sim, inf_period_nohosp_mean, inf_period_nohosp_sd, lower=0))

# Using Lauren's
incid_data <- readr::read_csv(incid_data_path) %>% rename(source=prov_state) %>% 
  mutate(source = ifelse(source=="Ningxia Hui", "Ningxia", source)) %>% 
  filter(China_source==TRUE)  %>% as.data.frame()

prov_sympts <- rep(length(unique(input_data$source)))
sources_ <- unique(input_data$source)

meanD_mat_ <- cbind(
  exp(rnorm(n_sim, mean = incub_mean_log, sd = incub_sd_log)),
  rgamma(n_sim, shape=inf_period_hosp_shape, scale=inf_period_hosp_scale),
  MCMCglmm::rtnorm(n_sim, inf_period_nohosp_mean, inf_period_nohosp_sd, lower=0))

meanD_mat <- meanD_mat_[,2] * 


hist(meanD, breaks=length(unique(meanD))-1)


# ~ Travel reductions -----------------------------------------------------
t_red <- rep(1, n_sim)
# t_red <- runif(n_sim, 0, 0.12)

# ~ Destination reporting rate --------------------------------------------
u_destination <- rep(1, n_sim)
# u_destination <- rbeta(n_sim, pars$alpha, pars$beta)
# u_destination <- rbeta(n_sim, 12, 12/4)

# ~ Origin reporting rate -------------------------------------------------
#u_origin <- rep(1, n_sim)
u_origin <- rep(p_sympt, n_sim)
# u_origin <- rbeta(n_sim, 12, 12/4)
# u_origin <- rbeta(n_sim, pars$alpha, pars$beta)








#.....................................................................................................
# SIMULATION --------------------------------------------------------------

# Exclude Guangdong
# --> need to figure out how to appropriately include it

input_data_guangdong <- input_data %>% filter(source=="Guangdong")
input_data_noguangdong <- input_data %>% filter(source!="Guangdong")

# ~ Run the simulation ----------------------------------------------------
source("source/import_model_source.R")

# All Sources
importation_sim <- run_daily_import_model(input_data, n_sim=10000, allow_travel_variance=FALSE,
                                          meanD, t_red, u_origin, u_destination, 
                                          project_name, batch, version)
# No Guangdong
importation_sim <- run_daily_import_model(input_data_noguangdong, n_sim=10000, allow_travel_variance=FALSE,
                                          meanD, t_red, u_origin, u_destination, 
                                          project_name, batch, version="NoGuangdong")
# Guangdong Only
importation_sim <- run_daily_import_model(input_data_noguangdong, n_sim=10000, allow_travel_variance=FALSE,
                                          meanD, t_red, u_origin, u_destination, 
                                          project_name, batch, version="OnlyGuangdong")




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











