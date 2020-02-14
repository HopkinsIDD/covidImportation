
rm(list = ls())

#..........................................................................
# SETUP -------------------------------------------------------------------

# Analysis Options
project_name <- "USA_import"
version <- "1"
batch <- "1st"
n_sim <- 10000
allow_travel_variance <- FALSE
# incid_data_path <- file.path("data","ncov_incid_rep.csv")
# travel_data_file   <- file.path("data","travel_data.csv")
# pop_data_file   <- file.path("data","worldpop_data.csv")

# Shenzen Model
source("source/setup_travel_data.R")
source("source/setup_pop_data.R")
source("source/ncov_incidence_data_processing.R") # Extracts current cases
write_csv(conf_cases, "data/conf_cases_current.csv")

incid_data_path <- file.path("data","conf_cases_current.csv") 
pop_data_file   <- file.path("data","pop_data.csv")
travel_data_file   <- file.path("data","shenzhen_travel_data.csv")

dir.create(file.path("output",project_name), recursive = TRUE)



# Packages
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
# PARAMETERS -------------------------------------------------------------

# Parameters:
# meanD: duration of infection (may include symptomatic and asymptomatic periods)
# t_red: reduction of travel likelihood due to illness
# u_origin: a number [0,1] that measures the fraction of cases reported at the country of origin
# u_destination: a number [0,1] that measures the fraction of cases reported by destination sentinel system



# ~ Proportion symptomatic/severe  ----------------------------------------

p_sympt <- 0.10


# ~ D (incub + dur infectious) --------------------------------------------

# Incubation
# mean: 5.174 days (95% CI 2.5-10.5)      # JHU estimate
# Time to rash 2--3 days
incub_mean_log <- log(5.174)
incub_sd_log   <- log(1.45)

samp <- exp(rnorm(1000, incub_mean_log, incub_sd_log))
plot(density(samp), type="l")
quantile(samp, probs=c(0.025, 0.975))
quantile(samp, probs=c(0.25, 0.75))
quantile(samp, probs=c(.5))

# Infectious period - not hospitalized
inf_period_nohosp_mean <- 14  # needs revision
inf_period_nohosp_sd   <- 4

samp <- MCMCglmm::rtnorm(1000, inf_period_nohosp_mean, inf_period_nohosp_sd, lower=0)
plot(density(samp), type="l")
quantile(samp, probs=c(0.025, 0.975))

# Infectious period - hospitalized (time to hospitalization)
inf_period_hosp_mean <- 5.5  # needs revision
inf_period_hosp_sd   <- 2

samp <- MCMCglmm::rtnorm(1000, inf_period_hosp_mean, inf_period_hosp_sd, lower=0)
plot(density(samp), type="l")
quantile(samp, probs=c(0.025, 0.975))


# ~ delta (days per time period) -----------------------------------------
delta <- 1




#..........................................................................
# PARAMETER SAMPLING ------------------------------------------------------

meanD <- round(exp(rnorm(n_sim, mean = incub_mean_log, sd = incub_sd_log)) + 
                 p_sympt*MCMCglmm::rtnorm(n_sim, inf_period_hosp_mean, inf_period_hosp_sd, lower=0) +
                 (1-p_sympt)*MCMCglmm::rtnorm(n_sim, inf_period_nohosp_mean, inf_period_nohosp_sd, lower=0))
# meanD <- round(rnorm(n_sim, mean = 14, sd = 2))

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






#..........................................................................
# DATA --------------------------------------------------------------------


# ~ Measles Incidence Data ------------------------------------------------

incid_data <- readr::read_csv(incid_data_path) %>% rename(source=loc) %>% 
  mutate(source = ifelse(source=="Ningxia Hui", "Ningxia", source))
# countries_ <- unique(incid_data$country) %>% sort()
# country_codes_ <- unique(incid_data$country_code) %>% sort()
# monthyear_ <- unique(incid_data$monthyear) %>% sort()
# years_ <- as.numeric(unique(str_extract(monthyear_, "[0-9]{4}")))



# ~ Travel Data -------------------------------------------------------------

# source("source/setup_travel_data.R")
travel_data <- read_csv(travel_data_file)



# ~ Population Data ---------------------------------------------------------

# source("source/setup_pop_data.R")
pop_data <- readr::read_csv(pop_data_file) %>% rename(source = loc)



# ~ Merge all data ----------------------------------------------------------

# make sure the location names match
#source("source/match_names_func.R")

# provinces_ <- pop_data$source
# travel_sources_ <- travel_data$source
# travel_provs <- sapply(travel_sources_, match_names, 
#        names_b=provinces_, 
#        return_match_scores=FALSE)
# travel_data <- travel_data %>% 
#   mutate(source_orig = source) %>%
#   mutate(source = travel_provs)
# 
# incid_sources_ <- incid_data$source
# incid_provs <- sapply(incid_sources_, match_names, 
#                        names_b=provinces_, 
#                        return_match_scores=FALSE)
# incid_data <- incid_data %>% 
#   mutate(source_orig = source) %>%
#   mutate(source = incid_provs)


# Check that we have the same locations and time values between data
sort(unique(travel_data$source))
sort(unique(incid_data$source))
sort(unique(pop_data$source))

sort(unique(travel_data$t))
sort(unique(incid_data$t))



# merge data (delimit it by travel data)
input_data <- left_join(right_join(pop_data, travel_data, by=c("source")), 
                        incid_data, by=c("source"="source", "t"="t"))

# filter data by time and location
input_data <- input_data %>%
  filter(t > as.Date("2019-12-31")) %>% 
  mutate(cases_incid=ifelse(is.na(cases_incid), 0, cases_incid))

# Add time_unit
input_data$days_per_t <- delta

# Make all negatives 0
input_data$travelers[input_data$travelers<0] <- 0


# # Intersecting intervals and countries
# {MonthYear_sim <- intersect(MonthYear_measles, MonthYear_travel)
# Country.Codes_sim <- intersect(Country.Codes_measles, Country.Codes_travel)
# MonthYear_sim.imports <- intersect(MonthYear_sim, MonthYear_imports)
# Country.Codes_sim.imports <- intersect(Country.Codes_sim, Origin.Codes_imports)}

sources_ <- sort(unique(input_data$source))
dests_ <- sort(unique(input_data$destination))
t_ <- sort(unique(input_data$t))


#..........................................................................
# SUMMARIZE DATA ----------------------------------------------------------

summ_dat <- input_data %>% group_by(destination, t) %>% summarise(travel_sum=sum(travelers, na.rm = TRUE),
                                                                  cases_incid_sum=sum(cases_incid, na.rm = TRUE))



#.....................................................................................................
# SIMULATION --------------------------------------------------------------

# Sims in longform
cases <- input_data$cases_incid
sim <- input_data %>% select(source, destination, t) %>% as.data.table()

# Sims as multidimensional arrays
importation_sim <- array(0, dim = c(length(sources_), length(dests_), length(t_), n_sim),
                                       dimnames = list(sources_, dests_, as.character(t_), 1:n_sim))


# ~ Run the simulation ----------------------------------------------------
{t.start <- proc.time() # start the timer
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
  
  # adjust probability by detection and travel probability reduction
  prob_travel_n_detection <- t_red[n]*u_destination[n]*Travelers_over_Population_and_days
  
  # Run simulations by day, in case travel likelihood is affected by symptoms on a day to day basis
  for (day in 1:meanD[n]){
    this.sim <- this.sim + rbinom(length(cases),
                                  prob = prob_travel_n_detection, size = round( cases/u_origin[n]))
  }

  # sim.airport <- acast(cbind(select(sim, -State), this.sim), Country.Code ~ Destination.airport ~ MonthYear, value.var = "this.sim")
  # Measles_importation_airport_sim[dimnames(sim.airport)[[1]], dimnames(sim.airport)[[2]], dimnames(sim.airport)[[3]], n] <- sim.airport
  
  sim.wide <- acast(cbind(sim, this.sim) %>% 
                      group_by(source, destination, t) %>% 
                      summarize(this.sim = sum(this.sim)), 
                    source ~ destination ~ t, value.var = "this.sim")
  importation_sim[dimnames(sim.wide)[[1]], dimnames(sim.wide)[[2]], dimnames(sim.wide)[[3]], n] <- sim.wide
  
}
print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))}



# Saving Sims
save(importation_sim, file = file.path("output",project_name, sprintf("nCoV_importation_sim_%s_batch_v%s.RData", batch, version)))









