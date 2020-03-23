
##' SARS-CoV-2 Importation

# This document details the full analysis process for estimating the COVID-19 importation risk and numbers into the U.S. from other countries. This work is an adaptation of work done for measles importation for the manuscript titled "Epidemics, Air Travel, and Elimination in a Globalized World: The Case of Measles".  
# The components of the model are detailed below, with all R scripts or functions included or sourced. The model requires a set of epidemiological parameters (incubation period, infectious period, proportion of cases reported), which currently come from data on Chinese cases and cases that have traveled from China. Case data on cases in source locations comes from publicly reported cases. We are using a smoothing technique to make these more uniform, and have location and time-specific assumptions about what proportion of cases are currently being reported. Travel data with full trip iteneraries is required, and currently we are using data provided by the U.S. CDC obtained from OAG. Unfortunately, these data publicly available and need express permissions to share.
# Alterations can be done in this R markdown file to explore scenarios, do sensitivity analyses, or update assumptions and data. Data changes should be done in the **DATA** block below or the sourced scripts. Parameter changes should be done in the **COVID-19 CHARACTERISTICS** section. Results tables are saved as CSV files and then used to produce the figures below.
# The current analysis includes estimates of importation into California via air travel up to `r as.Date(Sys.time())`. On January 23, Hubei was put under quarantine, after which flights from Hubei were cancelled, though we allow a couple extra days for travel due to time to travel and layovers.


print(getwd())

start_time <- Sys.time()


# Get arguments from bash script ------------------------------------------

# Pull in Arguments from bash script
args <- commandArgs(trailingOnly = TRUE) # provides runs, cores
print(args)

cores <- as.integer(args[2])
n_sim <- as.integer(args[1])


print(cores)




# MODEL SETUP -------------------------------------------------------------

# Analysis Options

destination=c("UT")
destination_type="state"
dest_0="USA"
dest_0_type="country"
dest_aggr_level="airport"


location_name <- "USA"
project_name <- "Utah_import"
version <- "global"
batch <- "1st"

print(project_name)




# End date of estimation
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-03-15")

# Hubei Travel shutdown
hubei_shutdown <- c("2020-01-24", "2020-04-01") # it might have been Jan 23, but we will give it 1 day for people already departed.




# GENERAL SETUP -----------------------------------------------------------

options(scipen = 999)
#if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
library(tidyverse)
# if(!require('gridExtra')) install.packages('gridExtra'); library(gridExtra)
# if(!require('grid')) install.packages('grid'); library(grid)
#if(!require('viridis')) install.packages('viridis'); library(viridis)
#library(globaltoolbox)
select <- dplyr::select



# Source Files
source("R/DataLoadUtils.R")
source("R/BasicEpiAnalyses.R")
source("R/DataUtils.R")
source("R/import_model_source.R")
source("R/setup_travel_data.R")


# Create needed directories
dir.create(file.path("output",project_name), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("data",project_name), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("figures",project_name), recursive = TRUE, showWarnings = FALSE)



# READ ALL THE DATA NEEDED
input_data_cases <- read_csv(file.path("data", project_name, "input_data_cases.csv"))
travel_data_monthly <- read_csv(file.path("data", project_name, "travel_data_monthly.csv"))
travel_restrictions <- read_csv(file.path("data", project_name, "travel_restrictions.csv"))




# COVID-19 CHARACTERISTICS    ---------------------------------------------

# ~ Incubation period -----------------------------------------------------

#We are assuming the incubation follows a lognormal distribution that follows estimates from Lauer et al. 2020 (mean = 5.2 days (95% CI 2.5-10.5)). Source: https://www.medrxiv.org/content/10.1101/2020.02.02.20020016v1.

# Incubation
# mean: 5.2 days (95% CI 2.5-10.5) Lauer et al. 2020
# incub_mean_log <- log(5.2)
# incub_sd_log   <- log(1.43)

# Shenzhen
# Mean: 5.9 days (95% CI 2.0-17.5)
incub_mean_log <- log(5.89)
incub_sd_log   <- log(1.74)

samp_incub <- exp(rnorm(10000, incub_mean_log, incub_sd_log))
#quantile(samp_incub, probs=c(0.025, .50,  0.975))




# ~ Infectious period -----------------------------------------------------

# We are using different infectious periods for those detected and hospitalized and those not.
# These estimates are based off a combination of anecdotal evidence and estimates from SARS. This will be updated using data from COVID-19 soon.

## Infectious period - not hospitalized
inf_period_nohosp_mean <- 15  # needs revision
inf_period_nohosp_sd   <- 5

#inf_nohosp <- exp(MCMCglmm::rtnorm(10000, log(inf_period_nohosp_mean), log(inf_period_nohosp_sd), lower=0))
inf_nohosp <- (MCMCglmm::rtnorm(10000, inf_period_nohosp_mean, inf_period_nohosp_sd, lower=0))
quantile(inf_nohosp, probs=c(0.025, .5, 0.975))
print(paste0("Mean time to recovery: ", round(mean(inf_nohosp),1), " days"))

#We are currently assuming a mean time to recovery of `r round(mean(inf_nohosp),1)` days.
    
  
  
##### Infectious period - hospitalized (time to hospitalization)

inf_period_hosp_shape <- 0.75 # increased based on Shenzhen (this is going to be a bit fluid)
inf_period_hosp_scale <- 5.367

inf_hosp <- rgamma(1000, shape=inf_period_hosp_shape, scale=inf_period_hosp_scale)
quantile(inf_hosp, probs=c(0.025, .5,  0.975))
print(paste0("Mean time to hospitalization: ", round(mean(inf_hosp),1), " days"))
# We are currently assuming a mean time to hospitalization of `r round(mean(inf_hosp),1)` days.
    

    
### Proportion cases reported
p_report_source <- c(0.05, 0.20)
# p_report_source <- data.frame(country=c("CHN", "KOR", "OTHER"),
#                               p_reportc(.05, .25, .15))
# 


#Currently the model is set up to take both time- and location-varying proportion of cases reported. For now, we are using `r p_report_source[1]*100`% reported for Hubei, and `r p_report_source[2]*100`% reported elsewhere, constant over time.  
   
     
### Shift in incidence date
shift_incid_incub <- -5
shift_incid_detect <- -3
shift_incid_report <- -2
shift_incid_days <- shift_incid_incub + shift_incid_detect + shift_incid_report




# To appropriately allign reported cases of COVID-19 in source locations with the time during which they might be traveling, we shift case reporting dates by `r shift_incid_days` days (i.e., backward). This is a crude adjustment for now that will be improved.
# ~ Time to detect importations -------------------------------------------
# -- If we assume people generally depart at some point during their incubation period, 
#     or very early in the symptomatic phase, 
#     we can generate a distribution of time from travel to detection.
# -- because we are only worried about those who are detected, we can ignore time to recover

time_inftodetect <- exp(rnorm(10000, mean = incub_mean_log, sd = incub_sd_log)) + 
  rgamma(10000, shape=inf_period_hosp_shape, scale=inf_period_hosp_scale)
hist(time_inftodetect, breaks=100)

# We assume people can and do travel during their incubation period and 
#  during that period during which symptoms are still minor. 
#  There are reports of travelers taking fever-reducers and a portion dont show fever
# We assume this is uniform
time_inftotravel <- sapply(time_inftodetect, runif, n=1, min=0)
hist(time_inftotravel, breaks=100)

time_traveltodetect <- time_inftodetect - time_inftotravel
hist(time_traveltodetect, breaks=100)
par(mfrow=c(1,1))





# Need to regen the meanD mat
meanD_mat <- make_meanD(input_data_cases, n_sim, 
                        incub_mean_log, incub_sd_log,
                        inf_period_hosp_shape, inf_period_hosp_scale,
                        inf_period_nohosp_mean, inf_period_nohosp_sd)



# ~ Travel reductions -----------------------------------------------------
tr_inf_redux <- rep(1, n_sim)


# ~ Destination reporting rate --------------------------------------------
u_destination <- rep(1, n_sim)


# ~ Origin reporting rate -------------------------------------------------
u_origin <- matrix(rep(input_data_cases$p_report_source, n_sim), nrow=n_sim, byrow = TRUE)


print("Setup complete. Now run it!")


t.start <- proc.time() # start timer to measure this



# Run the model
importation_sim <- run_daily_import_model_par(
  n_sim=100,
  input_data = input_data_cases,
  travel_data_monthly = travel_data_monthly,
  travel_dispersion=3,
  travel_restrictions=travel_restrictions,
  allow_travel_variance=FALSE,
  meanD_mat=meanD_mat, 
  tr_inf_redux=tr_inf_redux, u_origin=u_origin, 
  get_detection_time=FALSE,
  incub_mean_log, incub_sd_log,
  inf_period_hosp_shape, inf_period_hosp_scale,
  inf_period_nohosp_mean, inf_period_nohosp_sd,
  project_name, batch, version, 
  print_progress=TRUE,
  cores=4)


# print time required
print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))



# Get Negative Binomial Parameters ----------------------------------------

import_pars_df <- calc_nb_import_pars(importation_sim, project_name, batch, version)
  








tmp <- load("output/california_import/nCoV_importation_sim_1st_batch_vglobal.RData")

import_pars_df <- calc_nb_import_pars(importation_sim, project_name, batch, version)

