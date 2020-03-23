
##' SARS-CoV-2 Importation

# This document details the full analysis process for estimating the COVID-19 importation risk and numbers into the U.S. from other countries. This work is an adaptation of work done for measles importation for the manuscript titled "Epidemics, Air Travel, and Elimination in a Globalized World: The Case of Measles".  
# The components of the model are detailed below, with all R scripts or functions included or sourced. The model requires a set of epidemiological parameters (incubation period, infectious period, proportion of cases reported), which currently come from data on Chinese cases and cases that have traveled from China. Case data on cases in source locations comes from publicly reported cases. We are using a smoothing technique to make these more uniform, and have location and time-specific assumptions about what proportion of cases are currently being reported. Travel data with full trip iteneraries is required, and currently we are using data provided by the U.S. CDC obtained from OAG. Unfortunately, these data publicly available and need express permissions to share.
# Alterations can be done in this R markdown file to explore scenarios, do sensitivity analyses, or update assumptions and data. Data changes should be done in the **DATA** block below or the sourced scripts. Parameter changes should be done in the **COVID-19 CHARACTERISTICS** section. Results tables are saved as CSV files and then used to produce the figures below.
# The current analysis includes estimates of importation into California via air travel up to `r as.Date(Sys.time())`. On January 23, Hubei was put under quarantine, after which flights from Hubei were cancelled, though we allow a couple extra days for travel due to time to travel and layovers.




print(getwd())


start_time <- Sys.time()

# Get arguments from bash script ------------------------------------------

# Pull in Arguments from bash script
args <- commandArgs(TRUE) # provides runs, cores
eval(parse(text=args)) 
print(args)



if (!exists("cores")){
  
  cores <- 4
  n_sim <- 100
  
}




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



get_travel <- TRUE # this needs to be TRUE for the first time this is run, but then can be FALSE. It adds about 1 min.

# End date of estimation
start_date <- as.Date("2020-01-01")
end_date <- as.Date("2020-03-15")

# Hubei Travel shutdown
hubei_shutdown <- c("2020-01-24", "2020-05-01") # it might have been Jan 23, but we will give it 1 day for people already departed.




# GENERAL SETUP -----------------------------------------------------------

options(scipen = 999)
if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
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
source("R/setup_pop_data.R")
#source("R/oag_data_cleaning.R")




# Create needed directories
dir.create(file.path("output",project_name), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("data",project_name), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("figures",project_name), recursive = TRUE, showWarnings = FALSE)







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
p_report_source <- c(0.05, 0.25)
# p_report_source <- data.frame(country=c("CHN", "KOR", "OTHER"),
#                               p_repor=tc(.05, .25, .15))


#Currently the model is set up to take both time- and location-varying proportion of cases reported. For now, we are using `r p_report_source[1]*100`% reported for Hubei, and `r p_report_source[2]*100`% reported elsewhere, constant over time.  
   
     
     
### Shift in incidence date
shift_incid_incub <- -5
shift_incid_detect <- -3
shift_incid_report <- -2
shift_incid_days <- shift_incid_incub + shift_incid_detect + shift_incid_report

# To appropriately allign reported cases of COVID-19 in source locations with the time during which they might be traveling, we shift case reporting dates by `r shift_incid_days` days (i.e., backward). This is a crude adjustment for now that will be improved.


  
    
    

# DATA --------------------------------------------------------------------


# Here we describe where we get data and do necessary formatting and cleaning of the data.
# 
# Three major sets of data are required for this importation estimation model: incidence in source locations, mobility or travel volume from source locations in to destination locations, and population of source locations. Data processing is done within separate R scripts for each. These each are setup to process and save the files in the correct formats for running the model.
# **To run these source scripts within this Rmd file, make sure to change `eval=FALSE` to `eval=TRUE`. 



# ~ Incidence data --------------------------------------------------------
#  Cases count data is acquired from the JHU CSSE COVID-19 Dashboard in real time  (https://gisanddata.maps.arcgis.com/apps/opsdashboard/index.html#/bda7594740fd40299423467b48e9ecf6). We pull the data each time the analysis is run, and then use spline fitting to estimate the incidence from these reported case counts. Smoothing is done to adjust for case reporting timing inconsistencies. 

# All the data pulling, processing, and cleaning is now done in the function below to keep it universal
# source("R/DataUtils.R")
incid_data_list <- get_incidence_data(first_date = ISOdate(2019,12,1), 
                                      last_date = Sys.time(), 
                                      pull_github_data=TRUE)

incid_data <- incid_data_list$incid_data
jhucsse    <- incid_data_list$jhucsse
incid_data <- incid_data %>% filter(source!="USA")

# We corrected for the large spike of cases on 13-14 February and smoothed over some of the heterogeneity that comes from reporting. These incidence estimates will be used in the importation model.




# ~ Travel Data  ----------------------------------------------------------

# Travel data are currently provided by the U.S. CDC through from OAG. These are monthly passenger volumes of complete iterneraries (i.e., airport of departure to final destination), which we then do some sampling with in the model to distribute volume to individual days.
# We will update this with travel forecasts eventually.


# source("R/setup_travel_data.R")
# #source("R/oag_data_cleaning.R")


if(get_travel)  travel_data_monthly <- get_oag_travel(destination=destination,
                                                      destination_type=destination_type,
                                                      dest_aggr_level=dest_aggr_level,
                                                      dest_0=dest_0, dest_0_type=dest_0_type)
travel_data_monthly <- read_csv(paste0("data/", paste(destination, collapse = "+"), "-", dest_aggr_level, "_oag_20172019.csv"), na=c(""," ","NA"))


# monthly average totals into destinations

travel_mean <- travel_data_monthly %>% mutate(dest = get(paste0("arr_", dest_aggr_level))) %>% group_by(dest, t_month) %>% 
  summarise(travelers = sum(travelers_mean,na.rm=TRUE)) %>% ungroup() %>%
  group_by(dest) %>%
  summarise(travelers = mean(travelers))

write_csv(travel_mean, paste0("data/", paste(destination, collapse = "+"), "-", dest_aggr_level, "_monthymeantravelers.csv"))


# Increase travel for Chinese New Year
travel_data_monthly <- travel_data_monthly %>% 
  mutate(travelers=travelers_mean, t_year=2020,) %>%
  mutate(travelers=ifelse(t_month == "01" & dep_country=="CHN", travelers*1.6, travelers)) 
travel_data_monthly <- travel_data_monthly %>% rename(source = dep_loc_aggr)

## Travel data
#  - Get daily for merging purposes
travel_data <- make_daily_travel(travel_data_monthly, travel_dispersion=3)

travel_data <- make_daily_travel_faster(travel_data=travel_data_monthly, 
                                        travel_data_daily=travel_data, 
                                        travel_dispersion=10)
  
   
    
# ~ Population Data -------------------------------------------------------
#    Population data are data for each province or country where transmission is 
#    currently occurring and from where importations could occur.

#source("R/setup_pop_data.R")
pop_data_file   <- file.path("data","pop_data.csv")
pop_data <- readr::read_csv(pop_data_file) %>% as.data.frame() 




# Merge Data ------------------------------------------------------------
#    Merge all of the input data into a single data.frame for inputting into the model.


# ~~ First Check that the variables match up ------------------------------

# Check that incidence data does not have duplicates
sum(incid_data %>% mutate(source_t = paste(source, t)) %>% mutate(dup_entry=duplicated(source_t)) %>%
      pull(dup_entry))
dup_entry <- incid_data %>% mutate(source_t = paste(source, t)) %>% mutate(dup_entry=duplicated(source_t)) %>%
      filter(dup_entry) %>% pull(source_t)
#View(incid_data %>% mutate(source_t = paste(source, t)) %>% filter(source_t %in% dup_entry))
## --> no duplicates at the moment...


# Check travel data
sum(travel_data %>% mutate(source_dest_t = paste(source, arr_airport, t)) %>% mutate(dup_entry=duplicated(source_dest_t)) %>%
      pull(dup_entry))
## --> no duplicates at the moment...


# Check Population data
sum(pop_data %>% mutate(dup_entry=duplicated(source)) %>%
      pull(dup_entry))
## --> no duplicates at the moment...


incid_sources <- sort(unique(incid_data$source))
travel_sources <- sort(unique(travel_data$source))
pop_sources <- sort(unique(pop_data$source))

# we really just need to make sure there are travel data and pop data for all source locations with incidence
incid_sources[!(incid_sources %in% travel_sources)]
incid_sources[!(incid_sources %in% pop_sources)]




# ~~ Merge it all ---------------------------------------------------------

input_data <- make_input_data(incid_data, travel_data, pop_data, 
                              shift_incid_days=shift_incid_days,
                              dest_aggr_level=dest_aggr_level)

# Summarize the data
summ_dat_dest <- input_data %>% 
  filter(t<=end_date) %>%
  group_by(destination, t) %>% 
  summarise(travel_sum=sum(travelers, na.rm = TRUE),
            cases_incid_sum=sum(cases_incid, na.rm = TRUE))
summ_dat_source <- input_data %>% 
  filter(t<=end_date) %>%
  group_by(source, destination) %>% 
  summarise(travel_sum=sum(travelers, na.rm = TRUE),
            cases_incid_sum=sum(cases_incid, na.rm = TRUE))




    


# MODEL -------------------------------------------------------------------


# ~ Model Setup -----------------------------------------------------------
#   Here we set up anything else for the model and running the simulations.    



# PARAMETERS

### Proportion cases reported
p_report_source <- c(0.05, 0.25)
# p_report_source <- data.frame(country=c("CHN", "KOR", "OTHER"),
#                               p_report=c(.05, .25, .15))

# For first pass, reporting rate is just Hubei/not Hubei
input_data <- input_data %>% 
  mutate(p_report_source = ifelse(source=="Hubei", p_report_source[1], p_report_source[2]))


# Add time unit to input data
# ~ delta: days per time period
delta <- 1
input_data$days_per_t <- delta

# Restrict forecast dates
input_data <- input_data %>% filter(t<=as.Date(end_date))


# FIX THIS LATER!!!!
# Drop small locations not in the pop data (FOR NOW)
input_data <- input_data %>% filter(!is.na(population))



# PARAMETER SAMPLING



# The "meanD_mat" here is the distribution of time during which an infected indivudal could 
#   potentially travel from a source to a sink/destination. This distribution includes the time 
#   from infection to isolation/quarantine for detected cases (typically hospitalized/reported), 
#   travel restriction or a decision not to travel, or for cases with asymptomatic or very mild illness, until recovery. 
#   This value is drawn from a combination of the other distributions show here.    

meanD_mat <- make_meanD(input_data, n_sim, 
                        incub_mean_log, incub_sd_log,
                        inf_period_hosp_shape, inf_period_hosp_scale,
                        inf_period_nohosp_mean, inf_period_nohosp_sd)


# ~ Time to detect importations -------------------------------------------
# -- If we assume people generally depart at some point during their incubation period, 
#     or very early in the symptomatic phase, 
#     we can generate a distribution of time from travel to detection.
# -- because we are only worried about those who are detected, we can ignore time to recover

time_inftodetect <- exp(rnorm(10000, mean = incub_mean_log, sd = incub_sd_log)) + 
  rgamma(10000, shape=inf_period_hosp_shape, scale=inf_period_hosp_scale)

# We assume people can and do travel during their incubation period and 
#  during that period during which symptoms are still minor. 
#  There are reports of travelers taking fever-reducers and a portion dont show fever
# We assume this is uniform
time_inftotravel <- sapply(time_inftodetect, runif, n=1, min=0)
time_traveltodetect <- time_inftodetect - time_inftotravel


# ~ Travel reductions -----------------------------------------------------
tr_inf_redux <- rep(0, n_sim)


# ~ Destination reporting rate --------------------------------------------
u_destination <- rep(1, n_sim)


# ~ Origin reporting rate -------------------------------------------------
u_origin <- matrix(rep(input_data$p_report_source, n_sim), nrow=n_sim, byrow = TRUE)


# save final input data
readr::write_csv(input_data, file.path("data", project_name, 
                                       sprintf("input_data_%s_batch_v%s.RData", batch, version)))



# ~ Travel restrictions -----------------------------------------------------

travel_restrictions <- 
  bind_rows(data.frame(loc=unique((input_data %>% filter(dep_country=="CHN"))$source), 
                     min=hubei_shutdown[1], max=hubei_shutdown[2], p_travel=.1) %>% filter(loc!="Hubei"), # Reduce travel from all chinese sources to 20% 
          data.frame(loc="Hubei", min=hubei_shutdown[1], max=hubei_shutdown[2], p_travel=0),
          data.frame(loc=unique((input_data %>% filter(dep_country=="USA"))$source), 
                     min="2020-03-02", max="2020-03-08", p_travel=.6), # Reduce travel from all US sources to 50%
          data.frame(loc=unique((input_data %>% filter(dep_country!="CHN" & dep_country!="USA"))$source), 
                     min="2020-03-02", max="2020-03-08", p_travel=.3), # Reduce travel from all US sources to 50%
          data.frame(loc=unique((input_data %>% filter(dep_country=="USA"))$source), 
                     min="2020-03-09", max=hubei_shutdown[2], p_travel=.3), # Reduce travel from all US sources to 50%
          data.frame(loc=unique((input_data %>% filter(dep_country!="CHN" & dep_country!="USA"))$source), 
                     min="2020-03-09", max=hubei_shutdown[2], p_travel=.1), # Reduce travel from all US sources to 50%
          data.frame(loc=unique((input_data %>% filter(dep_country!="CHN" & dep_country!="USA"))$source), 
                     min="2020-03-16", max=hubei_shutdown[2], p_travel=.2)) # Reduce travel from all US sources to 50%

# expand these data out to include all dates. This will allow for merging more easily.
travel_restrictions_long <- expand_travel_restrict(travel_restrictions)


travel_data_monthly <- travel_data_monthly %>% 
  dplyr::select(source, destination=paste0("arr_", dest_aggr_level), t_month, t_year, travelers)




# Run Model -------------------------------------------------------------

#source("R/import_model_source.R")
print(paste0("Proportion Reported in the sources: ",p_report_source))


t.start <- proc.time() # start timer to measure this

# input_data <- readr::read_csv(file.path("data", project_name, sprintf("input_data_%s_batch_v%s.RData", batch, version)))
input_data <- input_data %>% mutate(source = as.character(source), 
                                    destination = as.character(destination))


## Filter to sources with cases -- to speed it up
source_w_cases <- input_data %>% filter(!duplicated(paste0(source, t))) %>% 
  group_by(source) %>% 
  summarise(cum_cases = sum(cases_incid, na.rm=TRUE)) %>%
  filter(cum_cases>0)

input_data_cases <- input_data %>% filter(source %in% source_w_cases$source)
travel_data_monthly <- travel_data_monthly %>% filter(source %in% source_w_cases$source) 


## Filter destination airports
# dest_airports <- NULL
# input_data_cases <- input_data_cases %>% filter(destination %in% dest_airports) 
# travel_data_monthly <- travel_data_monthly %>% filter(destination %in% dest_airports)


# SAVE ALL THE DATA NEEDED
dir.create(file.path("data", project_name))
write_csv(input_data_cases, file.path("data", project_name, "input_data_cases.csv"))
write_csv(travel_data_monthly %>% filter(as.integer(t_month)<=3), file.path("data", project_name, "travel_data_monthly.csv"))
write_csv(travel_restrictions, file.path("data", project_name, "travel_restrictions.csv"))



# MOVED TO RUN FILE

# Need to regen the meanD mat (because we got rid of sources/destinations)
meanD_mat <- make_meanD(input_data_cases, n_sim,
                        incub_mean_log, incub_sd_log,
                        inf_period_hosp_shape, inf_period_hosp_scale,
                        inf_period_nohosp_mean, inf_period_nohosp_sd)

# Run the model
importation_sim <- run_daily_import_model_par(
  n_sim=100,
  input_data = input_data_cases,
  travel_data_monthly = travel_data_monthly,
  travel_dispersion=3,
  travel_restrictions=travel_restrictions,
  allow_travel_variance=FALSE,
  meanD_mat=meanD_mat, 
  tr_inf_redux=tr_inf_redux, 
  u_origin=u_origin, 
  get_detection_time=FALSE,
  incub_mean_log, incub_sd_log,
  inf_period_hosp_shape, inf_period_hosp_scale,
  inf_period_nohosp_mean, inf_period_nohosp_sd,
  project_name=project_name, batch=batch, version=version, 
  print_progress=TRUE,
  cores=4)

# print time required
print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))




