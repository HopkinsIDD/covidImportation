
library(globaltoolbox)
library(tidyverse)
library(countrycode)



# Get Airport Info --------------------------------------------------------

##' Get Airport Location Data
##' These functions are all vectorized

get_airport_city <- function(airport_code = "ORD"){
    airport_data <- read_csv("data/airport-codes.csv")
    return((airport_data %>% filter(iata_code %in% airport_code))$municipality)
}


get_airport_state <- function(airport_code = "ORD"){
    airport_data <- read_csv("data/airport-codes.csv")
    return(substr((airport_data %>% filter(iata_code %in% airport_code))$municipality, 4,5))
}


get_airport_country <- function(airport_code = "ORD"){
    airport_data <- read_csv("data/airport-codes.csv")
    return((airport_data %>% filter(iata_code %in% airport_code))$iso_country)
}












# Get Incidence Data ------------------------------------------------------



## This function requires globaltoolbox package, a package that is not yet public, to get ISO from country names.

get_incidence_data <- function(first_date = ISOdate(2019,12,1), last_date = Sys.time(), pull_github_data=TRUE){
    
    ## Get case count data (from JHU CSSE's github)
    if (pull_github_data){
        pull_JHUCSSE_github_data() # Pull and save data from github
    }
    
    # Combine JHU CSSE data
    jhucsse <- read_JHUCSSE_cases(last_time = Sys.time(), append_wiki=TRUE)
    # Make all Diamond Princess Cases same source
    jhucsse <- jhucsse %>% mutate(Province_State = ifelse(grepl("diamond princess", Province_State, ignore.case = TRUE), "Diamond Princess", Province_State))
    
    ## Estimate incidence using spline fits.
    incid_data <- est_daily_incidence_corrected(jhucsse, first_date, last_date, tol=50, na_to_zeros=FALSE) %>%
        mutate(Incidence=round(Incidence, 2))
    
    ## Incidence Data
    incid_data <- incid_data %>% rename(source=Province_State, cases_incid=Incidence) %>% 
        mutate(source = as.character(source)) %>%
        mutate(source = ifelse(source=="Ningxia Hui", "Ningxia", source), t = as.Date(Date)) %>% 
        as.data.frame()
    
    
    # Get Country --
    
    # Add Country_Region back in
    incid_data <- left_join(incid_data, 
                            jhucsse %>% select(Province_State, Country_Region) %>%
                                mutate(prov_country = paste0(Province_State,"-", Country_Region)) %>%
                                filter(!duplicated(prov_country)) %>% select(-prov_country), 
                            by=c("source"="Province_State"))  
    
    # Keep the original source Variable
    incid_data <- incid_data %>% mutate(source_orig = source)
    
    # Fix China
    #unique(grep("China", incid_data$Country_Region, value = TRUE, ignore.case = TRUE))
    incid_data <- incid_data %>% mutate(country_names=Country_Region) %>% 
        mutate(country_names = ifelse(country_names %in% c("Mainland China", "Hong Kong", "Macau", "Taiwan", "Nei Mongol"),
                                      "China", country_names)) %>% 
        mutate(country_names = ifelse(source %in% c("Nei Mongol"), 
                                      "China", country_names))
    
    # Get ISO Code
    incid_data$country <- globaltoolbox::get_iso(incid_data$country_names)
    #unique((incid_data %>% filter(is.na(country)))$source)
    
    # Separate out states
    incid_data <- incid_data %>% separate(source, sep = ', ', c('city', 'state'), convert = TRUE, remove=FALSE) %>%
        mutate(city = ifelse(is.na(state), NA, city))
    # -- lots of NAs where no commas. Not a problem.
    
    # Get states where not already there
    incid_data <- incid_data %>% mutate(state_tmp = state.abb[match(source, state.name)]) %>%
        mutate(state = ifelse(!is.na(state_tmp) & is.na(state), state_tmp, state)) %>%
        select(-state_tmp)
    
    # # Aggregate sources
    # incid_data <- incid_data %>% mutate(source = ifelse())
    
    
    # Set up sources
    #unique(incid_data$source)
    incid_data <- incid_data %>% mutate(source = ifelse(source=="Inner Mongolia", "Nei Mongol", source))
    incid_data <- incid_data %>% mutate(source_orig = ifelse(source_orig=="Inner Mongolia", "Nei Mongol", source_orig))
    incid_data <- incid_data %>% mutate(source = ifelse(country=="USA" & !is.na(state), state, source))
    incid_data <- incid_data %>% mutate(source = ifelse(country!="USA" & country!="CHN", country, source))
    #unique(incid_data$source)
    
    # Drop NA source
    #View(incid_data %>% filter(is.na(source)))
    incid_data <- incid_data %>% filter(!is.na(source))
    
    return(list(incid_data=incid_data, jhucsse=jhucsse))
}






# Produce Combined Input Data ---------------------------------------------


make_input_data <- function(incid_data, travel_data, pop_data, 
                            shift_incid_days=NA){
    
    ## Incidence data
    #  - Shift incid_data dates to align with incubation period
    if (!is.na(shift_incid_days)){
        incid_data <- incid_data %>% mutate(t = as.Date(t) + shift_incid_days)
    }
    
    # Drop the cruise ship
    incid_data <- incid_data %>% filter(!grepl("cruise ship", source, ignore.case = TRUE) | 
                                                      !grepl("diamond princess", source, ignore.case = TRUE))
    
    # merge data (delimit it by travel data)
    pop_data_    <- pop_data %>% mutate(source = as.character(source)) %>%
        dplyr::select(source, dep_country=country, population)
    travel_data_ <- travel_data %>% mutate(source = as.character(source)) %>% 
        dplyr::select(source, dep_country, arr_city, arr_country, t, t_day, t_month, t_year, travelers, travelers_month)
    incid_data_  <- incid_data %>% mutate(source = as.character(source)) %>%
        dplyr::select(source, t, cases_incid, dep_country=country)
    
    # combine them all
    input_data <- full_join(right_join(pop_data_, 
                                       travel_data_,  by=c("source", "dep_country")), 
                            incid_data_,   by=c("source", "dep_country", "t"))
    
    start_date <- min((incid_data %>% filter(cases_incid>0))$t)
    
    # filter data by time and location
    input_data <- input_data %>%
        #filter(t > as.Date("2019-12-31")) %>% 
        filter(t >= as.Date(start_date)) %>% 
        mutate(cases_incid=ifelse(is.na(cases_incid), 0, cases_incid),
               epiweek = lubridate::epiweek(t))
    input_data <- input_data %>% filter(!is.na(travelers)) 
    
    
    # Make all negatives 0
    input_data$travelers[input_data$travelers<0] <- 0
    
    # Set the destination to be the City 
    input_data$destination <- input_data$arr_city
    

    return(input_data)
}






# Make MeanD Matrix -------------------------------------------------------

make_meanD <- function(input_data, n_sim, 
                       incub_mean_log, incub_sd_log,
                       inf_period_hosp_shape, inf_period_hosp_scale,
                       inf_period_nohosp_mean, inf_period_nohosp_sd){
    
    # Sample the components of meanD -- will apply the p_report_source to these
    meanD_mat_ <- cbind(
        exp(rnorm(n_sim, mean = incub_mean_log, sd = incub_sd_log)),
        rgamma(n_sim, shape=inf_period_hosp_shape, scale=inf_period_hosp_scale),
        MCMCglmm::rtnorm(n_sim, inf_period_nohosp_mean, inf_period_nohosp_sd, lower=0))
    
    # Apply p_report_source by location and time to get the meanD matrix, where each simulation run has a pre-sampled set of D for each time/location combination
    meanD_mat <- meanD_mat_[,1] + 
                 meanD_mat_[,2] %*% matrix(input_data$p_report_source, nrow=1) + 
                 meanD_mat_[,3] %*% matrix((1-input_data$p_report_source), nrow=1)
    
    return(meanD_mat)
}






# Subset OAG Travel Data --------------------------------------------------

##'
##' Get subsetted and cleaned OAG data for a specific destination
##' 
##' @param destination destination of interest; can be a vector
##' @param destination_type options: "airport", "city", "state", "country"
##' @param dest_aggr_level level to which travel will be aggregated for destination. Includes "airport", "city", "state", "country"

get_oag_travel <- function(destination="CA", destination_type="state", dest_aggr_level="city"){
    
    # Read full data
    # these data are clean in  `oag_data_cleaning.R`
    data_travel_all <- read_csv(file.path("data", "complete_OAG_data.csv"), na=c(""," ", "NA"),
                                col_types = list(
                                    `Dep Airport Code` = col_character(),
                                    `Dep City Name` = col_character(),
                                    `Dep State Code` = col_character(),
                                    `Dep Country Code` = col_character(),
                                    `Arr Airport Code` = col_character(),
                                    `Arr City Name` = col_character(),
                                    `Arr State Code` = col_character(),
                                    `Arr Country Code` = col_character(),
                                    `Total Est. Pax` = col_double(),
                                    `Time Series` = col_double()))
    
    if (destination_type=="city"){
        dest_data <- data_travel_all %>% filter(`Arr City Name` %in% destination)
    } else if (destination_type=="airport"){
        dest_data <- data_travel_all %>% filter(`Arr Airport Code` %in% destination)
    } else if (destination_type=="state"){
        dest_data <- data_travel_all %>% filter(`Arr State Code` %in% destination)
    } else if (destination_type=="country"){
        dest_data <- data_travel_all %>% filter(`Arr Country Code` %in% destination)
    }
        

    # Give Chinese airports the provinces 
    airport_attribution <- read_csv(file ='data/airport_attribution.csv')
    
    dest_data <- left_join(dest_data, 
                           airport_attribution %>% mutate(Province = gsub(" Province", "", Province)) %>% 
                               select(-attribution),
                           by=c("Dep Airport Code"="airport_iata"))
    
    
    # Get us State codes for departures
    airport_data <- read_csv("data/airport-codes.csv")
    airport_data <- airport_data %>% mutate(iso_country = ifelse(iso_country=="XK", "KOS",
                                                                 countrycode::countrycode(iso_country, origin = "iso2c", destination = "iso3c")))
    airport_data_us <- airport_data %>% filter(iso_country=="USA")
    
    dest_data <- left_join(dest_data,
                           airport_data_us %>% mutate(state = substr(iso_region, 4,5)) %>%
                               select(state, iata_code),
                           by=c("Dep Airport Code"="iata_code"))
    dest_data <- dest_data %>% mutate(`Dep State Code`=ifelse(is.na(`Dep State Code`) & !is.na(state), state, `Dep State Code`))
    
    
    # Aggregate SOURCE LOCATION to province (China) or state (US) or country (all others) for source
    dest_data <- dest_data %>% rename(dep_airport = `Dep Airport Code`,
                                      dep_state = `Dep State Code`,
                                      dep_country = `Dep Country Code`,
                                      dep_city = `Dep City Name`,
                                      arr_airport = `Arr Airport Code`,
                                      arr_city = `Arr City Name`,
                                      arr_state = `Arr State Code`,
                                      arr_country = `Arr Country Code`,
                                      arr_city = `Arr City Name`,
                                      travelers = `Total Est. Pax`,
                                      yr_month = `Time Series`,
                                      dep_province = Province)
    
    # Fix US cities with "(US) [STATE]" in name
    dest_data <- dest_data %>% mutate(arr_city = gsub(" \\(US\\).*", "", arr_city))


    # Aggregate to dest_aggr_level, then get mean across 3 years ...................

    dest_data_aggr <- dest_data %>%
        mutate(dep_loc_aggr = ifelse(dep_country=="CHN", dep_province, ifelse(dep_country=="USA", dep_state, dep_country)),
               t_year = substr(yr_month, 1,4), 
               t_month = as.character(substr(yr_month, 5,6)))    # Get year and month variables
    
    # aggregation levels for destination
    aggr_levels <- factor(c("airport", "city", "state", "country"), levels=c("airport", "city", "state", "country"), ordered = TRUE)
    loc_vars_aggr <- c("arr_airport", "arr_city", "arr_state", "arr_country")[aggr_levels>=dest_aggr_level]
    other_vars_aggr <- c("yr_month", "t_year", "t_month", "dep_loc_aggr", "dep_country")
    
    dest_data_aggr <- dest_data_aggr %>% group_by(.dots = c(other_vars_aggr, loc_vars_aggr)) %>%
        summarise(travelers = sum(travelers, na.rm = TRUE))
    
    # Get Monthly means across the 3 year (using geometric means)
    other_vars_aggr <- c("t_month", "dep_loc_aggr", "dep_country")
    dest_data_aggr <- dest_data_aggr %>%
        group_by(.dots = c(other_vars_aggr, loc_vars_aggr)) %>%
        summarise(travelers_sd = sd(travelers),
                  travelers_mean = exp(mean(log(travelers+1)))-1)
    
    dest_data_aggr <- dest_data_aggr %>% mutate(travelers_sd = ifelse(is.nan(travelers_sd), travelers_mean/1.96, travelers_sd)) # for those with only 1 value for travel, just use that /2 for the SD
    
    # Save it
    write_csv(dest_data_aggr, paste0("data/", destination, "_oag_20172019_aggr.csv"))
    
}



# # Save CA cities
# dest_data_aggr <- get_oag_travel(destination="CA", destination_type="state", dest_aggr_level="airport")
# ca_airports <- dest_data_aggr %>% group_by(arr_airport, arr_city, arr_state, arr_country) %>% summarise(n_occur = n())
# write_csv(ca_airports, "data/ca_airports.csv")


