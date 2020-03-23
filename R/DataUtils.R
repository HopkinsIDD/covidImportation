
#library(globaltoolbox)
library(tidyverse)
library(countrycode)

source("R/DataLoadUtils.R")



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

get_incidence_data <- function(first_date = ISOdate(2019,12,1), 
                               last_date = Sys.time(), 
                               pull_github_data=TRUE){
    
    ## Get case count data (from JHU CSSE's github)
    if (pull_github_data){
        pull_JHUCSSE_github_data() # Pull and save data from github
    }
    
    # Combine JHU CSSE data
    jhucsse <- read_JHUCSSE_cases(last_time = Sys.time(), append_wiki=TRUE) 
    # Make all Diamond Princess Cases same source
    jhucsse <- jhucsse %>% mutate(Province_State = ifelse(grepl("diamond princess", Province_State, ignore.case = TRUE), "Diamond Princess", Province_State)) 

    
    # These data are getting messy. Let's clean them up
    
    # Fix bad locations
    jhucsse <- jhucsse %>% filter(!(Province_State %in% c("US"))) %>%
                           mutate(Province_State = ifelse(grepl("Chicago", Province_State), "Chicago, IL", Province_State)) %>%
                           mutate(Province_State = ifelse(grepl("Ningxia", Province_State), "Ningxia", Province_State)) %>%
                           mutate(Province_State = ifelse(Province_State=="Inner Mongolia", "Nei Mongol", Province_State)) %>%
                           mutate(Province_State = ifelse(Province_State=="Hong Kong", "HKG", Province_State))
    
    
    
    # Get US States ................
    # Separate out states
    jhucsse <- jhucsse %>% separate(Province_State, sep = ', ', c('city', 'state'), convert = TRUE, remove=FALSE) %>%
        mutate(city = ifelse(is.na(state), NA, city))
    # -- lots of NAs where no commas. Not a problem.
    
    # Get states where not already there
    jhucsse <- jhucsse %>% mutate(state_tmp = state.abb[match(Province_State, state.name)]) %>%
        mutate(state = ifelse(!is.na(state_tmp) & is.na(state), state_tmp, state)) %>%
        select(-state_tmp) %>%
        mutate(state = ifelse(state=="D.C.", "DC", state))
    
    # # Australian states .............
    #  --(for now we will just use the country of Australia. need to change to state eventually)
    # aus_states <- c("New South Wales", "Victoria", "Queensland", "Western Australia", "South Australia", "Tasmania")
    
    # Fix China ..............
    #unique(grep("China", incid_data$Country_Region, value = TRUE, ignore.case = TRUE))
    jhucsse <- jhucsse %>% 
        mutate(Country_Region = ifelse(Province_State %in% c("Inner Mongolia"), 
                                       "China", Country_Region)) %>%
        mutate(Country_Region = ifelse(Country_Region %in% c("Mainland China", "Hong Kong", "Macau", "Taiwan", "Nei Mongol"),
                                       "China", Country_Region))
    
    
    # Get ISO Code .....................
    library(globaltoolbox)
    jhucsse <- jhucsse %>% 
        mutate(country = globaltoolbox::get_iso(Country_Region)) %>%
        mutate(country_name = globaltoolbox::get_country_name_ISO3(country))
    jhucsse <- jhucsse %>% mutate(country_name = ifelse(country=="KOS", "Kosovo", country_name))
    #unique((incid_data %>% filter(is.na(country)))$source)
    # country_ <- countrycode::countrycode(unique(jhucsse$Country_Region), 
    #                                      origin="country.name", destination = "iso3c", 
    #                                      origin_regex = TRUE, nomatch = NULL)
    
    # Define a single source location variable
    # - USA: States used for source
    # - China: Provinces used for source
    # - Others: Country used for source
    jhucsse <- jhucsse %>% mutate(source = ifelse(country=="USA" & !is.na(state), state,
                                                  ifelse(country=="CHN" & !is.na(Province_State), Province_State, country)))
    
    # Get rid of duplicate rows
    jhucsse <- jhucsse %>% distinct()
    
    # Get incident cases, sum them , then get cumulative from that
    jhucsse <- jhucsse %>% arrange(country, Province_State, Update) %>%
        group_by(Province_State, country) %>% 
        mutate(incid_conf = diff(c(0,Confirmed))) %>% ungroup()
    
    # Fix counts that go negative
    negs_ind <- which(jhucsse$incid_conf < 0)
    jhucsse$Confirmed[negs_ind - 1] <- jhucsse$Confirmed[negs_ind - 1] + jhucsse$incid_conf[negs_ind] 
    jhucsse <- jhucsse %>% arrange(country, Province_State, Update) %>%
        group_by(Province_State, country) %>% 
        mutate(incid_conf = diff(c(0,Confirmed))) %>% ungroup()
    jhucsse <- jhucsse %>% filter(incid_conf>=0)  
    
    
    # Manually get rid of bad data
    jhucsse <- jhucsse %>% filter(!(Province_State %in% c("The Bahamas", "Republic of the Congo"))) %>%
        filter(!(Province_State=="UK" & Update=="2020-03-11 21:33:03")) %>%
        mutate(Province_State = ifelse(Province_State=="United Kingdom", "UK", Province_State))
    
    
    ## GET INCIDENCE FITS ..........................
    ## Estimate incidence using spline fits.
    incid_data <- est_daily_incidence_corrected(jhucsse, 
                                                first_date, last_date, tol=100, na_to_zeros=FALSE) %>%
        mutate(Incidence=round(Incidence, 2))
    
    ## Incidence Data
    incid_data <- incid_data %>% rename(source=Province_State, cases_incid=Incidence) %>% 
        mutate(source = as.character(source),
               t = as.Date(Date)) %>% 
        as.data.frame()
    
    # Add country_name back in
    incid_data <- left_join(incid_data, 
                            jhucsse %>% select(Province_State, country_name, country, source_new=source) %>%
                                mutate(prov_country = paste0(Province_State,"-", country_name)) %>%
                                filter(!duplicated(prov_country)) %>% select(-prov_country), 
                            by=c("source"="Province_State"))  
    # Add confirmed cases back in
    incid_data <- left_join(incid_data, 
                            jhucsse %>% mutate(t = as.Date(Update)) %>% 
                                select(t, Province_State, country_name,incid_conf, Confirmed) %>%
                                mutate(prov_country = paste0(Province_State,"-", country_name)) %>%
                                filter(!duplicated(prov_country)) %>% select(-prov_country, -country_name), 
                            by=c("source"="Province_State", "t"="t"))  
    incid_data <- incid_data %>% mutate(incid_conf=ifelse(is.na(incid_conf), 0, incid_conf))
    
    
    # Group by source location, and add up incidence, then get cumulatives 
    incid_data <- incid_data  %>%  dplyr::select(-source) %>%
        group_by(source_new, country, country_name, t) %>% 
        summarise(incid_est = sum(cases_incid)) %>% 
        as.data.frame() %>%
        rename(source=source_new)
    

    # Drop NA source
    #View(incid_data %>% filter(is.na(source)))
    incid_data <- incid_data %>% filter(!is.na(source))
    
    return(list(incid_data=incid_data, jhucsse=jhucsse))
}











# Subset OAG Travel Data --------------------------------------------------

##'
##' Get subsetted and cleaned OAG data for a specific destination
##' 
##' @param destination destination of interest; can be a vector.
##' @param destination_type options: "airport", "city", "state", "country"
##' @param dest_0 default=NULL; change to specify higher level destination (i.e. dest_0="USA")
##' @param dest_0_type default=NULL; must specify if specifying a `dest_0` option.
##' @param dest_aggr_level level to which travel will be aggregated for destination. Includes "airport", "city", "state", "country", "metro" (only available for CA currently)

get_oag_travel <- function(destination=c("CA"), destination_type="state", 
                           dest_0=NULL, dest_0_type=NULL,
                           dest_aggr_level="city"){
    
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

    if (!is.null(dest_0)){
        if (dest_0_type=="city"){
            dest_data <- dest_data %>% filter(`Arr City Name` %in% dest_0)
        } else if (dest_0_type=="airport"){
            dest_data <- dest_data %>% filter(`Arr Airport Code` %in% dest_0)
        } else if (dest_0_type=="state"){
            dest_data <- dest_data %>% filter(`Arr State Code` %in% dest_0)
        } else if (dest_0_type=="country"){
            dest_data <- dest_data %>% filter(`Arr Country Code` %in% dest_0)
        }
    }
    

    # Give Chinese airports the provinces 
    airport_attribution <- read_csv(file ='data/airport_attribution.csv')
    
    dest_data <- left_join(dest_data, 
                           airport_attribution %>% 
                               mutate(Province = gsub(" Province", "", Province)) %>% 
                               mutate(Province = gsub(" province", "", Province)) %>% 
                               mutate(Province = gsub(" Special Administrative Region", "", Province)) %>% 
                               mutate(Province = gsub(" Autonomous Region", "", Province)) %>% 
                               mutate(Province = gsub(" Municipality", "", Province)) %>% 
                               mutate(Province = ifelse(grepl("Xinjiang", Province), "Xinjiang", Province)) %>% 
                               mutate(Province = ifelse(grepl("Guangxi", Province), "Guangxi", Province)) %>% 
                               mutate(Province = ifelse(grepl("Ningxia", Province), "Ningxia", Province)) %>% 
                               mutate(Province = ifelse(grepl("Inner Mongolia", Province), "Nei Mongol", Province)) %>% 
                               mutate(Province = ifelse(grepl("Macao", Province), "Macau", Province)) %>% 
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
    
    
    # Get Metro areas (only available for CA currently)
    if (length(destination)==1 && destination=="CA"){
        
        ca_airport_attibution <- read_csv("data/ca/airport_attribution.csv")
        ca_airport_attibution <- ca_airport_attibution %>% mutate(metrop_labels=ifelse(is.na(metrop_labels), "Other", metrop_labels))
        
        # choose 1 place to give attribution
        ca_airport_attibution <- ca_airport_attibution %>% group_by(metrop_labels, airport_iata) %>%
            summarise(attribution = sum(attribution, na.rm = TRUE))
        ca_airport_attibution <- ca_airport_attibution %>% group_by(airport_iata) %>% filter(attribution==max(attribution)) %>% ungroup()
        
        dest_data_aggr <- left_join(dest_data_aggr, 
                                    ca_airport_attibution %>% select(airport_iata, metrop_labels),
                                    by=c("arr_airport"="airport_iata")) %>% 
            rename(arr_metro = metrop_labels)
        dest_data_aggr <- dest_data_aggr %>% mutate(arr_metro = ifelse(is.na(arr_metro), "Other", arr_metro))
    }
    
    # aggregation levels for destination
    aggr_levels <- factor(c("airport", "city", "metro", "state", "country"), levels=c("airport", "city", "metro", "state", "country"), ordered = TRUE)
    loc_vars_aggr <- c("arr_airport", "arr_city","arr_metro", "arr_state", "arr_country")[aggr_levels>=dest_aggr_level]
    loc_vars_aggr <- loc_vars_aggr[loc_vars_aggr %in% colnames(dest_data_aggr)]
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
    write_csv(dest_data_aggr, paste0("data/", paste(destination, collapse = "+"), "-", dest_aggr_level, "_oag_20172019.csv"))
    
}



# # Save CA cities
# dest_data_aggr <- get_oag_travel(destination="CA", destination_type="state", dest_aggr_level="airport")
# ca_airports <- dest_data_aggr %>% group_by(arr_airport, arr_city, arr_state, arr_country) %>% summarise(n_occur = n())
# write_csv(ca_airports, "data/ca_airports.csv")




##' 
##' Get Metro Areas for CA
##' 
##'  define metropolitan areas
##'  
get_metro_labels <- function(data){
    
    LA <- c('06037', '06059', '06065', '06071', '06111')
    SF <- c('06001', '06013', '06075', '06081', '06041', '06085', '06069', 
            '06077', '06099', '06095', '06097', '06087', '06047', '06055')
    SD <- c('06073')
    FN <- c('06019','06031','06039')
    SC <- c('06067', '06061', '06113', '06017', '06101', '06115', '06057')
    RD <- c('06089', '06103')
    
    data$county <- paste0("0",data$county)
    data$new_metrop <- 0
    data$new_metrop[data$county %in% LA] <- "LA"
    data$new_metrop[data$county %in% SF] <- "SF"
    data$new_metrop[data$county %in% SD] <- "SD"
    data$new_metrop[data$county %in% FN] <- "FN"
    data$new_metrop[data$county %in% SC] <- "SC"
    data$new_metrop[data$county %in% RD] <- "RD"
    
    ##Update the labels
    data$metrop_labels <- NA
    data$metrop_labels[data$new_metrop=="LA"] <- "Los Angeles"
    data$metrop_labels[data$new_metrop=="SF"] <- "San Francisco"
    data$metrop_labels[data$new_metrop=="SD"] <- "San Diego"
    data$metrop_labels[data$new_metrop=="FN"] <- "Fresno"
    data$metrop_labels[data$new_metrop=="SC"] <- "Sacremento"
    data$metrop_labels[data$new_metrop=="RD"] <- "Redding"
    data$metrop_labels <- as.factor(data$metrop_labels)
    
    return(data)
}






# Produce Combined Input Data ---------------------------------------------


make_input_data <- function(incid_data, travel_data, pop_data, 
                            dest_aggr_level,
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
        dplyr::select(source, dep_country=country, population=pop)
    incid_data_  <- incid_data %>% mutate(source = as.character(source)) %>%
        dplyr::select(source, t, incid_est, dep_country=country)
    
    
    
    # ~~  Check that the variables match up
    
    duplicates <- c(
        # Check that incidence data does not have duplicates
        sum(incid_data %>% mutate(source_t = paste(source, t)) %>% 
                mutate(dup_entry=duplicated(source_t)) %>% pull(dup_entry)),
        ## --> no duplicates at the moment...
        
        # Check travel data
        sum(travel_data %>% mutate(source_dest_t = paste(source, arr_airport, t)) %>% 
                mutate(dup_entry=duplicated(source_dest_t)) %>% pull(dup_entry)),
        ## --> no duplicates at the moment...
        
        # Check Population data
        sum(pop_data %>% mutate(dup_entry=duplicated(source)) %>% pull(dup_entry))
        ## --> no duplicates at the moment...
    )
    
    if (sum(duplicates)>0){
        return(paste0("Error: There were ", 
                            duplicates[1], " in incidence data, ",
                            duplicates[2], " in travel data, and ",
                            duplicates[3], " in population data."))
    }
    
    
    # aggregation levels for destination
    arr_vars <- c("arr_airport", "arr_city","arr_metro", "arr_state", "arr_country")
    other_vars <- c("source", "dep_country", "t", "t_day", "t_month", "t_year", "travelers", "travelers_month")
    arr_vars <- arr_vars[arr_vars %in% colnames(travel_data)]
    
    travel_data_ <- travel_data %>% mutate(source = as.character(source)) %>% 
        dplyr::select(c(other_vars, arr_vars))
    
    # combine them all
    input_data <- full_join(
        right_join(pop_data_, travel_data_, by=c("source", "dep_country")), 
        incid_data_, by=c("source", "dep_country", "t")) 
    input_data <- input_data %>% rename(cases_incid=incid_est)
    
    start_date <- min((input_data %>% filter(cases_incid>0))$t)
    
    # filter data by time and location
    input_data <- input_data %>%
        #filter(t > as.Date("2019-12-31")) %>% 
        filter(t >= as.Date(start_date)) %>% 
        mutate(cases_incid=ifelse(is.na(cases_incid), 0, cases_incid),
               epiweek = lubridate::epiweek(t))
    input_data <- input_data %>% filter(!is.na(travelers)) 
    
    
    # Make all negatives 0
    input_data$travelers[input_data$travelers<0] <- 0
    
    # Set the destination to be the correct level
    if (dest_aggr_level=="city"){
        input_data$destination <- input_data$arr_city
    } else if (dest_aggr_level=="airport"){
        input_data$destination <- input_data$arr_airport
    } else if (dest_aggr_level=="metro"){
        input_data$destination <- input_data$arr_metro
    } else if (dest_aggr_level=="state"){
        input_data$destination <- input_data$arr_state
    } else if (dest_aggr_level=="country"){
        input_data$destination <- input_data$arr_country
    }
    
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



