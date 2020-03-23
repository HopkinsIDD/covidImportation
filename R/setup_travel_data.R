
# TRAVEL DATA 

library(tidyverse) 
library(lubridate)



# Distribute travelers over days ------------------------------------------

# Create Daily Travel
##'
##' Function to extract approximate epidemic curves
##' from the cumulative case data.
##'
##' @param travel_data monthly travel data
##' @param travel_dispersion How dispersed daily travel should be. 
##'  -- Set to 10 for very (i.e., most of travel on a couple days)
##'  -- Set to 3  for moderate
##'  -- Set to .01 for none (evenly mixed across days)
##' 
##'
##' @return a data frame with randomly distributed travel into days
##'
make_daily_travel <- function(travel_data, travel_dispersion=10){
    
    travel_data <- travel_data %>% 
        rename(travelers_month = travelers) %>% 
        mutate(days_month = lubridate::days_in_month(as.integer(t_month)))
    
    rows_ <- nrow(travel_data)
    
    # First sample out the monthly travelers into days
    x <- as.integer(unlist(lapply(X=1:rows_, 
                                  FUN=function(x=X) rmultinom(1, travel_data$travelers_month[x], 
                                                    rgamma(travel_data$days_month[x], shape=1/travel_dispersion)))))
    
    # get an indicator for day of the month
    t_day <- unlist(lapply(X=1:rows_, FUN=function(x=X) 1:travel_data$days_month[x]))
    # generate a daily dataset
    data_daily <- as.data.frame(lapply(travel_data, rep, travel_data$days_month))
    # Add new daily travel volume to it
    data_daily <- data.frame(data_daily, t_day=t_day, travelers=x)

    data_daily <- data_daily %>% mutate(t = as.Date(paste(t_year, t_month, t_day, sep="-")))
    return(data_daily)
}





##'
##' Convert monthly travel to daily travel data -- fast
##' - When we have already built the daily data, we reuse that and just fill in the new daily volume each time
##' 
##' @param travel_data Data.frame. Monthly travel data with columns travelers, t_month, and days_month
##' @param travel_data_daily Data.frame. Daily travel data that was previously built. We replace the travelers column in this.
##' @param travel_dispersion Numeric. Value defining how evenly distributed daily travel is across a month. 
##' 
make_daily_travel_faster <- function(travel_data, travel_data_daily, travel_dispersion=10){
    
    travel_data <- travel_data %>% 
        rename(travelers_month = travelers) %>% 
        mutate(days_month = lubridate::days_in_month(as.integer(t_month)))
    
    rows_ <- nrow(travel_data)
    
    # First sample out the monthly travelers into days
    x <- as.integer(unlist(lapply(X=1:rows_, 
                                  FUN=function(x=X) rmultinom(1, travel_data$travelers_month[x], 
                                                              rgamma(travel_data$days_month[x], shape=1/travel_dispersion)))))
    
    travel_data_daily$travelers <- x 
    
    return(travel_data_daily)
}




##'
##' Expand the travel restrictions to include every date, to allow for merging with travel data. 
##' 
##' @param travel_restrictions data.frame of travel restrictions with columns loc, min, max, p_travel
##' 
##' 
expand_travel_restrict <- function(travel_restrictions){
    
    travel_restrictions <- travel_restrictions %>% mutate(min=as.Date(min), 
                                                          max=as.Date(max))
    travel_restrict_ <- list()
    for (r in 1:nrow(travel_restrictions)){
        travel_restrict_[[r]] <- data.frame(loc=travel_restrictions$loc[r], 
                                            p_travel=travel_restrictions$p_travel[r], 
                                            t = seq(travel_restrictions$min[r], travel_restrictions$max[r], by="days"))
    }
    travel_restrict_ <- data.table::rbindlist(travel_restrict_)
}




##'
##' Apply a set of travel restrictions to the travel data, reducing or increasing to a proportion of the average travel.
##'
##' @param travel_data Data.frame. Daily travel data that was previously built. We replace the travelers column in this.
##' @param travel_restrictions_long Data.frame. Daily travel restrictions, including dates, source location, and proportion of cases.
##' 
apply_travel_restrictions <- function(travel_data, travel_restrictions_long){

    travel_data <- full_join(travel_data, travel_restrictions_long, by=c("t","source"="loc")) %>% 
        replace_na(list(p_travel=1)) %>%
        mutate(travelers=travelers*p_travel)
    
    return(travel_data)
}






