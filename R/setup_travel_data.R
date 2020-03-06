
# TRAVEL DATA 

library(tidyverse) 
library(lubridate)



# CDC Data Cleaning -------------------------------------------------------


## Wuhan to US Airports
wuhan_us_travel <- read_csv("data/CDC_travel_data/OAG PAX WUH to US with Point Of Sale Oct 2018-Nov 2019 ID analysis.csv")
wuhan_us_travel <- wuhan_us_travel %>% 
    mutate(t_month = substr(`Time Series`, 5,6), 
           t_year = substr(`Time Series`,1,4)) %>%
    mutate(destination = FinalArrival,
           source = `Dep Airport Code`,
           travelers = Pax) %>% 
    rename(dest_state = `Arr State Name`) %>% 
    dplyr::select(t_month, t_year, source, destination, dest_state, travelers)

# Sum up all iteneraries
wuhan_us_travel <- wuhan_us_travel %>% group_by(t_month, t_year, source, destination, dest_state) %>% summarise(travelers = sum(travelers))
wuhan_us_travel$source <- "Hubei"
write_csv(wuhan_us_travel, "data/wuhan_us_travel_monthly.csv")




# FIX CHINESE NEW YEAR PROBLEM --------------------------------------------

# # Look at the travel ------------------------------------------------------
# wuhan_us_travel_tmp <- wuhan_us_travel %>% mutate(month_yr = paste0(t_month,"-",t_year)) %>%
#     mutate(month_yr = factor(month_yr, levels=paste0(c("10","11","12", "01","02","03","04","05","06","07","08","09","10","11"), "-", c(rep(2018, 3), rep(2019,11))),  ordered = TRUE))
# 
# wuhan_us_travel_tmp <- wuhan_us_travel_tmp %>% mutate(color=ifelse(month_yr=="01-2019" | month_yr=="02-2019", "red", "black"))
# dest_locs <- unique(wuhan_us_travel_tmp$destination)
# 
# 
# ggplot(wuhan_us_travel_tmp %>% filter(destination=="LAX") , aes(month_yr, travelers, color=color, group=1)) + geom_point() + geom_line() +
#     theme(axis.text.x = element_text(angle = 90))
# ggplot(wuhan_us_travel_tmp %>% filter(destination=="SFO") , aes(month_yr, travelers, color=color, group=1)) + geom_point() + geom_line() +
#     theme(axis.text.x = element_text(angle = 90))
# ggplot(wuhan_us_travel_tmp %>% filter(destination=="SAN") , aes(month_yr, travelers, color=color, group=1)) + geom_point() + geom_line() +
#     theme(axis.text.x = element_text(angle = 90))
# 
# 
# wuhan_us_travel_tmp <- wuhan_us_travel_tmp %>% mutate(month = as.integer(month_yr)) %>% arrange(source, destination, month)
# 
# for (d in 1:length(dest_locs)){
#     fit_dat <- wuhan_us_travel_tmp %>% filter(destination==dest_locs[d])
#     fit_dat2 <- fit_dat %>% filter(month!=4 & month!=5)
#     sm <- smooth.spline(fit_dat$month, fit_dat$travelers)
#     predict(sm, x=1:14)$y
#     plot(predict(sm, x=1:14))
#     # difference
#     (wuhan_us_travel_tmp %>% filter(destination=="SFO"))$travelers - predict(sm, x=1:14)$y
# }
# 
# fit_dat <- wuhan_us_travel_tmp %>% filter(destination=="SFO" & month!=4 & month!=5)
# sm <- smooth.spline(fit_dat$month, fit_dat$travelers)
# predict(sm, x=1:14)$y
# plot(predict(sm, x=1:14))
# 
# ggplot(wuhan_us_travel_tmp %>% filter(destination=="SFO") , aes(month_yr, travelers, group=1)) + geom_point() + geom_line() +
#     theme(axis.text.x = element_text(angle = 90)) +
#     geom_point(data=as.data.frame(predict(sm, x=1:14)), aes(x, y), color="red")
# 
# # difference
# (wuhan_us_travel_tmp %>% filter(destination=="SFO"))$travelers - predict(sm, x=1:14)$y
# 




# Distribute travelers over days ------------------------------------------



# Create Daily 

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
make_daily_travel_SLOW <- function(travel_data=wuhan_us_travel, travel_dispersion=10){
    
    travel_data <- travel_data %>% 
        rename(travelers_month = travelers) %>% 
        mutate(days_month = days_in_month(as.integer(t_month)))
    
    data_daily <- list()
    for (m in 1:nrow(travel_data)){
        x <- as.integer(rmultinom(1, travel_data$travelers_month[m], rgamma(travel_data$days_month[m], 1/travel_dispersion)))
        data_daily_ <- data.frame(travel_data[m, ], t_day = 1:travel_data$days_month[m], travelers = x)
        data_daily[[m]] <- data_daily_
    }
    data_daily <- rbindlist(data_daily)
    data_daily <- data_daily %>% mutate(t = as.Date(paste(t_year, t_month, t_day, sep="-")))
    return(data_daily)
}


make_daily_travel <- function(travel_data=wuhan_us_travel, travel_dispersion=10){
    
    travel_data <- travel_data %>% 
        rename(travelers_month = travelers) %>% 
        mutate(days_month = days_in_month(as.integer(t_month)))
    
    rows_ <- nrow(travel_data)
    
    # Sample travlers across full dataset
    x <- as.integer(unlist(lapply(X=1:rows_, FUN=function(x=X) rmultinom(1, travel_data$travelers_month[x], 
                                                                         rgamma(travel_data$days_month[x], shape=1/travel_dispersion)))))
    t_day <- unlist(lapply(X=1:rows_, FUN=function(x=X) 1:travel_data$days_month[x]))
    data_daily <- as.data.frame(lapply(travel_data, rep, travel_data$days_month))
    data_daily <- data.frame(data_daily, t_day=t_day, travelers=x)

    data_daily <- data_daily %>% mutate(t = as.Date(paste(t_year, t_month, t_day, sep="-")))
    return(data_daily)
}







wuhan_us_travel_daily <- make_daily_travel(travel_data=wuhan_us_travel)
    
# Save a sample. This should be generated in each simulation
write_csv(wuhan_us_travel_daily, "data/wuhan_us_travel_daily.csv")



