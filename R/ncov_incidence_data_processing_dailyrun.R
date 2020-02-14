##' Import, Merge, and Format incidence data
##' - Data are used in plotting curves and predicting importations



# SETUP -------------------------------------------------------------------

if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('lubridate')) install.packages('lubridate'); library(lubridate)
if(!require('gsheet')) install.packages('gsheet'); library(gsheet)

# For scheduling to run daily
# install.packages('data.table')
# install.packages('knitr')
# install.packages('miniUI')
# install.packages('shiny')
# install.packages("taskscheduleR", repos = "http://www.datatailor.be/rcube", type = "source")
 
ncov_timeseries_url <- "https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/htmlview?usp=sharing&sle=true"

todays_date <- Sys.Date()

save_dir <- "C:/Users/shaun/OneDrive - Johns Hopkins University/Work/Projects/nCoV/nCoV_Importation"


# LAUREN'S DATA -----------------------------------------------------------

# Read and use data from Lauren Gardners website
conf_cases <- gsheet::gsheet2tbl(ncov_timeseries_url)
case_loc_data <- conf_cases[,1:5]
conf_cases <- conf_cases %>% gather(key="t", value="cum_cases",-`Province/State`,-`Country/Region`,-Lat,-Long, -`First confirmed date in country (Est.)`)
conf_cases <- conf_cases %>% mutate(cum_cases = ifelse(is.na(cum_cases), 0, cum_cases))

# Sum days with multiple
conf_cases <- conf_cases %>% mutate(date = as.Date(lubridate::mdy_hm(t))) %>% 
    rename(prov_state=`Province/State`, country=`Country/Region`) %>%
    arrange(country, prov_state, date) %>%
    group_by(prov_state, country, date) %>%
    summarize(cum_cases = max(cum_cases))
conf_cases_first <- conf_cases %>% group_by(prov_state, country) %>% 
    summarize(date = min(date)-1) %>% mutate(cum_cases = 0)
conf_cases <- conf_cases %>% bind_rows(conf_cases_first) %>% arrange(prov_state, country, date)

conf_cases <- conf_cases %>% mutate(cases_incid=0) %>% group_by(prov_state, country) %>% 
    mutate(cases_incid = c(0,diff(cum_cases)))
conf_cases <- conf_cases %>% mutate(epiweek = lubridate::epiweek(date)) 
conf_cases <- conf_cases %>% mutate(China_source=(grepl("china", tolower(country)) | grepl("hong kong", tolower(country)) | 
                                                      grepl("macau", tolower(country))) | grepl("taiwan", tolower(country)))
#conf_cases <- conf_cases %>% complete(date, epiweek, prov_state, country)

#dir.create("data/dailycounts", recursive = TRUE)
write_csv(conf_cases, file.path(save_dir,"data","dailycounts",paste0("china_incid_data_report_",todays_date,".csv")))




# REMOVE EVERYTHING FROM THE CURRENT WORKSPACE
#rm(list = ls()); gc()
