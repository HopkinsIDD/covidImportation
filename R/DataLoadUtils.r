

##'
##' Pulls the JHUCSSE total case count data up to current date from github.
##' This version checks what is already saved, and downloads those that are not. 
##' Eventually, we would like automate this.
##'
##' @return NA (saves a CSV of the current data to the data directory)
##' 
pull_JHUCSSE_github_data <- function(){
  
  require(tidyverse)
  require(httr)
  require(lubridate)
  
  # First get a list of files so we can get the latest one
  req <- GET("https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1")

  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  data_files <- grep(".csv", grep("csse_covid_19_data/csse_covid_19_daily_reports/", filelist, value=TRUE), value=TRUE)
  dates_ <- gsub("csse_covid_19_data/csse_covid_19_daily_reports/", "", data_files)
  dates_ <- gsub(".csv", "", dates_)
  dates_reformat_ <- as.POSIXct(dates_, format="%m-%d-%Y")
  dates_tocheck_ <- paste(month(dates_reformat_), day(dates_reformat_), year(dates_reformat_), sep="-")
  
  
  # Check which we have already
  #dir.create(file.path("data"), recursive = TRUE, showWarnings = FALSE)
  files_in_dir <- list.files("data", "JHUCSSE Total Cases")
  files_in_dir_dates <- gsub("JHUCSSE Total Cases ", "", files_in_dir)
  files_in_dir_dates <- gsub(".csv", "", files_in_dir_dates)
  tmp <- which.max(mdy(files_in_dir_dates))
  files_in_dir_dates <- files_in_dir_dates[-tmp]
  
  # select list to download
  data_files <- data_files[!(dates_tocheck_ %in% files_in_dir_dates)]
  dates_tocheck_ <- dates_tocheck_[!(dates_tocheck_ %in% files_in_dir_dates)]
  
  for (i in 1:length(data_files)){
    file_name_ <- data_files[i]   # file to pull
    date_ <- dates_tocheck_[i]     # date formatted for saving csv
    
    # Read in the file
    url_ <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",file_name_)
    case_data <- readr::read_csv(url(url_))
    
    # Save it
    readr::write_csv(case_data, file.path("data", paste0("JHUCSSE Total Cases ", date_,".csv")))
  }
}







##'
##' Reads in the JHUCSSE total case count data up
##' until (and including) a given dat.
##'
##' @param last_time the last time to consider data from
##' @param append_wiki sjpi;d we also append data from wikipedia.
##' @param print_file_path whether or not to print the file path
##'
##' @return a data frame with the basic data.
##'
read_JHUCSSE_cases <- function(last_time, append_wiki, print_file_path=FALSE) {

  ## first get a list of all of the files in the directory
  ## starting with "JHUCSSE Total Cases"
  file_list <- list.files("data","JHUCSSE Total Cases",
                          full.names = TRUE)

  file_list <- rev(file_list)
  
  ##Now combine them into one data frame
  rc <- NULL

  for (file in file_list) {
    if(print_file_path) print(file)
    tmp <- read_csv(file)%>%
      rename(Province_State=`Province/State`)%>%
      rename(Update = `Last Update`) %>%
      mutate(Update=lubridate::parse_date_time(Update, 
          c("%m/%d/%Y %I:%M %p", "%m/%d/%Y %H:%M", "%m/%d/%y %I:%M %p","%m/%d/%y %H:%M", "%Y-%m-%d %H:%M:%S")))
    
    if("Country"%in%colnames(tmp)) {
      tmp <- rename(tmp, Country_Region=Country)
    } else {
      tmp <- rename(tmp, Country_Region=`Country/Region`)
    }
    
    if ("Demised"%in% colnames(tmp)) {
      tmp <- rename(tmp, Deaths=Demised)
    }

    rc <-bind_rows(rc,tmp)
  }
  
  rc <- rc %>% as.data.frame() %>% mutate(Update = lubridate::ymd_hms(Update)) %>% 
    filter(Update <= last_time) 
  
  ##Now drop any after the date given
  rc <- rc %>%
    mutate(Country_Region=replace(Country_Region, Country_Region=="China", "Mainland China")) %>%
    mutate(Country_Region=replace(Country_Region, Province_State=="Macau", "Macau")) %>%
    mutate(Country_Region=replace(Country_Region, Province_State=="Hong Kong", "Hong Kong")) %>%
    mutate(Country_Region=replace(Country_Region, Province_State=="Taiwan", "Taiwan")) %>% 
    mutate(Province_State=ifelse(is.na(Province_State),Country_Region, Province_State))

  if (append_wiki) {
    wiki <- read_csv("data/WikipediaWuhanPre1-20-2020.csv",
                     col_types=cols(Update = col_datetime("%m/%d/%Y")))
    rc <- bind_rows(rc,wiki)
  }

  return(rc)
}


