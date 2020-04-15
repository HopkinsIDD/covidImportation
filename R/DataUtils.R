##'
##' Pull JHU CSSE GitHub data
##'
##' Pulls the JHUCSSE total case count data up to current date from GitHub.
##' This version checks what is already saved, and downloads those that are not.
##' Eventually, we would like automate this.
##'
##' @param case_data_dir directory where daily reported case data files are stored by the function.
##' @param repull_all whether to repull all data to make sure it's up to date and catch error fixes
##' @return NA (saves a CSV of the current data to the data directory)
##'
##' @import httr dplyr
##' @importFrom lubridate mdy month day year
##' @importFrom readr read_csv write_csv
##'
##' @export
##'
pull_JHUCSSE_github_data <- function(case_data_dir = "data/case_data", repull_all=FALSE){

    # Create directory to hold all the data
    dir.create(case_data_dir, showWarnings = FALSE, recursive = FALSE)
    print(paste0("Pulled JHUCSSE data files are saved in ", case_data_dir, "."))

    # First get a list of files so we can get the latest one
    req <- httr::GET("https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1")

    httr::stop_for_status(req)
    filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
    data_files <- grep(".csv", grep("csse_covid_19_data/csse_covid_19_daily_reports/", filelist, value=TRUE), value=TRUE)
    dates_ <- gsub("csse_covid_19_data/csse_covid_19_daily_reports/", "", data_files)
    dates_ <- gsub(".csv", "", dates_)
    dates_reformat_ <- as.POSIXct(dates_, format="%m-%d-%Y")
    dates_tocheck_ <- paste(lubridate::month(dates_reformat_),
                            lubridate::day(dates_reformat_),
                            lubridate::year(dates_reformat_), sep="-")

    # Check which we have already
    #dir.create(file.path("data"), recursive = TRUE, showWarnings = FALSE)
    files_in_dir <- list.files(case_data_dir, "JHUCSSE Total Cases")
    files_in_dir_dates <- gsub("JHUCSSE Total Cases ", "", files_in_dir)
    files_in_dir_dates <- gsub(".csv", "", files_in_dir_dates)
    tmp <- which.max(lubridate::mdy(files_in_dir_dates))
    files_in_dir_dates <- files_in_dir_dates[-tmp]

    # select list to download
    if (!repull_all){
        data_files <- data_files[!(dates_tocheck_ %in% files_in_dir_dates)]
    }
    dates_tocheck_ <- dates_tocheck_[!(dates_tocheck_ %in% files_in_dir_dates)]

    for (i in seq_len(length(data_files))){
        file_name_ <- data_files[i]   # file to pull
        date_ <- dates_tocheck_[i]     # date formatted for saving csv

        # Read in the file
        url_ <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",file_name_)
        case_data <- readr::read_csv(url(url_))

        # Save it
        readr::write_csv(case_data, file.path(case_data_dir, paste0("JHUCSSE Total Cases ", date_,".csv")))
    }
}




##'
##' Reads in the JHUCSSE total case count data up
##' until (and including) a given dat.
##'
##' @param last_date Date, the last time to consider data from
##' @param append_wiki logical, should we also append data from wikipedia
##' @param case_data_dir directory where daily reported case data files are stored by the function.
##' @param print_file_path logical whether or not to print the file path
##'
##' @return a data frame with the basic data.
##'
##' @import dplyr 
##' @importFrom lubridate mdy parse_date_time ymd_hms
##' @importFrom readr read_csv write_csv
##'
##' @export
##'
read_JHUCSSE_cases <- function(last_date=Sys.Date(),
                               append_wiki=TRUE,
                               case_data_dir = "data/case_data",
                               print_file_path=FALSE) {

    ## first get a list of all of the files in the directory
    ## starting with "JHUCSSE Total Cases"
    file_list <- list.files(case_data_dir,"JHUCSSE Total Cases",
                            full.names = TRUE)

    file_list <- rev(file_list)

    ##Now combine them into one data frame
    rc <- list()

    for (f in seq_along(file_list)) {
        if(print_file_path) print(file_list[f])
        tmp <- readr::read_csv(file_list[f])

        # Fix the different file column names
        colnames_ <- colnames(tmp)
        colnames_[grepl("Province", colnames_)] <- "Province_State"
        colnames_[grepl("Country", colnames_)] <- "Country_Region"
        colnames_[grepl("Demised", colnames_)] <- "Deaths"
        colnames_[grepl("Update", colnames_)] <- "Update"
        colnames_[grepl("Lat", colnames_)] <- "Latitude"
        colnames_[grepl("Long", colnames_)] <- "Longitude"

        colnames(tmp) <- colnames_

        tmp <- tmp %>% dplyr::mutate(Update=lubridate::parse_date_time(Update,
                                 c("%m/%d/%Y %I:%M %p", "%m/%d/%Y %H:%M", "%m/%d/%y %I:%M %p","%m/%d/%y %H:%M", "%Y-%m-%d %H:%M:%S")))
        rc[[f]] <- tmp
    }
    rc <- data.table::rbindlist(rc, fill = TRUE)

    ##Now drop any after the date given
    rc <- rc %>% as.data.frame() %>% dplyr::mutate(Update = lubridate::ymd_hms(Update)) %>%
        dplyr::filter(as.Date(Update) <= as.Date(last_date))

    # Fix Chinese provinces and
    rc <- rc %>%
        dplyr::mutate(Country_Region=replace(Country_Region, Country_Region=="China", "Mainland China")) %>%
        dplyr::mutate(Country_Region=replace(Country_Region, Province_State=="Macau", "Macau")) %>%
        dplyr::mutate(Country_Region=replace(Country_Region, Province_State=="Hong Kong", "Hong Kong")) %>%
        dplyr::mutate(Country_Region=replace(Country_Region, Province_State=="Taiwan", "Taiwan")) %>%
        dplyr::mutate(Province_State=ifelse(is.na(Province_State),Country_Region, Province_State))

    # Fix bad locations
    rc <- rc %>% dplyr::filter(!(Province_State %in% c("US"))) %>%
        dplyr::mutate(Province_State = ifelse(grepl("Chicago", Province_State), "Chicago, IL", Province_State)) %>%
        dplyr::mutate(Province_State = ifelse(grepl("Ningxia", Province_State), "Ningxia", Province_State)) %>%
        dplyr::mutate(Province_State = ifelse(Province_State=="Inner Mongolia", "Nei Mongol", Province_State)) %>%
        dplyr::mutate(Province_State = ifelse(Province_State=="Hong Kong", "HKG", Province_State))


    if (append_wiki) {
        data("wikipedia_cases", package="covidImportation")
        rc <- dplyr::bind_rows(rc,wikipedia_cases)
    }
    # Remove any duplicate rows
    rc <- rc %>% dplyr::distinct()

    return(rc)
}


##'
##' Pull JHU CSSE GitHub data
##'
##' Pulls the JHUCSSE total case count data up to current date from GitHub.
##' This version checks what is already saved, and downloads those that are not.
##'
##' @param case_data_dir directory where daily reported case data files are stored by the function.
##' @param last_date last date for which to include case data
##' @param check_saved_data whether to check for existing saved case data
##' @param save_data whether to save the cleaned and combined data
##'
##' @import dplyr httr 
##' @importFrom lubridate mdy month day year parse_date_time ymd_hms
##' @importFrom readr read_csv write_csv
##' @importFrom data.table rbindlist
##'
##' @return NA (saves a CSV of the current data to the data directory)
##'
##' @export
##' 
update_JHUCSSE_github_data <- function(case_data_dir = "data/case_data",
                                       last_date=Sys.time(),
                                       check_saved_data=FALSE,
                                       save_data=FALSE){

    # Create directory to hold all the data
    if (check_saved_data | save_data){
        dir.create(case_data_dir, showWarnings = FALSE, recursive = FALSE)
        print(paste0("Combined data is saved in ", case_data_dir, "."))
    }

    # First get a list of files so we can get the latest one
    req <- httr::GET("https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1")
    httr::stop_for_status(req)

    filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
    data_files <- grep(".csv", grep("csse_covid_19_data/csse_covid_19_daily_reports/", filelist, value=TRUE), value=TRUE)
    dates_ <- gsub("csse_covid_19_data/csse_covid_19_daily_reports/", "", data_files)
    dates_ <- gsub(".csv", "", dates_)
    dates_reformat_ <- as.POSIXct(dates_, format="%m-%d-%Y")
    dates_tocheck_ <- paste(lubridate::month(dates_reformat_),
                            lubridate::day(dates_reformat_),
                            lubridate::year(dates_reformat_), sep="-")


    if (check_saved_data){
        
        # First check the data that comes with the package
        data('jhucsse_case_data', package = 'covidImportation')
        update_dates <- sort(unique(as.Date(jhucsse_case_data$Update)))
        tmp <- which.max(update_dates)
        update_dates <- update_dates[-tmp]
        update_dates <- paste(lubridate::month(update_dates),
                              lubridate::day(update_dates),
                              lubridate::year(update_dates), sep="-")
        
        # Check which we have already
        #dir.create(file.path("data"), recursive = TRUE, showWarnings = FALSE)
        files_in_dir <- list.files(case_data_dir, "JHUCSSE Total Cases")
        files_in_dir_dates <- gsub("JHUCSSE Total Cases ", "", files_in_dir)
        files_in_dir_dates <- gsub(".csv", "", files_in_dir_dates)
        tmp <- which.max(lubridate::mdy(files_in_dir_dates))
        files_in_dir_dates <- files_in_dir_dates[-tmp]

        # check for previously combined data
        comb_file_in_dir <- file.exists(file.path(case_data_dir, "jhucsse_case_data.csv"))
        if (comb_file_in_dir){
            comb_data <- readr::read_csv(file.path(case_data_dir,"jhucsse_case_data.csv"))
            comb_data_in_dir_dates <- sort(unique(as.Date(comb_data$Update)))
            tmp <- which.max(comb_data_in_dir_dates)
            comb_data_in_dir_dates <- comb_data_in_dir_dates[-tmp]
            comb_data_in_dir_dates <- paste(lubridate::month(comb_data_in_dir_dates),
                                  lubridate::day(comb_data_in_dir_dates),
                                  lubridate::year(comb_data_in_dir_dates), sep="-")
        } else {
            comb_data_in_dir_dates <- NULL
        }

    } else {
        files_in_dir_dates <- comb_data_in_dir_dates <- update_dates <- NULL
    }

    # select list to download (minus the latest one as multiple updates to the data are made daily)
    dates_have_ <- unique(c(update_dates, files_in_dir_dates, comb_data_in_dir_dates))
    data_files <- data_files[!(dates_tocheck_ %in% dates_have_)]
    dates_tocheck_ <- dates_tocheck_[!(dates_tocheck_ %in% files_in_dir_dates)]


    # pull and combine data from github
    rc <- list()

    for (i in seq_len(length(data_files))){
        file_name_ <- data_files[i]   # file to pull
        date_ <- dates_tocheck_[i]     # date formatted for saving csv

        # Read in the file
        url_ <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",file_name_)
        case_data <- readr::read_csv(url(url_))

        # Fix the different file column names
        colnames_ <- colnames(case_data)
        colnames_[grepl("Province", colnames_)] <- "Province_State"
        colnames_[grepl("Country", colnames_)] <- "Country_Region"
        colnames_[grepl("Demised", colnames_)] <- "Deaths"
        colnames_[grepl("Update", colnames_)] <- "Update"
        colnames_[grepl("Lat", colnames_)] <- "Latitude"
        colnames_[grepl("Long", colnames_)] <- "Longitude"
        colnames(case_data) <- colnames_

        case_data <- case_data %>% dplyr::mutate(Update=lubridate::parse_date_time(Update,
                                                                c("%m/%d/%Y %I:%M %p", "%m/%d/%Y %H:%M", "%m/%d/%y %I:%M %p","%m/%d/%y %H:%M", "%Y-%m-%d %H:%M:%S")))
        rc[[i]] <- case_data
    }
    rc <- data.table::rbindlist(rc, fill = TRUE)

    ##Now drop any after the date given
    rc <- rc %>% as.data.frame() %>% mutate(Update = lubridate::ymd_hms(Update)) %>%
        dplyr::filter(as.Date(Update) <= as.Date(last_date))

    # Fix Chinese provinces and autonomous regions
    rc <- rc %>%
        dplyr::mutate(Country_Region=replace(Country_Region, Country_Region=="China", "Mainland China")) %>%
        dplyr::mutate(Country_Region=replace(Country_Region, Province_State=="Macau", "Macau")) %>%
        dplyr::mutate(Country_Region=replace(Country_Region, Province_State=="Hong Kong", "Hong Kong")) %>%
        dplyr::mutate(Country_Region=replace(Country_Region, Province_State=="Taiwan", "Taiwan")) %>%
        dplyr::mutate(Province_State=ifelse(is.na(Province_State),Country_Region, Province_State))

    # Fix bad locations
    rc <- rc %>% dplyr::filter(!(Province_State %in% c("US"))) %>%
        dplyr::mutate(Province_State = ifelse(grepl("Chicago", Province_State), "Chicago, IL", Province_State)) %>%
        dplyr::mutate(Province_State = ifelse(grepl("Ningxia", Province_State), "Ningxia", Province_State)) %>%
        dplyr::mutate(Province_State = ifelse(Province_State=="Inner Mongolia", "Nei Mongol", Province_State)) %>%
        dplyr::mutate(Province_State = ifelse(Province_State=="Hong Kong", "HKG", Province_State))


    # merge with data from the package
    rc <- dplyr::bind_rows(jhucsse_case_data, rc) %>% dplyr::distinct()


    # Save if desired
    if (save_data){
        readr::write_csv(rc, file.path(case_data_dir,"jhucsse_case_data.csv"))
    }

    return(rc)
}




##' Get airport city
##'
##' These functions are all vectorized
##'
##' @param airport_code character, airport code
##'
##' @import dplyr
##'
##' @return City of the aiport
##'
get_airport_city <- function(airport_code = "ORD"){
    data(airport_data)
    return((airport_data %>%
                dplyr::filter(iata_code %in% airport_code))$municipality)
}


#' Get airport state
#'
#' @param airport_code character, airport code
#'
#' @import dplyr
#'
#' @return State/province of the airport
#'
get_airport_state <- function(airport_code = "ORD"){
    data(airport_data)
    return(substr((airport_data %>%
                       dplyr::filter(iata_code %in% airport_code))$municipality, 4,5))
}


#' Get airport country
#'
#' @param airport_code character, airport code
#'
#' @return ISO3 code for the country where the airport is
#'
#' @import dplyr
#'
#' @examples get_airport_country()
#'
get_airport_country <- function(airport_code = "ORD"){
    data(airport_data)
    return((airport_data %>% dplyr::filter(iata_code %in% airport_code))$iso_country)
}



#' Get incidence data from JHUCSSE
#'
#' @param first_date
#' @param last_date
#' @param update_case_data whether to update the case data from the JHUCSSE github or just use data that are part of this package
#' @param case_data_dir directory where case data is being saved
#' @param check_saved_data whether to check locally saved github data
#' @param save_data whether to save data locally
#'
#' @return
#'
#' @examples
#'
#' @import globaltoolboxlite dplyr tidyr
#' @importFrom globaltoolboxlite get_country_name_ISO3 get_iso
#'
#' @export
#'
get_incidence_data <- function(first_date = ISOdate(2019,12,1),
                               last_date = Sys.time(),
                               update_case_data=TRUE,
                               case_data_dir = "data/case_data",
                               check_saved_data=TRUE,
                               save_data=TRUE){

    ## Get case count data (from JHU CSSE's github)
    ## Update the data provided in the package if desired
    if (update_case_data){
        jhucsse_case_data <- update_JHUCSSE_github_data(case_data_dir = case_data_dir,
                                              last_date=last_date,
                                              check_saved_data=check_saved_data,
                                              save_data=save_data)
    } else {
        data('jhucsse_case_data', package = 'covidImportation')
    }

    # Make all Diamond Princess Cases same source
    jhucsse_case_data <- jhucsse_case_data %>%
       dplyr::mutate(Province_State = ifelse(grepl("diamond princess", Province_State, ignore.case = TRUE), "Diamond Princess", Province_State))

    # Fix US locations
    jhucsse_case_data <- jhucsse_case_data %>%
       dplyr::mutate(Province_State = ifelse(grepl("Seattle", Province_State, ignore.case = TRUE), "King County, WA", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("Chicago", Province_State, ignore.case = TRUE), "Cook County, IL", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("New York City", Province_State, ignore.case = TRUE), "New York County, NY", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("Washington, D.C.", Province_State, ignore.case = TRUE), "District of Columbia", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("Washington, DC", Province_State, ignore.case = TRUE), "District of Columbia", Province_State)) %>%
       dplyr::mutate(FIPS = ifelse(grepl("District of Columbia", Province_State, ignore.case = TRUE), "11001", FIPS)) %>%
       dplyr::mutate(Admin2 = ifelse(grepl("District of Columbia", Province_State, ignore.case = TRUE), "District of Columbia", Admin2)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("Santa Clara", Province_State, ignore.case = TRUE), "Santa Clara County, CA", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("San Mateo", Province_State, ignore.case = TRUE), "San Mateo County, CA", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("San Benito", Province_State, ignore.case = TRUE), "San Benito County, CA", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("Portland", Province_State, ignore.case = TRUE), "Multnomah, OR", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("Los Angeles", Province_State, ignore.case = TRUE), "Los Angeles County, CA", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("Boston", Province_State, ignore.case = TRUE), "Suffolk County, MA", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("San Antonio", Province_State, ignore.case = TRUE), "Bexar County, TX", Province_State)) %>%
       dplyr::mutate(Province_State = ifelse(grepl("Umatilla", Province_State, ignore.case = TRUE), "Umatilla County, CA", Province_State))
        #mutate(US_county = ifelse(grepl("San Marino", Province_State, ignore.case = TRUE), "Los Angeles County, CA", Province_State)) %>%
        #mutate(US_county = ifelse(grepl("Lackland", Province_State, ignore.case = TRUE), "Bexar County, CA", Province_State))



    # Fix counties
    us_co_inds <- which(grepl("County", jhucsse_case_data$Province_State) & is.na(jhucsse_case_data$FIPS))
    dat_ <- jhucsse_case_data[us_co_inds,] %>% tidyr::separate(Province_State, c("county", "state"), sep=", ") %>%
               dplyr::mutate(county = gsub("\\.","",county))

    countystate_ <- paste0(dat_$county, ", ", dat_$state)
    data("us_counties", package = "covidImportation") # load county info
    us_counties <- us_counties %>% dplyr::mutate(countystate = paste0(Name, " County, ", State))
    FIPS_ <- us_counties$FIPS[match(countystate_, us_counties$countystate)]

    jhucsse_case_data$FIPS[us_co_inds] <- FIPS_



    # Get US States ................
    # tidyr::separate out states
    jhucsse_case_data <- suppressWarnings(
        jhucsse_case_data %>%
           tidyr::separate(Province_State, sep = ', ', c('city', 'state'), convert = TRUE, remove=FALSE) %>%
           dplyr::mutate(city = ifelse(is.na(state), NA, city))
        )

    # Get states where not already there
    jhucsse_case_data <- jhucsse_case_data %>% 
       dplyr::mutate(state_tmp = state.abb[match(Province_State, state.name)]) %>%
       dplyr::mutate(state = ifelse(!is.na(state_tmp) & is.na(state), state_tmp, state)) %>%
       dplyr::select(-state_tmp) %>%
       dplyr::mutate(state = ifelse(state=="D.C.", "DC", state))

    # # Australian states .............
    #  --(for now we will just use the country of Australia. need to change to state eventually)
    # aus_states <- c("New South Wales", "Victoria", "Queensland", "Western Australia", "South Australia", "Tasmania")

    # Fix China ..............
    #unique(grep("China", incid_data$Country_Region, value = TRUE, ignore.case = TRUE))
    jhucsse_case_data <- jhucsse_case_data %>%
       dplyr::mutate(Country_Region = ifelse(Province_State %in% c("Inner Mongolia"), "China", Country_Region)) %>%
       dplyr::mutate(Country_Region = ifelse(Country_Region %in% c("Mainland China", "Hong Kong", "Macau", "Taiwan", "Nei Mongol"),
                                       "China", Country_Region))


    # Get ISO Code .....................
    jhucsse_case_data <- jhucsse_case_data %>%
       dplyr::mutate(country = globaltoolboxlite::get_iso(Country_Region)) %>%
       dplyr::mutate(country_name = globaltoolboxlite::get_country_name_ISO3(country))
    jhucsse_case_data <- jhucsse_case_data %>% dplyr::mutate(country_name = ifelse(country=="KOS", "Kosovo", country_name))
    #unique((incid_data %>% dplyr::filter(is.na(country)))$source)
    # country_ <- countrycode::countrycode(unique(jhucsse_case_data$Country_Region),
    #                                      origin="country.name", destination = "iso3c",
    #                                      origin_regex = TRUE, nomatch = NULL)

    # Define a single source location variable
    # - USA: States used for source
    # - China: Provinces used for source
    # - Others: Country used for source
    jhucsse_case_data <- jhucsse_case_data %>% dplyr::mutate(source = ifelse(country=="USA" & !is.na(state), state,
                                                  ifelse(country=="CHN" & !is.na(Province_State), Province_State, country)))

    # Manually get rid of bad data
    jhucsse_case_data <- jhucsse_case_data %>% dplyr::filter(!(Province_State %in% c("The Bahamas", "Republic of the Congo"))) %>%
        dplyr::filter(!(Province_State=="UK" & Update=="2020-03-11 21:33:03")) %>%
       dplyr::mutate(Province_State = ifelse(Province_State=="United Kingdom", "UK", Province_State))

    # Get rid of duplicate rows
    jhucsse_case_data <- jhucsse_case_data %>% distinct()

    # Get new base location on which to run splines
    jhucsse_case_data <- jhucsse_case_data %>% dplyr::mutate(source_loc = ifelse(country =="USA" & !is.na(FIPS), FIPS, Province_State))

    # Get incident cases by source_loc (US counties, Chinese provinces, Countries otherwise)
    jhucsse_case_data <- jhucsse_case_data %>% dplyr::arrange(country, source_loc, Update) %>%
         dplyr::group_by(source_loc, country) %>%
       dplyr::mutate(incid_conf = diff(c(0,Confirmed))) %>% dplyr::ungroup()


    # Fix counts that go negative
    negs_ind <- which(jhucsse_case_data$incid_conf < 0)
    jhucsse_case_data$Confirmed[negs_ind - 1] <- jhucsse_case_data$Confirmed[negs_ind - 1] + jhucsse_case_data$incid_conf[negs_ind]
    jhucsse_case_data <- jhucsse_case_data %>% dplyr::arrange(country, source_loc, Update) %>%
         dplyr::group_by(source_loc, country) %>%
       dplyr::mutate(incid_conf = diff(c(0,Confirmed))) %>% dplyr::ungroup()
    jhucsse_case_data <- jhucsse_case_data %>% dplyr::filter(incid_conf>=0)


    # Get cum incidence for states/provinces, countries
    jhucsse_case_data_state <- jhucsse_case_data %>%
        dplyr::arrange(country, source, Update) %>%
        dplyr::group_by(source, country, Update) %>%
        dplyr::summarise(incid_conf = sum(incid_conf)) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(source, country) %>%
        dplyr::mutate(cum_incid = cumsum(incid_conf)) %>%
        dplyr::ungroup()


    ## GET INCIDENCE FITS ..........................
    ## Estimate incidence using spline fits.
    incid_data <- est_daily_incidence_corrected(jhucsse_case_data_state %>% dplyr::mutate(Province_State=source, Confirmed=cum_incid),
                                                first_date, last_date, tol=100, na_to_zeros=FALSE) %>%
       dplyr::mutate(Incidence=round(Incidence, 2))

    ## Incidence Data
    incid_data <- incid_data %>% dplyr::rename(source=Province_State, cases_incid=Incidence) %>%
       dplyr::mutate(source = as.character(source),
               t = as.Date(Date)) %>%
        as.data.frame()

    # Add country_name back in
    incid_data <- left_join(incid_data,
                            jhucsse_case_data %>% dplyr::select(source, country_name, country) %>%
                               dplyr::mutate(prov_country = paste0(source,"-", country_name)) %>%
                                dplyr::filter(!duplicated(prov_country)) %>% dplyr::select(-prov_country),
                            by=c("source"="source"))
    # Add confirmed cases back in
    incid_data <- left_join(incid_data,
                            jhucsse_case_data_state %>% dplyr::mutate(t = as.Date(Update)) %>%
                                 dplyr::group_by(t, source, country) %>%
                                 dplyr::summarise(incid_conf = sum(incid_conf, na.rm=TRUE)) %>%
                                 dplyr::arrange(country, source, t) %>%
                                 dplyr::group_by(source, country) %>%
                                 dplyr::mutate(cum_incid = cumsum(incid_conf)) %>% dplyr::ungroup(),
                            by=c("source"="source", "t"="t", "country"))
    incid_data <- incid_data %>% dplyr::mutate(incid_conf=ifelse(is.na(incid_conf), 0, incid_conf))


    # Drop NA source
    #View(incid_data %>% dplyr::filter(is.na(source)))
    incid_data <- incid_data %>% dplyr::filter(!is.na(source))


    # Get cumulative estimated incidence
    incid_data <- incid_data %>%
        dplyr::arrange(country, source, t) %>%
        dplyr::group_by(source, country) %>%
        dplyr::mutate(cum_est_incid = cumsum(cases_incid)) %>% dplyr::ungroup()


    return(list(incid_data=incid_data, jhucsse_case_data=jhucsse_case_data, jhucsse_case_data_state=jhucsse_case_data_state))
}



##' Get OAG travel data
##'
##' Get subsetted and cleaned OAG data for a specific destination, using the full OAG data set
##'
##' @param destination destination of interest; can be a vector.
##' @param destination_type options: "airport", "city", "state", "country"
##' @param dest_0 default=NULL; change to specify higher level destination (i.e. dest_0="USA")
##' @param dest_0_type default=NULL; must specify if specifying a `dest_0` option.
##' @param dest_aggr_level level to which travel will be aggregated for destination. Includes "airport", "city", "state", "country", "metro" (only available for CA currently)
##'
##' @import dplyr
##' @importFrom readr read_csv write_csv
##' @importFrom countrycode countrycode
##'
##' @export
##'
get_oag_travel_fulldata <- function(destination=c("CA"),
                           destination_type="state",
                           dest_0=NULL,
                           dest_0_type=NULL,
                           dest_aggr_level="city",
                           oag_file="data/complete_OAG_data.csv"){

    if (!file.exists(oag_file)){
        print(paste0("Error: ", oag_file, " does not exist."))
        return(NA)
    }

    # Read full data
    # these data are clean in  `oag_data_cleaning.R`
    data_travel_all <- readr::read_csv(oag_file, na=c(""," ", "NA"),
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
        dest_data <- data_travel_all %>%
            dplyr::filter(`Arr City Name` %in% destination)
    } else if (destination_type=="airport"){
        dest_data <- data_travel_all %>%
            dplyr::filter(`Arr Airport Code` %in% destination)
    } else if (destination_type=="state"){
        dest_data <- data_travel_all %>%
            dplyr::filter(`Arr State Code` %in% destination)
    } else if (destination_type=="country"){
        dest_data <- data_travel_all %>%
            dplyr::filter(`Arr Country Code` %in% destination)
    }

    if (!is.null(dest_0)){
        if (dest_0_type=="city"){
            dest_data <- dest_data %>%
                dplyr::filter(`Arr City Name` %in% dest_0)
        } else if (dest_0_type=="airport"){
            dest_data <- dest_data %>%
                dplyr::filter(`Arr Airport Code` %in% dest_0)
        } else if (dest_0_type=="state"){
            dest_data <- dest_data %>%
                dplyr::filter(`Arr State Code` %in% dest_0)
        } else if (dest_0_type=="country"){
            dest_data <- dest_data %>%
                dplyr::filter(`Arr Country Code` %in% dest_0)
        }
    }

    data('pop_data', package = 'covidImportation')

    # Give Chinese airports the provinces
    data(airport_attribution)

    # merge with travel data
    dest_data <- dplyr::left_join(dest_data,
                           airport_attribution,
                           by=c("Dep Airport Code"="airport_iata"))
    # Adjust travel volume based on attribution
    dest_data <- dest_data %>%
        tidyr::replace_na(list(attribution=1)) %>%
        dplyr::mutate(`Total Est. Pax` = `Total Est. Pax` * attribution) %>%
        dplyr::select(-attribution, pop)


    # Get us State codes for departures
    data(airport_data)
    airport_data <- airport_data %>%
       dplyr::mutate(iso_country = ifelse(iso_country=="XK", "KOS",
                                    countrycode::countrycode(iso_country,
                                                             origin = "iso2c",
                                                             destination = "iso3c")))
    airport_data_us <- airport_data %>%
        dplyr::filter(iso_country=="USA")

    dest_data <- dest_data %>%
        dplyr::left_join(airport_data_us %>%
                     dplyr::mutate(state = substr(iso_region, 4,5)) %>%
                      dplyr::select(state, iata_code),
                  by=c("Dep Airport Code"="iata_code")) %>%
       dplyr::mutate(`Dep State Code`=ifelse(is.na(`Dep State Code`) & !is.na(state),
                                       state, `Dep State Code`)) %>%
        # Aggregate SOURCE LOCATION to province (China) or state (US) or country (all others) for source
       dplyr::rename(dep_airport = `Dep Airport Code`,
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
               dep_province = Province) %>%
        # Fix US cities with "(US) [STATE]" in name
       dplyr::mutate(arr_city = gsub(" \\(US\\).*", "", arr_city)) %>%
        # Aggregate to dest_aggr_level, then get mean across 3 years ..............

    dest_data_aggr <- dest_data %>%
       dplyr::mutate(dep_loc_aggr = ifelse(dep_country=="CHN", dep_province, ifelse(dep_country=="USA", dep_state, dep_country)),
               t_year = substr(yr_month, 1,4),
               t_month = as.character(substr(yr_month, 5,6)))    # Get year and month variables


    # Get Metro areas (only available for CA currently)
    if (length(destination)==1 && destination=="CA"){

        ca_airport_attibution <- readr::read_csv("data/ca/airport_attribution.csv")
        ca_airport_attibution <- ca_airport_attibution %>% dplyr::mutate(metrop_labels=ifelse(is.na(metrop_labels), "Other", metrop_labels))

        # choose 1 place to give attribution
        ca_airport_attibution <- ca_airport_attibution %>% dplyr::group_by(metrop_labels, airport_iata) %>%
            dplyr::summarise(attribution = sum(attribution, na.rm = TRUE))
        ca_airport_attibution <- ca_airport_attibution %>% dplyr::group_by(airport_iata) %>% dplyr::filter(attribution==max(attribution)) %>% dplyr::ungroup()

        dest_data_aggr <- dplyr::left_join(dest_data_aggr,
                                    ca_airport_attibution %>% dplyr::select(airport_iata, metrop_labels),
                                    by=c("arr_airport"="airport_iata")) %>%
           dplyr::rename(arr_metro = metrop_labels)
        dest_data_aggr <- dest_data_aggr %>% dplyr::mutate(arr_metro = ifelse(is.na(arr_metro), "Other", arr_metro))
    }

    # aggregation levels for destination
    aggr_levels <- factor(c("airport", "city", "metro", "state", "country"), levels=c("airport", "city", "metro", "state", "country"), ordered = TRUE)
    loc_vars_aggr <- c("arr_airport", "arr_city","arr_metro", "arr_state", "arr_country")[aggr_levels>=dest_aggr_level]
    loc_vars_aggr <- loc_vars_aggr[loc_vars_aggr %in% colnames(dest_data_aggr)]
    other_vars_aggr <- c("yr_month", "t_year", "t_month", "dep_loc_aggr", "dep_country")

    dest_data_aggr <- dest_data_aggr %>% dplyr::group_by(.dots = c(other_vars_aggr, loc_vars_aggr)) %>%
        dplyr::summarise(travelers = sum(travelers, na.rm = TRUE))

    # Get Monthly means across the 3 year (using geometric means)
    other_vars_aggr <- c("t_month", "dep_loc_aggr", "dep_country")
    dest_data_aggr <- dest_data_aggr %>%
        dplyr::group_by(.dots = c(other_vars_aggr, loc_vars_aggr)) %>%
        dplyr::summarise(travelers_sd = sd(travelers),
                  travelers_mean = exp(mean(log(travelers+1)))-1)

    dest_data_aggr <- dest_data_aggr %>% dplyr::mutate(travelers_sd = ifelse(is.nan(travelers_sd), travelers_mean/1.96, travelers_sd)) # for those with only 1 value for travel, just use that /2 for the SD

    # Save it
    readr::write_csv(dest_data_aggr, paste0("data/", paste(destination, collapse = "+"), "-", dest_aggr_level, "_oag_20172019.csv"))

}


# # Save CA cities
# dest_data_aggr <- get_oag_travel(destination="CA", destination_type="state", dest_aggr_level="airport")
# ca_airports <- dest_data_aggr %>%  dplyr::group_by(arr_airport, arr_city, arr_state, arr_country) %>%  dplyr::summarise(n_occur = n())
# write_csv(ca_airports, "data/ca_airports.csv")







##' Get OAG travel data
##'
##' Get subsetted and cleaned OAG data for a specific destination, using the full OAG data set
##'
##' @param destination destination of interest; can be a vector.
##' @param destination_type options: "airport", "city", "state", "country"
##' @param dest_0 default=NULL; change to specify higher level destination (i.e. dest_0="USA")
##' @param dest_0_type default=NULL; must specify if specifying a `dest_0` option.
##' @param dest_aggr_level level to which travel will be aggregated for destination. Includes "airport", "city", "state", "country", "metro" (only available for CA currently)
##'
##' @import dplyr
##'


get_oag_travel <- function(destination=c("CA"),
                           destination_type="state",
                           dest_country="USA",
                           dest_aggr_level="city"){

    # check if destination is in the USA
    # -- only US aggregated data are available in the package
    if (dest_country!="USA"){
        print("Only aggregated, averaged travel data for travel into the USA are included in this package.
              For other countries, provide your own data and use 'get_oag_travel_fulldata' or contact Shaun Truelove (shauntruelove@jhu.edu).")
        return(NA)
    }

    # Load the data
    load_travel_dat <- function(dest_country){
        env <- new.env()
        dest_data <- data(list=tolower(paste0(dest_country, "_oag_aggr_travel")),
                          package = "covidImportation",
                          envir = env)[1]
        return(env[[dest_data]])
    }
    dest_data <- load_travel_dat(dest_country)

    # subset to the destination of interest
    dest_data <- dest_data %>% as.data.frame() %>%
        dplyr::filter(get(paste0("arr_",destination_type)) %in% destination)

    return(dest_data)
}









##'
##' Get Metro Areas for CA
##'
##'  define metropolitan areas
##'
##' @param data
get_CA_metro_labels <- function(data){

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




#' Make input data
#'
#' Produce combined input data.
#'
#' @param incid_data
#' @param travel_data
#' @param pop_data
#' @param dest_aggr_level
#' @param shift_incid_days
#'
#' @import dplyr
#' @importFrom lubridate epiweek
#' @return
#'
#' @examples
make_input_data <- function(incid_data,
                            travel_data,
                            pop_data,
                            dest_aggr_level,
                            shift_incid_days=NA){

    ## Incidence data
    #  - Shift incid_data dates to align with incubation period
    if (!is.na(shift_incid_days)){
        incid_data <- incid_data %>% dplyr::mutate(t = as.Date(t) + shift_incid_days)
    }

    # Drop the cruise ship
    incid_data <- incid_data %>% dplyr::filter(!grepl("cruise ship", source, ignore.case = TRUE) |
                                            !grepl("diamond princess", source, ignore.case = TRUE))

    # merge data (delimit it by travel data)
    pop_data    <- pop_data %>% dplyr::mutate(source = as.character(source)) %>%
        dplyr::select(source, dep_country=country, population=pop)
    incid_data  <- incid_data %>% dplyr::mutate(source = as.character(source)) %>%
        dplyr::select(source, t, incid_est, dep_country=country)



    # ~~  Check that the variables match up

    duplicates <- c(
        # Check that incidence data does not have duplicates
        sum(incid_data %>% dplyr::mutate(source_t = paste(source, t)) %>%
               dplyr::mutate(dup_entry=duplicated(source_t)) %>% dplyr::pull(dup_entry)),
        ## --> no duplicates at the moment...

        # Check travel data
        sum(travel_data %>% dplyr::mutate(source_dest_t = paste(source, arr_airport, t)) %>%
               dplyr::mutate(dup_entry=duplicated(source_dest_t)) %>% dplyr::pull(dup_entry)),
        ## --> no duplicates at the moment...

        # Check Population data
        sum(pop_data %>% dplyr::mutate(dup_entry=duplicated(source)) %>% dplyr::pull(dup_entry))
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
    all_vars <- c(other_vars, arr_vars)
    all_vars <- all_vars[all_vars %in% colnames(travel_data)]

    travel_data <- travel_data %>% dplyr::mutate(source = as.character(source)) %>%
        dplyr::select(all_vars)

    # combine them all
    input_data <- dplyr::full_join(
        dplyr::right_join(pop_data, travel_data, by=c("source", "dep_country")),
        incid_data, by=c("source", "dep_country", "t"))
    input_data <- input_data %>% dplyr::rename(cases_incid=incid_est)

    start_date <- min((input_data %>% dplyr::filter(cases_incid>0))$t)

    # filter data by time and location
    input_data <- input_data %>%
        #dplyr::filter(t > as.Date("2019-12-31")) %>%
        dplyr::filter(t >= as.Date(start_date)) %>%
       dplyr::mutate(cases_incid=ifelse(is.na(cases_incid), 0, cases_incid),
               epiweek = lubridate::epiweek(t))
    input_data <- input_data %>% dplyr::filter(!is.na(travelers))


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



#' Make MeanD Matrix
#'
#' @param input_data
#' @param n_sim
#' @param incub_mean_log
#' @param incub_sd_log
#' @param inf_period_hosp_mean_log
#' @param inf_period_hosp_sd_log
#' @param inf_period_nohosp_mean
#' @param inf_period_nohosp_sd
#'
#' @importFrom truncnorm rtruncnorm
#'
#' @return
#'
#' @examples
make_meanD <- function(input_data,
                       n_sim,
                       incub_mean_log,
                       incub_sd_log,
                       inf_period_hosp_mean_log,
                       inf_period_hosp_sd_log,
                       inf_period_nohosp_mean,
                       inf_period_nohosp_sd){

    # Sample the components of meanD -- will apply the p_report_source to these
    meanD_mat_ <- cbind(
        exp(rnorm(n_sim, mean = incub_mean_log, sd = incub_sd_log)),
        rlnorm(n_sim, meanlog=inf_period_hosp_mean_log, sdlog=inf_period_hosp_sd_log),
        truncnorm::rtruncnorm(n_sim, mean=inf_period_nohosp_mean, sd=inf_period_nohosp_sd, a=0))

    # Apply p_report_source by location and time to get the meanD matrix, where each simulation run has a pre-sampled set of D for each time/location combination
    meanD_mat <- meanD_mat_[,1] +
        meanD_mat_[,2] %*% matrix(input_data$p_report_source, nrow=1) +
        meanD_mat_[,3] %*% matrix((1-input_data$p_report_source), nrow=1)

    return(meanD_mat)
}







##' Create Daily Travel
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
##' @import dplyr
##' @importFrom lubridate days_in_month ydm
##'
##' @return a data frame with randomly distributed travel into days
##'
make_daily_travel <- function(travel_data, travel_dispersion=10){

    travel_data <- travel_data %>%
       dplyr::rename(travelers_month = travelers) %>%
       dplyr::mutate(days_month = lubridate::days_in_month(as.integer(t_month)))

    rows_ <- nrow(travel_data)

    # First sample out the monthly travelers into days
    x <- as.integer(unlist(lapply(X=seq_len(rows_),
                FUN=function(x=X) rmultinom(n = 1,
                                            size = travel_data$travelers_month[x],
                                            prob = rgamma(travel_data$days_month[x], shape=1/travel_dispersion)))))

    # get an indicator for day of the month
    t_day <- unlist(lapply(X=seq_len(rows_), FUN=function(x=X) seq_len(travel_data$days_month[x])))
    # generate a daily dataset
    data_daily <- as.data.frame(lapply(travel_data, rep, travel_data$days_month))
    # Add new daily travel volume to it
    data_daily <- data.frame(data_daily, t_day=t_day, travelers=x)

    data_daily <- data_daily %>% dplyr::mutate(t = lubridate::ymd(paste(t_year, t_month, t_day, sep="-")))
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
##' @importFrom lubridate days_in_month
##' @import dplyr
##'
##' @return data.frame of daily travel data
##'
make_daily_travel_faster <- function(travel_data, travel_data_daily, travel_dispersion=10){

    travel_data <- travel_data %>%
       dplyr::rename(travelers_month = travelers) %>%
       dplyr::mutate(days_month = lubridate::days_in_month(as.integer(t_month)))

    rows_ <- nrow(travel_data)

    # First sample out the monthly travelers into days
    x <- as.integer(unlist(lapply(X=seq_len(rows_),
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
##' @import dplyr
##' @importFrom data.table rbindlist
##'
##' @return expanded data.frame of travel restrictions by day and source location
##'
expand_travel_restrict <- function(travel_restrictions){

    travel_restrictions <- travel_restrictions %>% dplyr::mutate(min=as.Date(min),
                                                          max=as.Date(max))
    travel_restrict_ <- list()
    for (r in seq_len(nrow(travel_restrictions))){
        travel_restrict_[[r]] <- data.frame(loc=travel_restrictions$loc[r],
                                            p_travel=travel_restrictions$p_travel[r],
                                            t = seq(travel_restrictions$min[r], travel_restrictions$max[r], by="days"))
    }
    travel_restrict_ <- data.table::rbindlist(travel_restrict_)

    # Throw an error if duplicates of days for locations
    dupls_ <- sum(duplicated(travel_restrict_ %>% dplyr::mutate(loc_t = paste(loc, t)) %>% dplyr::pull(loc_t)))
    if (dupls_>0){
        stop("Duplicates in travel restrictions. Fix the travel_restrictions file.", call. = FALSE)
    }

    return(travel_restrict_)
}



## Test
#travel_restrictions_long <- expand_travel_restrict(travel_restrictions)



##'
##' Apply a set of travel restrictions to the travel data, reducing or increasing to a proportion of the average travel.
##'
##' @param travel_data Data.frame. Daily travel data that was previously built. We replace the travelers column in this.
##' @param travel_restrictions_long Data.frame. Daily travel restrictions, including dates, source location, and proportion of cases.
##'
##' @import dplyr
##'
apply_travel_restrictions <- function(travel_data, travel_restrictions_long){
    travel_restrictions_long <- travel_restrictions_long %>% dplyr::distinct()
    travel_data <- dplyr::left_join(travel_data,
                             travel_restrictions_long, by=c("t","source"="loc")) %>%
                                    tidyr::replace_na(list(p_travel=1)) %>%
                                   dplyr::mutate(travelers=travelers*p_travel)

    return(travel_data)
}






#' find_recent_file
#'
#' @param name_start character string, first letters in file name
#' @param path character string, path to folder of interest, end with "/"
#' @param exclude character string, patterns to exclude from the file names of interest
#'
#' @return character string, path to most recent file
#'
#' @examples
find_recent_file <- function(name_start, path, exclude=NULL){
    if(substring(path, nchar(path))!="/")
        warning("Path does not end with a '/', problems may ensue.")
    ## view all files of that name at that path
    file_list <- list.files(path=path,
                            pattern=paste0(name_start, "*"))
    ## remove files with unwanted patterns
    if(!is.null(exclude)){
        for(i in seq_len(length(exclude)))
            file_list <- file_list[!grepl(pattern = exclude[i], file_list)]
    }
    if(length(file_list)==0){
        warning('File not found')
        return(NA)
    }
    ## view file info
    file_info <- file.info(paste0(path, file_list))
    ## find most recent file
    most_recent_file <- paste0(path,
                               file_list[which.max(file_info$mtime)])
    cat(sprintf("Loaded file: \n %s last updated on \n %s \n",most_recent_file,file_info$mtime[which.max(file_info$mtime)]))
    return(most_recent_file)
}










##'
##' Pull JHU CSSE GitHub data
##'
##' Pulls the JHUCSSE total case count data up to current date from GitHub.
##' This version checks what is already saved, and downloads those that are not.
##'
##' @param case_data_dir directory where daily reported case data files are stored by the function.
##' @param last_date last date for which to include case data
##' @param save_data whether to save raw data locally
##' @param us_data_only whether to only pull US data
##' @param append_wiki TRUE/FALSE whether to append the data from wikipedia for early china
##'
##' @import dplyr httr
##' @importFrom lubridate mdy
##' @importFrom readr read_csv write_csv
##' @importFrom data.table rbindlist
##'
##' @return NA (saves a CSV of the current data to the data directory)
##'
##' @export
##' 
get_JHUCSSE_data <- function(case_data_dir = "data/case_data",
                             last_date=Sys.time(),
                             save_data=FALSE,
                             us_data_only=TRUE,
                             append_wiki=TRUE){
    
    # Create directory to hold all the data
    if (save_data){
        dir.create(case_data_dir, showWarnings = FALSE, recursive = FALSE)
        print(paste0("Combined data is saved in ", case_data_dir, "."))
    }
    
    # First get a list of files so we can get the latest one
    req <- httr::GET("https://api.github.com/repos/CSSEGISandData/COVID-19/git/trees/master?recursive=1")
    httr::stop_for_status(req)
    
    filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
    file_name_us_ <- grep("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", filelist, value=TRUE)
    file_name_global_ <- grep("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", filelist, value=TRUE)
    
    # read data from github
    us_url_ <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",file_name_us_)
    time_series_dat <- readr::read_csv(url(us_url_))
    
    case_data <- time_series_dat %>% tibble::as_tibble() %>%
        tidyr::pivot_longer(cols=-(UID:Combined_Key), names_to="Update", values_to="Confirmed") 
    case_data <- case_data %>% 
        dplyr::mutate(Update = as.Date(lubridate::mdy(Update)),
                      FIPS = ifelse(stringr::str_length(FIPS)==2, paste0(FIPS, "000"),
                                    stringr::str_pad(FIPS, 5, pad = "0")))
    case_data <- case_data %>% 
        dplyr::arrange(UID, Update) %>%
        dplyr::group_by(UID) %>%
        dplyr::mutate(incidI = diff(c(0,Confirmed))) %>% dplyr::ungroup()
    
    # Fix the different file column names
    colnames_ <- colnames(case_data)
    colnames_[grepl("Province", colnames_)] <- "Province_State"
    colnames_[grepl("Country", colnames_)] <- "Country_Region"
    colnames_[grepl("Update", colnames_)] <- "Update"
    colnames_[grepl("Lat", colnames_)] <- "Latitude"
    colnames_[grepl("Long", colnames_)] <- "Longitude"
    colnames(case_data) <- colnames_
    
    if (!us_data_only){
        
        # read data from github
        global_url_ <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/",file_name_global_)
        time_series_dat <- readr::read_csv(url(global_url_))
        
        # Fix the different file column names
        colnames_ <- colnames(time_series_dat)
        colnames_[grepl("Province", colnames_)] <- "Province_State"
        colnames_[grepl("Country", colnames_)] <- "Country_Region"
        colnames_[grepl("Update", colnames_)] <- "Update"
        colnames_[grepl("Lat", colnames_)] <- "Latitude"
        colnames_[grepl("Long", colnames_)] <- "Longitude"
        colnames(time_series_dat) <- colnames_
        
        case_data_global <- time_series_dat %>% tibble::as_tibble() %>%
            dplyr::mutate(iso3 = globaltoolboxlite::get_iso(Country_Region)) %>%
            dplyr::mutate(iso2 = globaltoolboxlite::get_iso2_from_ISO3(iso3),
                          UID = ifelse(!is.na(Province_State), paste0(iso3, "-", Province_State), iso3)) %>%
            dplyr::select(UID, Province_State:Longitude,iso2,iso3, tidyselect::everything()) %>%
            tidyr::pivot_longer(cols=-(UID:iso3), names_to="Update", values_to="Confirmed") %>% 
            dplyr::mutate(Update = as.Date(lubridate::mdy(Update)))
        
        if (append_wiki) {
            data("wikipedia_cases", package="covidImportation")
            wikipedia_cases <- wikipedia_cases %>% 
                dplyr::mutate(Country_Region = ifelse(Country_Region=="Mainland China", "China", Country_Region),
                              Update = as.Date(Update),
                              iso2 = "CN", iso3 = "CHN",
                              Latitude = 30.9756, Longitude = 112.2707) %>%
                dplyr::mutate(UID = ifelse(!is.na(Province_State), paste0(iso3, "-", Province_State), iso3)) %>%
                dplyr::filter(!is.na(Confirmed))
            
            case_data_global <- dplyr::bind_rows(case_data_global, wikipedia_cases)
        }
        
        case_data_global <- case_data_global %>% 
            dplyr::arrange(Country_Region, Province_State, Update) %>%
            dplyr::group_by(Country_Region, Province_State) %>%
            dplyr::mutate(incidI = diff(c(0,Confirmed))) %>% dplyr::ungroup()
        
        # Join them
        case_data <- case_data %>% 
            tibble::as_tibble() %>%
            dplyr::mutate(UID = as.character(UID)) %>%
            dplyr::full_join(case_data_global %>% 
                          tibble::as_tibble() %>% 
                              dplyr::mutate(UID = as.character(UID)), 
                      by=c("UID", "Province_State", "Country_Region", "iso2", "iso3", 
                           "Latitude", "Longitude", "Update", "Confirmed", "incidI"))
    }
    
    ##Now drop any after the date given
    case_data <- case_data %>% 
        dplyr::filter(as.Date(Update) <= as.Date(last_date)) %>% 
        dplyr::distinct()
    
    # Get US States ................
    # tidyr::separate out states
    case_data <- suppressWarnings(
        case_data %>%
            dplyr::mutate(state_abb = state.abb[match(Province_State, state.name)]) %>%
            dplyr::mutate(state_abb = ifelse(Province_State=="District of Columbia", "DC",
                                             ifelse(is.na(state_abb) & Country_Region=="US", iso2, state_abb)))
    )
    
    # Define a single source location variable
    # - USA: States used for source
    # - China: Provinces used for source
    # - Others: Country used for source
    case_data <- case_data %>% 
        dplyr::mutate(source = ifelse(Country_Region=="US" & !is.na(state_abb), state_abb,
                                      ifelse(iso3=="CHN" & !is.na(Province_State), Province_State, iso3))) %>%
        dplyr::filter(Update < (as.Date(Sys.time())-1))
    
    
    # Save if desired
    if (save_data){
        if (us_data_only){
            readr::write_csv(case_data, file.path(case_data_dir,"jhucsse_us_case_data_crude.csv"))
        } else {
            readr::write_csv(case_data, file.path(case_data_dir,"jhucsse_case_data_crude.csv"))
        }
    }
    
    return(case_data)
}






#' Clean crude data from JHUCSSE and aggregated it to state or county level
#'
#' @param aggr_level "UID", "source", level at which to calculate the cumulative and incident cases. with UID, unassigned locations are dropped
#' @param last_date
#' @param case_data_dir directory where case data is being saved
#' @param save_raw_data whether to save raw data locally
#' @param us_data_only whether to only pull US data
#' 
#' @return
#'
#' @examples
#'
#' @import globaltoolboxlite dplyr tidyr
#' @importFrom globaltoolboxlite get_country_name_ISO3 get_iso
#'
#' @export
#'
get_clean_JHUCSSE_data <- function(aggr_level = "UID", #"source",
                               last_date = Sys.time(),
                               case_data_dir = "data/case_data",
                               save_raw_data=TRUE,
                               us_data_only=FALSE){
    
    ## Get case count data (from JHU CSSE's github)
    jhucsse_case_data_raw <- suppressMessages(suppressWarnings(get_JHUCSSE_data(case_data_dir = case_data_dir,
                                                                                last_date=last_date,
                                                                                save_data=save_raw_data,
                                                                                us_data_only=us_data_only,
                                                                                append_wiki=TRUE)))
    
    if (aggr_level=="UID"){
        
        # get rid of "Unassigned" and  "Out of" 
        jhucsse_case_data <- jhucsse_case_data_raw %>% 
            dplyr::filter(!grepl("out of", Admin2, ignore.case = TRUE) &
                              !grepl("unassigned", Admin2, ignore.case = TRUE))
        jhucsse_case_data <- jhucsse_case_data %>% 
            dplyr::filter(!grepl("princess", Province_State, ignore.case = TRUE))
        
        # Get incident cases by UID (US counties, Chinese provinces, Countries otherwise)
        jhucsse_case_data <- jhucsse_case_data %>% 
            dplyr::arrange(Country_Region, source, UID, Update) 
        
        # Fix counts that go negative
        jhucsse_case_data <- jhucsse_case_data %>% dplyr::mutate(incid_conf = incidI,
                                                                 Confirmed_new = Confirmed)
        #counter <- 0
        while(sum(jhucsse_case_data$incid_conf<0)>0){
            
            # first try to just remove the row
            jhucsse_case_data <- jhucsse_case_data %>% 
                dplyr::arrange(Country_Region, source, UID, Update) 
            jhucsse_case_data <- jhucsse_case_data %>% dplyr::filter(jhucsse_case_data$incid_conf >= 0)
            jhucsse_case_data <- jhucsse_case_data %>%
                dplyr::group_by(UID, Country_Region) %>%
                dplyr::mutate(incid_conf = diff(c(0,Confirmed_new))) %>% dplyr::ungroup()
            
            
            #counter <- counter + 1
            #print(counter)
            
            negs_ind <- which(jhucsse_case_data$incid_conf < 0)
            if (length(negs_ind)>0){
                jhucsse_case_data <- jhucsse_case_data %>% 
                    dplyr::arrange(Country_Region, source, UID, Update) 
                jhucsse_case_data$Confirmed_new[negs_ind - 1] <- jhucsse_case_data$Confirmed_new[negs_ind - 1] + jhucsse_case_data$incid_conf[negs_ind]
                jhucsse_case_data <- jhucsse_case_data %>%
                    dplyr::group_by(UID, Country_Region) %>%
                    dplyr::mutate(incid_conf = diff(c(0,Confirmed_new))) %>% dplyr::ungroup()
            }
        }
        
    } else if (aggr_level=="source"){
        
        # Get cum incidence for states/provinces, countries
        jhucsse_case_data <- jhucsse_case_data_raw %>%
            dplyr::arrange(Country_Region, iso3, iso2, source, UID, Update) %>%
            dplyr::group_by(Country_Region,iso3, iso2, source, Update) %>%
            dplyr::summarise(Confirmed = sum(Confirmed)) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(Country_Region, iso3, iso2, source, Update) %>%
            dplyr::group_by(Country_Region, iso3, iso2, source) %>%
            dplyr::mutate(incidI = diff(c(0,Confirmed))) %>%
            dplyr::ungroup()
        
        # Fix counts that go negative
        jhucsse_case_data <- jhucsse_case_data %>% dplyr::mutate(incid_conf = incidI,
                                                                 Confirmed_new = Confirmed)
        #counter <- 0
        while(sum(jhucsse_case_data$incid_conf<0)>0){
            
            # first try to just remove the row
            jhucsse_case_data <- jhucsse_case_data %>% 
                dplyr::arrange(Country_Region, iso3, iso2, source, Update) 
            jhucsse_case_data <- jhucsse_case_data %>% dplyr::filter(jhucsse_case_data$incid_conf >= 0)
            jhucsse_case_data <- jhucsse_case_data %>%
                dplyr::group_by(Country_Region, iso3, iso2, source) %>%
                dplyr::mutate(incid_conf = diff(c(0,Confirmed_new))) %>% dplyr::ungroup()
            
            
            #counter <- counter + 1
            #print(counter)
            
            negs_ind <- which(jhucsse_case_data$incid_conf < 0)
            if (length(negs_ind)>0){
                jhucsse_case_data <- jhucsse_case_data %>% 
                    dplyr::arrange(Country_Region, iso3, iso2, source, Update) 
                jhucsse_case_data$Confirmed_new[negs_ind - 1] <- jhucsse_case_data$Confirmed_new[negs_ind - 1] + jhucsse_case_data$incid_conf[negs_ind]
                jhucsse_case_data <- jhucsse_case_data %>%
                    dplyr::group_by(Country_Region, iso3, iso2) %>%
                    dplyr::mutate(incid_conf = diff(c(0,Confirmed_new))) %>% dplyr::ungroup()
            }
            
        }
    }
    
    jhucsse_case_data <- jhucsse_case_data %>% 
        dplyr::mutate(Confirmed = Confirmed_new, incidI = incid_conf) %>%
        dplyr::select(-Confirmed_new, -incid_conf)
    
    return(jhucsse_case_data)
}





#' Get incidence data from JHUCSSE
#'
#' @param aggr_level "UID", "source", level at which to calculate the cumulative and incident cases. with UID, unassigned locations are dropped; "source" refers to US state, Chinese province, and national otherwise.
#' @param first_date
#' @param last_date
#' @param case_data_dir directory where case data is being saved
#' @param check_saved_data whether to check locally saved github data
#' @param save_raw_data whether to save raw data locally
#' @param us_data_only whether to only pull US data
#'
#' @return
#'
#' @examples
#'
#' @import globaltoolboxlite dplyr tidyr
#' @importFrom globaltoolboxlite get_country_name_ISO3 get_iso
#'
#' @export
#'
get_incidence_fits <- function(aggr_level = "UID", #"source",
                               first_date = ISOdate(2019,12,1), 
                               last_date = Sys.time(),
                               case_data_dir = "data/case_data",
                               save_raw_data=TRUE,
                               us_data_only=FALSE){
    
    ## Get case count data (from JHU CSSE's github)
    case_data <- get_clean_JHUCSSE_data(aggr_level = aggr_level, 
                                 last_date = last_date,
                                 case_data_dir = case_data_dir,
                                 save_raw_data=save_raw_data,
                                 us_data_only=us_data_only)
    
    if (aggr_level=="UID"){
        case_data <- case_data %>% dplyr::select(-source) %>% mutate(source=UID)
    }
    
    
    ## GET INCIDENCE FITS ..........................
    ## Estimate incidence using spline fits.
    incid_data <- est_daily_incidence_corrected(case_data %>% 
                                                    dplyr::mutate(Province_State=source),
                                                first_date, last_date, tol=100, na_to_zeros=FALSE) %>%
        dplyr::mutate(Incidence=round(Incidence, 2))
    
    ## Incidence Data
    incid_data <- incid_data %>% dplyr::rename(source=Province_State, cases_incid=Incidence) %>%
        dplyr::mutate(source = as.character(source),
                      t = as.Date(Date)) %>% tibble::as_tibble()
    
    # Add country_name back in
    incid_data <- dplyr::left_join(incid_data,
                            case_data %>% dplyr::select(source, country_name=Country_Region, country=iso3) %>%
                                dplyr::mutate(prov_country = paste0(source,"-", country_name)) %>%
                                dplyr::filter(!duplicated(prov_country)) %>% dplyr::select(-prov_country),
                            by=c("source"="source"))
    # Add confirmed cases back in
    incid_data <- dplyr::left_join(incid_data,
                            case_data  %>% dplyr::select(Update, source, country_name=Country_Region, country=iso3, incid_conf=incidI) %>% 
                                dplyr::mutate(t = as.Date(Update)) %>%
                                dplyr::group_by(t, source, country) %>%
                                dplyr::summarise(incid_conf = sum(incid_conf, na.rm=TRUE)) %>%
                                dplyr::arrange(country, source, t) %>%
                                dplyr::group_by(source, country) %>%
                                dplyr::mutate(cum_incid = cumsum(incid_conf)) %>% dplyr::ungroup(),
                            by=c("source"="source", "t"="t", "country"))
    incid_data <- incid_data %>% dplyr::mutate(incid_conf=ifelse(is.na(incid_conf), 0, incid_conf))
    
    
    # Drop NA source
    #View(incid_data %>% dplyr::filter(is.na(source)))
    incid_data <- incid_data %>% dplyr::filter(!is.na(source))
    
    
    # Get cumulative estimated incidence
    incid_data <- incid_data %>%
        dplyr::arrange(country, source, t) %>%
        dplyr::group_by(source, country) %>%
        dplyr::mutate(cum_est_incid = cumsum(cases_incid)) %>% dplyr::ungroup()
    
    return(incid_data)
}
