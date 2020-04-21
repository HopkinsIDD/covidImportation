
##'
##' Function to fit monotonically increasing spline. Returns a function that
##' can be used to predict the spline for each date.
##'
##'
##' @param dates dates of observations
##' @param obs the observations
##' @param df degrees of freedon.
##'
##' @return a function that takse in some number of dates and gives predictions on those
##'
##' @importFrom nnls nnls
##' @importFrom splines2 iSpline
##' @export
##' 
fit_ispline <- function (dates, obs, df=round(length(obs)/3)) {

  #first get the basis
  h <- splines2::iSpline(as.numeric(dates), df=df, intercept=T)


  #fit the nnls model to the data
  mdl <- nnls::nnls(h, obs)
  coefs <- coef(mdl)


  rc <- function(dates) {
    if(length(dates)==0) {return(NULL)}
    hnew <- predict(h, as.numeric(dates))
    return(hnew%*%coefs)
  }

  return(rc)
}


##'
##' Function to extract approximate epidemic curves
##' from the cumulative case data.
##'
##' @param cum_data a data frame with cumulative case data in oit
##' @param first_date the first date to infer
##' @param last_date  the latest date to infer incidence over
##' @param na_to_zeros logical; whether to turn NAs to 0s.
##'
##' @return a data frame with roughly estimated incidence in it
##'
##' @import dplyr 
##' @importFrom purrr map map2
##' 
##' @export
##' 
est_daily_incidence <- function (cum_data,
                                 first_date,
                                 last_date,
                                 na_to_zeros=FALSE) {
  if (na_to_zeros) {
    analyze <-   cum_data %>% replace(is.na(.), 0)
  } else {
    analyze <-   cum_data %>% tidyr::drop_na(Confirmed)
  }


  ##Get the implied daily incidence for each province
  ##by fitting a monitonically increasing spline and then
  ##taking the difference (a little less sensitive to
  ##perturbations in reporting than taking raw difference).
  ##Making sure only to infer over the suport
  tmp_dt_seq <- as.Date(seq(first_date, last_date, "days"))
  incidence_data <- analyze %>% 
    tidyr::nest(data=-Province_State) %>%
    dplyr::mutate(cs = purrr::map(data, ~fit_ispline(dates=.$Update, obs=.$Confirmed))) %>%
    dplyr::mutate(Incidence = purrr::map2(cs, data, ~data.frame(Date=tmp_dt_seq[tmp_dt_seq>=min(.y$Update) & tmp_dt_seq<=max(.y$Update)],
                                                                Incidence= diff(c(0, pmax(0,.x(tmp_dt_seq[tmp_dt_seq>=min(.y$Update) & tmp_dt_seq<=max(.y$Update)]))))))) %>%
    tidyr::unnest(Incidence) %>% dplyr::select(-data) %>% dplyr::select(-cs)

  return(incidence_data)

}







##'
##' Function to correct for the changes in reporting in Hubei
##'
##' @param cumdat data frame with the cumulative number of cases
##' @param first_date the first date to infer incidence over
##' @param last_date  the latest date to infer incidence over
##' @param tol error tolerance, in terms of number of cases
##'
##' @return a version of the inferred incidence data corrected for reportin changes.
##'
##' @export
##' 
correct_for_Hubei_reporting <- function (cum_data, first_date, last_date, tol=100) {

  ## Reduce to just Hubei
  cum_data <- cum_data %>% dplyr::filter(Province_State=="Hubei")

  ## Keep the original data for the 13th and 14th for later
  confirmed_13 <- (cum_data %>% dplyr::filter(as.Date(Update)==as.Date("2020-02-13")))$Confirmed
  confirmed_14 <- (cum_data %>% dplyr::filter(as.Date(Update)==as.Date("2020-02-14")))$Confirmed

  ##Get a version of the data going only to the 12
  cum_data_to12 <- dplyr::filter(cum_data, as.Date(Update)<as.Date("2020-02-13"))

  ## Fit the incidence curve to all data up unto the 2th
  incidence_data <- est_daily_incidence(cum_data_to12,
                                        first_date,
                                        ISOdate(2020,2,13))


  ## Now get the difference between the inferred confirmed 13 and the actual
  inferred_13_smth <- incidence_data$Incidence[nrow(incidence_data)]
  inferred_13_cum_data <- confirmed_13 - sum((incidence_data %>% dplyr::filter(Date<"2020-02-13"))$Incidence)
  inferred_14_cum_data <- confirmed_14-confirmed_13


  ##subtract...not here we are projecting the same incidence forward fo the 13th and 14th
  diff_inferred <- inferred_13_cum_data - inferred_13_smth +  inferred_14_cum_data - inferred_13_smth

  #get incidence inferring only from after the 14th
  late_incidence <-  est_daily_incidence(cum_data %>%
                                           dplyr::filter(Update>"2020-02-14") %>%
                                           dplyr::mutate(Confirmed=Confirmed-confirmed_14),
                                         first_date,
                                         last_date)
  #print(late_incidence)

  ##Create data for everything and drop in what we have here
  rc_incidence <- est_daily_incidence(cum_data,
                                      first_date,
                                      last_date)

  rc_incidence$Incidence[rc_incidence$Date<"2020-02-13"] <- incidence_data$Incidence
  rc_incidence$Incidence[rc_incidence$Date=="2020-02-13"] <- inferred_13_smth
  rc_incidence$Incidence[rc_incidence$Date=="2020-02-14"] <- inferred_13_smth
  rc_incidence$Incidence[rc_incidence$Date>"2020-02-14"] <- late_incidence$Incidence
  ## Keep the incidence that we want to return
  #rc_incidence <- incidence_data


  while (abs(diff_inferred)>tol) {
    to_add <- (incidence_data %>% dplyr::filter(Date<"2020-02-13"))$Incidence
    to_add <- to_add/sum(to_add) * diff_inferred
    rc_incidence$Incidence[seq_len(length(to_add))] <-
      rc_incidence$Incidence[seq_len(length(to_add))]+ to_add

    ## create a new cumsum data
    tmp_cum_data <- data_frame(Update = rc_incidence$Date,
                               Confirmed = cumsum(rc_incidence$Incidence),
                               Province_State = as.factor("Hubei"))
    rc_incidence <- est_daily_incidence(tmp_cum_data,
                                        first_date,
                                        last_date)

    inferred_13_smth <- dplyr::filter(rc_incidence, as.Date(Date)==as.Date("2020-02-13"))$Incidence
    inferred_14_smth <- dplyr::filter(rc_incidence, as.Date(Date)==as.Date("2020-02-14"))$Incidence

    inferred_13_cum_data <- confirmed_13 -
      sum((rc_incidence %>% dplyr::filter(Date<"2020-02-13"))$Incidence)

    diff_inferred <- inferred_13_cum_data - inferred_13_smth +  inferred_14_cum_data - inferred_13_smth

    #print(diff_inferred)
  }

  return(rc_incidence)
}






##'
##' Wrapper function to correct for the changes in reporting in Hubei and merge with data for all incidence
##'
##' @param cum_data   data frame with the cumulative number of cases
##' @param first_date the first date to infer incidence over
##' @param last_date  the latest date to infer incidence over
##' @param tol error tolerance, in terms of number of cases
##'
##' @return a corrected version of the inferred incidence data corrected for reporting changes in Hubei.
##'
##' @import dplyr
##' 
##' @export
##' 
est_daily_incidence_corrected <- function(cum_data, first_date, last_date, tol=100, na_to_zeros=FALSE){

  ## Get estimated daily incidence for all provinces
  incid_uncorr <- est_daily_incidence(cum_data, first_date, last_date, na_to_zeros)

  ## Get estimated and corrected daily incidence for Hubei
  incid_hubei_corr <- correct_for_Hubei_reporting(cum_data, first_date, last_date, tol)

  ## Merge these, keeping the corrected Hubei incidence estimates.
  incid_data <- bind_rows(incid_uncorr %>%
                            dplyr::filter(!(Province_State == "Hubei" & Date <= max(incid_hubei_corr$Date))),
                          incid_hubei_corr)
  incid_data <- incid_data %>% mutate(Province_State = factor(Province_State,
                                                              labels = sort(unique(Province_State)),
                                                              levels = sort(unique(Province_State)))) %>%
    dplyr::arrange(Province_State, Date)

  return(incid_data)
}




##'
##' Get cumulative case counts by Province_state
##'
##' @param df JHUCSSE cleaned data
##' @param case_limit limit to Province_State locations above a certain case count
##'
##' @return Cumulative confirmed cases by Province_State.
##'
##' @import dplyr
##' 
##' @export
##' 
get_global_cum <- function(df = jhucsse, case_limit=100){

  conf_cases_global <- df %>%
    dplyr::filter(Country_Region != "Mainland China" & !(Province_State %in% c("Hong Kong", "Macau", "Taiwan"))) %>%
    dplyr::mutate(t = as.Date(Update)) %>% dplyr::arrange(Province_State, Country_Region, Update) %>%
    dplyr::group_by(Country_Region, Province_State) %>% dplyr::mutate(Incidence = diff(c(0, Confirmed), na.rm=TRUE)) %>% dplyr::ungroup() %>%
    dplyr::group_by(Country_Region, Province_State, t) %>% dplyr::summarise(Incidence = sum(Incidence, na.rm = TRUE))

  conf_cases_global <- conf_cases_global %>% dplyr::filter(t >= as.Date("2020-01-01"))
  t_values <- as.character(sort(conf_cases_global$t))
  conf_cases_global_cum <- conf_cases_global %>% dplyr::group_by(Province_State) %>%
    dplyr::summarise(cum_cases = sum(Incidence)) %>% dplyr::filter(cum_cases>=case_limit)

  return(conf_cases_global_cum)
}





#' Get mean importations across geoids
#'
#' @param yr year of population data
#' @param output_dir where output files are saved
#' @param local_dir directory of the population data
#' @param n_sim number of simulations to include
#'
#' @return
#' 
#' @importFrom readr read_csv
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#' @importFrom tidyr replace_na
#' @importFrom stringr str_pad
#' @import dplyr
#'
#' @export
#'
get_mean_imports <- function(yr = 2018,
                             output_dir = file.path("model_output", "importation"),
                             local_dir="data/",
                             n_sim=100){
  # get possible geoids
  county_data <- readr::read_csv(paste0(local_dir, "/county_pops_", yr, ".csv"))
  
  
  ## Get filenames 
  import_files <- list.files(output_dir, "importation_*.*.csv$", full.names = TRUE)
  if (length(import_files)<n_sim){
    n_sim <- length(import_files)
  }
  
  # Run the for loop to estimate importations for n simulations
  imports_sim <- data.table::fread(import_files[1]) %>% tibble::as_tibble()
  imports_all <- expand.grid(place = county_data$GEOID, 
                             date = seq(min(as.Date(imports_sim$date))-30, as.Date(max(imports_sim$date))+30, by="days"), 
                             amount = 0, 
                             amountsq = 0)  %>% tibble::as_tibble() %>%
    dplyr::mutate(id = as.character(paste(place, date, sep = "-")))
  
  for(n in seq_len(n_sim)){
    imports_sim <- data.table::fread(import_files[n]) %>% 
      dplyr::mutate(place = stringr::str_pad(place, 5, pad = "0")) %>%
      dplyr::mutate(id = as.character(paste(place, date, sep = "-")))
    
    match_id <- match(imports_sim$id, imports_all$id)
    imports_all$amount[match_id] <- imports_all$amount[match_id] + imports_sim$amount  
    imports_all$amountsq[match_id] <- imports_all$amountsq[match_id] + (imports_sim$amount^2)
  }
  
  imports_all <- imports_all %>% tibble::as_tibble() %>% 
    dplyr::mutate(import_mean = round(amount / n_sim, 1),
                  importsq_mean = round(amountsq / n_sim, 1)) %>%
    dplyr::mutate(import_var = importsq_mean - (import_mean^2)) %>%
    tidyr::replace_na(list(import_mean=0, import_var=0)) %>% 
    dplyr::select(-importsq_mean, -amount, -amountsq)
  
  #Add county info
  imports_all <- suppressWarnings(imports_all %>% dplyr::left_join(county_data %>% dplyr::select(GEOID, state=id, name=NAME.x), by=c("place"="GEOID")))
  
  return(imports_all)
}

