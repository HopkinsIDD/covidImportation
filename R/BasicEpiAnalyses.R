
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
fit_ispline <- function (dates, obs, df=round(length(obs)/3)) {
  require(nnls)
  require(splines2)

  #first get the basis
  h <- iSpline(as.numeric(dates), df=df, intercept=T)


  #fit the nnls model to the data
  mdl <- nnls(h, obs)
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
##' @param second_date the last date to infer...shold be iwthin data range
##'
##'
##' @return a data frame with roughly estimated incidence in it
##'
est_daily_incidence <- function (cum_data,
                                 first_date,
                                 last_date,
                                 na_to_zeros=FALSE) {
  if (na_to_zeros) {
    analyze <-   cum_data %>% replace(is.na(.), 0)
  } else {
    analyze <-   cum_data %>% drop_na(Confirmed)
  }


  ##Get the implied daily incidence for each province
  ##by fitting a monitonically increasing spline and then
  ##taking the difference (a little less sensitive to
  ##perturbations in reporting than taking raw difference).
  ##Making sure only to infer over trhe suport
  tmp_dt_seq <- seq(first_date, last_date, "days")
  incidence_data<- analyze %>% nest(-Province_State) %>%
    mutate(cs=map(data, ~fit_ispline(dates=.$Update, obs=.$Confirmed))) %>%
    mutate(Incidence=map2(cs,data, ~data.frame(Date=tmp_dt_seq[tmp_dt_seq>=min(.y$Update) & tmp_dt_seq<=max(.y$Update)],
                                               Incidence= diff(c(0, pmax(0,.x(tmp_dt_seq[tmp_dt_seq>=min(.y$Update) & tmp_dt_seq<=max(.y$Update)]))))))) %>%
    unnest(Incidence) %>% dplyr::select(-data) %>% dplyr::select(-cs)

  return(incidence_data)

  #####OLD VERSION
  # tmp_dt_seq <- seq(first_date, last_date, "days")
  # incidence_data<- analyze %>% nest(-Province_State) %>%
  #   mutate(cs=map(data, ~splinefun(x=.$Update, y=.$Confirmed,
  #                                  method="hyman"))) %>%
  #   mutate(Incidence=map2(cs,data, ~data.frame(Date=tmp_dt_seq[tmp_dt_seq>=min(.y$Update) & tmp_dt_seq<=max(.y$Update)],
  #                                              Incidence= diff(c(0, pmax(0,.x(tmp_dt_seq[tmp_dt_seq>=min(.y$Update) & tmp_dt_seq<=max(.y$Update)]))))))) %>%
  #   unnest(Incidence) %>% dplyr::select(-data) %>% dplyr::select(-cs)

  return(incidence_data)

}




##'
##' Function to automate the comparison of MERS-CoV deaths with
##' that from the Kudos line list.
##'
##'
##' @param kudos the data from the kudos line list.
##'
##' @return a list with a data frame of results and a ggplot object.
##'
compare_deaths_to_MERS <- function (kudos) {
  mers_dat <- read_csv("data/MERSDeathPublic.csv",
                       col_types = cols(age_class=col_factor(levels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"))))

  ##make look like the kudos dat enough for us to run same table
  mers_dat <- mers_dat %>% rename(death=died) %>%
    rename(age_cat=age_class)

  mers_OR_tbl <- OR_table_age(mers_dat, combine_CIs = FALSE)

  #make the nCoV table look like the MERS one for a more direct
  #comparison.

  kudos <- kudos %>%
    mutate(age_cat=cut(age,breaks=c(0,10,20,30,40,50,60,70,1000),
                       labels=c("0-9","10-19",
                                "20-29","30-39",
                                "40-49","50-59",
                                "60-69","70+")))


  age_OR_tbl <- OR_table_age(kudos, combine_CIs = FALSE)

  ##Replace observations with no data wiht NAs
  no_data_index <- which(age_OR_tbl$dead==0)
  age_OR_tbl$OR[no_data_index] <- NA
  age_OR_tbl$CI_low[no_data_index] <- NA
  age_OR_tbl$CI_high[no_data_index] <- NA

  ##combine to plot
  comb_OR_tbl <- bind_rows(nCoV=age_OR_tbl,
                           MERS=mers_OR_tbl, .id="disease")

  comb_OR_tbl$label <- sprintf("%1.2f (%1.2f, %1.2f)",
                               comb_OR_tbl$OR,
                               comb_OR_tbl$CI_low,
                               comb_OR_tbl$CI_high)
  comb_OR_tbl$label[comb_OR_tbl$OR==1] <- NA

  mers_comp_plt <- ggplot(comb_OR_tbl, aes(x=age_cat, y=OR, color=disease, label=label)) +
    geom_pointrange(aes(ymin=CI_low, ymax=CI_high),
                    position = position_dodge2(width = 0.5, padding = 0.5)) +
    scale_y_log10() + ylab("OR of death")+
    xlab("Age") +
    theme_bw()

  comb_OR_wide <- comb_OR_tbl %>%
    dplyr::select(disease,age_cat,label) %>%
    pivot_wider(names_from=disease, values_from = label)

  comb_OR_wide[6,c("nCoV","MERS")] <- "1"
  comb_OR_wide$nCoV[no_data_index] <- "-"

  return(list(plt = mers_comp_plt, table = comb_OR_wide))
}







##'
##' Function to correct for the changes in reporting in Hubei
##'
##' @param cumdat data frame with the cumulative number of cases
##' @param first_date the first date to infer incidence over
##' @param last_date  the latest date to infer incidence over
##'
##' @return a version of the inferred incidence data corrected for reportin changes.
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
                                           mutate(Confirmed=Confirmed-confirmed_14),
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
    rc_incidence$Incidence[1:length(to_add)] <-
      rc_incidence$Incidence[1:length(to_add)]+ to_add

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
##'
##' @return a corrected version of the inferred incidence data corrected for reporting changes in Hubei.
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
    arrange(Province_State, Date)

  return(incid_data)
}




##'
##' Function to plot the estimated and reported case counts
##'
##' @param conf_cases Confirmed case data from JHU CSSE
##' @param incid_ests Estimated incidence
##' @param locations  Locations to plot, can be a vector
##'
##' @return a corrected version of the inferred incidence data corrected for reporting changes in Hubei.
##'
plot_incidence_ests_report <- function(conf_cases=jhucsse, incid_ests=incidence_data,
                                       locations="All", ncol_facet=2){

  if (locations=="All"){
    incid_ests <- incid_ests %>% mutate(Incidence = ceiling(Incidence))
  } else {
    incid_ests <- incid_ests %>% mutate(Incidence = ceiling(Incidence)) %>% dplyr::filter(Province_State %in% locations)
    conf_cases <- conf_cases %>% dplyr::filter(Province_State %in% locations)
  }
  incid_ests$Date <- as.Date(incid_ests$Date, "%m/%d/%Y", tz = "UTC")

  # Make conf_cases daily
  # Get daily calculated incidence (from reporting)
  conf_cases_daily <- conf_cases %>% dplyr::filter(!is.na(Confirmed)) %>%
    mutate(Date = as.Date(Update)) %>% group_by(Province_State, Date) %>% dplyr::filter(Update == max(Update, na.rm=TRUE)) %>% ungroup()
  conf_cases_daily <- conf_cases_daily %>% group_by(Province_State) %>% arrange(Date) %>% mutate(Incidence = diff(c(0, Confirmed))) %>% ungroup()

  # Merge in reported incidence
  incid_data_ <- left_join(incid_ests, conf_cases_daily %>% rename(Incid_rep = Incidence), by=c("Province_State", "Date")) %>% as_tibble()

  if (locations=="All"){
    incid_data_ <- incid_data_ %>% group_by(Date) %>% summarize(Incidence=sum(Incidence, na.rm = TRUE), Incid_rep=sum(Incid_rep, na.rm = TRUE))
  }

  # Plot
  p <- ggplot(incid_data_, aes(x=Date)) +
    geom_bar(stat = "identity", aes(y=Incidence, fill="Estimated Incidence", group=1)) +
    geom_point(aes(x=Date, y=Incid_rep, color="Confirmed Cases")) +
    coord_cartesian(xlim=c(as.Date("2020-01-15"), max(as.Date(incid_data_$Date))),
                    ylim=c(0,max(incid_data_$Incidence*1.25))) +
    theme_classic() +
    scale_fill_manual(values="maroon", name=NULL, label="Estimated Incidence") +
    scale_color_manual(values="navyblue", name=NULL, label="Confirmed Cases") +
    theme(legend.position = c(0.125, .92),
          legend.spacing.y = unit(0, 'cm'),
          legend.key.size = unit(6, "pt"),
          legend.text=element_text(size=6))

  if (length(locations)>1){
    p <- p + facet_wrap(vars(Province_State), ncol=ncol_facet)
  }
  return(p)
  #plot(p)
}




get_global_cum <- function(df = jhucsse, case_limit=100){

  conf_cases_global <- df %>%
    dplyr::filter(Country_Region != "Mainland China" & !(Province_State %in% c("Hong Kong", "Macau", "Taiwan"))) %>%
    mutate(t = as.Date(Update)) %>% arrange(Province_State, Country_Region, Update) %>%
    group_by(Country_Region, Province_State) %>% mutate(Incidence = diff(c(0, Confirmed), na.rm=TRUE)) %>% ungroup() %>%
    group_by(Country_Region, Province_State, t) %>% summarise(Incidence = sum(Incidence, na.rm = TRUE))

  conf_cases_global <- conf_cases_global %>% dplyr::filter(t >= as.Date("2020-01-01"))
  t_values <- as.character(sort(conf_cases_global$t))
  conf_cases_global_cum <- conf_cases_global %>% group_by(Province_State) %>%
    summarise(cum_cases = sum(Incidence)) %>% dplyr::filter(cum_cases>=case_limit)

  return(conf_cases_global_cum)
}
