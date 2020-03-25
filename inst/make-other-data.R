
## make underreporting data
source("https://raw.githubusercontent.com/salauer/CFR_calculation/master/global_estimates/scripts/main_script_clean.R")
underreporting <- reportDataFinal; rm(reportDataFinal)
#save(underreporting, file="data/underreporting.rda")
usethis::use_data(underreporting, overwrite = TRUE)




## make travel_restrictions data (THIS DOESN"T WORK CURRENTLY)
travel_restrictions <- data.frame(loc=unique((input_data %>%
                           filter(dep_country=="CHN"))$source),
           min=param_list$hubei_shutdown[1],
           max=param_list$hubei_shutdown[2],
           # Reduce travel from all Chinese sources to 10%
           p_travel=.1) %>%
    filter(loc!="Hubei") %>%
    bind_rows(data.frame(loc="Hubei",
                         min=param_list$hubei_shutdown[1],
                         max=param_list$hubei_shutdown[2],
                         # Reduce travel from Hubei to 0
                         p_travel=0)) %>%
    bind_rows(data.frame(loc=unique((input_data %>%
                                         filter(dep_country=="USA"))$source),
                         min="2020-03-02",
                         max="2020-03-08",
                         # Reduce travel from all US sources to 50%
                         p_travel=.6)) %>%
    bind_rows(data.frame(loc=unique((input_data %>%
                                         filter(dep_country!="CHN" & dep_country!="USA"))$source),
                         min="2020-03-02",
                         max="2020-03-08",
                         # Reduce travel from non-China to US to 30%
                         p_travel=.3)) %>%
    bind_rows(data.frame(loc=unique((input_data %>%
                                         filter(dep_country=="USA"))$source),
                         min="2020-03-09",
                         # Reduce travel from all US sources to 30%
                         max=param_list$hubei_shutdown[2], p_travel=.3)) %>%
    bind_rows(data.frame(loc=unique((input_data %>%
                                         filter(dep_country!="CHN" & dep_country!="USA"))$source),
                         min="2020-03-09",
                         max=param_list$hubei_shutdown[2],
                         # Reduce travel from all US sources to 10%
                         p_travel=.1)) %>%
    bind_rows(data.frame(loc=unique((input_data %>% filter(dep_country!="CHN" & dep_country!="USA"))$source),
                         min="2020-03-16",
                         max=param_list$hubei_shutdown[2],
                         # Reduce travel from all US sources to 20%
                         p_travel=.2))
#usethis::use_data(travel_restrictions, overwrite = TRUE)




## make airport_attributions data
airport_attribution <- read_csv("data-raw/airport_attribution.csv") %>%
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
    # Attribute travel according to normalizaed attribution score,
    #  weighted by population
    left_join(read_csv("data-raw/pop_data.csv") %>% dplyr::select(source, pop),
              by=c("Province"="source")) %>%
    group_by(airport_iata) %>%
    mutate(attribution = attribution*pop / sum(attribution*pop)) %>%
    ungroup()
#save(airport_attribution, file="data/airport_attribution.rda")
usethis::use_data(airport_attribution, overwrite = TRUE)




## Generate combined JHU CSSE data for packages (so users dont have to create the full data)
update_jhucsse_package_data <- function(){
    
    # pull the data from github
    pull_JHUCSSE_github_data(case_data_dir = "data/case_data")
    # read and merge data
    jhucsse_case_data <- read_JHUCSSE_cases(last_time=Sys.Date(), 
                                       append_wiki=TRUE, 
                                       case_data_dir = "data/case_data", 
                                       print_file_path=FALSE) 
    
    #save(jhucsse_case_data, file="data/jhucsse_case_data.rda")
    usethis::use_data(jhucsse_case_data, overwrite = TRUE)
}
update_jhucsse_package_data()




wikipedia_cases <- readr::read_csv("data-raw/WikipediaWuhanPre1-20-2020.csv",
                        col_types=readr::cols(Update = readr::col_datetime("%m/%d/%Y")))
usethis::use_data(wikipedia_cases, overwrite = TRUE)



