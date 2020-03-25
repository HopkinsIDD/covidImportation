
## make underreporting data
source("https://raw.githubusercontent.com/salauer/CFR_calculation/master/global_estimates/scripts/main_script_clean.R")
underreporting <- reportDataFinal
save(underreporting, file="data/underreporting.rda")

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

## make airport_attributions data
airport_attribution <- read_csv("data_raw/airport_attribution.csv") %>%
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
    left_join(read_csv("data_raw/pop_data.csv") %>% dplyr::select(source, pop),
              by=c("Province"="source")) %>%
    group_by(airport_iata) %>%
    mutate(attribution = attribution*pop / sum(attribution*pop)) %>%
    ungroup()
save(airport_attribution, file="data/airport_attribution.rda")
