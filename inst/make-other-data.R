source("https://raw.githubusercontent.com/salauer/CFR_calculation/master/global_estimates/scripts/main_script_clean.R")
underreporting <- reportDataFinal
save(underreporting, file="data/underreporting.rda")

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
