
# SETUP POPULATION DATA - FROM WORLDPOP

library(tidyverse)


# CHINA PROVINCE DATA -----------------------------------------------------

china_pop_data <- read_csv("data/China_popsize_census_2010.csv")
china_pop_data <- china_pop_data %>% rename(loc = province, population=pop)
write_csv(china_pop_data, "data/china_province_pop_data.csv")



# WORLDPOP DATA -----------------------------------------------------------



# Loading World Pop data
# {load("../Data/WorldPop_longform_1960.to.2017.RData")
#     WorldPop <- WorldPop %>% mutate(Country.Code = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
#         filter(!is.na(Country.Code))
#     
#     # if (!any(WorldPop$Year == 2016)) WorldPop <- WorldPop %>% union(filter(WorldPop, Year == 2015) %>% mutate(Year = 2016))     # Assuming 2015 population for 2016
#     # Extrapolating for pop of 2017
#     for (this.country in unique(WorldPop$Country)) {
#         # print(this.country)
#         
#         Fit <- lm(Population ~ Year, data = filter(WorldPop, Country == this.country, Year >= 2008))
#         
#         Correlate.vars <- data.frame(Country = this.country,
#                                      Country.Code = countrycode(this.country, origin = "country.name", destination = "iso3c"),
#                                      Year = 2018:2019)
#         
#         Preds <- predict(Fit, newdata = Correlate.vars)
#         
#         New.Rows <- Correlate.vars %>% mutate(Population = round(Preds))
#         
#         WorldPop <- WorldPop %>% bind_rows(New.Rows)
#     }
#     Years_pop <- sort(unique(WorldPop$Year))
#     Countries_pop <- str_replace_all(WorldPop$Country, " ", ".") %>% unique
#     Country.Codes_pop <- WorldPop$Country.Code %>% unique}
