
# SETUP POPULATION DATA - FROM WORLDPOP

if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)


# CHINA PROVINCE DATA -----------------------------------------------------

china_pop_data <- read_csv("data/China_popsize_census_2010.csv")
china_pop_data <- china_pop_data %>% rename(loc = province, population=pop)
china_pop_data <- china_pop_data %>% mutate(country = "China") %>% rename(pop = population)
write_csv(china_pop_data, "data/china_province_pop_data.csv")


# COUNTRY DATA -----------------------------------------------------------

country_pop_url <- "https://pkgstore.datahub.io/JohnSnowLabs/population-figures-by-country/population-figures-by-country-csv_csv/data/630580e802a621887384f99527b68f59/population-figures-by-country-csv_csv.csv"
country_pop_data <- readr::read_csv(country_pop_url)
country_pop_data <- country_pop_data[, c(1:2, ncol(country_pop_data))] #take only the most recent year
country_pop_data <- country_pop_data %>% rename(pop = colnames(country_pop_data)[3]) %>% mutate(pop = as.integer(pop))
country_pop_data <- country_pop_data %>% rename(country = Country, country_code=Country_Code)


# MERGE COUNTRY AND CHINA PROVINCE ----------------------------------------

pop_data <- full_join(country_pop_data, china_pop_data)

write_csv(pop_data, "data/pop_data.csv")




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
