##' Get list of chinese airports from CDC data
##' 
##'


library(tidyverse)





#Get list of chinese airports from CDC data
cdc_cn_us_travel <- read_csv("data/CDC_travel_data/OAG PAX CN to US with Point Of Sale Jul 2019-Nov 2019 ID analysis.csv")

cn_airport_list <- cdc_cn_us_travel[,1:5]  %>% filter(!duplicated(`Dep Airport Code`))

#Save it 
write_csv(cn_airport_list, "data/china_airports_intravel.csv")


