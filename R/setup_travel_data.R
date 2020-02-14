
# TRAVEL DATA 

library(tidyverse) 
library(lubridate)

# CHINA DATA --------------------------------------------------------------
# ** These are currently fake data**

shenzhen_travel_data <- read_csv("data/Shenzhen_travel.csv")
shenzhen_travel_data <- shenzhen_travel_data %>% gather(key="t", value="travelers", -loc) %>%
    mutate(t=lubridate::mdy(t)) %>% rename(source = loc) %>% mutate(destination="Shenzhen") %>%
    select(destination, source, t, travelers)

#write_csv(shenzhen_travel_data, "data/shenzhen_travel_data.csv")






# CHINA DATA - BAIDU --------------------------------------------------------------
# ** These come from Baidu**

shenzhen_travel_data <- read_csv("data/shenzhen_data/baidu_travel_data.csv")
shenzhen_travel_data <- shenzhen_travel_data %>% mutate(travelers = round(travelers)) 
unique(shenzhen_travel_data$t)




# BAIDU DATA - other ------------------------------------------------------

travel_data <- read_csv("data/shenzhen_data/flow.csv") %>% as.data.frame()
unique(travel_data$t)



# MERGE -------------------------------------------------------------------

shenzhen_travel_data <- full_join(travel_data, shenzhen_travel_data, by = c("t", "source", "travelers", "destination"))

write_csv(shenzhen_travel_data, "data/shenzhen_travel_data.csv")
