
# TRAVEL DATA 

if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('lubridate')) install.packages('lubridate'); library(lubridate)

# CHINA DATA --------------------------------------------------------------

shenzhen_travel_data <- read_csv("data/Shenzhen_travel.csv")
shenzhen_travel_data <- shenzhen_travel_data %>% gather(key="t", value="travelers", -loc) %>%
    mutate(t=lubridate::mdy(t)) %>% rename(source = loc) %>% mutate(destination="Shenzhen") %>%
    select(destination, source, t, travelers)

write_csv(shenzhen_travel_data, "data/shenzhen_travel_data.csv")




