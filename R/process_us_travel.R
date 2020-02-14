# Build US travel Array



# SETUP ------------------------------------------------------------------


library(tidyverse)

data_dir <- file.path("data", "CDC_travel_data")

list.files(data_dir)

source("source/importation_source.R")

# DATA --------------------------------------------------------------------

# other countries

dat1 <- readxl::read_xlsx(file.path(data_dir,"PriorityCountries_JobId663034.xlsx"), 
                          sheet="Results", skip=7, col_names = TRUE)
dat1
colnames(dat1) <- c("source", "airport","airport_name","city","state_name", "travelers","month_yr")                          
dat1 <- dat1 %>% mutate(state = state.abb[match(tolower(state_name), tolower(state.name))])
dat1 <- dat1 %>% mutate(state = ifelse(state_name=="District Of Columbia", "DC", state))
# missing values
unique(dat1$state_name[is.na(dat1$state)])
dat1 %>% filter(is.na(state))

dat1 <- dat1 %>% filter(!is.na(airport)) # remove blanks

# month_yr
dat1 <- dat1 %>%  
    mutate(month_yr = paste0(month.abb[as.integer(substr(month_yr, 5,6))] , "-", substr(month_yr, 1,4)))







# Europe data

dat_eur <- readxl::read_xlsx(file.path(data_dir,"Europe to the US by Country of Origin - State and Month from Jan-2005 to Aug-2016.xlsx"),
                             skip=6, col_names = F)

row_ind <- paste0(dat_eur[1,], " - ", dat_eur[2,])
names(row_ind) <- colnames(dat_eur)
dat_eur <- bind_rows(row_ind, dat_eur)
colnames(dat_eur) <- dat_eur[1,]
dat_eur <- dat_eur[-(1:4),]
colnames(dat_eur)[1] <- "destination"
dat_eur <- dat_eur %>%  
    tidyr::pivot_longer(cols = "Austria - Jan 2005":(colnames(dat_eur)[ncol(dat_eur)]),
                        names_to = c("source", "month_yr"), 
                        names_pattern = "(.*) - (.*)",
                        values_to = "travelers", 
                        values_drop_na=TRUE)
dat_eur <- dat_eur %>% filter(!is.na(destination))
dat_eur <- dat_eur %>% filter(destination != "TOTAL")
# dat_eur <- dat_eur %>% tidyr::extract(col=destination, 
#                                       into=c("airport", "city", "state", "country"), 
#                                       regex="([[:alnum:]]+) : ([[:alnum:]]+), ([[:alnum:]]+), ([[:alnum:]]+)", remove=FALSE)
dat_eur <- dat_eur %>% tidyr::separate(col=destination, 
                                         into=c("airport", "city", "state", "country"), 
                                         sep="([\\:\\,])", remove=FALSE)
dat_eur <- dat_eur %>% mutate(airport = trimws(airport),
                                city = trimws(city),
                                state = trimws(state),
                                country=trimws(country))




dat_eur2 <- readxl::read_xls(file.path(data_dir,"Europe Selected.xls"),
                             sheet="FMg Dynamic Table Report", skip=4, col_names = F)

colnames(dat_eur2) <- dat_eur2[1,]
dat_eur2 <- dat_eur2[-(1:2),]
colnames(dat_eur2)[1:2] <- c("destination", "month_yr")
dat_eur2 <- dat_eur2 %>%  
    mutate(month_yr = as.Date(as.numeric(month_yr), origin = "1900-01-01")) %>%
    mutate(month_yr = paste0(lubridate::month(month_yr, label=TRUE), "-", lubridate::year(month_yr)))
dat_eur2 <- dat_eur2 %>%  
    tidyr::pivot_longer(cols = "Austria":(colnames(dat_eur2)[ncol(dat_eur2)]),
                        names_to = "source", 
                        values_to = "travelers", 
                        values_drop_na=TRUE)
dat_eur2 <- dat_eur2 %>% filter(!is.na(destination))
dat_eur2 <- dat_eur2 %>% filter(destination != "TOTAL")

# dat_eur2 <- dat_eur2 %>% tidyr::extract(col=destination, 
#                                       into=c("airport", "city", "state", "country"), 
#                                       regex="([[:alnum:]]+) : ([[:alnum:]]+), ([[:alnum:]]+), ([[:alnum:]]+)", remove=FALSE)
dat_eur2 <- dat_eur2 %>% tidyr::separate(col=destination, 
                                        into=c("airport", "city", "state", "country"), 
                                        sep="([\\:\\,])", remove=FALSE)
View(dat_eur2 %>% filter(is.na(airport)))

dat_eur2 <- dat_eur2 %>% mutate(airport = trimws(airport),
                                city = trimws(city),
                                state = trimws(state),
                                country=trimws(country))


# MERGE DATA --------------------------------------------------------------

data_eur_all <- full_join(dat_eur, dat_eur2)

data_all <- full_join(data_eur_all %>% mutate(travelers = as.numeric(travelers)), 
                      dat1 %>% mutate(travelers = as.numeric(travelers)))
data_all <- data_all %>% mutate(month_yr = gsub(" ", "-", month_yr)) %>%
    mutate(month_yr = gsub("_", "-", month_yr))
data_all <- set_monthyr_factor(data_all)
data_all$country <- "USA"
data_all <- data_all %>% mutate(year = as.integer(substr(month_yr, 5,8)),
                                month = factor(substr(month_yr, 1,3), levels=month.abb, labels = month.abb))
data_all <- data_all %>% filter(source != "TOTAL")

unique(data_all$source)
unique(data_all$year)




# SAVE IT -----------------------------------------------------------------

write_csv(data_all, "data/CDC_travel_data/US-global_travel_2006-2016.csv")
