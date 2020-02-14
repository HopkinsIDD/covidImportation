
# TRAVEL DATA 

if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('lubridate')) install.packages('lubridate'); library(lubridate)
if(!require('brms')) install.packages('brms'); library(brms)

# USA-CHINA DATA --------------------------------------------------------------

usa_WUH_travel_data <- read_csv("data/CDC_travel_data/OAG PAX WUH to US with Point Of Sale Oct 2018-Nov 2019 ID analysis.csv")
usa_china_travel_data <- read_csv("data/CDC_travel_data/OAG PAX CN to US with Point Of Sale Jul 2019-Nov 2019 ID analysis.csv")


usa_WUH_travel_data_airports <- usa_WUH_travel_data %>% group_by(`Time Series`, `Dep City Name`, `Arr Airport Code`) %>%
    summarise(Pax=sum(Pax, na.rm = TRUE)) %>% rename(t=`Time Series`, source=`Dep City Name`, destination=`Arr Airport Code`) %>%
    mutate(year=substr(t, 1,4), month=substr(t,5,6)) %>% rename(Travelers=Pax)

usa_china_travel_data_airports <- usa_china_travel_data %>% group_by(`Time Series`, `Dep City Name`, `Arr Airport Code`) %>%
summarise(Pax=sum(Pax, na.rm = TRUE)) %>% rename(t=`Time Series`, source=`Dep City Name`, destination=`Arr Airport Code`) %>%
    mutate(year=substr(t, 1,4), month=substr(t,5,6)) %>% rename(Travelers=Pax)
    


# Need to complete the data so all options have a value (i.e., 0 values needed)
sort(unique(usa_WUH_travel_data_airports$t))
usa_WUH_travel_data_airports <- usa_WUH_travel_data_airports %>% as_tibble() %>%
    complete(t, source, destination, fill=list(Travelers=0)) %>% mutate(year=substr(t, 1,4), month=substr(t,5,6))

usa_china_travel_data_airports <- usa_china_travel_data_airports %>% as_tibble() %>%
    complete(t, source, destination, fill=list(Travelers=0)) %>% mutate(year=substr(t, 1,4), month=substr(t,5,6))




# VISUALIZE THE TRAVEL DATA -----------------------------------------------

mean_vol <- usa_WUH_travel_data_airports %>% group_by(source, destination) %>% summarize(Travelers = mean(Travelers))
top_dests <- (mean_vol %>% arrange(desc(Travelers), source, destination))[1:10,]
vol_top_dests <- usa_WUH_travel_data_airports %>% filter(destination %in% top_dests$destination)

ggplot(vol_top_dests, aes(as.factor(t), Travelers, group=destination, color=destination)) + 
    geom_line() 

usa_china_travel_data_airports <- usa_china_travel_data_airports %>% mutate(route = paste0(source, "-", destination))
mean_vol <- usa_china_travel_data_airports %>% group_by(route) %>% summarize(Travelers = mean(Travelers)) 
top_dests <- (mean_vol %>% arrange(desc(Travelers), route))[1:50,]
vol_top_routes <- usa_china_travel_data_airports %>% filter(route %in% top_dests$route)

ggplot(vol_top_routes, aes(as.factor(t), Travelers, group=route, color=route)) + 
    geom_line() 
    
# plot by source and dest separately -- to get trends




# TRAVEL PROJECTIONS ------------------------------------------------------

# Just Wuhan travel
if(!require('parglm')) install.packages('parglm'); library(parglm)  # parallel implementation of glm to speed up

# Restricting Prediction to 2010-2016 & Simplified
t_pois_wuhan <- parglm(Travelers ~ as.factor(month) + destination, 
                  family = poisson(link=log), data = usa_china_travel_data_airports,
                  control = parglm.control(nthreads = 4L))
summary(t_pois_wuhan)


t_pois_wuhan_multilev_stan <- brms::make_stancode(Travelers ~ as.factor(month) + (1|destination), 
                             family = poisson(link=log), data = usa_china_travel_data_airports,
                             chains = 4, cores = 4, iter = 200, warmup = 100)

t_pois_wuhan_multilev <- brms::brm(Travelers ~ as.factor(month) + (1|destination), 
                       family = poisson(link=log), data = usa_china_travel_data_airports,
                       chains = 4, cores = 4, iter = 200, warmup = 100)
summary(t_pois_wuhan_multilev)
marginal_effects(t_pois_wuhan_multilev)








t_pois_china <- parglm(Travelers ~ as.factor(month) + 
                        destination + as.factor(month)*destination +
                        source + as.factor(month)*source, 
                    family = poisson(link=log), data = usa_WUH_travel_data_airports,
                    control = parglm.control(nthreads = 4L))
summary(t_pois_china)




T.pois.reg <- glm(Travelers ~ Month + Year + Month*Year + Month*Year*TRAVEL, 
                  family = poisson(link=log), data = T_nostate %>% filter(Year>=2010, Country=='Japan'))
summary(T.pois.reg)

preds <- predict.glm(T.pois.reg, newdata=T_nostate %>% filter(Country=='Japan'), type='response', se.fit=TRUE)
tmp <- T_nostate %>% filter(Country=='Japan')
tmp$Predicted <- round(preds$fit)
View(tmp)






















write_csv(usa_china_travel_data, "data/usa_china_travel_data.csv")




