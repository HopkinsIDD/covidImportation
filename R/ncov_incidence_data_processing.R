##' Import, Merge, and Format incidence data
##' - Data are used in plotting curves and predicting importations



# SETUP -------------------------------------------------------------------

if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('lubridate')) install.packages('lubridate'); library(lubridate)
#if(!require('gsheet')) install.packages('gsheet'); library(gsheet)

# OLD VERSION
#ncov_timeseries_url <- "https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/htmlview?usp=sharing&sle=true"
# NEW VERSION
ncov_timeseries_url <- "https://raw.githubusercontent.com/CSSEGISandData/2019-nCoV/master/time_series/time_series_2019-ncov-Confirmed.csv"




# CHINA DATA --------------------------------------------------------------

# data provided from china
china_incid_data <- read_csv("data/nCoV_china_confirmed_suspected.csv") %>% filter(Status=="Confirmed")
china_incid_data[is.na(china_incid_data)] <- 0
china_incid_data <- china_incid_data %>% mutate(`12/30/2019`=0) %>% rename(`12/31/2019`=`12/31/2020`)




china_incid_data <- china_incid_data %>% select(-Status) %>%
    #group_by(Province) %>% select(-Status) %>% summarize_all(sum, na.rm=TRUE) %>% # add suspected and confirmed
    gather(key="t", value="cases", -Province) %>%
    mutate(t=lubridate::mdy(t)) %>% rename(loc=Province) %>% 
    arrange(loc, t)
china_incid_data <- china_incid_data %>% rename(cases_cum = cases) %>% mutate(cases_incid=0) %>%
    group_by(loc) %>% mutate(cases_incid = c(0,diff(cases_cum))) %>% ungroup()

#unique(china_incid_data$loc)

     
#write_csv(china_incid_data, "data/china_incid_data_report.csv")





# LAUREN'S DATA -----------------------------------------------------------

library(tidyverse)

# Read and use data from Lauren Gardners website
#conf_cases <- gsheet::gsheet2tbl(ncov_timeseries_url) # OLD VERSION 
conf_cases <- readr::read_csv(url(ncov_timeseries_url))
case_loc_data <- conf_cases[,1:4]
conf_cases <- conf_cases %>% gather(key="t", value="cum_cases",-`Province/State`,-`Country/Region`,-Lat,-Long)#, -`First confirmed date in country (Est.)`)
conf_cases <- conf_cases %>% mutate(cum_cases = ifelse(is.na(cum_cases), 0, cum_cases))

# Sum days with multiple
conf_cases <- conf_cases %>% mutate(date = as.Date(lubridate::mdy_hm(t))) %>% 
    rename(prov_state=`Province/State`, country=`Country/Region`) %>%
    arrange(country, prov_state, date) %>%
    group_by(prov_state, country, date) %>%
    summarize(cum_cases = max(cum_cases))
conf_cases_first <- conf_cases %>% group_by(prov_state, country) %>% 
    summarize(date = min(date)-1) %>% mutate(cum_cases = 0)
conf_cases <- conf_cases %>% bind_rows(conf_cases_first) %>% arrange(prov_state, country, date)

conf_cases <- conf_cases %>% mutate(cases_incid=0) %>% group_by(prov_state, country) %>% 
    mutate(cases_incid = c(0,diff(cum_cases)))
conf_cases <- conf_cases %>% mutate(epiweek = lubridate::epiweek(date)) 
conf_cases <- conf_cases %>% mutate(China_source=(grepl("china", tolower(country)) | grepl("hong kong", tolower(country)) | 
                                                      grepl("macau", tolower(country))) | grepl("taiwan", tolower(country)))
conf_cases <- conf_cases %>% rename(t = date)
#conf_cases <- conf_cases %>% complete(date, epiweek, prov_state, country)

write_csv(conf_cases, "data/china_incid_data_report.csv")



# Get total data

conf_cases_total <- conf_cases %>% filter(China_source==TRUE) %>% group_by(t) %>%
    summarize(cases_incid = sum(cases_incid, na.rm = TRUE)) %>% 
    mutate(cases_cum = cumsum(cases_incid))

write_csv(conf_cases_total, "data/china_incid_total.csv")

plot_current_totals <- function(){
    library(ggplot2)
    conf_cases_total <- readr::read_csv("data/china_incid_total.csv", col_types = cols())
    ggplot(conf_cases_total, aes(t, cases_incid)) + geom_bar(stat="identity") + 
        ylab("Incident Cases (no.)")
}
#plot_current_totals()

    




#Load in the JHU CSSE Data
# jhucsse <- read_JHUCSSE_cases("2020-01-28 23:59", 
#                               append_wiki = TRUE)

##Continue to filter to China for the moment.
##also total the suspected and confirmed cases.
jhucsse_china <- jhucsse %>% 
    filter(Country_Region%in%c("Mainland China", "Macau", "Hong Kong")) 




## Fit a monotinically increasing spline to each area with
## at least 25 cases at any ppint
tmp <- jhucsse_china%>%filter(Confirmed>=25)
tmp <- unique(tmp$Province_State)

## Look at consitencey in exponential groqth by areas.
analyze <-   jhucsse_china %>% drop_na(Confirmed) %>% 
    filter(Province_State%in%tmp)


##Get the implied daily incidence for each province
##by fitting a monitonically increasing spline and then 
##taking the difference (a little less sensitive to 
##perturbations in reporting than taking raw difference).
##Making sure only to infer over trhe suport
tmp_dt_seq <- seq(ISOdate(2019,12,1), ISOdate(2020,1,28), "days")
incidence_data<- analyze %>% nest(-Province_State) %>%
    mutate(cs=map(data, ~splinefun(x=.$Update, y=.$Confirmed,
                                   method="hyman"))) %>%
    mutate(Incidence=map2(cs,data, ~data.frame(Date=tmp_dt_seq[tmp_dt_seq>=min(.y$Update)], 
                                               Incidence= diff(c(0, pmax(0,.x(tmp_dt_seq[tmp_dt_seq>=min(.y$Update)]))))))) %>%
    unnest(Incidence) %>% select(-data) %>% select(-cs) 

inc_plt <- incidence_data%>%filter(Date>"2020-1-1") %>% 
    ggplot(aes(x=Date,   y=Incidence, fill=Province_State)) +
    geom_bar(stat="identity", position="stack") +
    theme_bw()+theme(legend.position="bottom")


inc_plt


## Now let's look at Hubei a little bit to see how we feel about
##this approach. Compare with just looking at raw differences.

jhucsse_hubei <-jhucsse %>% 
    filter(Province_State=="Hubei") %>% 
    filter(Update>="2020-01-22")

compare_points <- data.frame(Date=sort(jhucsse_hubei$Update[-1]),
                             Incidence=diff(sort(jhucsse_hubei$Confirmed)))

incidence_data%>%filter(Date>"2020-1-1") %>% 
    filter(Province_State=="Hubei") %>% 
    ggplot(aes(x=Date,   y=Incidence))+
    geom_bar(stat="identity", position="stack") +
    theme_bw() + 
    geom_point(data=compare_points,
               x=round(compare_points$Date,"day")+
                   as.difftime(12,unit="hours"), 
               y=compare_points$Incidence, color="green")









# LINELIST DATA -----------------------------------------------------------
# ** this is important to use for validation of importations

linelist_url <- "https://docs.google.com/spreadsheets/d/1itaohdPiAeniCXNlntNztZ_oRvjh0HsGuJXUJWET008/htmlview#"
ll_data <- gsheet::gsheet2tbl(linelist_url); rm(linelist_url)
colnames(ll_data)
if (!("linelist_current.csv" %in% list.files("data"))){ # save first backup
    write_csv(ll_data, "data/linelist_backup.csv"); rm(ll_data_backup)
} else {
    ll_data_backup <- read_csv("data/linelist_current.csv") # load and resave a backup. Just in case the googlesheet gets screwed up
    write_csv(ll_data_backup, "data/linelist_backup.csv")
    rm(ll_data_backup)
}

# Format Dates
date_origs <- ll_data %>% as_tibble() %>% dplyr::select_at(vars(matches("date"))) %>% rename_all(funs(paste0(., "_orig")))
ll_data <- ll_data %>% mutate_at(vars(contains("date")), dmy)
ll_data <- ll_data %>% bind_cols(date_origs)
#mutate(date_onset_symptoms = dmy(date_onset_symptoms))


write_csv(ll_data, "data/linelist_current.csv") # save current linelist


# Filter to just those when any mention of shenzhen
shen_rows <- apply(ll_data, 1, FUN=function(x) sum(grepl("shenzhen", x, ignore.case = TRUE)))>0
ll_data_shenzhen <- ll_data[shen_rows, ]

# get aggragated data
shen_data_aggr <- ll_data_shenzhen %>% count(date_confirmation)

# # Plot the epi curve of these
# ggplot(shen_data_aggr, aes(x=date_confirmation, y=n)) +
#     geom_bar(stat="identity")






# SHENZHEN CASE COUNTS ----------------------------------------------------
# ** these data are currently collected from the Shenzhen CDC reports
# go to https://docs.google.com/spreadsheets/d/1iL0TP00i8U5fF6_JiO7Hg_ysx3GsAtaMDC2_MghpTBw/edit?ts=5e3f3904#gid=0


# shen_cases <- gsheet::gsheet2tbl("https://docs.google.com/spreadsheets/d/1iL0TP00i8U5fF6_JiO7Hg_ysx3GsAtaMDC2_MghpTBw/edit?ts=5e3f3904#gid=0")
shen_cases <- read_csv("data/shenzhen_data/shenzhen_case_counts.csv")
shen_cases <- shen_cases %>% filter(!is.na(count)) %>% mutate(date = as.Date(date))
write_csv(shen_cases, "data/shenzhen_data/shenzhen_case_counts.csv")








# REMOVE EVERYTHING FROM THE CURRENT WORKSPACE
#rm(list = ls()); gc()
