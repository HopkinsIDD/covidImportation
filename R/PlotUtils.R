
##'
##' Function to plot the estimated and reported case counts
##'
##' @param conf_cases Confirmed case data from JHU CSSE
##' @param incid_ests Estimated incidence
##' @param locations  Locations to plot, can be a vector
##' @param ncol_facet number of columns in the facet plotting
##'
##' @return a corrected version of the inferred incidence data corrected for reporting changes in Hubei.
##'
##' @importFrom tibble as_tibble
##' @import dplyr ggplot2
##' 
##' @export
##' 
plot_incidence_ests_report <- function(conf_cases=jhucsse, 
                                       incid_ests=incidence_data,
                                       locations="All", 
                                       ncol_facet=2){
    
    if (locations=="All"){
        incid_ests <- incid_ests %>% 
            dplyr::mutate(Incidence = ceiling(Incidence))
    } else {
        incid_ests <- incid_ests %>% 
            dplyr::mutate(Incidence = ceiling(Incidence)) %>% 
            dplyr::filter(Province_State %in% locations)
        conf_cases <- conf_cases %>% 
            dplyr::filter(Province_State %in% locations)
    }
    incid_ests$Date <- as.Date(incid_ests$Date, "%m/%d/%Y", tz = "UTC")
    
    # Make conf_cases daily
    # Get daily calculated incidence (from reporting)
    conf_cases_daily <- conf_cases %>% 
        dplyr::filter(!is.na(Confirmed)) %>%
        dplyr::mutate(Date = as.Date(Update)) %>% 
        dplyr::group_by(Province_State, Date) %>% 
        dplyr::filter(Update == max(Update, na.rm=TRUE)) %>% 
        dplyr::ungroup()
    conf_cases_daily <- conf_cases_daily %>% 
        dplyr::group_by(Province_State) %>% 
        dplyr::arrange(Date) %>% 
        dplyr::mutate(Incidence = diff(c(0, Confirmed))) %>% 
        dplyr::ungroup()
    
    # Merge in reported incidence
    incid_data_ <- dplyr::left_join(incid_ests, 
                                    conf_cases_daily %>% 
                                        dplyr::rename(Incid_rep = Incidence), by=c("Province_State", "Date")) %>% tibble::as_tibble()
    
    if (locations=="All"){
        incid_data_ <- incid_data_ %>% 
            dplyr::group_by(Date) %>% 
            dplyr::summarize(Incidence=sum(Incidence, na.rm = TRUE), Incid_rep=sum(Incid_rep, na.rm = TRUE))
    }
    
    # Plot
    p <- ggplot(incid_data_, aes(x=Date)) +
        geom_bar(stat = "identity", aes(y=Incidence, fill="Estimated Incidence", group=1)) +
        geom_point(aes(x=Date, y=Incid_rep, color="Confirmed Cases")) +
        coord_cartesian(xlim=c(as.Date("2020-01-15"), max(as.Date(incid_data_$Date))),
                        ylim=c(0,max(incid_data_$Incidence*1.25))) +
        theme_classic() +
        scale_fill_manual(values="maroon", name=NULL, label="Estimated Incidence") +
        scale_color_manual(values="navyblue", name=NULL, label="Confirmed Cases") +
        theme(legend.position = c(0.125, .92),
              legend.spacing.y = unit(0, 'cm'),
              legend.key.size = unit(6, "pt"),
              legend.text=element_text(size=6))
    
    if (length(locations)>1){
        p <- p + facet_wrap(vars(Province_State), ncol=ncol_facet)
    }
    return(p)
}



##'
##' Facet plot of cumulative and incident cases at locations desired
##'
##' @param case_data Confirmed case data from JHU CSSE
##' @param state State abbreviation; NULL if not a state
##' @param county_name  Vector of counties of interest; NULL if not wanting counties
##' @param aggr_locs whether aggregated cases across specified states or counties is wanted
##' @param date_limits date boundaries desired for the plot
##' @param ncol_facet number of columns in the facet plotting
##'
##' @return a facet plot of confirmed and incident cases 
##'
##' @importFrom lubridate as_date
##' @import dplyr ggplot2
##' 
##' @export
##' 
plot_rep_cases <- function(case_data = jhucsse,
                         state = "NY", 
                         county_name = c("New York","Nassau"), 
                         aggr_locs = FALSE, 
                         date_limits = c("2020-01-15", "2020-03-15"),
                         ncol_facet=2){
    
    if (!is.null(state)){
        case_data <- case_data %>% 
            dplyr::filter(state_abb %in% state)
    } 
    if (!is.null(county_name)){
        case_data <- case_data %>% 
            dplyr::filter(Admin2 %in% county_name)
    }
    
    if (aggr_locs){
        case_data <- case_data %>% dplyr::group_by(Update) %>%
            dplyr::summarise(Confirmed = sum(Confirmed, na.rm = TRUE),
                             incidI = sum(incidI, na.rm = TRUE))
    }
    
    case_data <- case_data %>% 
        dplyr::mutate(Date = lubridate::as_date(Update))
    
    if (!is.null(date_limits)){
        case_data <- case_data %>% dplyr::filter(Date>=lubridate::as_date(date_limits[1]) & 
                                                 Date<=lubridate::as_date(date_limits[2]))
    }

    # Plot
    p <- ggplot(case_data, aes(x=Update)) +
        geom_bar(stat = "identity", aes(y=incidI, fill="Incident Cases", group=1)) +
        geom_line(aes(y=Confirmed, color="Confirmed Cases")) +
        #coord_cartesian(xlim=c(as.Date(date_limits[1]), max(as.Date(date_limits[2])))) +
        theme_classic() +
        scale_fill_manual(values="maroon", name=NULL, label="Estimated Incidence") +
        scale_color_manual(values="navyblue", name=NULL, label="Confirmed Cases") +
        #scale_y_continuous(trans="log", breaks = scales::pretty_breaks()) +
        ylab(NULL) +
        theme(legend.position = c(0.125, .7),
              legend.spacing.y = unit(0, 'cm'),
              legend.key.size = unit(10, "pt"),
              legend.text=element_text(size=10))
        
    if (length(state)>1 | length(county_name)>1){
        p <- p + facet_wrap(vars(state_abb, Admin2), ncol=ncol_facet)
    }
    p
    
    return(p)
}





##'
##' Barchart plot of importations stacked by source
##'
##' @param data data of importations to plot. Must have a columns c("t", "loc", "amount")
##' @param t_limits Date limits of plot
##' @param axes_text_size axis value font size (2 values required)
##' @param leg_text_size
##' @param leg_title_size
##' @param time_int time interval for x-axis
##' @param n_cols number of colors/locations to keep individually
##'
##' @return a corrected version of the inferred incidence data corrected for reporting changes in Hubei.
##'
##' @importFrom tibble as_tibble
##' @importFrom lubridate as_date
##' @import dplyr ggplot2
##' 
##' @export
##' 
plot_imports_stackeddest <- function(data=imports_mean %>% rename(amount = import_mean), 
                                     t_limits=lubridate::as_date(c("2020-01-01","2020-04-20")),
                                     axes_text_size = c(9,9),
                                     leg_text_size = 8, 
                                     leg_title_size = 10,
                                     time_int = 5,
                                     n_cols = 20){ 
    
    # Limit the time of the plots
    if (!is.na(t_limits[1])){
        data <- data %>% 
            dplyr::mutate(t = lubridate::as_date(t)) %>% 
            dplyr::filter(t >= lubridate::as_date(t_limits[1]) & lubridate::as_date(t) <= lubridate::as_date(t_limits[2])) %>%
            dplyr::mutate(t_num = as.integer(as.factor(t)))
    }
    t_values <- as.character(unique(sort(data$t)))
    
    data <- data %>% 
        dplyr::group_by(loc) %>%
        dplyr::mutate(loc_mean = mean(amount)) %>%  
        dplyr::ungroup() %>% 
        dplyr::mutate(loc = as.character(loc)) %>%
        dplyr::arrange(desc(loc_mean), loc, t) %>%
        dplyr::mutate(loc = as.character(loc)) 
    
    loc_grp <- unique(data$loc)[!(unique(data$loc) %in% unique(data$loc)[1:n_cols])]
    data <- data %>% 
        tibble::as_tibble() %>%
        dplyr::mutate(loc2 = ifelse(loc %in% loc_grp, "other", loc)
    )
    
    # N columns
    n_loc <- length(unique(data$loc2))
    n_columns <- ceiling(n_loc / 30)
    
    p <- ggplot(data=data, aes(x=t_num, y=amount, fill=loc2)) + 
        geom_bar(position="stack", stat="identity") +
        ylab("Mean est. imported nCoV cases (n)") +
        scale_x_continuous(breaks=seq(1,length(t_values),time_int),
                           labels=t_values[seq(1,length(t_values),time_int)]) +
        scale_linetype_manual(values=c("solid"), guide=guide_legend(title=element_blank())) +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(), panel.background=element_blank(),
              axis.text.y = element_text(size=axes_text_size[2]), 
              axis.text.x = element_text(size=axes_text_size[1], angle = 90, hjust = 1, vjust=.5, margin=margin(t=5)),
              axis.title.x = element_blank(),
              legend.title=element_text(size=leg_title_size), 
              legend.text=element_text(size=leg_text_size), # legend text size
              legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,10,-20,-20),
              legend.key.size=unit(8, "pt"),
              legend.key = element_blank(),
              legend.background = element_blank(), legend.box.background = element_blank(),
              plot.title = element_text(size=leg_title_size, face="bold", hjust = 0.025),
              plot.margin = unit(c(0.5,.25,0.25,0.25), "cm")) +
        guides(fill=guide_legend(title = element_blank(), ncol=n_columns))
    return(p)
} 





    