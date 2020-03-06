
##' Plot Confirmed and estimated Cases of COVID-19
##' 
##' Shaun Truelove
##' shauntruelove@jhu.edu
##' 


## Plot current confirmed cases in China
plot_conf_china <- function(df = jhucsse){
    
    conf_cases_ch <- df %>% 
    filter(Country_Region == "Mainland China" | Province_State %in% c("Hong Kong", "Macau", "Taiwan")) %>%
    mutate(t = as.Date(Update)) %>% arrange(Province_State, Country_Region, Update) %>%
    group_by(Country_Region, Province_State) %>% mutate(Incidence = diff(c(0, Confirmed), na.rm=TRUE)) %>% ungroup() %>%
    group_by(Country_Region, Province_State, t) %>% summarise(Incidence = sum(Incidence, na.rm = TRUE))

    conf_cases_ch <- conf_cases_ch %>% filter(t >= as.Date("2020-01-01"))
    t_values <- as.character(sort(conf_cases_ch$t))
    
    p_china <- ggplot(data=conf_cases_ch, aes(x=as.Date(t), y=Incidence, fill=Province_State)) + 
        geom_bar(position="stack", stat="identity", color="black") +
        ylab("Confirmed nCoV cases (n)") +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(), panel.background=element_blank(),
              axis.text.y = element_text(size=9), 
              axis.text.x = element_text(size=9, angle=45, hjust = 1),
              axis.title.x = element_blank(), legend.title=element_blank(), 
              legend.text=element_text(size=8), legend.key.size=unit(7, "pt"),
              legend.background = element_blank(), legend.box.background = element_blank(),
              plot.title = element_text(size=8, face="bold", hjust = 0.025),
              plot.margin = unit(c(0.5,.25,0.25,0.25), "cm"),
              legend.position = c(0.2,.55)) +
        guides(fill=guide_legend(ncol=1))
    plot(p_china)
}




## Plot current confirmed cases Outside of China
plot_conf_notchina <- function(df = jhucsse){
        
    conf_cases_global <- df %>% 
        filter(Country_Region != "Mainland China" & !(Province_State %in% c("Hong Kong", "Macau", "Taiwan"))) %>%
        mutate(t = as.Date(Update)) %>% arrange(Province_State, Country_Region, Update) %>%
        group_by(Country_Region, Province_State) %>% mutate(Incidence = diff(c(0, Confirmed), na.rm=TRUE)) %>% ungroup() %>%
        group_by(Country_Region, Province_State, t) %>% summarise(Incidence = sum(Incidence, na.rm = TRUE))
    
    conf_cases_global <- conf_cases_global %>% filter(t >= as.Date("2020-01-01"))
    t_values <- as.character(sort(conf_cases_global$t))
    
    p_global <- ggplot(data=conf_cases_global, aes(x=as.Date(t), y=Incidence, fill=Province_State)) + 
        geom_bar(position="stack", stat="identity", color="black") +
        ylab("Confirmed nCoV cases (n)") +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(), panel.background=element_blank(),
              axis.text.y = element_text(size=9), 
              axis.text.x = element_text(size=9, angle=45, hjust = 1),
              axis.title.x = element_blank(), legend.title=element_blank(), 
              legend.text=element_text(size=5.5), legend.key.size=unit(4, "pt"),
              legend.background = element_blank(), legend.box.background = element_blank(),
              plot.title = element_text(size=8, face="bold", hjust = 0.025),
              plot.margin = unit(c(0.5,.25,0.25,0.25), "cm"),
              legend.position = c(0.3,.55)) +
        guides(fill=guide_legend(ncol=3))
    plot(p_global)
}








## Plot current confirmed outbreaks Outside of China
plot_conf_largestnotchina <- function(df = jhucsse, scale="free_y"){
    
    conf_cases_global <- df %>% 
        filter(Country_Region != "Mainland China" & !(Province_State %in% c("Hong Kong", "Macau", "Taiwan"))) %>%
        mutate(t = as.Date(Update)) %>% arrange(Province_State, Country_Region, Update) %>%
        group_by(Country_Region, Province_State) %>% mutate(Incidence = diff(c(0, Confirmed), na.rm=TRUE)) %>% ungroup() %>%
        group_by(Country_Region, Province_State, t) %>% summarise(Incidence = sum(Incidence, na.rm = TRUE))
    
    conf_cases_global <- conf_cases_global %>% filter(t >= as.Date("2020-01-01"))
    t_values <- as.character(sort(conf_cases_global$t))
    conf_cases_global_cum <- conf_cases_global %>% group_by(Province_State) %>% 
        summarise(cum_cases = sum(Incidence)) %>% filter(cum_cases>=100)
    
    p_big <- ggplot(data=conf_cases_global %>% filter(Province_State %in% conf_cases_global_cum$Province_State), 
                    aes(x=as.Date(t), y=Incidence, fill=Province_State)) + 
        geom_bar(position="stack", stat="identity", color="black") +
        ylab("Confirmed nCoV cases (n)") +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.border = element_blank(), panel.background=element_blank(),
              axis.text.y = element_text(size=9), 
              axis.text.x = element_text(size=9, angle=45, hjust = 1),
              axis.title.x = element_blank(), legend.title=element_blank(), 
              plot.title = element_text(size=8, face="bold", hjust = 0.025),
              plot.margin = unit(c(0.5,.25,0.25,0.25), "cm"),
              legend.position = "none") +
        facet_wrap(vars(Province_State), ncol=2, scales = scale)
    plot(p_big)
}








# 
# # travel_data <- travel_data %>% mutate(t = lubridate::ymd(t))  %>% as.data.frame()
# 
# # Plot Travelers
# t_ <- unique(travel_data$t)
# source_levels <- c("Guangdong", unique((travel_data %>% filter(source!="Guangdong"))$source))
# 
# all_travel <- ggplot(travel_data %>% mutate(source = factor(source, levels=source_levels)),
#                      aes(t, travelers, group=source, fill=source)) +
#     guides(fill=guide_legend(title = element_blank(), ncol=1)) +
#     geom_area(color='black') +
#     theme_classic() +
#     theme(axis.line = element_line(colour = "black"),
#           axis.text.x = element_text(angle=45, hjust=1),
#           axis.title.x = element_blank(), legend.title=element_blank(),
#           legend.margin=margin(0,0,0,0), legend.box.margin=margin(0,0,0,0),
#           legend.text=element_text(size=7), legend.key.size=unit(6, "pt"),
#           legend.background = element_blank(), legend.box.background = element_blank(),
#           plot.title = element_text(size=8, face="bold", hjust = 0.025),
#           plot.margin = unit(c(0.25,.25,0.25,0.25), "cm"))
# 
# # Hubei only
# hubei_travel <- ggplot(travel_data %>% filter(source=="Hubei"),
#                        aes(t, travelers, group=source, fill=source)) +
#     guides(fill=guide_legend(title = element_blank(), ncol=1)) +
#     theme_classic() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1),
#           legend.text=element_text(size=7), legend.key.size=unit(6, "pt"),
#           legend.margin=margin(0,15,0,5), legend.box.margin=margin(0,0,0,0),
#           plot.margin = unit(c(0.25,.25,0.25,0.25), "cm")) +
#     geom_area(color='black') + viridis::scale_fill_viridis(discrete = T) +
#     xlab(NULL)
# 
# p1 <- ggplot_gtable(ggplot_build(all_travel))
# p2 <- ggplot_gtable(ggplot_build(hubei_travel))
# maxWidth = grid::unit.pmax(p1$widths[2:5], p2$widths[2:5])
# p1$widths[2:5] <- as.list(maxWidth)
# p2$widths[2:5] <- as.list(maxWidth)
# gridExtra::grid.arrange(p1, p2, nrow=2, heights=c(2.5,1))
