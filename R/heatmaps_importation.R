##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
##'  nCoV Importation Analyses -- Heatmaps of importations    ####
##'                 
##:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# SETUP -------------------------------------------------------------------

if (!exists("version") | is.list(version)){     version <- "1"                  }
if (!exists("batch") | is.list(batch)){         batch <- "1st"                    }
if (!exists("project_name")){                   project_name <- "california_import" }
#import_sim_file <- file.path("output",project_name, sprintf("nCoV_importation_sim_%s_batch_v%s.RData", batch, version))
results_dir <- file.path("figures", project_name)
dir.create(results_dir, recursive = TRUE)
options(scipen=999)


# SETUP PACKAGES AND dest -----------------------------------------------

source("R/ggheat_func_source.R")
#dest("dest/ISO_code_dest.R")
#library(globaltoolbox)
if(!require('tidyverse')) install.packages('tidyverse'); library(tidyverse)
if(!require('gridExtra')) install.packages('gridExtra'); library(gridExtra)


# LOAD SIMULATED DATA ---------------------------------------------------------------

# Summary data

# dest by time, single destination 
import_results_desttime <- read_csv(file.path("results",project_name,sprintf("import_results_desttime_v%s.csv", version)))
import_results_desttime$t <- as.Date(import_results_desttime$t)
t_values <- sort(unique(import_results_desttime$t))
import_results_desttime <- import_results_desttime %>% 
    mutate(t = factor(as.character(t))) %>% 
    mutate(t_num = as.integer(t))

coord_ratio <- length(unique(import_results_desttime$destination)) / (length(unique(import_results_desttime$t))) 
#print(coord_ratio)


# Limit the dates

# t_limits <- as.Date(c("2020-01-01","2020-01-27"))
if (!exists("t_limits")){ t_limits <- range(t_values) }



# PLOTS -------------------------------------------------------------------

# RR importATION HEATMAP
summary(import_results_desttime$RR_mean)
quantile(import_results_desttime$RR_mean, probs=seq(0,1,.1), na.rm=TRUE)

p_rr <- ggheat_import(data=import_results_desttime %>% select(t, destination, value=RR_mean) %>% 
                       rowwise %>% filter(as.Date(t) >= as.Date(t_limits[1]) & as.Date(t) <= as.Date(t_limits[2])),
                       zCuts=c(0,.01,.05,.1,.2,.5,.8,1.25,2,5,10,20,50,100, ceiling(max(import_results_desttime$RR_mean, na.rm = TRUE))),
                       x.size=8, y.size=8, 
                       title='Daily RR\nCase\nimported\ninto California', 
                       labCol='t', na.value="grey75",
                       aspect_ratio = coord_ratio, t.skip=3)

ggsave(file.path("figures",project_name,sprintf("heatmap_RR_importation_v%s.png", version)), plot=p_rr, width = 4, height = 8, dpi=600)


# PROB importATION HEATMAP
summary(import_results_desttime$prob_any_import)
quantile(import_results_desttime$prob_any_import, probs=seq(0,1,.1), na.rm=TRUE)

p_prob <- ggheat_import(data=import_results_desttime %>% select(t, destination, value=prob_any_import) %>%
                        rowwise %>% filter(as.Date(t) >= as.Date(t_limits[1]) & as.Date(t) <= as.Date(t_limits[2])), 
                        zCuts=seq(0,1,.1), x.size=8, y.size=8, 
                        title='Daily\nProbability\nCase\nimported\ninto California', 
                        labCol='t', na.value="grey85", pal_colors=c("lightgoldenrod1","darkorange1","darkred"),
                        aspect_ratio = coord_ratio, t.skip=3)

ggsave(file.path("figures",project_name,sprintf("heatmap_PROB_importation_v%s.png", version)),plot=p_prob, width = 4, height = 8, dpi=600)



# Mean Number importation Heatmap
summary(import_results_desttime$import_mean)
quantile(import_results_desttime$import_mean, probs=seq(0,1,.1), na.rm=TRUE)
max_val_ <-  max(import_results_desttime$import_mean, na.rm=TRUE)
max_val_ <- ifelse(max_val_>=10, ceiling(max_val_/10)*10, ceiling(max_val_))        
        
if(max_val_>10){
    zcuts_ <- c(0, .2, .5, 1, 3, 5, seq(10, max_val_, 5))
}else {
    zcuts_ <- c(0, .2, .5, seq(1, max_val_, 2))
}

p_mean <- ggheat_import(data=import_results_desttime %>% select(t, destination, value=import_mean) %>% 
                       rowwise %>% filter(as.Date(t) >= as.Date(t_limits[1]) & as.Date(t) <= as.Date(t_limits[2])),
                   zCuts=zcuts_,
                   x.size=8, y.size=8, title='Mean\nCases\nimported\ninto California', 
                   labCol='t', na.value="grey75", pal_colors=c("seashell","magenta4"),
                   aspect_ratio = coord_ratio, t.skip=3)

ggsave(file.path("figures",project_name,sprintf("heatmap_MEANNUMBER_importation_v%s.png", version)), plot=p_mean, width = 4, height = 8, dpi=600)

rm(max_val_, zcuts_)

