
##'
##' Distribute importations into airports out to counties
##' 
##' @title distrib_county_imports 
##'
##' @param import_sims_clusters number of simulations to run
##' @param airport_attribution Airport attribution data.frame
##' @param local_dir local data directory
##' @param regioncode Region/project name
##' @param yr Year of county population data
##'
##' @return A data.frame of counties, dates, and nmber of importations
##' 
##' @export
##'
distrib_county_imports <- function(import_sims_clusters,
                                   airport_attribution,
                                   local_dir, 
                                   regioncode,
                                   yr=2010){
    
    county_pops_df <- readr::read_csv(paste0(local_dir, regioncode, "/county_pops_", yr, ".csv"))

    # merge county pop
    airport_attribution <- left_join(airport_attribution, county_pops_df %>% select(GEOID, population=estimate), by=c("county"="GEOID"))
    airport_attribution <- airport_attribution %>% as.data.frame() %>%
        mutate(pop_adj = as.numeric(population) * as.numeric(attribution)) %>%
        group_by(airport_iata) %>% mutate(attribution = pop_adj/sum(pop_adj, na.rm = TRUE)) %>% ungroup() %>% 
        select(-pop_adj) 
    airport_attribution <- airport_attribution %>% mutate(attribution = round(attribution,4))
    
    # Sample the importations out to counties based on population and attribution
    samp_res <- list()
    import_sims_clusters_no0 <- import_sims_clusters %>% filter(imports>0)
    for (i in 1:nrow(import_sims_clusters_no0)){
        imports_ <- import_sims_clusters_no0[i,]
        co_info <- airport_attribution %>% filter(airport_iata == imports_$airport)
        if (nrow(co_info)==0) next ## need to figure out why we lost some airports in the attribution (EAT)
        samp_ <- base::sample(co_info$county, imports_$imports, replace=TRUE, prob=co_info$attribution)
        samp_res[[i]] <- data.frame(GEOID=samp_, t=imports_$date)
    }
    samp_res <- data.table::rbindlist(samp_res)
    samp_res <- samp_res %>% count(GEOID, t) %>% rename(imports=n)
    
    return(samp_res)
}

#write_csv(samp_res, file.path(output_dir, "county_imports_sim1.csv"))

