

# # # Settings
# states_of_interest <- sort(c("CA","NV","WA","OR","AZ"))
# regions_of_interest <- paste("US", states_of_interest, sep = "-")
# year <- "2010"
# regioncode <- "west-coast"
# shapefile_path <- paste0('data/', regioncode, '/shp/','counties_2010_', regioncode, '.shp')
# 
# plot = FALSE
# travelers_threshold <- 60000
# airport_cluster_threshold <- 160 # km
# 
# census_api_key(key="c235e1b5620232fab506af060c5f8580604d89c1", install=TRUE)
# 
# imports_sim <- imports_sim %>% group_by(destination, t) %>%
#   summarise(this.sim = sum(this.sim, na.rm = TRUE))



## The purpose of this code is to take in and determine how air importations to a specific US region will be distributed to surrounding counties to seed the SEIR epidemic model. 

airport_estimation <- function(
    states_of_interest=c("CA","NV","WA","OR","AZ"),
    regioncode="west-coast-AZ-NV",
    yr=2010,
    airport_codes_csv_file,
    airport_monthly_mean_travelers_csv_file,
    travelers_threshold=60000, ## airports must meet this average number of monthly travelers to be included
    airport_cluster_threshold=160, # units: km. Airports that are separated by Haversine distance
    imports_sim,
    local_dir="data/",
    plot=FALSE) {
  
  states_of_interest <- sort(states_of_interest)
  county_pops_shp_path <- get_county_pops(states_of_interest, 
                                          regioncode, yr, 
                                          local_dir=local_dir)
  airports_to_consider <- get_airports_to_consider(airport_monthly_mean_travelers_csv_file, 
                                                   states_of_interest, travelers_threshold)
  airport_attribution <- do_airport_attribution(airports_to_consider,
                                                airport_cluster_threshold,
                                                shapefile_path,
                                                regioncode,
                                                yr=yr,
                                                local_dir=local_dir,
                                                plot=plot)
  
  import_sims_clusters <- imports_airport_clustering(imports_sim, 
                                                     airport_attribution, 
                                                     regioncode, 
                                                     local_dir=local_dir)
  
  county_imports <- distrib_county_imports(import_sims_clusters,
                                     airport_attribution,
                                     local_dir, 
                                     regioncode,
                                     yr=2010)
  
  return(import_sims_clusters)
}
