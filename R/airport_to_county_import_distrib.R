
##' These are all of the functions needed to do the airport-to-county importation distribution process.
##' 




##' 
##' Query the census API to get the county populations for the states of interest, assigned
##'  to the given region code.
##'  
##' Assumes census_api_key is already called by the calling client with 
##'  `census_api_key(key="[KEY]", install=TRUE)``
##'      and
##'  `options(tigris_use_cache = TRUE)`  
##' 
##' 
##' @title get_county_pops 
##'
##' @param states_of_interest States for which to get county populations
##' @param regioncode Region/project name
##' @param yr Year of county population data
##' @param local_dir local data directory
##'
##' @return A data.frame of clustered airports, dates, and nmber of importations
##' 
##' @import sf dplyr
##' @importFrom tidycensus get_acs
##' @importFrom readr write_csv
##' @importFrom purrr map map2 reduce
##' 
##' 
##' @export
##'
get_county_pops <- function(states_of_interest, 
                            regioncode, 
                            yr=2010, 
                            local_dir="data/", 
                            write_county_shapefiles=TRUE) {
    county_pops <- purrr::map(states_of_interest,
                              ~tidycensus::get_acs(geography = "county",
                                                   variables = "B01003_001", ## total population data
                                                   state = .x, 
                                                   year = yr,
                                                   keep_geo_vars = TRUE,
                                                   geometry = TRUE,
                                                   show_call = TRUE)) %>%
        purrr::map2(states_of_interest, ~dplyr::mutate(.x, id = .y))
    county_pops2 <- purrr::reduce(county_pops, rbind)
    
    ## write populations dataframe only
    county_pops_df <- sf::st_drop_geometry(county_pops2)
    dir.create(file.path(local_dir), recursive = TRUE, showWarnings = FALSE)
    print(paste("population frame written to",paste0(local_dir, "/county_pops_", yr, ".csv")))
    readr::write_csv(county_pops_df, paste0(local_dir, "/county_pops_", yr, ".csv"))
    
    if (write_county_shapefiles){
        ## write shapefiles for counties in region of interest
        county_pops_sf <- county_pops2 %>%
            dplyr::select(STATEFP, COUNTYFP, GEOID, NAME.x, id) %>%
            dplyr::rename(NAME = NAME.x)
        
        shp_path <- paste0(local_dir, "/shp/counties_", yr, "_", regioncode, ".shp")  
        dir.create(paste0(local_dir, "/shp"), recursive = TRUE, showWarnings = FALSE)
        if (!file.exists(shp_path)) {
            sf::st_write(county_pops_sf, shp_path)
        }
        
        print(paste0("Shapefile written to: ",shp_path))
    }
    
    return(county_pops_df)
}
















#gpclibPermit()         # Seems wacky, shouldn't require this.
# The map projection may not adjusted correctly but the error is minor.


# # Settings...................
# states_of_interest <- sort(c("CA","NV","WA","OR","AZ"))
# regions_of_interest <- paste("US", states_of_interest, sep = "-")
# year <- "2010"
# regioncode <- "west-coast"
# shapefile_path <- paste0('data/', regioncode, '/shp/','counties_2010_', regioncode, '.shp')
# 
# plot = TRUE
# travelers_threshold <- 60000
# airport_cluster_threshold <- 160 # km
# 
# 
# airports_to_consider <- get_airports_to_consider(mean_travel_file, states_of_interest, travelers_threshold)
#   
# airport_attibution <- do_airport_attribution(airports_to_consider, airport_cluster_threshold, shapefile_path, regioncode, yr=2010, local_dir="data/", plot=FALSE)
#   


##' 
##' Query the census API to get the county populations for the states of interest, assigned
##'  to the given region code.
##'  
##' 
##' @title get_airports_to_consider 
##'
##' @param mean_travel_file Filename of airport monthly mean travelers
##' @param states_of_interest States for which to get county populations
##' @param travelers_threshold Minimum monthly average travel volume to be included
##'
##' @return A data.frame of clustered airports, dates, and nmber of importations
##'
##' @import dplyr
##'
get_airports_to_consider <- function(mean_travel_file, 
                                     states_of_interest, 
                                     travelers_threshold=10000) {
    # Airport data
    data("airport_data")
    
    ## dplyr::filter from all airports in region based on number of travelers
    ## monthly mean travelers file like: paste0("data/", regioncode, "/airport_monthlymeantravelers.csv")
    big_airports_region <- readr::read_csv(mean_travel_file) %>%
        dplyr::rename(iata_code = destination) %>%
        dplyr::full_join(airport_data, by = c("iata_code")) %>%
        dplyr::filter(travelers > travelers_threshold) %>%
        dplyr::select(iata_code) %>% unlist %>% unname
    
    regions_of_interest <- paste("US", states_of_interest, sep = "-")
    airports_to_consider <- airport_data %>% 
        dplyr::filter(!is.na(iata_code)) %>%
        dplyr::filter(iso_region %in% regions_of_interest) %>%
        dplyr::filter(iata_code %in% big_airports_region) %>%
        tidyr::separate(coordinates, sep = ',', c('coor_lat', 'coor_lon'), convert = TRUE) %>%
        dplyr::mutate(id = seq_along(iata_code))
    
    return(airports_to_consider)
}




##' 
##' Cluster Airports in close proximity
##'  
##' 
##' @title do_airport_attribution 
##'
##' @param airports_to_consider data.frame of airports specific to the region of interest
##' @param airport_cluster_threshold distance (km) in which airports should be considered clustered.
##' @param shapefile_path file path to shapefile for region
##' @param regioncode Region/project name
##' @param yr Year of county population data
##' @param local_dir local data directory
##' @param plot logical, whether to plot tesselation maps
##'
##' @return A data.frame of airports and counties with attributions
##'
##' @importFrom igraph make_full_graph set_vertex_attr as_edgelist
##' @importFrom geosphere distHaversine
##' @importFrom rlist list.remove
##' @importFrom rgdal readOGR
##' @importFrom maptools unionSpatialPolygons
##' @importFrom raster intersect crs projection 
##' @importFrom ggvoronoi voronoi_polygon
##' @importFrom rgeos gBuffer
##' @importFrom purrr map flatten map_dfr
##' @import dplyr tibble ggplot2
##'
##' @export
##'
do_airport_attribution <- function(airports_to_consider, 
                                   airport_cluster_threshold=80, 
                                   shapefile_path = paste0('data/shp/','counties_2010_', regioncode, '.shp'), 
                                   regioncode, 
                                   yr=2010, 
                                   local_dir="data/",
				   cores=4,
                                   plot=FALSE,
                                   print_attr_error=FALSE) {
    
    ## airport edgelist to start dataframe
    airnet <- igraph::make_full_graph(nrow(airports_to_consider), directed = FALSE, loops = FALSE) %>%
        igraph::set_vertex_attr("name", value = airports_to_consider$iata_code)
    
    airedgelist <- data.frame(igraph::as_edgelist(airnet, names = TRUE), stringsAsFactors = FALSE) %>%
        dplyr::tbl_df() %>%
        dplyr::rename(iata1 = X1, iata2 = X2) %>%
        dplyr::left_join(airports_to_consider %>% dplyr::select(iata_code, coor_lat, coor_lon), by = c("iata1" = "iata_code")) %>%
        dplyr::rename(lat1 = coor_lat, lon1 = coor_lon) %>%
        dplyr::left_join(airports_to_consider %>% dplyr::select(iata_code, coor_lat, coor_lon), by = c("iata2" = "iata_code")) %>%
        dplyr::rename(lat2 = coor_lat, lon2 = coor_lon) 
    
    lonlat1 <- airedgelist %>% dplyr::select(lon1, lat1) %>% as.matrix
    lonlat2 <- airedgelist %>% dplyr::select(lon2, lat2) %>% as.matrix
    dist_km <- geosphere::distHaversine(lonlat1, lonlat2) / 1000
    
    airdf <- airedgelist %>% dplyr::mutate(dist_km = dist_km)
    clusters <- airdf %>% dplyr::filter(dist_km <= airport_cluster_threshold)
    
    ## aggregate clusters -- first pass
    clusters_ls <- list()  
    clusters_ls <- lapply(unique(clusters$iata1), function(airport){
        proposed_cluster <- clusters %>% dplyr::filter(iata1 == airport)
        proposed_cluster2 <- clusters %>% dplyr::filter(iata1 %in% proposed_cluster$iata2)
        proposed_cluster3 <- clusters %>% dplyr::filter(iata1 %in% proposed_cluster2$iata2)
        cluster <- sort(unique(c(airport, proposed_cluster$iata2, proposed_cluster2$iata2, proposed_cluster3$iata2)))
        return(cluster)
    })
    
    ## aggregate clusters -- second pass, and clean up
    ## 1) in case the clusters are expansive and are more than 2x removed from each other,
    ##    combine clusters with any overlapping airports;
    ## 2) then remove clusters that are subsets of others
    
    rm_ix <- c()
    joint_ls <- lapply(seq_len(length(clusters_ls)), function(i) {
        ix <- c()
        subi <- i:length(clusters_ls)
        for (j in subi[-1]) {
            ix <- c(ix, ifelse(any(clusters_ls[[i]] %in% clusters_ls[[j]]), j, 0))
        }
        ix <- ix[ix != 0]
        
        cluster <- sort(unique(c(clusters_ls[[i]], 
                                 unlist(purrr::flatten(purrr::map(ix, function(k) { return(clusters_ls[[k]]) }))))))
        
        rm_ix <- c(rm_ix, ix)
        return(list(cluster, rm_ix))
    })
    
    ## identify indexes of clusters that are subsets of others
    clusters_ls_cl <- lapply(seq_len(length(joint_ls)), function(i) { joint_ls[[i]][[1]] })
    remove_indexes <- sort(unique(unlist(lapply(seq_len(length(joint_ls)), function(i) { joint_ls[[i]][[2]] }))))
    
    ## remove clusters that are subsets of others
    if (length(remove_indexes)>0){
      clusters_ls_cl <- rlist::list.remove(clusters_ls_cl, range = remove_indexes)
    }  
    
    # Get centroid of airport coordinate clusters
    cluster_ids <- purrr::map_dfr(seq_len(length(clusters_ls_cl)), function(i) {
        data.frame(iata_code = clusters_ls_cl[[i]], c_id = i, stringsAsFactors = FALSE)
    })
    if(nrow(cluster_ids) > 0){
        clustered_airports <- airports_to_consider %>%
            dplyr::right_join(cluster_ids, by = c("iata_code")) %>%
            dplyr::select(iata_code, c_id, coor_lat, coor_lon) %>%
            dplyr::group_by(c_id) %>%
            dplyr::summarise(iata_code = paste(iata_code, collapse = "_"), coor_lat = mean(coor_lat), coor_lon = mean(coor_lon)) %>%
            dplyr::ungroup() %>% 
            dplyr::select(iata_code, coor_lat, coor_lon)
        
    } else {
      clustered_airports <- data_frame(iata_code = NA)[0,]
    }
    
    ## remerge clustered airports with other airports
    airports_to_consider_cl <- airports_to_consider %>%
      dplyr::filter(!(iata_code %in% cluster_ids$iata_code)) %>%
      dplyr::select(iata_code, coor_lat, coor_lon) %>%
      dplyr::bind_rows(clustered_airports)
    
    
    # ~ Get Shapefile 
    # shape file at adm1 and adm0 level
    print(paste("Reading in shapefile_path:", shapefile_path))
    loc_map <- rgdal::readOGR(shapefile_path) 
    adm0_loc <- maptools::unionSpatialPolygons(loc_map, loc_map@data$STATEFP)
    adm1_loc <- maptools::unionSpatialPolygons(loc_map, loc_map@data$GEOID)
    if (plot) {
        plot(loc_map)
    }
    
    # Voronoi tesselation by airports
    voronoi_tess <- ggvoronoi::voronoi_polygon(airports_to_consider_cl, x = "coor_lon", y = "coor_lat",
                                               outline = adm0_loc)
    
    ## change projections of voronoi tesselation to match county shapefiles
    crs_shp <- raster::crs(adm1_loc)
    reg_loc <- maptools::unionSpatialPolygons(voronoi_tess, voronoi_tess@data$iata_code)
    reg_loc <- rgeos::gBuffer(reg_loc, byid=TRUE, width=0)
    raster::projection(reg_loc) <- crs_shp
    
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)

    print(paste("Number of pairs is:", length(levels(loc_map@data$GEOID)) * length(voronoi_tess@data$iata_code)))
    airport_attribution <- foreach (co = levels(loc_map@data$GEOID),.combine = dplyr::bind_rows) %:%
        foreach (iata = voronoi_tess@data$iata_code, .combine = dplyr::bind_rows) %dopar% {
            airport_attribution <- NULL
            if (!is.na(iata)) {
                inter <- tryCatch({
                    raster::intersect(reg_loc[iata], adm1_loc[co])
                }, error = function(err) { NULL })
                if (!is.null(inter)) {
                    if (length(inter@polygons)>0) {
                        percent_to_iata <- raster::area(inter) / raster::area(adm1_loc[co])
                        airport_attribution <- dplyr::tibble(county = co, airport_iata = iata, attribution = percent_to_iata)
                    }
                }
            }
            airport_attribution
    }
    parallel::stopCluster(cl)

    ## for loop over voronoi_tess@data$iata_code introduced duplicates (1 part of a county per adjacenet airport)
    lhs <- nrow(distinct(airport_attribution, county, airport_iata))
    rhs <- nrow(distinct(airport_attribution, county, airport_iata, attribution))
    if (lhs != rhs) {
        warning("There are duplicate county-airport pairs. Please check the data again.")
    }
    
    
    airport_attribution <- distinct(airport_attribution)
    airport_attribution <- dplyr::left_join(airport_attribution, data.frame(GEOID=loc_map$GEOID, countyname=loc_map$NAME), by=c("county"="GEOID"))
    
    counties_with_errors <- airport_attribution %>% 
        dplyr::group_by(county) %>%
        dplyr::summarise(check = sum(attribution)) %>%
        dplyr::filter(round(check, 2) != 1)
    
    if (nrow(counties_with_errors) > 0) {
        warning(paste("county fips", counties_with_errors$county, "may have errors"))
        
        if (regioncode == "around_md") {
            warning(paste0("Special warning for airport attribution in ", regioncode, "Manual fix for Kent County, DE was implemented."))
            
            ## For some reason the remainder of FIPS 10001 is not being appropriately assigned to the BWI catchment area, so do it manually
            airport_attribution <- dplyr::bind_rows(airport_attribution,
                                                    data.frame(county = "10001", airport_iata = "BWI", attribution = 1-0.915, countyname = "Kent")) %>%
                dplyr::arrange(county)
        }
        
        counties_with_errors_v2 <- airport_attribution %>% 
            dplyr::group_by(county) %>%
            dplyr::summarise(check = sum(attribution)) %>%
            dplyr::filter(round(check,2) != 1)
        
        if (nrow(counties_with_errors_v2) > 0) {
            warning(paste("county fips", counties_with_errors$county, "were not fixed"))
        }
    }
    
    # Save it
    dir.create(file.path(local_dir, regioncode), recursive=TRUE, showWarnings = FALSE)
    path <- paste0(local_dir, "/", regioncode, "/airport_attribution_", yr, ".csv")
    print(paste("Saving airport attribution to path", path))
    data.table::fwrite(airport_attribution, file=path, row.names=FALSE)
    
    if (plot) {
        airport_map <- ggplot2::ggplot() + 
            ggplot2::geom_point(data = airports_to_consider_cl, aes(coor_lon, coor_lat)) +
            ggplot2::geom_polygon(data = ggplot2::fortify(reg_loc), 
                                  ggplot2::aes(long, lat, group = group),
                                  alpha = .4, size = .5,  colour = 'red') + 
            ggplot2::theme(legend.position = "none")
        print(airport_map)
    }
    
    return(airport_attribution)
}















##' 
##' Sum importation counts to airport attribution clusters
##' Airport clusters are generated using `do_airport_attribution` for aiports in close proximity to 
##' each other e.g. ORD/MDY, DCA/IAD, SFO/OAK
##'
##' 
##' @title imports_airport_clustering 
##'
##' @param imports_sim single simulation result from importation model
##' @param airport_attribution Airport attribution data.frame
##' @param regioncode Region/project name
##' @param local_dir local data directory
##'
##' @return A data.frame of clustered airports, dates, and nmber of importations
##' 
##' @import dplyr
##' @importFrom purrr flatten map_dfr
##' 
##' @export
##'
imports_airport_clustering <- function(imports_sim, 
                                       airport_attribution, 
                                       model_output_dir="model_output/importation") {
    
    imports_sim_orig <- imports_sim %>% rename(airport = destination, date=t, imports=this.sim) %>% # read_csv(paste0("data/", regioncode, "/import_nb_params_nocluster.csv"))
        dplyr::group_by(airport, date) %>% 
        summarise(imports = sum(imports, na.rm=TRUE)) %>% as.data.frame()
        
    cl_names <- airport_attribution %>%
        dplyr::filter(nchar(airport_iata)>3) %>%
        distinct(airport_iata) %>% unlist %>% unname
    cl_names_ls <- lapply(seq_len(length(cl_names)), function(i) {
        unlist(strsplit(cl_names[i], "_"))
    })
    
    imports_cluster <- purrr::map_dfr(seq_len(length(cl_names_ls)), function(i){
        imports_sim_orig %>%
            dplyr::filter(airport %in% cl_names_ls[[i]]) %>%
            dplyr::group_by(date) %>%
            dplyr::summarise(airports_incl = paste(airport, collapse = "_"), 
                             imports = sum(imports),
                             airport = cl_names[i]) 
    })
    
    
    imports_sim_tot <- imports_sim_orig %>% 
        dplyr::filter(!(airport %in% unlist(purrr::flatten(cl_names_ls)))) %>%
        dplyr::bind_rows(imports_cluster) %>%
        dplyr::mutate(date = as.character(date))
    
    #write_csv(imports_sim_tot, file.path(model_output_dir, "import_sims_cluster.csv"))
    return(imports_sim_tot)
}







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
##' @importFrom data.table rbindlist
##' @import dplyr readr
##' 
##' @export
##'
distrib_county_imports <- function(import_sims_clusters,
                                   airport_attribution,
                                   local_dir, 
                                   regioncode,
                                   county_pops_df = NULL,
                                   yr=2010){
    
    if (is.null(county_pops_df)){
        county_pops_df <- readr::read_csv(paste0(local_dir, "/county_pops_", yr, ".csv"))
    }
    
    # merge county pop
    airport_attribution <- dplyr::left_join(airport_attribution, county_pops_df %>% dplyr::select(GEOID, population=estimate), by=c("county"="GEOID"))
    airport_attribution <- airport_attribution %>% as.data.frame() %>%
        dplyr::mutate(pop_adj = as.numeric(population) * as.numeric(attribution)) %>%
        dplyr::group_by(airport_iata) %>% dplyr::mutate(attribution = pop_adj/sum(pop_adj, na.rm = TRUE)) %>% ungroup() %>% 
        dplyr::select(-pop_adj) %>% 
        dplyr::mutate(attribution = round(attribution,4))
    
    # Sample the importations out to counties based on population and attribution
    samp_res <- list()
    import_sims_clusters_no0 <- import_sims_clusters %>% dplyr::filter(imports>0)
    for (i in seq_len(nrow(import_sims_clusters_no0))){
        imports_ <- import_sims_clusters_no0[i,]
        co_info <- airport_attribution %>% dplyr::filter(airport_iata == imports_$airport)
        if (nrow(co_info)==0) next ## need to figure out why we lost some airports in the attribution (EAT)
        samp_ <- co_info$county[base::sample(length(co_info$county), imports_$imports, replace=TRUE, prob=co_info$attribution)]
        samp_res[[i]] <- data.frame(GEOID=samp_, t=imports_$date)
    }
    samp_res <- data.table::rbindlist(samp_res)
    samp_res <- samp_res %>% count(GEOID, t) %>% rename(imports=n)
    
    return(samp_res)
}








# 
# # Testing
# 
# states_of_interest=c("CA","NV","WA","OR","AZ")
# regioncode="west-coast-AZ-NV"
# yr=2010
# mean_travel_file = file.path("output", 
#                              paste0(paste(dest, collapse="+"),"_", as.Date(Sys.Date())), 
#                              "travel_mean.csv")
# travelers_threshold=10000 ## airports must meet this average number of monthly travelers to be included
# airport_cluster_threshold=80 # units: km. Airports that are separated by Haversine distance
# imports_sim_file = file.path("output", 
#                              paste0(paste(dest, collapse="+"),"_", as.Date(Sys.Date())), 
#                              "imports_sim1.csv")
# local_dir="data/"
# plot=FALSE




setup_airport_attribution <- function(
  states_of_interest,
  regioncode,
  yr,
  local_dir = 'data/',
  write_county_shapefiles,
  mean_travel_file = file.path('data','travel_mean.csv'),
  travelers_threshold =10000,
  airport_cluster_threshold = 80,
  shapefile_path = NULL,  
  plot=FALSE,
  print_attr_error=FALSE,
  cores=4
){
    print("HERE")
    ## -- Set up the attribution/distribution for all the simulations -- 
    
    # sort the states
    states_of_interest <- sort(states_of_interest)
    
    
    ## get populations for each county in each state of interest, 
    ##   and save the population to a csv, and return a data.frame
    # county_pops_df <- readr::read_csv(paste0(local_dir, "/county_pops_", yr, ".csv"))
    county_pops_df <- get_county_pops(
        states_of_interest, 
        regioncode, 
        yr, 
        local_dir=local_dir,
        write_county_shapefiles=write_county_shapefiles
    )
    print("County populations: Success")
    
    ## Query the census API to get the county populations for the states of interest, assigned
    ##   to the given region code.
    airports_to_consider <- get_airports_to_consider(mean_travel_file=mean_travel_file, 
                                                     states_of_interest, 
                                                     travelers_threshold)
    print("Airports to include: Success")
    
    ## Cluster Airports in close proximity
    do_airport_attribution(airports_to_consider,
                           airport_cluster_threshold,
                           regioncode,
                           yr=yr,
			   shapefile_path=paste0(local_dir, "/shp/counties_", yr, "_", regioncode, ".shp"),
                           local_dir=local_dir,
                           plot=plot,
                           cores=cores,
                           print_attr_error=print_attr_error)
    # This is saved to paste0(local_dir, "/", regioncode, "/airport_attribution_", yr, ".csv")
    print(paste0("Shapefile saved to: ", paste0(local_dir, "/shp/counties_", yr, "_", regioncode, ".shp")))
}



##' 
##' Sum importation counts to airport attribution clusters
##' Airport clusters are generated using `do_airport_attribution` for aiports in close proximity to 
##' each other e.g. ORD/MDY, DCA/IAD, SFO/OAK
##'
##' 
##' @title run_distrib_imports 
##'
##' @param states_of_interest single simulation result from importation model
##' @param regioncode Region/project name
##' @param yr Year of county population data
##' @param mean_travel_file Filename of monthly mean travelers into each airport in the region
##' @param states_of_interest States for which to get county populations
##' @param travelers_threshold Minimum monthly average travel volume to be included
##' @param airport_cluster_threshold Distance by which airports can be separated to be in the same cluster, in km; Haversine distance.
##' @param imports_sim_file file name root for each importation simulation. 
##' @param local_dir local data directory
##'
##' @return A data.frame of clustered airports, dates, and nmber of importations
##' 
##' @import doParallel parallel readr dplyr
##' 
##' @export
##'
run_full_distrib_imports <- function(states_of_interest=c("CA","NV","WA","OR","AZ"),
                                     regioncode="west-coast-AZ-NV",
                                     yr=2010,
                                     travelers_threshold=10000,
                                     airport_cluster_threshold=80, 
                                     mean_travel_file = file.path("data", "travel_mean.csv"),
                                     shapefile_path = NULL, ## This is no longer needed but left to not break it.
                                     model_output_dir = file.path("model_output", "importation"),
                                     local_dir="data/",
                                     plot=FALSE,
                                     cores=5,
                                     n_sim=10){
    
    if (!is.null(shapefile_path)){
      print("Manual shapefile path is depricated. Shapefile will be pulled, built, and saved automatically.")  
    }
    # sort the states
    states_of_interest <- sort(states_of_interest)
    
    
    print("Airport attribution: Success")
    
    if(!(
      file.exists(paste0(local_dir, "/county_pops_", yr, ".csv")) & 
      file.exists(paste0(local_dir, "/", regioncode, "/airport_attribution_", yr, ".csv"))
    )){
      setup_airport_attribution(
        states_of_interest = states_of_interest,
        regioncode = regioncode,
        yr = yr,
        local_dir = local_dir,
        write_county_shapefiles = TRUE,
        mean_travel_file = mean_travel_file,
        travelers_threshold = travelers_threshold,
        airport_cluster_threshold = airport_cluster_threshold,
        shapefile_path = shapefile_path,
        plot=FALSE,
        print_attr_error=FALSE
      )
    }
    county_pops_df <- readr::read_csv(paste0(local_dir, "/county_pops_", yr, ".csv"))
    airport_attribution <- readr::read_csv(paste0(local_dir, "/", regioncode, "/airport_attribution_", yr, ".csv"))
    
    
    ## --- Run through the full set of simulations and make new versions distributed out to counties instead of airports ---
    
    ## Get filenames 
    import_files <- list.files(model_output_dir, "imports_sim*.*.csv$")
    if (length(import_files)!=n_sim){
        print(paste0("Number of simulations changed to ",length(import_files)," to match the number of importation simulations."))
        n_sim <- length(import_files)
    }
    
    
    # Setup parallelization    
    library(doParallel)
    print(paste0("Making a cluster of ", cores," for parallelization."))
    cl <- parallel::makeCluster(cores)
    doParallel::registerDoParallel(cl)
    
    # Run the foreach loop to estimate importations for n simulations
    foreach(n=seq_len(n_sim),
            .export=c("imports_airport_clustering", "distrib_county_imports"),
            .packages=c("dplyr","tidyr","readr")) %dopar% {
                
                
                ## Sum importation counts to airport attribution clusters
                ##  - these still need to be distributed out to the counties
                import_sims_clusters <- imports_airport_clustering(
                    imports_sim = readr::read_csv(file.path(model_output_dir, paste0("imports_sim",n,".csv"))), 
                    airport_attribution=airport_attribution, 
                    model_output_dir = model_output_dir
                )
                
                if (sum(import_sims_clusters$imports)==0){
                  NULL    # - if no importations, skip to next
                } else {
                
                  ## Distribute the importations out to counties based on the tesselation and population
                  county_imports <- distrib_county_imports(import_sims_clusters,
                                                           airport_attribution=airport_attribution,
                                                           local_dir=local_dir, 
                                                           regioncode=regioncode,
                                                           county_pops_df=county_pops_df,
                                                           yr=yr)
                  county_imports <- county_imports %>% dplyr::rename(place=GEOID, date=t, amount=imports)
                  
                  ## Save the new importation file
                  readr::write_csv(county_imports, file.path(model_output_dir, paste0("importation_", n, ".csv")))
                  
                  #print("success")
                }
                
            }
    parallel::stopCluster(cl)
    
    print("Successfully distributed importations from airports to counties for all simulations.")
    
}

