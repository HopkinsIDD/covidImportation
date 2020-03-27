## Change importation model parameters so that they match the airport attribution clusters

imports_airport_clustering <- function(imports_sim, airport_attribution, regioncode, local_dir="data/") {
  air_cl <- airport_attribution # read_csv(paste0("data/", regioncode, "/airport_attribution_", yr, ".csv"))
  
  imports_sim_orig <- imports_sim %>% rename(airport = destination, date=t, imports=this.sim) # read_csv(paste0("data/", regioncode, "/import_nb_params_nocluster.csv"))

  cl_names <- air_cl %>%
      dplyr::filter(nchar(airport_iata)>3) %>%
      distinct(airport_iata) %>% unlist %>% unname
  cl_names_ls <- lapply(1:length(cl_names), function(i) {
      unlist(strsplit(cl_names[i], "_"))
  })


  imports_cluster <- purrr::map_dfr(1:length(cl_names_ls), function(i){
    imports_sim_orig %>%
      dplyr::filter(airport %in% cl_names_ls[[i]]) %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(airport = paste(airport, collapse = "_"), imports = sum(imports)) 
  })
  
  
  imports_sim_tot <- imports_sim_orig %>% 
      dplyr::filter(!(airport %in% unlist(purrr::flatten(cl_names_ls)))) %>%
      dplyr::bind_rows(imports_cluster) %>%
      dplyr::mutate(date = as.character(date))

  write_csv(imports_sim_tot, paste0(local_dir, "/", regioncode, "/import_sims_cluster.csv"))
  return(imports_sim_tot)
}
