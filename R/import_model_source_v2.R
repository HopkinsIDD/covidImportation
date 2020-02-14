


# DAILY IMPORTATION MODEL - TIME AND LOCATION  -------------------------------------------------

run_daily_import_timeloc_model2 <- function(input_data, n_sim=3000, allow_travel_variance=FALSE,
                                            t_red, u_origin, 
                                            project_name, batch, version){
    
    sources_ <- sort(unique(input_data$source))
    dests_ <- sort(unique(input_data$destination))
    t_ <- sort(unique(input_data$t))
    
    # Sims in longform
    cases <- input_data$cases_curr_total
    sim <- input_data %>% select(source, destination, t) %>% data.table::as.data.table()
    
    # Sims as multidimensional arrays
    importation_sim <- array(0, dim = c(length(sources_), length(dests_), length(t_), n_sim),
                             dimnames = list(sources_, dests_, as.character(t_), 1:n_sim))
    
    
    # start the timer
    t.start <- proc.time() 
    
    for (n in 1:n_sim){
        if (n %% 100 == 0) print(paste('sim', n, 'of', n_sim, sep = ' '))
        
        this.sim <- rep(0, length(cases))
        
        # Get p_s,d,t  (probability of infected individual traveling from d to s during time t
        if (allow_travel_variance){  # if allowing variance in travel, using travelers SE
            Travelers_over_Population_and_days <- MCMCglmm::rtnorm(dim(input_data)[1],
                                                                   mean = input_data$travelers,
                                                                   sd = input_data$travelers_SE,
                                                                   lower = 0) / input_data$days_per_t / input_data$population
        } else {
            Travelers_over_Population_and_days <- input_data$travelers / input_data$days_per_t / input_data$population
        }
        
        # adjust probability by travel probability reduction
        prob_travel_n_detection <- t_red[n]*Travelers_over_Population_and_days
        
        # Run simulations by day, in case travel likelihood is affected by symptoms on a day to day basis
        # for (c in 1:length(cases)){
        #   this.sim[c] <- sum(rbinom(meanD_mat[n,c], prob = prob_travel_n_detection[c], size = round(cases[c]/u_origin[n,c])))
        # }
        this.sim <- rbinom(n = length(cases), size = round(cases), prob = prob_travel_n_detection)
        
        sim.wide <- acast(cbind(sim, this.sim) %>%
                              group_by(source, destination, t) %>%
                              summarize(this.sim = sum(this.sim)),
                          source ~ destination ~ t, value.var = "this.sim")
        importation_sim[dimnames(sim.wide)[[1]], dimnames(sim.wide)[[2]], dimnames(sim.wide)[[3]], n] <- sim.wide
        
    }
    
    # Saving Sims
    dir.create(file.path("output",project_name), recursive = TRUE)
    save(importation_sim, file = file.path("output",project_name, sprintf("nCoV_importation_sim_%s_batch_v%s.RData", batch, version)))
    
    print(paste0('Simulation required ', round(as.list(proc.time() - t.start)$elapsed/60, 3), ' minutes'))
    return(importation_sim)
}