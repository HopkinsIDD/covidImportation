# Matching province names



match_names <- function(name_a, names_b,
         return_match_scores=FALSE #clean_a=FALSE, clean_b=FALSE
         ){
    
    # Check for error (NA in name_a)
    if (is.na(name_a)){
        return(stats::setNames(as.integer(NA), name_a))
    }
    
    # If location ids, limit to unique ones
    # if (!is.null(names_b_data$id)){
    #   names_b_data <- names_b_data[!duplicated(names_b_data$id),]
    # }
    names_b <- as.character(names_b)
    
    # ## Clean name if not already done
    # if (clean_a){
    #     name_a <- standardize_location_strings(name_a)
    # }
    # ## Clean aliases if not done
    # if (clean_b){
    #     names_b <- standardize_location_strings(names_b)
    # }
    
    
    # First check for an exact match  .............................................................
    exact_match <- (names_b %in% name_a)
    
    # If 1 exact match
    if (sum(exact_match)==1){
        
        if (return_match_scores){
            return(list(best = setNames(as.character(names_b[exact_match]), rep(name_a, times=length(names_b))),
                        match_scores = NA))
        } else {
            return(setNames(as.character(names_b[exact_match]), name_a))
        }
        
        ## if  more than 1 best match, return the number of matches and the description data
        
        
        
        # If no exact match, do the string matching algorithms
    } else {
        
        ## string matching methods
        ## Consider passing in methods as an argument?
        ##   lv      : Levenshtein distance - Minimal number of insertions, deletions and replacements needed for transforming string a into string b.
        ##   dl      : Damerau-Levenshtein distance - Like Levenshtein distance, but transposition of adjacent symbols is allowed.
        ##   osa     : Optimal String Alignment - Like (full) Damerau-Levenshtein distance but each substring may only be edited once.
        ##   lcs     : Longest Common Substring distance - Minimum number of symbols that have to be removed in both strings until resulting substrings are identical.
        ##   qgram   : q-gram distance - Sum of absolute differences between N-gram vectors of both strings.
        ##   cosine  : Cosine distance - 1 minus the cosine similarity of both N-gram vectors.
        ##   jaccard : Jaccard distance - 1 minues the quotient of shared N-grams and all observed N-grams.
        ##   jw      : Jaro-Winkler distance - This distance is a formula of 5 parameters determined by the two compared strings (A,B,m,t,l) and p chosen from [0, 0.25].
        ##   soundex :
        methods <- c("osa", "jw", "soundex")
        #methods <- c('osa','lv','dl','lcs','qgram','cosine','jaccard','jw','soundex')
        dists <- as.data.frame(matrix(
            NA,
            nrow = length(names_b),
            ncol = length(methods),
            dimnames = list(names_b, methods)
        ))
        for (j in 1:length(methods)){
            dists[, j]  <- suppressWarnings(
                stringdist::stringdist(name_a, names_b, method = methods[j], q=2, p=.1)
            )
        }
        dists$soundex <- dists$soundex*3 # use soundex to weed out poor matches
        #dists$lcs <- dists$lcs*2 # use longest common substring to weed out poor matches
        
        dists <- data.frame(names_clean = names_b,
                            dists,
                            score_sums = rowSums(dists))
        
        # Check for numbers in name_a and require numbers in names_b
        if (grepl("[0-9]", name_a)){
            names_b_numeric <- grepl("[0-9]", names_b)
            dists$score_sums <- dists$score_sums + ((!names_b_numeric) * 20)
        }
        
        if(!all(is.na(dists))){
            dists$score_sums_normalized <- 1 - (dists$score_sums / max(dists$score_sums))
        } else {
            dists$score_sums_normalized <- dists$score_sums
        }
        dists$osa <- as.integer(dists$osa)
        
        
        ## get best match from results
        best_ <- NULL
        if (any(dists$osa <= 1)){
            ## OSA less than 1 is highly likely a match
            best_ <- which(dists$osa <= 1 & dists$score_sums == min(dists$score_sums))
        } else if (any(dists$jw <= .1)){
            best_ <- which(dists$jw <= .1 & dists$score_sums == min(dists$score_sums))
        } else if (any(dists$osa <= 3 & dists$jw <= 0.31 & dists$soundex == 0)){
            best_ <- which(dists$osa <= 3 & dists$jw <= 0.31 & dists$soundex == 0)
        }
        
        # ## Modified return if name has 5 or fewer characters 
        # ## --> can generate spurious matches [NEED TO FIX THIS]
        # if ( (nchar(name_a) <= 5) & !return_match_scores){
        #   return(setNames(as.integer(NA),name_a))
        # } else if ( (nchar(name_a) <= 5) & return_match_scores){
        #   return(list(best = setNames(as.integer(NA, name_a)),
        #               match_scores = rep(as.numeric(NA), times = min(20, nrow(dists)))
        #   ))
        # }
        
        ## If no good match was found, return either nothing, or the score matrix (up to 20 long)
        if (length(best_) == 0 & !return_match_scores){
            return(setNames(as.integer(NA), name_a))
        } else if (length(best_) == 0 & return_match_scores){
            return(list(best = setNames(as.integer(NA), name_a),
                        match_scores = dists[order(dists$score_sums), ][1:min(20, nrow(dists)),]))
        }
        
        ## If more than 1 meeting the criteria for best match
        ## -- first check if they are the same, otherwise, return all
        ## if only 1 best, replace best_ with ID
        if (length(best_)==1){
            best_match <- as.character(dists$names_clean[best_])
            best_match <- stats::setNames(best_match, name_a)
            
            if (!return_match_scores){
                return(best_match)
            } else {
                return(list(best = best_match,
                            match_scores = dists[order(dists$score_sums), ][1:min(20, nrow(dists)),]))
            }
            
        } else if (length(best_)>1){
            dists_best <- dists[best_, ]
            ## Order by score sum
            dists_best <- dists_best[order(dists_best$score_sums), ]
            ## Remove duplicate ids
            dists_best <- dists_best[!duplicated(dists_best$names_clean), ]
            ## Take the score of minimal depth
            # if (!return_match_scores){
            #     dists_best <- dists_best[dists_best$depth == min(dists_best$depth), ]
            # }
            
            ## if still more than 1 best match, return the number of matches or the distance matrix
            if (nrow(dists_best) > 1){
                message(paste(name_a,"has",nrow(dists_best),"best matches.  Ignoring them."))
                if (return_match_scores){
                    return(list(best = setNames(as.integer(NA), name_a),
                                match_scores = names_b[exact_match]))
                } else {
                    return(setNames(as.integer(NA), name_a))
                }
            } else {
                best_match <- dists_best$names_clean
                best_match <- stats::setNames(best_match, name_a)
                
                if (!return_match_scores){
                    return(best_match)
                } else {
                    return(list(best = best_match,
                                match_scores = dists[order(dists$score_sums), ][1:min(20, nrow(dists)),]))
                }
            }
        }
    }
}
