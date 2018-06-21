GspSequencer <- function() {
    this <- list();
    class(this) <- append(class(this), "GspSequencer");
    return(this);
}

do_sequencing <- function(gsp_sequencer, mapping_of_patterns, list_of_sequences, min_support) {
    UseMethod("do_sequencing", gsp_sequencer);
}

do_sequencing.GspSequencer <- function(gsp_sequencer, mapping_of_patterns, list_of_sequences, min_support) {
    initialize_pattern_l_set <- function(mapping_of_patterns) {
        pattern_l_set <- list();
        length_of_list_of_patterns <- length(mapping_of_patterns);
        for (idx in c(1: length_of_list_of_patterns)) {
            pattern <- mapping_of_patterns[[idx]]$transformed_pattern$sequence$items[[1]];
            pattern_l_set[[idx]] <- mapping_of_patterns[[idx]]$transformed_pattern;
        }
        return(pattern_l_set);
    }
    k         <- 2;
    gsp_finder <- GspFinder();
    
    pattern_l_set     <- initialize_pattern_l_set(mapping_of_patterns);
    apriori   <- Apriori(pattern_l_set);
    pattern_c_set     <- apriori_generate(apriori);
    idx       <- length(pattern_l_set) + 1;
    while (length(pattern_c_set) > 0) {
        print(paste('Looking for frequent', k, '-patterns'));
        patterns <- find_patterns(gsp_finder, list_of_sequences, min_support, pattern_c_set);
        counter <- 1;
        for (pattern in patterns) {
            if (!is_on_list(pattern, pattern_l_set)) {
                pattern_l_set[[idx]] <- pattern;
                idx <- idx + 1;
            }
            counter <- counter + 1;
        }
        candidate_patterns <- list();
        i <- 1;
        for (pattern in pattern_l_set) {
            if (length(pattern$sequence$items) == k) {
                candidate_patterns[[i]] <- pattern;
                i <- i + 1;
            }
        }
        k <- k + 1;
        apriori   <- Apriori(candidate_patterns);
        pattern_c_set <- apriori_generate(apriori);
    }
    return(pattern_l_set);
}