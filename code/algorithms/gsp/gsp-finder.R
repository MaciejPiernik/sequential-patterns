library("gtools");
source("./algorithms/gsp/gsp-support-calculator.r");

GspFinder <- function(){
    this <- list();
    class(this) <- append(class(this), "GspFinder");
    return(this);
}

find_patterns <- function(gsp_finder, list_of_sequences_as_patterns, min_support, list_of_candidate_patterns = NULL) {
    UseMethod("find_patterns", gsp_finder);
}

find_patterns.GspFinder <- function(gsp_finder, list_of_sequences_as_patterns, min_support, list_of_candidate_patterns = NULL) {
    get_all_possible_variants <- function(vector) {
        result <- list(); #list of vectors
        result_id <- 1;
        sum_of_bits <- 0;
        while (sum_of_bits < length(vector)) {
            bits <- intToBits(result_id);
            bits_id <- 1;
            combination <- c();
            for (bit in bits) {
                if (bit == 01) {
                    combination <- c(combination, vector[bits_id]);
                }
                bits_id <- bits_id + 1;
            } #TODO: to optimalize
            result[[result_id]] <- combination;
            result_id <- result_id + 1;
            sum_of_bits <- sum(as.integer(bits));
        }
        return(result);
    }
  
    find_unique_list_of_items <- function(list_of_sequences_as_patterns) {
        list_of_patterns <- list();
        list_of_patterns_id <- 1;
        for (pattern in list_of_sequences_as_patterns) {
            for (item in pattern$sequence$items) {
                items <- get_all_possible_variants(item);
                for (variant in items) {
                    lst <- list();
                    lst[[1]] <- variant;
                    new_sequence  <- Sequence(lst, NULL);
                    new_pattern   <- Pattern(new_sequence, NULL, NULL);
                    if (!is_on_list(new_pattern, list_of_patterns)) {
                        list_of_patterns[[list_of_patterns_id]] <- new_pattern;
                        list_of_patterns_id <- list_of_patterns_id + 1;
                    }
                }
            }
        }
        return(list_of_patterns);
    }
    
    list_of_patterns <- list();
    list_of_patterns_id <- 1;
    if (is.null(list_of_candidate_patterns)) {
        list_of_candidate_patterns <- find_unique_list_of_items(list_of_sequences_as_patterns); #returns list of patterns
    }
    counter <- 1;
    for (pattern in list_of_candidate_patterns) {
        gspSupportCalculator <- GspSupportCalculator();
        support <- calculate_support(gspSupportCalculator, pattern, list_of_sequences_as_patterns);
        if (support >= min_support) {
            list_of_patterns[[list_of_patterns_id]] <- Pattern(pattern$sequence, support, NULL);
            list_of_patterns_id <- list_of_patterns_id + 1;
        }
        print(paste(counter, 'of', length(list_of_candidate_patterns), ' pattern processed.'));
        counter <- counter + 1;
    }
    return(list_of_patterns);
}