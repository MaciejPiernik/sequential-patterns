Pattern <- function(sequence, support, uplift){
    this <- list(
        sequence = sequence,
        support  = support,
        uplift   = uplift
    );
    class(this) <- append(class(this), "Pattern");
    return(this);
}

is_on_list <- function(pattern, list_of_patterns) {
    UseMethod("is_on_list", pattern);
}

is_on_list.Pattern <- function(pattern, list_of_patterns){
    list_of_candidate_items <- pattern$sequence$items;
    for (pattern_from_list in list_of_patterns) {
        list_of_pattern_items <- pattern_from_list$sequence$items;
        if (length(list_of_candidate_items) == length(list_of_pattern_items)) {
            item_id <- 1;
            number_of_the_same_items <- 0;
            for (item in list_of_pattern_items) {  
                are_all_the_same <- all(item == list_of_candidate_items[[item_id]])
                if (length(are_all_the_same[are_all_the_same == FALSE]) == 0) {
                    number_of_the_same_items <- number_of_the_same_items + 1;
                }
                item_id <- item_id + 1;
            }
            if (number_of_the_same_items == length(list_of_candidate_items)){
                return(T);
            }
        }
    }
    return(F);
}