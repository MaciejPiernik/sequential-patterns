library("gtools");

Apriori <- function(list_of_gsp_patterns) {
    this <- list(
        patterns = list_of_gsp_patterns
    );
    class(this) <- append(class(this), "Apriori");
    return(this);
}

apriori_generate <- function(apriori){
    UseMethod('apriori_generate', apriori);
}

apriori_generate.Apriori <- function(apriori) {
    get_unique_elements <- function(list_of_patterns) {
        elements <- c();
        for (pattern in list_of_patterns) {
            elements <- c(elements, unlist(pattern$sequence));
        }
        elements <- unique(elements);
        return(elements);
    }

    convert_permutations_to_list_of_patterns <- function(perms){
        list_of_candidates <- list();
        list_of_candidates_id <- 1;
        for (i in c(1:nrow(perms))) {
            row <- perms[i,];
            lst <- list();
            lst_id <- 1;
            for(item in row) {
              lst[[lst_id]] <- item;
              lst_id <- lst_id + 1;
            }
            sequence <- Sequence(lst, NULL);
            list_of_candidates[[list_of_candidates_id]] <- Pattern(sequence, NULL, NULL);
            list_of_candidates_id <- list_of_candidates_id + 1;
        }
        return (list_of_candidates);
    }

    generate_two_element_candidates <- function(list_of_patterns) {
        elements <- get_unique_elements(list_of_patterns);
        perms <- permutations(n = length(elements), r = 2, v = elements, repeats.allowed = T);
        list_of_candidates <- convert_permutations_to_list_of_patterns(perms);
        return(list_of_candidates);
    }

    prepare_to_join <- function(list_of_patterns){
        list_of_left_side_join <- list();
        list_of_right_side_join <- list();
        list_id <- 1;
        for (pattern in list_of_patterns) {
            left <- list();
            rigth <- list();
            left$join_field         <- pattern$sequence$items[-1];
            left$first              <- pattern$sequence$items[1];
            rigth$join_field        <- pattern$sequence$items[-length(pattern$sequence$items)];
            rigth$last              <- pattern$sequence$items[length(pattern$sequence$items)];
            list_of_left_side_join[[list_id]]  <- left;
            list_of_right_side_join[[list_id]] <- rigth;
            list_id <- list_id + 1;
        }
        result <- list();
        result$list_of_left_side_join  <- list_of_left_side_join;
        result$list_of_right_side_join <- list_of_right_side_join;
        return(result);
    }

    join <- function(list_of_left_side_join, list_of_right_side_join) {
        list_of_candidates <- list();
        list_of_candidates_id <- 1;
        for (left in list_of_left_side_join) {
            for (right in list_of_right_side_join) {
                if (all(unlist(left$join_field) == unlist(right$join_field))){
                    lst <- list();
                    lst_id <- 1;
                    for(item in c(left$first, left$join_field, right$last)) {
                        lst[[lst_id]] <- item;
                        lst_id <- lst_id + 1;
                    }
                    joined_sequence <- Sequence(lst, NULL);
                    candidate <- Pattern(joined_sequence, NULL, NULL);
                    list_of_candidates[[list_of_candidates_id]] <- candidate;
                    list_of_candidates_id <- list_of_candidates_id + 1;
                }
            }
        }
        return(list_of_candidates);
    }
    
    generate_longer_candidates <- function(list_of_patterns){
        prepared_data <- prepare_to_join(list_of_patterns);
        list_of_candidates <- join(prepared_data$list_of_left_side_join, prepared_data$list_of_right_side_join);
        return(list_of_candidates);
    }

    if (length(apriori$patterns) == 0) {
        return(list());
    } else if (length(apriori$patterns[[1]]$sequence$items) == 1) {
        candidates <- generate_two_element_candidates(apriori$patterns);
    } else {
        candidates <- generate_longer_candidates(apriori$patterns);
    }
    return(candidates);
}