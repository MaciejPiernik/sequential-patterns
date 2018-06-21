GspTransformator <- function() {
    this <- list();
    class(this) <- append(class(this), "GspTransformator");
    return(this);
}

transform_patterns <- function(gsp_transformator, list_of_patterns) {
    UseMethod("transform_patterns", gsp_transformator);
}

transform_sequences <- function(gsp_transformator, list_of_sequences, patterns_mapping) {
    UseMethod("transform_sequences", gsp_transformator);
}

transform_patterns_back <- function(gsp_transformator, list_of_patterns) {
    UseMethod("transform_patterns_back", gsp_transformator);
}

transform_patterns.GspTransformator <- function(gsp_transformator, patterns) {
    mapping <- list();
    mapping_id <- 1;
    for (pattern in patterns) {
        tuple <- list();
        items <- list();
        items[[1]] <- mapping_id;
        transformed_pattern <- pattern;
        transformed_pattern$sequence$items <- items;
        tuple$transformed_pattern <- transformed_pattern;
        tuple$pattern             <- pattern;
        mapping[[mapping_id]] <- tuple;
        mapping_id <- mapping_id + 1;
    }
    return(mapping);
}

transform_sequences.GspTransformator <- function(GspTransformator, list_of_sequences, patterns_mapping) {
  
    transform_sequence_element <- function(candidate_elements, patterns_mapping){
        rst <- c();
        if (length(patterns_mapping) == 0) {
            stop("There is no frequent items to transform")
        }
        for (index in c(1:length(patterns_mapping))) {
            pattern_elements <- unlist(patterns_mapping[[index]]$pattern$sequence$items);
            transformed_element <- unlist(patterns_mapping[[index]]$transformed_pattern$sequence$items[[1]]); # in our case we will always have there one-element list with integer
            is_in <- pattern_elements %in% candidate_elements;
            is_correct <- F;
            is_correct <- (length(is_in[is_in == FALSE]) == 0  && length(candidate_elements) >= length(pattern_elements));
            if (is_correct) {
                rst <- c(rst, transformed_element);
            }
        }
        return (rst);
    }
    
    transform_sequence <- function(patterns_mapping, sequence) {
        transformed_sequence <- list();
        transformed_sequence_id <- 1;
        for (element_of_sequence in sequence$items) {
            transformed_element_of_sequence <- transform_sequence_element(element_of_sequence, patterns_mapping); #returns vector
            if (length(transformed_element_of_sequence) > 0) {
                transformed_sequence[[transformed_sequence_id]] <- transformed_element_of_sequence;
                transformed_sequence_id <- transformed_sequence_id + 1;
            }
        }
        sequence <- Sequence(transformed_sequence, NULL);
        transformed_gsp_sequence <- Pattern(sequence, NULL, NULL);
        return(transformed_gsp_sequence);
    }
    
    list_of_transformed_gsp_sequences <- list();
    list_of_transformed_gsp_sequences_id <- 1;
    
    for (sequence in list_of_sequences) {
        transformed_gsp_sequence <- transform_sequence(patterns_mapping, sequence);
        list_of_transformed_gsp_sequences[[list_of_transformed_gsp_sequences_id]] <- transformed_gsp_sequence;
        list_of_transformed_gsp_sequences_id <- list_of_transformed_gsp_sequences_id + 1;
    }
    return(list_of_transformed_gsp_sequences);
}

transform_patterns_back <- function(gsp_transformator, list_of_patterns, patterns_mapping) {
    find_in_mapping <- function (event, patterns_mapping) {
        return(patterns_mapping[[event]]$pattern$sequence$items);
    }

    list_of_mapped_patterns <- list();
    list_of_mapped_patterns_id <- 1;
    for (pattern in list_of_patterns){
        lst <- list();
        lst_id <- 1;
        for (item in pattern$sequence$items) {
            for (event in item) {
                mapped_event <- find_in_mapping(event, patterns_mapping);
                lst[[lst_id]] <- mapped_event;
                lst_id <- lst_id + 1;
            }
        }
        sequence <- Sequence(lst, NULL);
        new_pattern <- Pattern(sequence, pattern$support, NULL);
        list_of_mapped_patterns[[list_of_mapped_patterns_id]] <- new_pattern;
        list_of_mapped_patterns_id <- list_of_mapped_patterns_id + 1;
    }
    return (list_of_mapped_patterns);
}