source("./algorithms/generator-of-sequences/generator-parameters.r");
source("./models/sequence.r");
source("./models/sequence-item.r");

Generator <- function(generator_parameters){
    this <- list(
        params = generator_parameters
    );
    class(this) <- append(class(this), "Generator");
    return(this);
}

generate_random_sequences <- function(generator)
{
        UseMethod("generate_random_sequences", generator);
}

generate_random_sequences.Generator <- function(generator){

    get_random_value_from_range <- function(start, stop) {
        if (class(start) != "integer" || class(stop) != "integer" || start < 0 || stop < 0) {
            stop("Parameters start and stop should be positive integer values");
        }
        return(round(runif(1, start, stop), 0));
    }

    generate_classes <- function(classes, length) {
        classes_vector <- c();
        for(idx in c(1:length)){
            value <- get_random_value_from_range(as.integer(1), as.integer(length(classes)));
            classes_vector <- c(classes_vector, classes[value]);
        }
        return(classes_vector);
    }

    generate_element <- function(elements, max_length_of_element) {
        element <- c();
        print
        length_of_element <- get_random_value_from_range(as.integer(1), as.integer(max_length_of_element));
        for (element_id in c(1:length_of_element)) {
            index_of_random_element <- get_random_value_from_range(as.integer(1), as.integer(length(elements)));
            element <- c(element, elements[index_of_random_element]);
        }
        element <- unique(sort(element));
        sequence_item <- SequenceItem(element);
        return(unique(sequence_item));
    }

    generate_sequence <- function(elements, classes, max_length_of_sequence, max_length_of_element){
        list_of_sequence_items <- list();
        length_of_sequence <- get_random_value_from_range(as.integer(1), as.integer(max_length_of_sequence));
        for (element_id in c(1:length_of_sequence)) {
            element <- generate_element(elements, max_length_of_element);
            list_of_sequence_items[[element_id]] <- element;
        }
        class_element <- generate_classes(classes, length_of_sequence);
        sequence <- Sequence(list_of_sequence_items, class_element);
        return(sequence);
    }
    result <- list();
    for (sequence_id in c(1:generator$params$number_of_sequences)) {
        sequence <- generate_sequence(generator$params$elements, generator$params$classes, generator$params$max_length_of_sequence, generator$params$max_length_of_element);
        result[[sequence_id]] <- sequence;
    }

    if (! is.null(generator$params$pattern_to_include)) {
        number_of_min_occurs <- ceiling(length(result) * generator$params$min_support);
        for(i in c(1: number_of_min_occurs)) {
            result[[i]] <- generator$params$pattern;
        }
    }
    return(result);
}