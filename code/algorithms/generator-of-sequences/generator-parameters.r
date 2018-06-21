GeneratorParameters <- function (number_of_sequences = 100, max_length_of_sequence = 7, max_length_of_element = 3, elements = c( "a", "b", "c"), classes = c("1", "2"), pattern_to_include = NULL, min_support = 0.2) {
    this <- list(
        number_of_sequences = number_of_sequences,
        max_length_of_sequence = max_length_of_sequence,
        max_length_of_element = max_length_of_element,
        elements = elements,
        classes = classes,
        pattern_to_include = pattern_to_include,
        min_support = min_support
    );
    class(this) <- append(class(this), "GeneratorParameters");
    return(this);
}