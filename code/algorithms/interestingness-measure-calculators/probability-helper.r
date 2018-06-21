source("./helpers/sequences-finder.r");

ProbabilityHelper <- function() {
    this <- list();
    class(this) <- append(class(this), "ProbabilityHelper");
    return(this);
}

calculate_probability_of_pattern_occurence <- function(probability_helper, pattern, sequences) {
    UseMethod("calculate_probability_of_pattern_occurence", probability_helper);
}

calculate_probability_of_class_occurence <- function(probability_helper, class, sequences) {
    UseMethod("calculate_probability_of_class_occurence", probability_helper);
}

calculate_combined_probability_of_pattern_and_class <- function(probability_helper, pattern, class, sequences) {
    UseMethod("calculate_combined_probability_of_pattern_and_class", probability_helper);
}

calculate_combined_probability_of_pattern_occurence_negation <- function(probability_helper, pattern, sequences) {
  UseMethod("calculate_combined_probability_of_pattern_occurence_negation", probability_helper);
}

calculate_combined_probability_of_pattern_occurence_negation_and_class <- function(probability_helper, pattern, class, sequences) {
  UseMethod("calculate_combined_probability_of_pattern_occurence_negation_and_class", probability_helper);
}

calculate_probability_of_pattern_occurence.ProbabilityHelper <- function(probability_helper, pattern, sequences) {
    sequences_finder <- SequencesFinder();
    sequences_with_pattern <- divide_sequences_by_pattern(sequences_finder, pattern, sequences)$with_pattern;
    A <- length(sequences_with_pattern);
    O <- length(sequences);
    P <- A/O;
    return(P);
}

calculate_probability_of_class_occurence.ProbabilityHelper <- function(probability_helper, class, sequences) {
  sequence_finder <- SequencesFinder();
  sequences_with_class <- find_sequences_with_class(sequence_finder, class, sequences);
  
  A <- length(sequences_with_class);
  O <- length(sequences);
  
  P <- A / O;
  
  return(P);
}

calculate_combined_probability_of_pattern_and_class.ProbabilityHelper <- function(probability_helper, pattern, class, sequences) {
  sequences_finder <- SequencesFinder();
  sequences_with_pattern <- divide_sequences_by_pattern(sequences_finder, pattern, sequences)$with_pattern;
  
  sequences_with_pattern_after_class <- find_sequences_with_class_after_pattern(sequences_finder, pattern, class, sequences_with_pattern);
  
  AandB <- length(sequences_with_pattern_after_class);
  O <- length(sequences);
  
  P <- AandB/O;
  return(P);
}

calculate_combined_probability_of_pattern_occurence_negation.ProbabilityHelper <- function(probability_helper, pattern, sequences) {
  sequences_finder <- SequencesFinder();
  sequences_without_pattern <- divide_sequences_by_pattern(sequences_finder, pattern, sequences)$without_pattern;
  A <- length(sequences_without_pattern);
  O <- length(sequences);
  P <- A/O;
  return(P);
}

calculate_combined_probability_of_pattern_occurence_negation_and_class.ProbabilityHelper <- function(probability_helper, pattern, class, sequences) {
  sequences_finder <- SequencesFinder();
  sequences_without_pattern <- divide_sequences_by_pattern(sequences_finder, pattern, sequences)$without_pattern;
  a <- 0;
  for(sequence in sequences_without_pattern){
    indexes_containing_class <- sequence$sequence$classes == class;
    if(length(indexes_containing_class[indexes_containing_class==T]) > 0) {
      a <- a + 1;
    }
  }
  O <- length(sequences);
  AandB <- a/O;
  return(AandB);
}