source("./algorithms/interestingness-measure-calculators/probability-helper.r");

ConfidenceCalculator <- function () {
  this <- list();
  class(this) <- append(class(this), "ConfidenceCalculator");
  return(this);
}

calculate <- function(confidence_calculator, pattern, class, sequences) {
  UseMethod("calculate", confidence_calculator)
}

#
# confidence -  confidence is the probability of occurrence the sequences containing pattern and class occurrence after pattern occurrence, 
#               divided by the probability of occurence the sequence containing pattern
#
calculate.ConfidenceCalculator <- function(support_calculator, pattern, class, sequences) {
  probability_helper <- ProbabilityHelper();
  AandB <- calculate_combined_probability_of_pattern_and_class(probability_helper, pattern, class, sequences);
  A <- calculate_probability_of_pattern_occurence(probability_helper, pattern, sequences);
  return (AandB/A);
}