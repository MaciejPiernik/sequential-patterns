source("./algorithms/interestingness-measure-calculators/probability-helper.r");

RecallCalculator <- function () {
  this <- list();
  class(this) <- append(class(this), "RecallCalculator");
  return(this);
}

calculate <- function(recall_calculator, pattern, class, sequences) {
  UseMethod("calculate", recall_calculator)
}

#
# confidence -  confidence is the probability of occurrence the sequences containing pattern and class occurrence after pattern occurrence, 
#               divided by the probability of occurence the sequence containing pattern
#
calculate.RecallCalculator <- function(recall_calculator, pattern, class, sequences) {
  probability_helper <- ProbabilityHelper();
  AandB <- calculate_combined_probability_of_pattern_and_class(probability_helper, pattern, class, sequences);
  B <- calculate_probability_of_class_occurence(probability_helper, "up", sequences);
  return (AandB/B);
}