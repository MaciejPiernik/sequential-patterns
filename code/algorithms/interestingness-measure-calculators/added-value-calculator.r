#P(B|A) − P(B)

source("./algorithms/interestingness-measure-calculators/probability-helper.r");

AddedValueCalculator <- function () {
  this <- list();
  class(this) <- append(class(this), "AddedValueCalculator");
  return(this);
}

calculate <- function(added_value_calculator, pattern, class, sequences) {
  UseMethod("calculate", added_value_calculator)
}

calculate.AddedValueCalculator <- function(added_value_calculator, pattern, class, sequences) {
  probability_helper <- ProbabilityHelper();
  AandB <- calculate_combined_probability_of_pattern_and_class(probability_helper, pattern, class, sequences);
  A <- calculate_probability_of_pattern_occurence(probability_helper, pattern, sequences);
  B <- calculate_probability_of_class_occurence(probability_helper, class, sequences);
  
  P1 <- AandB / A;
  
  return (P1-B);
}