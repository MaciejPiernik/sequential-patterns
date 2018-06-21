#P(B|A)/P(B)

source("./algorithms/interestingness-measure-calculators/probability-helper.r");

LiftCalculator <- function () {
  this <- list();
  class(this) <- append(class(this), "LiftCalculator");
  return(this);
}

calculate <- function(lift_calculator, pattern, class, sequences) {
  UseMethod("calculate", lift_calculator)
}

calculate.LiftCalculator <- function(lift_calculator, pattern, class, sequences) {
  probability_helper <- ProbabilityHelper();
  AandB <- calculate_combined_probability_of_pattern_and_class(probability_helper, pattern, class, sequences);
  A <- calculate_probability_of_pattern_occurence(probability_helper, pattern, sequences);
  B <- calculate_probability_of_class_occurence(probability_helper, "up", sequences);
  
  P <- AandB / A;

  return (P/B);
}