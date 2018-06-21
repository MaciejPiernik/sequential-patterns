#P(B|A) − P(A)P(B)

source("./algorithms/interestingness-measure-calculators/probability-helper.r");

LeverageCalculator <- function () {
  this <- list();
  class(this) <- append(class(this), "LeverageCalculator");
  return(this);
}

calculate <- function(lift_calculator, pattern, class, sequences) {
  UseMethod("calculate", lift_calculator)
}

calculate.LeverageCalculator <- function(lift_calculator, pattern, class, sequences) {
  probability_helper <- ProbabilityHelper();
  AandB <- calculate_combined_probability_of_pattern_and_class(probability_helper, pattern, class, sequences);
  A <- calculate_probability_of_pattern_occurence(probability_helper, pattern, sequences);
  B <- calculate_probability_of_class_occurence(probability_helper, class, sequences);
  
  P1 <- AandB / A;
  P2 <- A * B;
  
  return (P1-P2);
}