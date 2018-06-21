source("./algorithms/interestingness-measure-calculators/probability-helper.r");

CoverageCalculator <- function () {
  this <- list();
  class(this) <- append(class(this), "CoverageCalculator");
  return(this);
}

calculate <- function(confidence_calculator, pattern, class, sequences) {
  UseMethod("calculate", confidence_calculator)
}

calculate.CoverageCalculator <- function(support_calculator, pattern, sequences) {
  probability_helper <- ProbabilityHelper();
  A <- calculate_probability_of_pattern_occurence(probability_helper, pattern, sequences);
  return (A);
}