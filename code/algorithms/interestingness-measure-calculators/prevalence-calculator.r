source("./algorithms/interestingness-measure-calculators/probability-helper.r");

PrevelenceCalculator <- function () {
  this <- list();
  class(this) <- append(class(this), "PrevelenceCalculator");
  return(this);
}

calculate <- function(prevalence_calculator, pattern, class, sequences) {
  UseMethod("calculate", prevalence_calculator)
}

calculate.PrevelenceCalculator <- function(prevalence_calculator, class, sequences) {
  probability_helper <- ProbabilityHelper();
  A <- calculate_probability_of_class_occurence(probability_helper, class, sequences);
  return (A);
}