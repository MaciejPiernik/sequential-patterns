#P(B|A) − P(B)

source("./algorithms/interestingness-measure-calculators/probability-helper.r");

JaccardCalculator <- function () {
  this <- list();
  class(this) <- append(class(this), "JaccardCalculator");
  return(this);
}

calculate <- function(jaccard_calculator, pattern, class, sequences) {
  UseMethod("calculate", jaccard_calculator)
}

calculate.JaccardCalculator <- function(jaccard_calculator, pattern, class, sequences) {
  probability_helper <- ProbabilityHelper();
  AandB <- calculate_combined_probability_of_pattern_and_class(probability_helper, pattern, class, sequences);
  A <- calculate_probability_of_pattern_occurence(probability_helper, pattern, sequences);
  B <- calculate_probability_of_class_occurence(probability_helper, class, sequences);
  
  P <- A + B - AandB;
  
  return (AandB/P);
}