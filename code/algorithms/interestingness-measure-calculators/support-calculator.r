source("./algorithms/interestingness-measure-calculators/probability-helper.r");

SupportCalculator <- function () {
  this <- list();
  class(this) <- append(class(this), "SupportCalculator");
  return(this);
}

#
# support - sequence that contains given pattern and class occurrence after pattern occurrence is counted as 1;
#           support is the number of sequences containing pattern and class occurrence after pattern occurrence, divided by the number of all sequences
#

calculate <- function(support_calculator, pattern, class, sequences) {
    UseMethod("calculate", support_calculator);
}

calculate.SupportCalculator <- function(support_calculator, pattern, class, sequences) {
  probability_helper <- ProbabilityHelper();
  P <- calculate_combined_probability_of_pattern_and_class(probability_helper, pattern, class, sequences);
  return (P);
}