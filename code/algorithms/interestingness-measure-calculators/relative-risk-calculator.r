#P(B|A)/P(B|¬A)

source("./algorithms/interestingness-measure-calculators/probability-helper.r");

RelativeRiskCalculator <- function () {
  this <- list();
  class(this) <- append(class(this), "RelativeRiskCalculator");
  return(this);
}

calculate <- function(relative_risk_calculator, pattern, class, sequences) {
  UseMethod("calculate", relative_risk_calculator)
}

calculate.RelativeRiskCalculator <- function(relative_risk_calculator, pattern, class, sequences) {
  probability_helper <- ProbabilityHelper();
  print("P(¬A and B)")
  negAandB <- calculate_combined_probability_of_pattern_occurence_negation_and_class(probability_helper, pattern, class, sequences);
  print(negAandB)
  
  print("P(¬A)")
  negA <- calculate_combined_probability_of_pattern_occurence_negation(probability_helper, pattern, sequences);
  print(negA)
  
  print("P(A and B)")
  AandB <- calculate_combined_probability_of_pattern_and_class(probability_helper, pattern, class, sequences);
  print(AandB)
  
  print("P(A)")
  A <- calculate_probability_of_pattern_occurence(probability_helper, pattern, sequences);
  print(A);
  
  print("P1")
  P1 <- AandB / A;
  print (P1)
  
  print("P2")
  P2 <- negAandB/negA;
  print(P2)
  
  print("P1/P2")
  print(P1/P2)
  return (P1/P2);
}