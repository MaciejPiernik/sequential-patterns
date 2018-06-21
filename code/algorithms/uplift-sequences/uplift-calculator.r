source("./helpers/sequences-finder.r");
source("./algorithms/interestingness-measure-calculators/support-calculator.r");
source("./algorithms/interestingness-measure-calculators/confidence-calculator.r");
source("./algorithms/interestingness-measure-calculators/coverage-calculator.r");
source("./algorithms/interestingness-measure-calculators/prevalence-calculator.r");
source("./algorithms/interestingness-measure-calculators/recall-calculator.r");
source("./algorithms/interestingness-measure-calculators/lift-calculator.r");
source("./algorithms/interestingness-measure-calculators/leverage-calculator.r");
source("./algorithms/interestingness-measure-calculators/added-value-calculator.r");
source("./algorithms/interestingness-measure-calculators/jaccard-calculator.r");
source("./algorithms/interestingness-measure-calculators/relative-risk-calculator.r");

UpliftCalculator <- function() {
    this <- list();
    class(this) <- append(class(this), "UpliftCalculator");
    return (this);
}

calculate_uplift <- function (uplift_calculator, result_of_mining_sequences, ordered_classes) {
    UseMethod("calculate_uplift", uplift_calculator);
}

calculate_uplift.UpliftCalculator <- function (uplift_calculator, result_of_mining_sequences, ordered_classes) {
    map_change_class <- function (prv_class, class, ordered_classes) {
        condition <- which(ordered_classes == prv_class) < which(ordered_classes == class);
        if(condition) {
          return (c("up"));
        }
        condition <- which(ordered_classes == prv_class) > which(ordered_classes == class);
        if(condition) {
            return (c("down"));
        }
        return (c("none"));
    }

    map_class_to_change_class <- function (ordered_classes, list_of_sequences) {
        result <- list();
        result_id <- 1;
        for (sequence in list_of_sequences) {
            prv_class <- NULL;
            mapped_classes <- c();
            for (class in sequence$sequence$classes) {
                if (is.null(prv_class)) {
                    prv_class <- class;
                    mapped_classes <- c(mapped_classes, c("none"));
                } else {
                    mapped_class <- map_change_class(prv_class, class, ordered_classes);
                    mapped_classes <- c(mapped_classes, mapped_class);
                }
            }
            sequence$sequence$classes <- mapped_classes;
            result[[result_id]] <- sequence;
            result_id <- result_id + 1;
        }
        return (result);
    }

    list_of_frequent_patterns <- result_of_mining_sequences$frequent_patterns;
    summary_df <- result_of_mining_sequences$summary;
    list_of_sequences <- result_of_mining_sequences$sequences;

    O <- length(list_of_sequences);
    print(ordered_classes)
    list_of_sequences <- map_class_to_change_class(ordered_classes, list_of_sequences);
    result <- list();
    result_id <- 1;
    
    vector_of_uplifts <- c();
    vector_of_seq_supports <- c();
    vector_of_seq_confidences <- c();
    vector_of_seq_coverages <- c();
    vector_of_seq_prevelences <- c();
    vector_of_seq_recall <- c();
    vector_of_seq_lift <- c();
    vector_of_seq_leverages <- c();
    vector_of_seq_added_values <- c();
    vector_of_seq_jaccard_values <- c();
    vector_of_seq_relative_risks <- c();
    
    sequences_finder <- SequencesFinder();
    list_of_frequent_patterns_id <- c(1:length(list_of_frequent_patterns));
    
    #other measures
    support_calculator <- SupportCalculator();
    confidence_calculator <- ConfidenceCalculator();
    coverageCalculator <- CoverageCalculator();
    prevelenceCalculator <- PrevelenceCalculator();
    recallCalculator <- RecallCalculator();
    liftCalculator <- LiftCalculator();
    leverageCalculator <- LeverageCalculator();
    addedValueCalculator <- AddedValueCalculator();
    jaccardCalculator <- JaccardCalculator();
    relativeRiskCalculator <- RelativeRiskCalculator();
    
    for (pattern in list_of_frequent_patterns){
        divided_sequences <- divide_sequences_by_pattern(sequences_finder, pattern, list_of_sequences);
        list_of_sequences_with_positive_change_after_pattern <- find_sequences_with_class_after_pattern(sequences_finder, pattern, "up", divided_sequences$with_pattern);
        list_of_sequences_with_positive_change <- find_sequences_with_class(sequences_finder, "up", divided_sequences$without_pattern); 
        
        #P(up|pattern)
        B <- length(divided_sequences$with_pattern);
        AandB <- length(list_of_sequences_with_positive_change_after_pattern);
        
        if ( AandB != 0 && B != 0) {
          Pa <- (AandB / O) / (B / O);
        } else {
          Pa <- 0;
        }

        #P(up|no-pattern)
        B <- length(divided_sequences$without_pattern);
        AandB <- length(list_of_sequences_with_positive_change);
        
        if ( AandB != 0 && B != 0) {
          Pb <- (AandB / O) / (B / O);
        } else {
          Pb <- 0;
        }

        #uplift
        uplift <- Pa - Pb;
        
        #pattern$uplift <- uplift;
        
        support <- calculate(support_calculator, pattern, "up", list_of_sequences);
        confidence <- calculate(confidence_calculator, pattern, "up", list_of_sequences);
        coverage <- calculate(coverageCalculator, pattern, list_of_sequences);
        prevelence <- calculate(prevelenceCalculator, "up", list_of_sequences);
        recall <- calculate(recallCalculator, pattern, "up", list_of_sequences);
        lift <- calculate(liftCalculator, pattern, "up", list_of_sequences);
        leverage <- calculate(leverageCalculator, pattern, "up", list_of_sequences);
        addedValue <- calculate(addedValueCalculator, pattern, "up", list_of_sequences);
        jaccard <- calculate(jaccardCalculator, pattern, "up", list_of_sequences);
        relativeRisk <- calculate(relativeRiskCalculator, pattern, "up", list_of_sequences);
        
        vector_of_uplifts <- c(vector_of_uplifts, uplift);
        vector_of_seq_supports <- c(vector_of_seq_supports, support);
        vector_of_seq_confidences <- c(vector_of_seq_confidences, confidence);
        vector_of_seq_coverages <- c(vector_of_seq_coverages, coverage);
        vector_of_seq_prevelences <- c(vector_of_seq_prevelences, prevelence);
        vector_of_seq_recall <- c(vector_of_seq_recall, recall);
        vector_of_seq_lift <- c(vector_of_seq_lift, lift);
        vector_of_seq_leverages <- c(vector_of_seq_leverages, leverage);
        vector_of_seq_added_values <- c(vector_of_seq_added_values, addedValue);
        vector_of_seq_jaccard_values <- c(vector_of_seq_jaccard_values, jaccard);
        vector_of_seq_relative_risks <- c(vector_of_seq_relative_risks, relativeRisk);

        result[[result_id]] <- pattern;
        result_id <- result_id + 1;
    }
    
    df <- data.frame( 
      sequence = summary_df$sequence, 
      support = summary_df$support, 
      vector_of_uplifts,
      vector_of_seq_supports,
      vector_of_seq_confidences,
      vector_of_seq_coverages,
      vector_of_seq_prevelences,
      vector_of_seq_recall,
      vector_of_seq_lift,
      vector_of_seq_leverages,
      vector_of_seq_added_values,
      vector_of_seq_jaccard_values,
      vector_of_seq_relative_risks
    );
    
    result_of_mining_sequences$summary <- df;
    result_of_mining_sequences$frequent_patterns <- result;
    return (result_of_mining_sequences);
}

