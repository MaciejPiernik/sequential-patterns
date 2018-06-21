source('./algorithms/generator-of-sequences/generator-parameters.r');
source('./algorithms/gsp/gsp.r');
source('./algorithms/uplift-sequences/uplift-calculator.r');

AlgorithmsFacade <- function(){
    this <- list();
    class(this) <- append(class(this), "AlgorithmFacade");
    return(this);
}

mine_patterns <- function (algorithms_facade, sequences, min_support, positive_classes, negative_classes) {
    UseMethod("mine_patterns", algorithms_facade);
}

mine_patterns.AlgorithmFacade <- function (algorithms_facade, sequences, min_support, ordered_classes) {
    gsp <- GSP();
    uplift_calculator <- UpliftCalculator();
    frequent_patterns_result <- find_frequent_patterns(gsp, sequences, min_support);
    uplift_result <- calculate_uplift(uplift_calculator, frequent_patterns_result, ordered_classes);
    return (uplift_result);
}