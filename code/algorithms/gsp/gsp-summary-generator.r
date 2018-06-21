GspSummaryGenerator <- function() {
    this <- list();
    class(this) <- append(class(this), "GspSummaryGenerator");
    return (this);
}

generate_summary <- function (gsp, list_of_patterns) {
    UseMethod("generate_summary", gsp);
}

generate_summary.GspSummaryGenerator <- function (gsp, list_of_patterns) {
    sequence <- c();
    support <- c();
    for (pattern in list_of_patterns) {
        items <- c();
        for (item in pattern$sequence$item) {
            pasted_item <- paste(unlist(item), collapse = ",", sep = "");
            pasted_item <- paste("{", pasted_item, "}", collapse = "", sep = "")
            items <- c(items, pasted_item);
        }
        sequence_as_string <- paste(items, collapse = ",", sep = "");
        sequence_as_string <- paste("<", sequence_as_string, ">", collapse = "", sep = "");
        sequence <- c(sequence, sequence_as_string);
        if (is.null(pattern$support) || is.na(pattern$support)) {
            pattern$support <- NA;
        }
        support <- c(support, pattern$support);
    }
    df <- data.frame(sequence = sequence, support = support);
    return (df);
}