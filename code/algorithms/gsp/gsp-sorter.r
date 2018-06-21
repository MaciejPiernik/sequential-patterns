GspSorter <- function(){
    this <- list();
    class(this) <- append(class(this), "GspSorter");
    return(this);
}

sort_sequences <- function(gspSorter, list_of_sequences) {
    UseMethod("sort_sequences", gspSorter);
}

sort_sequences.GspSorter <- function(gspSorter, list_of_sequences) {
    result <- list();
    for (index in c(1:length(list_of_sequences))) {
        sequence <- list_of_sequences[[index]];
        sorted_item <- list();
        idx <- 1;
        for (item in sequence$items) {
            sorted_item[[idx]] <- sort(item);
            idx <- idx + 1;
        }
        sorted_sequence <- Sequence(sorted_item, sequence$class);
        result[[index]] <- sorted_sequence;
    }
    return (result);
}