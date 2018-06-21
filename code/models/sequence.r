Sequence <- function(list_of_sequence_items, classes){
    this <- list(
        items = list_of_sequence_items,
        classes = classes
    );
    class(this) <- append(class(this), "Sequence");
    return(this);
}

add_item_to_sequence.Sequence <- function(sequence, item)
{
    l <- length(sequence$items);
    sequence$items[[l + 1]] <- item;
    return(sequence);
}