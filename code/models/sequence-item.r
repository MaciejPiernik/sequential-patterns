SequenceItem <- function(vector){
    this <- vector;
    class(this) <- append(class(this), "SequenceItem");
    return(this);
}