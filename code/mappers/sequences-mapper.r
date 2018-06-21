source("./models/sequence.r");

SequencesMapper <- function() {
    this <- list();
    class(this) <- append(class(this), "SequencesMapper");
    return (this);
}

map_data_frame_to_sequences <- function (sequences_mapper, df, items_col_name, sequence_id_col_name, event_id_col_name, class_col_name) {
    UseMethod("map_data_frame_to_sequences", sequences_mapper);
}

map_data_frame_to_sequences.SequencesMapper <- function(sequences_mapper, df, items_col_name, sequence_id_col_name, event_id_col_name, class_col_name) {
    sequence_id_col_idx <- grep(sequence_id_col_name, colnames(df))
    event_id_col_idx    <- grep(event_id_col_name, colnames(df));
    items_col_idx       <- grep(items_col_name, colnames(df));
    class_col_idx       <- grep(class_col_name, colnames(df));
    df <- df[order(df[, sequence_id_col_idx], df[, event_id_col_idx]),];  
    sequences <- list();

    for(i in 1:nrow(df)) {
        row <- df[i,];
        elements <- row[, items_col_idx];
        elements <- unlist(strsplit(as.character(elements), ","));
        elements <- gsub('\\{', '', elements)
        elements <- gsub('\\}', '', elements)
        sequence_identifier <- as.character(row[sequence_id_col_idx]);
        if(!is.null(sequences[[sequence_identifier]])){
            s <- sequences[[sequence_identifier]];
            class_value <- as.character(unlist(row[class_col_idx]));
            len <- length(s$items);
            s$items[[len + 1]] <- elements;
            s$classes <- c(s$classes, class_value);
            sequences[[sequence_identifier]] <- s;
        } else {
            tmp <- list();
            tmp[[1]] <- elements;
            c <- as.character(unlist(row[class_col_idx]));
            s <- Sequence(tmp, c);
            sequences[[sequence_identifier]] <- s;
        }
    }
    return(sequences);
}

map_two_sequences <- function(sequences_mapper, events, classes){
    UseMethod("map_two_sequences", sequences_mapper);
}

map_two_sequences.SequencesMapper <- function(sequences_mapper, events, classes) {
    # method requires two data frames with following attributes:
    # - sequence_attr
    # - ordering_attr
    # moreover, the classes data frame requires following attribute:
    # - class_attr
    # the ordering attribute can be one of two data types:
    # - numeric
    # - date
    # - otherwise, the function will throw exception
    
    isDate <- function(date) {
      tryCatch(!is.na(as.POSIXct(as.character(date))),  
               error = function(err) {FALSE})  
    }
    
    isNumeric <- function(number){
      tryCatch(!is.na(as.numeric(number)),  
               error = function(err) {FALSE})  
    }
    
    #assign proper method based on ordering_attr data type
    convertDateValues <- function(ordinal_val){
      val <- as.POSIXct(as.character(ordinal_val));
      val;
    }
    
    convertNumericValues <- function(ordinal_val){
      val <- as.numeric(as.character(ordinal_val));
      val;
    }
    
    if(isDate(events[1, "ordering_attr"])) {
      convert <- convertDateValues;
      print('Ordering attribute is date')
    } else if (isNumeric(events[1, ordering_attr])) {
      convert <- convertNumericValues;
      print('Ordering attribute is numeric')
    } else {
      stop("Unsupported data type of ordering_attr");
    }
    
    events <- events[order(events$sequence_attr, decreasing = T),]
    classes <- classes[order(events$sequence_attr, decreasing = T),]
    
    #foreach event
    events_idx <- c(1:nrow(events));
    for(idx in events_idx){
      
      #get an event assigned with current index
      event <- events[idx, ];
      event_sequence_id <- event[, "sequence_attr"];
      event_ordinal_val <- convert(event[, "ordering_attr"]);
      
      #limit the classes values to the given sequence
      classes_of_the_sequence <- classes[classes[,"sequence_attr"] == event_sequence_id,];
      classes_of_the_sequence_before_event <- classes_of_the_sequence[convert(classes_of_the_sequence[, "ordering_attr"]) < event_ordinal_val,];
      ordered_classes_of_the_sequence_before_event <- classes_of_the_sequence_before_event[order(classes_of_the_sequence_before_event$ordering_attr, decreasing = T),];
      class_value <- ordered_classes_of_the_sequence_before_event[1, "class_attr"];
      
      events[idx, "class_attr"] <- class_value;
    }
    return(events);
}