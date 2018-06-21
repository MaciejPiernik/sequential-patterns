source("./models/sequence.r");
source("./models/pattern.r");
source("./algorithms/gsp/apriori.r");
source("./algorithms/gsp/gsp-sorter.r");
source("./algorithms/gsp/gsp-finder.r");
source("./algorithms/gsp/gsp-transformator.r");
source("./algorithms/gsp/gsp-sequencer.r");
source("./algorithms/gsp/gsp-summary-generator.r");

GSP <- function() {
    this <- list();
    class(this) <- append(class(this), "GSP");
    return(this);
}

find_frequent_patterns <- function(gsp, list_of_sequences, min_support) {
    UseMethod("find_frequent_patterns", gsp);
}

find_frequent_patterns.GSP <- function(gsp, list_of_sequences, min_support) {
    print("GSP algorithm started...");
    gsp_sorter                          <- GspSorter();
    gsp_finder                          <- GspFinder();
    gsp_transformator                   <- GspTransformator();
    gsp_sequencer                       <- GspSequencer();
    gspSummaryGenerator                 <- GspSummaryGenerator();

    print("Sorting sequences...");

    sorted_sequences                    <- sort_sequences(gsp_sorter, list_of_sequences);

    print("Preparing objects...");

    sequences_as_patterns               <- convert_sequences_to_patterns(gsp, sorted_sequences);

    print("Finding unique elements...");
    
    unique_frequent_patterns            <- find_patterns(gsp_finder, sequences_as_patterns, min_support);
    
    print("Transforming patterns...");
    
    patterns_mapping                    <- transform_patterns(gsp_transformator, unique_frequent_patterns);
    
    print("Transforming sequences...");
    
    transformed_sequences               <- transform_sequences(gsp_transformator, sorted_sequences, patterns_mapping);
    
    print("Sequencing...");
    
    frequent_patterns                   <- do_sequencing(gsp_sequencer, patterns_mapping, transformed_sequences, min_support);
    
    print("Transforming patterns back...");   
    
    transformed_frequent_patterns       <- transform_patterns_back(gsp_transformator, frequent_patterns, patterns_mapping);
    
    print("Generating summary...");
    
    summary                             <- generate_summary(gspSummaryGenerator, transformed_frequent_patterns);
    
    result                              <- list(frequent_patterns = transformed_frequent_patterns, sequences = sequences_as_patterns, summary = summary);
    
    return (result);
}

convert_sequences_to_patterns <- function(gsp, sequences) {
    patterns <- list();
    patterns_id <- 1;
    for(sequence in sequences) {
        pattern <- Pattern(sequence, NULL, NULL);
        patterns[[patterns_id]] <- pattern;
        patterns_id <- patterns_id + 1;
    }
    return (patterns);
}