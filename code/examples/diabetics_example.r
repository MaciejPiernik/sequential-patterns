setwd("/Users/jachnika/Code/MiningSequencesWithUpliftMeasure/")

source("./algorithms/algorithms-facade.r");
source("./mappers/sequences-mapper.r");

index <- c(1:70);
sequenceIndex <- 1;
code <- c();
value <- c();
sequenceId <- c();
eventId <- c();

for(i in index) {
    file_number <- NULL;
    if(i < 10){
        file_number <- paste("0", i, sep='', collapse = '');
    } else {
        file_number <- as.character(i);
    }
    file_name <- paste("data-", file_number);
    file_path <- paste("./data/diabetics/", file_name);
    data <- read.table(gsub(" ", "", file_path), sep="\t");
    data$SequenceId <- i;
    for(date in unique(data$V1)){
        dates <- which(data$V1 == date);
        data$SequenceId[dates] <- sequenceIndex;
        sequenceIndex <- sequenceIndex + 1;
    }
    data$EventId <- c(1:nrow(data));
    code <- c(code, data$V3);
    
    data$V4 <- as.numeric(data$V4);

    high <- unique(data$V4[data$V4 > 200]);
    low <- unique(data$V4[data$V4 < 80]);
    normal <- unique(data$V4[(data$V4 >= 80) & (data$V4 <= 200)]);
    high <- which(data$V4 %in% high);
    low <- which(data$V4 %in% low);
    normal <- which(data$V4 %in% normal);

    data$V4[high] <- "H";
    data$V4[low] <- "L";
    data$V4[normal] <- "N";

    value <- c(value, data$V4);
    sequenceId <- c(sequenceId, data$SequenceId);
    eventId <- c(eventId, data$EventId);
} 

df <- data.frame(sequence = sequenceId, event = eventId, code = code, value = value);

sequences_mapper <- SequencesMapper();
algorithms_facade <- AlgorithmsFacade();

sequences <- map_data_frame_to_sequences(sequences_mapper, df, "code", "sequence", "event", "value");

gsp <- GSP();
uplift_calculator <- UpliftCalculator();
frequent_patterns_result <- find_frequent_patterns(gsp, sequences, 0.5);
uplift_result <- calculate_uplift(uplift_calculator, frequent_patterns_result, c("H", "N", "L"));

uplift_result$summary[with(uplift_result$summary, order(uplift, decreasing = T)), ]
