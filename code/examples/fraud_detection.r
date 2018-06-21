setwd("/Users/jachnika/Code/MiningSequencesWithUpliftMeasure/")

library("Matrix");
library("arules");
library("arulesSequences");

source("./algorithms/algorithms-facade.r");
source("./mappers/sequences-mapper.r");


data <- read.csv("./data/fraud_detection.csv");

frauds <- data[data$step == 1,];

sequence_id <- c(1:nrow(frauds));

frauds$sequence_id <- sequence_id;

step <- data[data$step >= 2,];
sequence_id <- max(sequence_id)
sequence_id <- sequence_id + 1;


for(idx in c(1:nrow(step))){
  row <- step[idx,];
  sequence <- subset(step, step <= row$step && as.character(nameDest) == as.character(row$nameOrig))
  if(nrow(sequence) == 0) {
    row$sequence_id <- sequence_id;
    sequence_id <- sequence_id + 1;
  } else {
    row$sequence_id = sequence$sequence_id;
  }
  rbind(frauds, row);
}



data <- data[with(data, order(Date)), ];
EventId <- c(1:nrow(data));
data["EventId"] <- EventId;

sequences_mapper <- SequencesMapper();
algorithms_facade <- AlgorithmsFacade();
sequences <- map_data_frame_to_sequences(sequences_mapper, data, "Product", "UserId", "EventId", "Class");

algorithms_facade <- AlgorithmsFacade();
result <- mine_patterns(algorithms_facade, sequences, 0.17, c("A", "B", "C"))

result$summary[with(result$summary, order(uplift, decreasing = T)), ] 
result$summary

