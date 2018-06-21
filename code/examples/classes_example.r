setwd("/Users/jachnika/Code/MiningSequencesWithUpliftMeasure/")

library("Matrix");
library("arules");
library("arulesSequences");

source("./algorithms/algorithms-facade.r");
source("./mappers/sequences-mapper.r");


data <- read.csv("./data/classes.csv");
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

