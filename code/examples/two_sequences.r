setwd("/Users/jachnika/Code/MiningSequencesWithUpliftMeasure/")

library("Matrix");
library("arules");
library("arulesSequences");

source("./algorithms/algorithms-facade.r");
source("./mappers/sequences-mapper.r");

classes <- read.csv("./data/classes.csv");
events <- read.csv("./data/events.csv");

colnames(classes) <- c("ordering_attr", "class_attr", "Product", "sequence_attr")
colnames(events) <- c("ordering_attr", "Time", "sequence_attr", "Event", "Action")

sequence_mapper <- SequencesMapper();
events <- map_two_sequences(sequence_mapper, events, classes)

events$id <- seq.int(nrow(events));
sequences <- map_data_frame_to_sequences(sequence_mapper, events[!is.na(events$class_attr),], "Event", "sequence_attr", "id", "class_attr");

algorithms_facade <- AlgorithmsFacade();
result <- mine_patterns(algorithms_facade, sequences, 0.45, c("A", "B", "C"))

result$summary[with(result$summary, order(uplift, decreasing = T)), ] 
result$summary