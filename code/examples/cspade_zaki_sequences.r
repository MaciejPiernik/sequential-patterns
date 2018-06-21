setwd("/Users/jachnika/Code/MiningSequencesWithUpliftMeasure/")

library("Matrix");
library("arules");
library("arulesSequences");

source("./algorithms/algorithms-facade.r");
source("./mappers/sequences-mapper.r");

zaki <- read_baskets(con = system.file("misc", "zaki.txt", package = "arulesSequences"), info = c("sequenceID","eventID","SIZE"))
zaki_df <-  as(zaki, "data.frame");
v <- rep("b", nrow(zaki_df));
v[c(4, 6, 7, 10)] <- "a";
zaki_df["class"] <- v;
sequences_mapper <- SequencesMapper();
algorithms_facade <- AlgorithmsFacade();

sequences <- map_data_frame_to_sequences(sequences_mapper, zaki_df, "items", "sequenceID", "eventID", "class");
result <- mine_patterns(algorithms_facade, sequences, 0.4, c("b", "a"))
result$summary