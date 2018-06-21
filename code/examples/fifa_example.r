setwd("/Users/jachnika/Code/MiningSequencesWithUpliftMeasure/")

library("Matrix");
library("arules");
library("arulesSequences");
library("foreach")
library("doParallel")

source("./algorithms/algorithms-facade.r");
source("./mappers/sequences-mapper.r");

fifa <- readLines("./data/fifa.txt");



sequence_identifiers <- c(1:length(fifa));

comb <- function(data, ...){
  r <- list(...);
  x <- data.frame(
    r$event_id,
    r$item,
    r$sequence_id,
    r$class
  );
  x
}

cl <- parallel::makeCluster(8);
doParallel::registerDoParallel(cl);
preds <- foreach(sequence_id=sequence_identifiers, .combine='rbind',  .multicombine=TRUE) %dopar% {
  line <- unlist(strsplit(fifa[sequence_id], ' '));
  i <- 0;
  sequence_id_vector <- c();
  event_id_vector <- c();
  item_vector <- c();
  class_vector <- c();
  for(element in line){
    if(element != "-1" && element != "-2"){
      i <- i + 1;
      event_id_vector <- c(event_id_vector, i);
      item_vector <- c(item_vector, element);
      if(i < 16){
        class <- "S";
      } else if (i >= 16 && i < 32){
        class <- "M";
      } else if (i >= 32) {
        class <- "L";
      }
      sequence_id_vector <- c(sequence_id_vector, sequence_id);
      class_vector <- c(class_vector, class);
    }
  }
  data.frame(event_id = event_id_vector, item = item_vector, sequence_id = sequence_id_vector, class = class_vector);
}

parallel::stopCluster(cl);

data <- data.frame(sequence_id_vector, event_id_vector, item_vector, class_vector);

sequences_mapper <- SequencesMapper();
algorithms_facade <- AlgorithmsFacade();

sequences <- map_data_frame_to_sequences(sequences_mapper, preds, "item", "sequence_id", "event_id", "class");
result <- mine_patterns(algorithms_facade, sequences, 0.3, c("S", "M", "L"));
result$summary
