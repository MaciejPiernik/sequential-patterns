setwd("/Users/jachnika/Code/MiningSequencesWithUpliftMeasure/")
source("./algorithms/algorithms-facade.r");
source("./mappers/sequences-mapper.r");



data <- read.csv("./data/events.csv");
rows <- data[,c(1,2)];
datetime <- c();
i <- 1;
for(i in c(1:nrow(rows))) {
    row <- rows[i,];
    v <- unlist(strsplit(paste(paste(unlist(strsplit(as.character(row$Date), '-')), collapse = ',', sep= ','), paste(unlist(strsplit(as.character(gsub('H', '', gsub('M', '', gsub('S', '', row$Time)))), " ")), collapse = ',', sep= ','), collapse = ',', sep= ','), ','))
    dt <- ISOdate(v[1], v[2], v[3], v[4], v[5], v[6]);
    datetime <- c(datetime, dt);
}
data$Timestamp <- datetime;
data <- data[with(data, order(Timestamp)), ];
EventId <- c(1:nrow(data));

df <- data.frame(Event = data$Event, UserId = data$UserId, Index = EventId, Action = data$Action);

sequences_mapper <- SequencesMapper();
algorithms_facade <- AlgorithmsFacade();
sequences <- map_data_frame_to_sequences(sequences_mapper, df, "Event", "UserId", "Index", "Action");

gsp <- GSP();
uplift_calculator <- UpliftCalculator();
frequent_patterns_result <- find_frequent_patterns(gsp, sequences, 0.4);
uplift_result <- calculate_uplift(uplift_calculator, frequent_patterns_result, c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14"));
uplift_result$summary
uplift_result$summary[with(uplift_result$summary, order(uplift, decreasing = F)), ]
