
classes <- read.csv("./data/classes.csv");
events <- read.csv("./data/events.csv");

head(classes)
head(events)

unique(classes$Class) # A, B, C so 3
unique(events$Action) # 7, 4, 10, 12, 6, 2, 1, 8, 9, 11, 13, 3, 14, 5 so 14

length(unique(classes$Product)) # 22
length(unique(events$Event)) # 114

length(unique(classes$UserId)) #10685
length(unique(events$UserId)) #4656

#avg sequence length classes  26.5
#avg sequence length events   21.2


#sequences in classes 10685
#sequences in event 4656