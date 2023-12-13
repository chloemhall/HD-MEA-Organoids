# Making functions for the organoid spike counts files. 
library(tidyverse)

read_org_data <- function(spikes_file, channels_file, condition) {
  # Load data
  org_data <- read.table(spikes_file, header = FALSE, sep = "\t")
  organoid_channels <- read.table(channels_file, header = TRUE, sep = "\t")
  
  # Combine data frames
  org_data <- cbind(org_data, organoid_channels)
  
  # Rename columns
  names(org_data)[1] <- "Num.Spikes"
  names(org_data)[2] <- "Channels"
  org_data$Condition <- condition
  
  # Calculate frequency
  time = 292.775
  org_data$Frequency <- org_data$Num.Spikes / time
  return(org_data)
}

process_organoid_data <- function(spikes_file, channels_file, condition) {
  # Load data
  org_data <- read.table(spikes_file, header = FALSE, sep = "\t")
  organoid_channels <- read.table(channels_file, header = TRUE, sep = "\t")
  
  # Combine data frames
  org_data <- cbind(org_data, organoid_channels)
  
  # Rename columns
  names(org_data)[1] <- "Num.Spikes"
  names(org_data)[2] <- "Channels"
  org_data$Condition <- condition
  
  # Calculate frequency
  time = 292.775 #taken from the matrix size / SamplingRate, calculated in Julia. 
  org_data$Frequency <- org_data$Num.Spikes / time
  
  # Remove specific dodgy channel
  org_data <- org_data %>%
    filter(Channels != '3849') #this channel is always saturated, regardless of organoid & drug. 
  
  # Filter for top 50
  top_50 <- head(org_data[order(-org_data$Num.Spikes), ], 50)
  
  return(top_50) 
}

graph_org_top50 <- function (combined_top50, title) {

#make channel a cat variable
  combined_top50$Channels <- as.factor(combined_top50$Channels)

#graph
graph1 <- ggplot(combined_top50, aes(x= Channels, y= Frequency, fill= Condition))+
  geom_col(alpha=0.9, position = "dodge")+
  scale_fill_manual(values = c("#333333", "#9933CC", "#66CC66")) +
  ylim(0, 3.0)+
  theme_bw()+
  labs(title = title, y = "Frequency (Hz)")+
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 5))+
  theme(plot.title = element_text(hjust = 0.5)) 

return(graph1)
}

find_active_channels <- function(org_data){
  active_channels <- subset(org_data, Frequency > 0.5)
  active_channels_by_condition <- data.frame(
    Condition = names(table(active_channels$Condition)),
    Organoids = names(table(active_channels$Organoids)),
    Active.Channels = as.vector(table(active_channels$Condition))
  )
  return(active_channels_by_condition)
}
