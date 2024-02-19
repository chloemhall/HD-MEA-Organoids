# Script to make heatmaps of PTX freq - ctrl frequency.
# begin here. Load giant data file.
giant.organoid.data <- read.csv(file.choose(), header=T) # navigate to file all.data.org.csv

# find the top 50 channels from the control conditions.
##### FUNCTION #####
find_top50_channels <- function(df, age, organoid, condition){
  df1 <- df %>%
    filter(Condition %in% c(condition)) %>%
    filter(Age %in% c(age)) %>%
    filter(Organoids %in% c(organoid))

  df2 <-  head(df1[order(-df1$Num.Spikes), ], 50)     # Filter for top 50

  return(df2) #This function subsets the data df to just find the top 50
  #channels from the entered situation.
}

o4_p90_prop <- find_top50_channels(giant.organoid.data, "P90", "four", "Propofol")
o4_p90_ptx <- find_top50_channels(giant.organoid.data, "P90", "four", "PTX")
#I want now to go back to giant.organoid.data and now grab the channels for this organoid.
org4_p90 <- giant.organoid.data %>%
  filter(Age %in% c("P90")) %>%
  filter(Organoids %in% c("four"))

#now make a df with the differences. Take the top 50 from o4_p90_ptx...
subtract_frequency <- function(targetdf, channelsdf){
  #take channels from the channelsdf and extract those channels from targetdf
  # ie in this case channelsdf has the channels you want.
  holding_df <- targetdf %>% filter(Channels %in% c(channelsdf$Channels))
  h2 <- holding_df %>% filter(Condition == "PTX") %>% arrange(Channels)

  h3 <- holding_df %>%  filter(Condition =="Control")%>% arrange(Channels)

   end_df <- inner_join(h2, h3, by = "Channels") %>%
     mutate(Frequency_Difference = Frequency.x - Frequency.y) %>%
     select(Channels, Frequency_Difference)
 #  end_df$Ch.Idx <- channels_to_index(ptx_ctrl$Channels)
 #  colnames(end_df)[3] <- "Row.idx"
#   colnames(end_df)[4] <- "Col.idx"

  return(end_df)
}

ptx_ctrl <-subtract_frequency(org4_p90, o4_p90_ptx)
ptx_ctrl$Ch.Idx <- channels_to_index(ptx_ctrl$Channels)
view(ptx_ctrl)

