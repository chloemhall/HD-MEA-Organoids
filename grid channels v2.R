#Script to make the grids of channels.
#to work on just one channel
channel_to_index <- function(Ch){
  max.full.rows <- floor(Ch / 64 )
  col.idx <- Ch - (max.full.rows*64)
  row.idx <- (max.full.rows+1)
  Ch.idx <- c(row.idx, col.idx)
  return(Ch.idx)
}
#to work on a list, not a single channel ####
channels_to_index <- function(Ch_list) {
  max.full.rows <- floor(Ch_list / 64 )
  col.idx <- Ch_list - (max.full.rows * 64)
  row.idx <- (max.full.rows + 1)
  Ch.idx <- cbind(row.idx, col.idx)
  return(Ch.idx)
}
# ^^ Already have this function to convert the raw number to the Ch.
# ####

# so I want to get all the channels out per organoid.
#let's begin with organoid 2, p60
o2.p60.ptxselect<- org2_p60_lowPTX$Channels
#this finds channels that were low and then increased with ptx
#now convert these to channel index.
test1<- channels_to_index(o2.p60.ptxselect)
test1 <- as.data.frame(test1)

# Create a 64 x 64 matrix with zeros
grid_matrix <- matrix(0, nrow = 64, ncol = 64)
# Loop through each row of the data frame
for (i in 1:nrow(test1)) {
  row_idx <- test1$row.idx[i]
  col_idx <- test1$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  grid_matrix[row_idx, col_idx] <- 1
}
#now find the channels from the propofol conditions
#let's begin with organoid 2, p60
o2.p60.propofol.select<- org2_p60_lowProp$Channels
#now convert these to channel index.
df.o2.young<- channels_to_index(o2.p60.propofol.select)
df.o2.young <- as.data.frame(df.o2.young)
for (i in 1:nrow(df.o2.young)) {
  row_idx <- df.o2.young$row.idx[i]
  col_idx <- df.o2.young$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  #grid_matrix[row_idx, col_idx] <- 2
  # Update the corresponding cell to 2 to indicate lowProp
  # If the cell is already 1 (from lowPTX), make it 3 (a combination of 1 and 2)
  grid_matrix[row_idx, col_idx] <- ifelse(grid_matrix[row_idx, col_idx] == 1, 3, 2)
}

# Plot the heatmap
heatmap(grid_matrix, Rowv = NA, Colv = NA, col = c("white", "purple", "green", "orange"), scale = "none",
        xlab = "Column Index", ylab = "Row Index", main = "Organoid 2 P60")

#ok but that overrides it.

### section for another org ####
# so I want to get all the channels out per organoid.
#let's begin with organoid 5, p60
o5.p60.ptxselect<- org5_p60_lowPTX$Channels #this finds channels that were low and then increased with ptx
#now convert these to channel index.
test1<- channels_to_index(o5.p60.ptxselect)
test1 <- as.data.frame(test1)

# Create a 64 x 64 matrix with zeros
grid_matrix <- matrix(0, nrow = 64, ncol = 64)
# Loop through each row of the data frame
for (i in 1:nrow(test1)) {
  row_idx <- test1$row.idx[i]
  col_idx <- test1$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  grid_matrix[row_idx, col_idx] <- 1
}
#now find the channels from the propofol conditions
#let's begin with organoid 5, p60
o5.p60.propofol.select<- org5_p60_lowProp$Channels
#now convert these to channel index.
df.o5.young<- channels_to_index(o5.p60.propofol.select)
df.o5.young <- as.data.frame(df.o5.young)
for (i in 1:nrow(df.o5.young)) {
  row_idx <- df.o5.young$row.idx[i]
  col_idx <- df.o5.young$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  #grid_matrix[row_idx, col_idx] <- 2
  # Update the corresponding cell to 2 to indicate lowProp
  # If the cell is already 1 (from lowPTX), make it 3 (a combination of 1 and 2)
  grid_matrix[row_idx, col_idx] <- ifelse(grid_matrix[row_idx, col_idx] == 1, 3, 2)
}

# Plot the heatmap
heatmap(grid_matrix, Rowv = NA, Colv = NA, col = c("white", "purple", "green", "orange"), scale = "none",
        xlab = "Column Index", ylab = "Row Index", main = "Organoid 5 P60")

#ok so no channels more active only in propofol...

#try org 3 p60 ####

# so I want to get all the channels out per organoid.
#let's begin with organoid 3, p60
o3.p60.ptxselect<- org3_p60_lowPTX$Channels #this finds channels that were low and then increased with ptx
#now convert these to channel index.
test1<- channels_to_index(o3.p60.ptxselect)
test1 <- as.data.frame(test1)

# Create a 64 x 64 matrix with zeros
grid_matrix <- matrix(0, nrow = 64, ncol = 64)
# Loop through each row of the data frame
for (i in 1:nrow(test1)) {
  row_idx <- test1$row.idx[i]
  col_idx <- test1$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  grid_matrix[row_idx, col_idx] <- 1
}
#now find the channels from the propofol conditions
#let's begin with organoid 3, p60
o3.p60.propofol.select<- org3_p60_lowProp$Channels
#now convert these to channel index.
test2<- channels_to_index(o3.p60.propofol.select)
test2 <- as.data.frame(test2)
for (i in 1:nrow(test2)) {
  row_idx <- test2$row.idx[i]
  col_idx <- test2$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  #grid_matrix[row_idx, col_idx] <- 2
  # Update the corresponding cell to 2 to indicate lowProp
  # If the cell is already 1 (from lowPTX), make it 3 (a combination of 1 and 2)
  grid_matrix[row_idx, col_idx] <- ifelse(grid_matrix[row_idx, col_idx] == 1, 3, 2)
}

# Plot the heatmap
heatmap(grid_matrix, Rowv = NA, Colv = NA, col = c("white", "purple", "green", "orange"), scale = "none",
        xlab = "Column Index", ylab = "Row Index", main = "Organoid 3 P60")



#try org 4 p60 ####

# so I want to get all the channels out per organoid.
#let's begin with organoid 4, p60
o4.p60.ptxselect<- org4_p60_lowPTX$Channels #this finds channels that were low and then increased with ptx
#now convert these to channel index.
test1<- channels_to_index(o4.p60.ptxselect)
test1 <- as.data.frame(test1)

# Create a 64 x 64 matrix with zeros
grid_matrix <- matrix(0, nrow = 64, ncol = 64)
# Loop through each row of the data frame
for (i in 1:nrow(test1)) {
  row_idx <- test1$row.idx[i]
  col_idx <- test1$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  grid_matrix[row_idx, col_idx] <- 1
}
#now find the channels from the propofol conditions
#let's begin with organoid 4 p60
o4.p60.propofol.select<- org4_p60_lowProp$Channels
#now convert these to channel index.
test2<- channels_to_index(o4.p60.propofol.select)
test2 <- as.data.frame(test2)
for (i in 1:nrow(test2)) {
  row_idx <- test2$row.idx[i]
  col_idx <- test2$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  #grid_matrix[row_idx, col_idx] <- 2
  # Update the corresponding cell to 2 to indicate lowProp
  # If the cell is already 1 (from lowPTX), make it 3 (a combination of 1 and 2)
  grid_matrix[row_idx, col_idx] <- ifelse(grid_matrix[row_idx, col_idx] == 1, 3, 2)
}

# Plot the heatmap
heatmap(grid_matrix, Rowv = NA, Colv = NA, col = c("white", "purple", "green", "orange"), scale = "none",
        xlab = "Column Index", ylab = "Row Index", main = "Organoid 4 P60")

#try org 6 p60 ####

# so I want to get all the channels out per organoid.
#let's begin with organoid 6, p60
o6.p60.ptxselect<- org6_p60_lowPTX$Channels #this finds channels that were low and then increased with ptx
#now convert these to channel index.
test1<- channels_to_index(o6.p60.ptxselect)
test1 <- as.data.frame(test1)

# Create a 64 x 64 matrix with zeros
grid_matrix <- matrix(0, nrow = 64, ncol = 64)
# Loop through each row of the data frame
for (i in 1:nrow(test1)) {
  row_idx <- test1$row.idx[i]
  col_idx <- test1$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  grid_matrix[row_idx, col_idx] <- 1
}
#now find the channels from the propofol conditions
#let's begin with organoid 6, p60
o6.p60.propofol.select<- org6_p60_lowProp$Channels
#now convert these to channel index.
test2<- channels_to_index(o6.p60.propofol.select)
test2 <- as.data.frame(test2)
for (i in 1:nrow(test2)) {
  row_idx <- test2$row.idx[i]
  col_idx <- test2$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  #grid_matrix[row_idx, col_idx] <- 2
  # Update the corresponding cell to 2 to indicate lowProp
  # If the cell is already 1 (from lowPTX), make it 3 (a combination of 1 and 2)
  grid_matrix[row_idx, col_idx] <- ifelse(grid_matrix[row_idx, col_idx] == 1, 3, 2)
}

# Plot the heatmap
heatmap(grid_matrix, Rowv = NA, Colv = NA, col = c("white", "purple", "green", "orange"), scale = "none",
        xlab = "Column Index", ylab = "Row Index", main = "Organoid 6 P60")


#never any green ones... maybe check the code. do a MWE--> input a fake value.

view(test2)
row.idx <- c(61)
col.idx <- c(64)
extra_fake <- data.frame(row.idx, col.idx)
test3 <-rbind(test2, extra_fake)
view(test3)

for (i in 1:nrow(test3)) {
  row_idx <- test3$row.idx[i]
  col_idx <- test3$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  #grid_matrix[row_idx, col_idx] <- 2
  # Update the corresponding cell to 2 to indicate lowProp
  # If the cell is already 1 (from lowPTX), make it 3 (a combination of 1 and 2)
  grid_matrix[row_idx, col_idx] <- ifelse(grid_matrix[row_idx, col_idx] == 1, 3, 2)
}

# Plot the heatmap
heatmap(grid_matrix, Rowv = NA, Colv = NA, col = c("white", "purple", "green", "orange"), scale = "none",
        xlab = "Column Index", ylab = "Row Index", main = "Organoid 6 P60")


