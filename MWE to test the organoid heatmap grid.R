#troubleshooting the 64 x 64 matrix
#firstly make some fake data
# data I want to be just purple ie. low in ctrl, high in PTX
row.idx <- c(5, 7, 9, 21, 29, 50)
col.idx <- c(15, 37, 49, 19, 27, 50)
fake1 <-   data.frame(row.idx, col.idx)

#data I want to be just green ie. low in ctrl, and PTX, but high in propofol
row.idx<- c(2, 12, 22, 32, 42, 44, 50)
col.idx <- c(32, 14, 38, 22, 60, 14, 50)
fake2 <- data.frame(row.idx, col.idx)
#data I want to be orange i.e. low in ctrl, high in both PTX and propofol...
# ... the last number of each ie. 50, 50
fake12 <- rbind(fake1, fake2)


# Create a 64 x 64 matrix with zeros
matrix <- matrix(0, nrow = 64, ncol = 64)
# Loop through each row of the data frame
for (i in 1:nrow(fake1)) {
  row_idx <- fake1$row.idx[i]
  col_idx <- fake1$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  matrix[row_idx, col_idx] <- 1
}
for (i in 1:nrow(fake2)) {
  row_idx <- fake2$row.idx[i]
  col_idx <- fake2$col.idx[i]
  # Set the corresponding cell to 1 to highlight it
  #grid_matrix[row_idx, col_idx] <- 2
  # Update the corresponding cell to 2 to indicate lowProp
  # If the cell is already 1 (from lowPTX), make it 3 (a combination of 1 and 2)
  matrix[row_idx, col_idx] <- ifelse(matrix[row_idx, col_idx] == 1, 3, 2)
}


# Plot the heatmap
heatmap(matrix[nrow(matrix):1,], Rowv = NA, Colv = NA, col = c("white", "purple", "green", "orange"), scale = "none",
        xlab = "Column Index", ylab = "Row Index", main = "TEST")

# oh ok! It does work. It must be that there
#just are not any channels that were low in ctrl and ptx but high in propofol...
