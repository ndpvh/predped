## Generate a Random Distribution of Shelves 

# set seed for reproducibility
set.seed(4355)  

# generate random nr of cols and shelves
num_cols <- sample(1:10, 1) # random nr of cols between 1-10
num_shelves <- max(sample(1:20, 1), num_cols) # random nr of shelves between 1-20

# for checks
num_cols 
num_shelves 

# initialize the distribution of shelves
shelves_dist <- rep(0, num_cols)

# make sure each column has at least one shelf
for (i in 1:min(num_shelves, num_cols)) {
  shelves_dist[i] <- 1 
}

# update the number of remaining shelves
num_shelves <- num_shelves - sum(shelves_dist)

# randomly distribute remaining shelves to columns
for (i in 1:num_shelves) {
  col_index <- sample(1:num_cols, 1)  # randomly select a column to add a shelf
  shelves_dist[col_index] <- shelves_dist[col_index] + 1
}

# print the distribution
data <- data.frame(Columns = 1:num_cols, Num_Shelves = shelves_dist)
print(data)

# plot the distribution
library(ggplot2)

# empty df for storing helf data
shelf_data <- data.frame(Column = integer(), Shelf = integer())

# iterate through each row of the distribution
for (i in 1:nrow(data)) {
  # generate col numbers based on num_shelves in each column
  columns <- rep(data$Columns[i], data$Num_Shelves[i])
  
  # generate shelf numbers based on the num_shelves in each column
  shelves <- 1:data$Num_Shelves[i]
  
  # temporary df for the current distribution
  temp_df <- data.frame(Column = columns, Shelf = shelves)
  
  # combine temp_df with the shelf_data
  shelf_data <- rbind(shelf_data, temp_df)
}

# Plot the shelves
ggplot(shelf_data, aes(x = Column, y = Shelf)) +
  # add padding of 0.5 to the sides for the block
  geom_rect(aes(xmin = Column - 0.5, xmax = Column + 0.5,
                ymin = Shelf - 0.5, ymax = Shelf + 0.5),
            fill = "blue", color = "black") +
  labs(title = "Distribution of Shelves",
       x = "Column",
       y = "Shelf") +
  theme_minimal()