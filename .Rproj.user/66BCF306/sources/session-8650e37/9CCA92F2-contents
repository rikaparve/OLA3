# What data do I need to use?
comb_df3 <- as.data.frame(comb_df)
comb_df3 <- comb_df3[order(as.numeric(rownames(comb_df3)), decreasing = TRUE),]

forbrug_period3 <- forbrug_period
forbrug_period3 <- forbrug_period3[order(as.numeric(rownames(forbrug_period3)), decreasing = TRUE),]

#### Stability test

# Initialize a list to store R² values for each iteration
r_squared_df2 <- NULL
r_squared_df2 <- list()

# Repeat the process until only one row is left
while (nrow(comb_df3) > 63) {
  
  # Initialize a vector to store R² values for the current iteration
  opt_r_squared_values2 <- numeric(ncol(comb_df3))
  
  # Loop through each column in the data frame
  for (i in 1:ncol(comb_df3)) {
    # Create a linear model with Vaekst as the dependent variable and each column as an independent variable
    opt_model2 <- lm(forbrug_period3$Vaekst ~ comb_df3[, i])
    
    # Store the R² value in the vector at index i
    opt_r_squared_values2[i] <- summary(opt_model2)$r.squared
  }
  
  # Add the vector of R² values to the r_squared_df2 list for this iteration
  r_squared_df2[[length(r_squared_df2) + 1]] <- opt_r_squared_values2
  
  # Remove the last row from both data frames for the next iteration
  comb_df3 <- comb_df3[-nrow(comb_df3), ]
  forbrug_period3 <- forbrug_period3[-nrow(forbrug_period3), ]
}

r_squared_df2 <- as.data.frame(r_squared_df2)

#Create a vector of new column names
num_columns2 <- ncol(r_squared_df2)
new_column_names2 <- paste0("forklaringsgrad", seq(from = 99, by = -1, length.out = num_columns))

colnames(r_squared_df2) <- new_column_names2
row.names(r_squared_df2) <- comb_names


# Calculate the averages
r_squared_df2$GNS <- NULL
r_squared_df2$GNS <- rowMeans(r_squared_df2)

# Calculate standard deviation
r_squared_df2$StD <- NULL
r_squared_df2$StD <- apply(r_squared_df2[, 1:35], 1, sd)
unique <- unique(r_squared_df2$StD) # check everything is OK

# Make a new dataframe with the new values
optimal_r_squared2 <- r_squared_df2[,c(37:38)]

# Add index value
optimal_r_squared2$index <- NULL
optimal_r_squared2$index <- optimal_r_squared2$GNS/optimal_r_squared2$StD

# Add rankings for each existing column
library(tidyverse)
optimal_r_squared2 <- arrange(optimal_r_squared2, desc(GNS))
optimal_r_squared2$GNS_ranking <- c(1:4095)

optimal_r_squared2 <- arrange(optimal_r_squared2, StD)
optimal_r_squared2$StD_ranking <- c(1:4095)

optimal_r_squared2 <- arrange(optimal_r_squared2, desc(index))
optimal_r_squared2$index_ranking <- c(1:4095)

which(row.names(optimal_r_squared2) == "Q3_Dan_nu, Q9_Forbrugsgoder_etår, Q11_Sparop_etår, Q12_Famøk_nu")
print(optimal_r_squared2[1841,1:6])
# GNS 0.4724  StD 0.0237  index 19,91  R^2 forskel  ???
#         6      3162       1841