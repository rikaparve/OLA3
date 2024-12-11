# What data do I need to use?
comb_df2 <- as.data.frame(comb_df)
forbrug_period2 <- forbrug_period

#### Stability test

# Initialize a list to store R² values for each iteration
r_squared_df <- NULL
r_squared_df <- list()

# Repeat the process until only one row is left
while (nrow(comb_df2) > 63) {
  
  # Initialize a vector to store R² values for the current iteration
  opt_r_squared_values <- numeric(ncol(comb_df2))
  
  # Loop through each column in the data frame
  for (i in 1:ncol(comb_df2)) {
    # Create a linear model with Vaekst as the dependent variable and each column as an independent variable
    opt_model <- lm(forbrug_period2$Vaekst ~ comb_df2[, i])
    
    # Store the R² value in the vector at index i
    opt_r_squared_values[i] <- summary(opt_model)$r.squared
  }
  
  # Add the vector of R² values to the r_squared_df list for this iteration
  r_squared_df[[length(r_squared_df) + 1]] <- opt_r_squared_values
  
  # Remove the last row from both data frames for the next iteration
  comb_df2 <- comb_df2[-nrow(comb_df2), ]
  forbrug_period2 <- forbrug_period2[-nrow(forbrug_period2), ]
}

r_squared_df <- as.data.frame(r_squared_df)

#Create a vector of new column names
num_columns <- ncol(r_squared_df)
new_column_names <- paste0("forklaringsgrad", seq(from = 99, by = -1, length.out = num_columns))

colnames(r_squared_df) <- new_column_names
row.names(r_squared_df) <- comb_names


###### DOUBLECHECK
r_squared_df2 <- list()

# Gendan dataframerne
comb_df2 <- as.data.frame(comb_df)
forbrug_period2 <- forbrug_period

comb_df2 <- comb_df2[-c(91:nrow(comb_df2)), ]
forbrug_period2 <- forbrug_period2[-c(91:nrow(forbrug_period2)), ]

for(i in 1:ncol(comb_df2)) {
model <- lm(forbrug_period2$Vaekst ~ comb_df2[,i])
r_squared_values[i] <- summary(model)$r.squared
}

test = ifelse(r_squared_values == r_squared_df$forklaringsgrad90, T, F)
sum(test)

# Calculate the averages
r_squared_df$GNS <- NULL
r_squared_df$GNS <- rowMeans(r_squared_df)

# Calculate standard deviation
r_squared_df$StD <- NULL
r_squared_df$StD <- apply(r_squared_df[, 1:35], 1, sd)
unique <- unique(r_squared_df$StD) # check everything is OK

# Make a new dataframe with the new values
optimal_r_squared <- r_squared_df[,c(37:38)]

# Add index value
optimal_r_squared$index <- NULL
optimal_r_squared$index <- optimal_r_squared$GNS/optimal_r_squared$StD

# Add rankings for each existing column
library(tidyverse)
optimal_r_squared <- arrange(optimal_r_squared, desc(GNS))
optimal_r_squared$GNS_ranking <- c(1:4095)

optimal_r_squared <- arrange(optimal_r_squared, StD)
optimal_r_squared$StD_ranking <- c(1:4095)

optimal_r_squared <- arrange(optimal_r_squared, desc(index))
optimal_r_squared$index_ranking <- c(1:4095)

which(row.names(optimal_r_squared) == "Q3_Dan_nu, Q9_Forbrugsgoder_etår, Q11_Sparop_etår, Q12_Famøk_nu")
print(optimal_r_squared[159,1:3])
# GNS 0.385 StD 0.0215 index 17,918  R^2 forskel  -0.078
#         53      667       159

###### Predict Q3 & Q4
print(row.names(optimal_r_squared)[1:7])
stab_ind_all <- c("Q3_Dan_nu, Q5_Forbrugsgoder_nu, Q9_Forbrugsgoder_etår, Q11_Sparop_etår, Q12_Famøk_nu",
                  "Q2_Fam_etår, Q3_Dan_nu, Q5_Forbrugsgoder_nu, Q9_Forbrugsgoder_etår, Q11_Sparop_etår, Q12_Famøk_nu",
                  "Q2_Fam_etår, Q3_Dan_nu, Q5_Forbrugsgoder_nu, Q7_Priser_etår, Q9_Forbrugsgoder_etår, Q10_Sparop_nu, Q11_Sparop_etår, Q12_Famøk_nu",
                  "Q3_Dan_nu, Q7_Priser_etår, Q9_Forbrugsgoder_etår, Q10_Sparop_nu, Q11_Sparop_etår",                                                  
                  "Q1_Fam_nu, Q2_Fam_etår, Q3_Dan_nu, Q4_Dan_etår, Q5_Forbrugsgoder_nu, Q7_Priser_etår, Q9_Forbrugsgoder_etår, Q10_Sparop_nu, Q11_Sparop_etår, Q12_Famøk_nu",
                  "Q1_Fam_nu, Q2_Fam_etår, Q3_Dan_nu, Q5_Forbrugsgoder_nu, Q7_Priser_etår, Q8_Arbejdsløshed, Q9_Forbrugsgoder_etår, Q10_Sparop_nu, Q11_Sparop_etår",
                  "Q3_Dan_nu, Q5_Forbrugsgoder_nu, Q7_Priser_etår, Q9_Forbrugsgoder_etår, Q10_Sparop_nu, Q11_Sparop_etår")

stab_ind_all_indeces <- which(comb_names %in% stab_ind_all)

stab_ind <- as.data.frame(comb_df[,stab_ind_all_indeces])
stab_ind$vaekst <- forbrug_period$Vaekst

stab_column_names <- paste0("Stability_ind", seq(from = 1, by = 1, length.out = 7))
colnames(stab_ind) <- c(stab_column_names, "vaekst")

# Prediction indicator values
prediction_stab_ind <- data.frame(matrix(data=NA, nrow=2, ncol=7))
colnames(prediction_stab_ind) <- stab_column_names  # Assuming stab_column_names is defined

# Define the indicator combinations
stab_combinations <- list(
  stab_combi1 = c(3, 5, 9, 11, 12),
  stab_combi2 = c(2, 3, 5, 7, 9, 10, 11, 12),
  stab_combi3 = c(2, 3, 5, 9, 11, 12),
  stab_combi4 = c(3, 5, 7, 9, 10, 11),
  stab_combi5 = c(1, 2, 3, 4, 5, 7, 9, 10, 11, 12),
  stab_combi6 = c(3, 7, 9, 10, 11),
  stab_combi7 = c(2, 3, 7, 9, 10, 11)
)

# Remove the time and FTI columns
ftillid_K_stab <- ftillid_K[, -c(1, 2)]
ftillid_wide_stab <- ftillid_wide[, -c(1, 2)]

# Calculate stability indicators from ftillid_K_stab and ftillid_wide_stab
for (i in seq_along(stab_combinations)) {
  comb <- stab_combinations[[i]]
  
  prediction_stab_ind[1,i] <- sum(ftillid_K_stab[115, comb]) / length(comb)
  prediction_stab_ind[2,i] <- sum(ftillid_wide_stab[346, comb]) / length(comb)
}

# Fit the model for each stability indicator and store predictions
for (i in seq_along(stab_combinations)) {
  stability_ind_column <- paste0("Stability_ind", i)
  
  # Fit the linear model
  stab_ind_model <- lm(vaekst ~ get(stability_ind_column), data = stab_ind)
  
  # Generate and store the predictions
  predictions <- predict(stab_ind_model, newdata = prediction_stab_ind)
  prediction_stab_ind[[stability_ind_column]] <- predictions
}


######## Add a calculation of gns r^2 - full period r^2
r_squared_df <- arrange(r_squared_df, desc(GNS))
optimal_r_squared$R2_forskel <- optimal_r_squared$GNS - r_squared_df$forklaringsgrad98

