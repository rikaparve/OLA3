# Find den bedste indikator, der alene består af mikroøkonomiske spørgsmål i 
# forbrugertillidsundersøgelsen og sammenlign indikatoren med jeres tidligere svar i opgave 1. 

# Q1_Fam_nu - micro                    
# Q2_Fam_etår - micro                    
# Q3_Dan_nu - macro                  
# Q4_Dan_etår - macro                          
# Q5_Forbrugsgoder_nu - macro
# Q6_Priser_nu - macro                                      
# Q7_Priser_etår - macro                                                 
# Q8_Arbejdsløshed - macro                                 
# Q9_Forbrugsgoder_etår - micro
# Q10_Sparop_nu - macro 
# Q11_Sparop_etår - micro    
# Q12_Famøk_nu - micro

lv <- colnames(ftillid_K_period) %in% c("Q1_Fam_nu", "Q2_Fam_etår", "Q9_Forbrugsgoder_etår", "Q11_Sparop_etår", "Q12_Famøk_nu")
micro_df <- ftillid_K_period[,lv]

#Create the lists
micro_comb_list <- list()
micro_comb_names <- list()

micro_n_cols <- ncol(micro_df)

# Loop over all combination sizes (from 1 to n_cols)
for (j in 1:micro_n_cols) {
  micro_combinations <- combn(1:micro_n_cols, j, simplify = FALSE)
  
  # Calculate row means for each combination of columns
  for (cols in micro_combinations) {
    micro_avg_value <- rowMeans(micro_df[, cols, drop = FALSE], na.rm = TRUE)  # Calculate the average for the current combination
    micro_comb_list[[length(micro_comb_list) + 1]] <- micro_avg_value  # Store results
    
    # Store the combination name as a character string
    micro_comb_names[[length(micro_comb_names) + 1]] <- paste(colnames(micro_df)[cols], collapse = ", ")
  }
}

# Check the number of combinations generated
length(micro_comb_list)  # Should be 31

micro_comb_list <- as.data.frame(micro_comb_list)
colnames(micro_comb_list) <- micro_comb_names

#### Calculate R^2

# Initialize a vector to store R² values
micro_r_squared_values <- numeric(ncol(micro_comb_list))

# Loop through each combination in the data frame
for (i in 1:ncol(micro_comb_list)) {
  # Create a linear model with consumption as the dependent variable
  micro_model <- lm(forbrug_period$Vaekst ~ micro_comb_list[, i])
  
  # Store the R² value
  micro_r_squared_values[i] <- summary(micro_model)$r.squared
}

# Find the index of the best R²
micro_best_r_squared_index <- which.max(micro_r_squared_values)

# Get the best R² value
micro_best_r_squared <- micro_r_squared_values[micro_best_r_squared_index]

micro_comb_column_name <- colnames(micro_comb_list)[micro_best_r_squared_index]
print(micro_comb_column_name)

# model for the optimal indicator
micro_indicator <- data.frame(
  vaekst=forbrug_period$Vaekst,
  combi = micro_df[,micro_best_r_squared_index])

micro_opt_model <- lm(vaekst ~ combi, data=micro_indicator)
summary(micro_opt_model)

micro_ind_Q3 <- ftillid_K$Q9_Forbrugsgoder_etår[115]

micro_ind_Q4 <- mean(c(ftillid_wide$Q9_Forbrugsgoder_etår[346:347]))

micro_ind <- data.frame(combi=c(micro_ind_Q4))

pred_micro <- predict(micro_opt_model, newdata=micro_ind, type="response")
print(pred_micro)
# 1,07




