#### Hent data på forbruget
library(dkstat)
forbrug_meta <- dst_meta(table = "NKH1", lang = "da")
forbrug_meta$values

myquery_forbrug <- list(
  TRANSAKT = "P.31 Husholdningernes forbrugsudgifter",
  PRISENHED = "2020-priser, kædede værdier",
  SÆSON = "Sæsonkorrigeret",
  Tid = "*"
)

forbrug_meta <- dst_get_data(table = "NKH1", query = myquery_forbrug, lang = "da")
forbrug_meta <- forbrug_meta[-c(1:4),-c(1:3)]
Vaekst <- diff(log(as.numeric(forbrug_meta$value)), lag = 4)*100
forbrug_meta$Vaekst <- c(rep(NA,4),Vaekst)


# Adjust the two datasets so they have the same length
forbrug_period <- forbrug_meta[-c(1:36),]
row.names(forbrug_period) <- NULL



#### Adjust the combinations to a dataframe
comb_df <- do.call(cbind, comb_list)

# Set the column names using the stored names
colnames(comb_df) <- unlist(comb_names)

#### Calculate R^2

# Initialize a vector to store R² values
r_squared_values <- numeric(ncol(comb_df))

# Loop through each combination in the data frame
for (i in 1:ncol(comb_df)) {
  # Create a linear model with consumption as the dependent variable
  model <- lm(forbrug_period$Vaekst ~ comb_df[, i])
  
  # Store the R² value
    r_squared_values[i] <- summary(model)$r.squared
}

# Find the index of the best R²
best_r_squared_index <- which.max(r_squared_values)

# Get the best R² value
best_r_squared <- r_squared_values[best_r_squared_index]

comb_column_name <- colnames(comb_df)[best_r_squared_index]
print(comb_column_name)

# Make a R^2 dataframe
R2_df <- data.frame(comb_names = unlist(comb_names),
                    r_squared_values = r_squared_values)

library(tidyverse)
R2_df <- arrange(R2_df, desc(r_squared_values))



###################### Check significance
model_666 <- lm(forbrug_period$Vaekst ~ comb_df[,666])
model_666_2 <- lm(forbrug_period$Vaekst ~ ftillid_K_period$Q3_Dan_nu + ftillid_K_period$Q9_Forbrugsgoder_etår + ftillid_K_period$Q11_Sparop_etår + ftillid_K_period$Q12_Famøk_nu)
summary(model_666)
summary(model_666_2)



##### Historic average of realvæksten
mean(forbrug_period$Vaekst)
