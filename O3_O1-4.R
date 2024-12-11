### Predict 3. og 4. quarter

# model for the optimal indicator
optimal_indicator <- data.frame(
  vaekst=forbrug_period$Vaekst,
  combi = comb_df[,best_r_squared_index])

optimal_model <- lm(vaekst ~ combi, data=optimal_indicator)
summary(optimal_model)

#indicator values for Q3 2024 and Oct 2024 (optimal and DI)
opt_ind_Q3 <- mean(c(ftillid_K$Q3_Dan_nu[115],ftillid_K$Q9_Forbrugsgoder_etår[115],
                     ftillid_K$Q11_Sparop_etår[115],ftillid_K$Q12_Famøk_nu[115]))

opt_ind_Q4 <- mean(c(ftillid_wide$Q3_Dan_nu[346:347],ftillid_wide$Q9_Forbrugsgoder_etår[346:347],
                      ftillid_wide$Q11_Sparop_etår[346:347],ftillid_wide$Q12_Famøk_nu[346:347]))

opt_ind <- data.frame(combi=c(opt_ind_Q4))

# Make a prediction
pred_optimal <- predict(optimal_model, newdata=opt_ind, type="response")
print(pred_optimal)
# 0,7280038 



##################### Correlation
library(corrplot)
cor_matrix <- cor(y=optimal_indicator$vaekst, x=optimal_indicator$combi)

cor_matrix_Q <- cor(forbrug_period$Vaekst, ftillid_K_period[,3:14])
corrplot(cor_matrix_Q,
         method="number")
