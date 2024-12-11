# Alternative solution for opgave 2 - Partial Least Squares

library(pls)
DST_PLS <- plsr(Vaekst ~ ., data=PCR_df, scale=TRUE, validation="CV")
summary(DST_PLS)

# Plot cross-validation results to choose the optimal number of components
validationplot(DST_PLS, val.type = "MSEP")


# Make predictions
pred_PLS <- predict(DST_PLS, newdata=weighted_ind, ncomp=1, type="response")
print(pred_PLS)
# 0.2927887 -0.2853662