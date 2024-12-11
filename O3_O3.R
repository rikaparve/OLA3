library(caret)

#### Make a new dataframe
DST_ind <- data.frame(TID=ftillid_K_period$TID,
                      DST_FTI=ftillid_K_period$DST_FTI,
                      Vaekst=forbrug_period$Vaekst)

str(DST_ind)

# Add a new column with up or down
DST_ind$Op_ned <- ifelse(DST_ind$Vaekst >= 0, 1, 0)
freq_opned <- data.frame(table(DST_ind$Op_ned))


# Run the logistic model
DST_ind$Op_ned <- as.factor(DST_ind$Op_ned)
logistic_model <- glm(Op_ned ~ DST_FTI, data=DST_ind, family="binomial")
summary(logistic_model)

# Make a prediction
newdata <- data.frame(DST_FTI=c(mean(c(ftillid_wide[346:347,2]))))
prediction <- predict(logistic_model, newdata=newdata, type="response")
prediction

if (prediction > 0.6) {
  prediction_Q4 <- "op"
} else {
  prediction_Q4 <- "ned"
}
print(prediction_Q4)
    

# Make a column with predictions
predict_DST <- data.frame(DST_FTI=DST_ind$DST_FTI)
predictions_jul <- predict(logistic_model, predict_DST, type="response")

# Lav en vector for sandsynligheder
prediction_jul <- c()

# Loop  for fortolkning af sandsynligheder
for (i in 1:length(predictions_jul)) {
  
  # Check if the current value is NA
  if (is.na(predictions_jul[i])) {
    prediction_jul[i] <- "unknown"  # Or some other label for missing values
  } else if (predictions_jul[i] > 0.795) {
    prediction_jul[i] <- "1"
  } else {
    prediction_jul[i] <- "0"
  }
}

DST_ind$Prediction <- as.factor(prediction_jul)


confusion_matrix_79 <- confusionMatrix(DST_ind$Prediction, DST_ind$Op_ned)
print(confusion_matrix_50)
print(confusion_matrix_55)
print(confusion_matrix_60)
print(confusion_matrix_65)
print(confusion_matrix_79)


### Make a ROC curve
library(pROC)
DST_ind$Prediction <- as.numeric(DST_ind$Prediction)
DST_ind$Op_ned <- as.numeric(DST_ind$Op_ned)
roc_79 <- roc(data=DST_ind, response=Op_ned, predictor=Prediction)

plot(roc_79, col="blue", main="ROC kurve")
