# function for an object with results of the model
# funkcja dla modelu zapisanego jako wynik 
# funkcji train() obliczy 
# Accuracy, spec, sens i AUC

accuracy_ROC <- function(model, 
                         data, 
                         target_variable = "UCURNINS",
                         predicted_class = "Yes") {
  
  library(pROC)
  
  # generate probabilities of level "predicted_class"
    forecasts_p <- predict(model, data,
                         type = "prob")[, predicted_class]
  
  # and the predicted  cateogry
  if (any(class(model) == "train")) forecasts_c <- predict(model, data) else
     forecasts_c <- predict(model, data, type = "class")
  
  # actual values - pull() transforms tibble into vector
  real <- (data[, target_variable]) %>% pull
  
  # area under the curve
  AUC <- roc(predictor = forecasts_p,
             response = real)
  
  # classification table and measures based on it 
  table <- confusionMatrix(forecasts_c,
                           real,
                           predicted_class) 
  # collect everything in a single object 
  result <- c(table$overall[1], # Accuracy
              table$byClass[1:2], # sens, spec
              ROC = AUC$auc)
  
  return(result)
  
}
