#Final Project
model_data <- read.csv("/Users/angel/Documents/ucf_classes/eco_4444_fall_2021/Final Data/model_data.csv")


attach(model_data)


#C. 
log_model <- glm(approve ~ debt_income + race + self_employed + married + education, family=binomial, data=model_data)
predicted_probabilities <- predict.glm(log_model, type=c("response"))
y <-table(approve)
log_prediction <- ifelse(predicted_probabilities > 0.70, 1, 0)
confmat <- table(log_prediction, approve)

log_tpr <- confmat[2,2]/y[[2]]
log_fpr <- confmat[2,1]/y[[1]]
log_tnr <- confmat[1,1]/y[[1]]
log_fnr <- confmat[1,2]/y[[2]]

log_tpr
log_fpr
log_tnr
log_fnr

#Change factors
log_prediction <- factor(log_prediction)
approve <- factor(approve)

#Confirming Results are same as function
confusionMatrix(log_prediction, approve, positive="1")

log_roc <- performance(prediction(predicted_probabilities, approve), "tpr", "fpr")
plot(log_roc, colorize=TRUE)

#AUC Calculation
log_pred <- prediction(predicted_probabilities, approve)
unlist(slot(performance(log_pred, "auc"), "y.values"))

summary(log_model)



#Remove Personal Characteristics Model to clean up global env
rm(log_pred, log_model, approve, confmat, log_fnr, log_fpr, log_prediction, log_tnr, log_tpr
   , predicted_probabilities, y)







#80/20 Test Train Split
parts <- sort(sample(nrow(model_data), nrow(model_data)*.8))
train<-model_data[parts,]
test<-model_data[-parts,]





null <- glm(approve~1, data=train, family=binomial)
full <- glm(approve ~ ., data=train, family=binomial)

#Model that stepwise selection producred, useful for further model creation
stepwise_model <- glm(formula = approve ~ sex + income + married + self_employed + 
    monthly_expense + purchase_price + credit_reports + met_guidlines + 
    mortgage_income + rate_type + loan_term + type_of_property + 
    sought_insurance + denied_mortgage + unverified_info + reviewed + 
    rtdum + age + housing_expense + high_val + loan_value_medium + 
    di_squared, family = binomial, data = train)



#Step AIC returns formula with lowest test AIC, providing a good basis upon which to create models
stepAIC(full, scope=list(lower=null, upper=full),data=test, direction='both')

stargazer(full, type="text")


attach(hmda_data)
#Two Best Models
model_10 <- glm(approve ~ denied_mortgage + housing_expense + race+loan_value + 
               met_guidlines + education + reviewed + married +
               di_squared + age + self_employed, family = binomial, data = train)

model_15 <- glm(formula = approve ~ I(debt_income^2) + denied_mortgage +housing_expense + 
      loan_value + met_guidlines + reviewed+married,+ age + self_employed, family = binomial,data=model_data)



#Generate Sequence used to identify optimal cutoff
prob_seq <- seq(0.01, 1, 0.01)

#Function that returns the cost 
costfunc = function(approve, pred_prob){
  weight1 = 1
  weight0 = 1
  c1 <- (approve==1)&(pred_prob<optimal_cutoff)
  c0 <- (approve==0)&(pred_prob>=optimal_cutoff)
  cost <- mean(weight1*c1 + weight0*c0)
  return(cost)
}


attach(test)
#Function that returns performance metrics to compare competing models: Test AUC, Accuracy
  perform <- function(model){
  pred_prob <- predict.glm(model,test)
  class_prediction <- ifelse(pred_prob > optimal_cutoff_cv, 1, 0)
  class_prediction <- factor(class_prediction)
  approve <- factor(approve)
  pred <- prediction(pred_prob,approve)
  #Prints area under curve
  print(unlist(slot(performance(pred, "auc"), "y.values")))
  #Returns confusionMatrix function results
  return (confusionMatrix(class_prediction, approve, positive="1"))
}

#Returns Optimal Cutoff from CV and test misclassification rate
  cv_cost <- rep(0, length(prob_seq))
  for (i in 1:length(prob_seq)){
    optimal_cutoff = prob_seq[i]
    set.seed(123)
    cv_cost[i] <- cv.glm(data=train, stepwise_model, cost = costfunc, K=10)$delta[2]
  }
  plot(prob_seq, cv_cost)
  
  missclass <- min(cv_cost)
  optimal_cutoff_cv <- prob_seq[which(cv_cost==min(cv_cost))]
  
perform(stepwise_model)
  

rm(full, model_11, null, test, train, cv_cost, i, missclass, optimal_cutoff, optimal_cutoff_cv, parts, prob_seq, costfunc, perform)
#Generate levels or something to get a variety of models
#Evaluate the model with the cross validation cost
#Re-estimate model will the full dataset and evaluate the confusion matrix, ROC, AUC, misclassification rate
attach(model_data)
best_model <- glm(formula = approve ~ I(debt_income^2) + denied_mortgage +housing_expense + 
                    loan_val + met_guidlines + reviewed+married,+ age + self_employed, family = binomial,data=model_data)

summary(best_model)

pred_prob <- predict.glm(best_model, type=c("response"))
class_prediction <- ifelse(pred_prob > 0.56, 1, 0)

class_prediction <- factor(class_prediction)
approve <- factor(approve)

#R confusion matrix
confusionMatrix(class_prediction, approve, positive="1")


#Hard Code Confusion Matrix to verify results
class_at_optimal <- (pred_prob > 0.56)*1
table(class_at_optimal, approve, dnn = c("Predicted", "True"))

tpr <- sum(approve==1 & class_at_optimal==1)/sum(approve==1)
tnr <- sum(approve==0 & class_at_optimal==0)/sum(approve==1)
accuracy <- mean(approve == class_at_optimal)

tpr
tnr
accuracy


fpr <- sum(approve==0 & class_at_optimal==1)/sum(approve==0)
fnr <- sum(approve==1 & class_at_optimal==0)/sum(approve==1)
mr <- mean(approve != class_at_optimal)


fpr
fnr
mr


#Plot the ROC Curve
roc <- performance(prediction(pred_prob, approve), "tpr", "fpr")
plot(roc, colorize=TRUE)

#AUC Calculation
pred <- prediction(pred_prob, approve)
unlist(slot(performance(pred, "auc"), "y.values"))





#Compare Results to Personal Characteristic Model
plot(log_roc, add=TRUE,colorize=TRUE)

























