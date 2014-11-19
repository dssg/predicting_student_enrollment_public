# ================================== #
#   grade-by-grade modeling script   #
# ================================== #


predictschool = function(data_test, data_train, algorithm_of_choice, modelformula, grade_overall){
  # data should have columns named "SCHOOL_CODE", "enrollment_to_predict", "projection" and "GRADE"
  # all other columns are main effect predictors
  # algorithm_of_choice can be "lm","randomForest","rollover"
  # gradebygrade or overall
  
  # 1. turn character features into factors 
  for (j in 1:ncol(data_train)){
    if (class(data_train[,j]) == "character"){
      data_train[,j] = as.factor(data_train[,j])
    }
  }
  
  for (j in 1:ncol(data_test)){
    if (class(data_test[,j]) == "character"){
      data_test[,j] = as.factor(data_test[,j])
    }
  }
  
  if (grade_overall == 'gradebygrade'){
  # 2. predict each grade
  predictschools=reshape(data_test[,c("SCHOOL_CODE","GRADE","enrollment_to_predict")], v.names = "enrollment_to_predict", timevar = "GRADE", idvar = "SCHOOL_CODE", direction = "wide")  
  
  for (grade in levels(data_train$GRADE)){
    
    currentnames = names(predictschools)
    
    temp = predictgrade(data_train, data_test, grade, algorithm_of_choice, modelformula)
    
    if (algorithm_of_choice == "lm"){
      predictschools = merge(predictschools, temp[,c("SCHOOL_CODE", "test_predictions", "test_var")], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")
      colnames(predictschools) = c(currentnames, paste('predictions', grade), paste('var', grade))
    }
    
    if ((algorithm_of_choice == "randomForest")|(algorithm_of_choice == "rollover")){
      predictschools = merge(predictschools, temp[,c("SCHOOL_CODE", "test_predictions")], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")
      colnames(predictschools) = c(currentnames, paste('predictions',grade))
    }
  }
  
  # 3. output is predictions for test set
  predictschools
  }
  
  else if (grade_overall == 'overall'){
    predictschools=reshape(data_test[,c("SCHOOL_CODE","GRADE","enrollment_to_predict")], v.names = "enrollment_to_predict", timevar = "GRADE", idvar = "SCHOOL_CODE", direction = "wide")  
    
    # predict overall
    overallpredicts = predictoverall(data_train, data_test, algorithm_of_choice, modelformula)     
    
    # output
    merge(predictschools, overallpredicts, all.x = TRUE)
    
  }

}



predictgrade = function(data_train, data_test, grade, algorithm_of_choice, modelformula){
  # data should have columns named "SCHOOL_CODE", "enrollment_to_predict", "projection" and "GRADE"

  data_train_grade = subset(data_train, (GRADE == grade)&(enrollment_to_predict > 0))
  data_test_grade = subset(data_test, (GRADE == grade)&(enrollment_to_predict > 0))
  
  # algorithm = lm
  if (algorithm_of_choice == "lm"){
    lm_train = lm(modelformula, data = data_train_grade)
    lm_test = predict(lm_train, newdata = data_test_grade, interval = "prediction", level = .95)
    lm_test = as.data.frame(lm_test)
    data_test_grade$test_predictions = lm_test$fit
    data_test_grade$test_var = ((lm_test$upr - lm_test$lwr)/4)^2
    
    data_test_grade[,c("SCHOOL_CODE", "test_predictions", "test_var")] # output
  }
  
  
  # algorithm = randomForest
  else if (algorithm_of_choice == "randomForest"){
    library(randomForest)
    rf_train = randomForest(modelformula, data = data_train_grade)
    rf_test = predict(rf_train, newdata = data_test_grade)
    data_test_grade$test_predictions = rf_test
    
    data_test_grade[,c("SCHOOL_CODE", "test_predictions")] # output
  }
  
  # algorithm = rollover
  else if (algorithm_of_choice == "rollover"){
    data_test_grade$test_predictions = data_test_grade$projection
    grademean = mean(data_test_grade[(data_test_grade$test_predictions > 0),"test_predictions"])
    data_test_grade[(data_test_grade$test_predictions == 0),"test_predictions"] = grademean
    
    data_test_grade[,c("SCHOOL_CODE", "test_predictions")] # output
  }
  
  
}



predictoverall = function(data_train, data_test, algorithm_of_choice, modelformula){
  # data should have columns named "SCHOOL_CODE", "enrollment_to_predict", "projection" and "GRADE"
  
  data_train = subset(data_train, (enrollment_to_predict > 0))
  data_test = subset(data_test, (enrollment_to_predict > 0))
  
  # algorithm = lm
  if (algorithm_of_choice == "lm"){
    lm_train = lm(modelformula, data = data_train)
    lm_test = predict(lm_train, newdata = data_test, interval = "prediction", level = .95)
    lm_test = as.data.frame(lm_test)
    data_test$predictions = lm_test$fit
    data_test$var = ((lm_test$upr - lm_test$lwr)/4)^2
    
    outputtemp = data_test[,c("SCHOOL_CODE", "GRADE", "predictions", "var")] # output
    output = reshape(outputtemp, v.names = c("predictions", "var"), timevar = "GRADE", idvar = "SCHOOL_CODE", direction = 'wide',sep=" ")
    output
  }
  
  # algorithm = randomForest
  else if (algorithm_of_choice == "randomForest"){
    library(randomForest)
    rf_train = randomForest(modelformula, data = data_train)
    rf_test = predict(rf_train, newdata = data_test)
    data_test$predictions = rf_test
    
    outputtemp = data_test[,c("SCHOOL_CODE", "GRADE", "predictions")] # output
    output = reshape(outputtemp, v.names = "predictions", timevar = "GRADE", idvar = "SCHOOL_CODE", direction = 'wide',sep=" ")
    output
  }
  
  
}

