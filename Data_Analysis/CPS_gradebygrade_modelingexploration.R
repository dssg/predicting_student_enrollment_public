# -------------------------------------------------- #
#   this code shows many of our exploratory models   #
#          for the top-down model approach           #
# -------------------------------------------------- #

# run CPS_gradebygrade_script.R
# run CPS_gradebygrade_dataprep.R

datafinal_train = datafinal12
datafinal = datafinal13

# fill in missing values
for (j in c(7,8,9,11,25,26)){
  datafinal_train[is.na(datafinal_train[,j]),j] = 0
  datafinal[is.na(datafinal[,j]),j] = 0
}

for (j in c(12:20,24)){
  datafinal_train[is.na(datafinal_train[,j]),j] = 'Unknown'
  datafinal[is.na(datafinal[,j]),j] = 'Unknown'
}

# make sure there's no more missing values
apply(datafinal_train, 2, function(x) sum(is.na(x)))
apply(datafinal, 2, function(x) sum(is.na(x)))

# all schools and grades: rollover 2012 to predict 2013
test = predictschool(data_test = datafinal, data_train = datafinal_train, algorithm_of_choice = "rollover", modelformula = NA, grade_overall = 'gradebygrade')
mean(abs(rowSums(test[,16:29],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 25) # 52%
mean(abs(rowSums(test[,16:29],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 50) # 74%
mean(abs(rowSums(test[,16:29],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 100) # 87%
hist(rowSums(test[,16:29],na.rm = T) - rowSums(test[,2:15],na.rm=T), 20)

# high school only
test = subset(test, (enrollment_to_predict.X9 >= 10)|(enrollment_to_predict.X10 >= 10)|(enrollment_to_predict.X11 >= 10)|(enrollment_to_predict.X12 >= 10))
test = test[,c("SCHOOL_CODE","enrollment_to_predict.X9","enrollment_to_predict.X10","enrollment_to_predict.X11","enrollment_to_predict.X12","predictions X9","predictions X10","predictions X11","predictions X12")]

mean(abs(rowSums(test[,c("predictions X9","predictions X10","predictions X11","predictions X12")],na.rm = T) - rowSums(test[,c("enrollment_to_predict.X9","enrollment_to_predict.X10","enrollment_to_predict.X11","enrollment_to_predict.X12")],na.rm=T)) <= 25) 
mean(abs(rowSums(test[,c("predictions X9","predictions X10","predictions X11","predictions X12")],na.rm = T) - rowSums(test[,c("enrollment_to_predict.X9","enrollment_to_predict.X10","enrollment_to_predict.X11","enrollment_to_predict.X12")],na.rm=T)) <= 50) 
mean(abs(rowSums(test[,c("predictions X9","predictions X10","predictions X11","predictions X12")],na.rm = T) - rowSums(test[,c("enrollment_to_predict.X9","enrollment_to_predict.X10","enrollment_to_predict.X11","enrollment_to_predict.X12")],na.rm=T)) <= 100) 


# Basic linear regression: train on 2012, test on 2013
test = predictschool(data_test = datafinal, data_train = datafinal_train, algorithm_of_choice = "lm", modelformula = enrollment_to_predict ~ projection + NEW, grade_overall = 'gradebygrade')
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 25) # 50%
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 50) # 72%
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 100) # 89%
hist(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T), 20)

# high school only
test = subset(test, (enrollment_to_predict.X9 >= 10)|(enrollment_to_predict.X10 >= 10)|(enrollment_to_predict.X11 >= 10)|(enrollment_to_predict.X12 >= 10))
test = test[,c("SCHOOL_CODE","enrollment_to_predict.X9","enrollment_to_predict.X10","enrollment_to_predict.X11","enrollment_to_predict.X12","predictions X9","predictions X10","predictions X11","predictions X12")]

mean(abs(rowSums(test[,c("predictions X9","predictions X10","predictions X11","predictions X12")],na.rm = T) - rowSums(test[,c("enrollment_to_predict.X9","enrollment_to_predict.X10","enrollment_to_predict.X11","enrollment_to_predict.X12")],na.rm=T)) <= 25) 
mean(abs(rowSums(test[,c("predictions X9","predictions X10","predictions X11","predictions X12")],na.rm = T) - rowSums(test[,c("enrollment_to_predict.X9","enrollment_to_predict.X10","enrollment_to_predict.X11","enrollment_to_predict.X12")],na.rm=T)) <= 50) 
mean(abs(rowSums(test[,c("predictions X9","predictions X10","predictions X11","predictions X12")],na.rm = T) - rowSums(test[,c("enrollment_to_predict.X9","enrollment_to_predict.X10","enrollment_to_predict.X11","enrollment_to_predict.X12")],na.rm=T)) <= 100) 


# Linear regression with features: train on 2012, test on 2013
test = predictschool(data_test = datafinal, data_train = datafinal_train, algorithm_of_choice = "lm", modelformula = enrollment_to_predict ~ projection*Rating + (SCHOOL_TYPE == 'Regular') + NEW, grade_overall = 'gradebygrade')
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 25) 
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 50) 
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 100)

# plot by grade
hist(test[,"predictions K"] - test[,"enrollment_to_predict.K"],20)
hist(test[,"predictions X1"] - test[,"enrollment_to_predict.X1"],20)
hist(test[,"predictions X2"] - test[,"enrollment_to_predict.X2"],20)
hist(test[,"predictions X3"] - test[,"enrollment_to_predict.X3"],20)
hist(test[,"predictions X4"] - test[,"enrollment_to_predict.X4"],20)
hist(test[,"predictions X5"] - test[,"enrollment_to_predict.X5"],20)
hist(test[,"predictions X6"] - test[,"enrollment_to_predict.X6"],20)
hist(test[,"predictions X7"] - test[,"enrollment_to_predict.X7"],20)
hist(test[,"predictions X8"] - test[,"enrollment_to_predict.X8"],20)
hist(test[,"predictions X9"] - test[,"enrollment_to_predict.X9"],20)
hist(test[,"predictions X10"] - test[,"enrollment_to_predict.X10"],20)
hist(test[,"predictions X11"] - test[,"enrollment_to_predict.X11"],20)
hist(test[,"predictions X12"] - test[,"enrollment_to_predict.X12"],20)
hist(test[,"predictions LRE"] - test[,"enrollment_to_predict.LRE"],20)

# 9th grade
plot(test[,"enrollment_to_predict.X9"], test[,"predictions X9"],xlim=c(0,1500),ylim=c(0,1500),pch=16, xlab = "Actual 9th grade 2012", ylab = "Predicted 9th grade 2012")
segments(x0 = test[,"enrollment_to_predict.X9"], y0 = test[,"predictions X9"] - 1.96*sqrt(test[,"var X9"]), x1 = test[,"enrollment_to_predict.X9"], y1 = test[,"predictions X9"] + 1.96*sqrt(test[,"var X9"]))
segments(x0=0,y0=0,x1=1500,y1=1500, col = 2)
mean((test[,"enrollment_to_predict.X9"] > test[,"predictions X9"] - 1.96*sqrt(test[,"var X9"]))&(test[,"enrollment_to_predict.X9"] < test[,"predictions X9"] + 1.96*sqrt(test[,"var X9"])), na.rm = T)

# 2nd grade
plot(test[,"enrollment_to_predict.X2"], test[,"predictions X2"],xlim=c(0,300),ylim=c(0,300),pch=16, xlab = "Actual 2nd grade 2012", ylab = "Predicted 2nd grade 2012")
segments(x0 = test[,"enrollment_to_predict.X2"], y0 = test[,"predictions X2"] - 1.96*sqrt(test[,"var X2"]), x1 = test[,"enrollment_to_predict.X2"], y1 = test[,"predictions X2"] + 1.96*sqrt(test[,"var X2"]))
segments(x0=0,y0=0,x1=1300,y1=1300, col = 2)
mean((test[,"enrollment_to_predict.X2"] > test[,"predictions X2"] - 1.96*sqrt(test[,"var X2"]))&(test[,"enrollment_to_predict.X2"] < test[,"predictions X2"] + 1.96*sqrt(test[,"var X2"])), na.rm = T)


# Random forest with features: train on 2012, test on 2013
test = predictschool(data_test = datafinal, data_train = datafinal_train, algorithm_of_choice = "randomForest", modelformula = enrollment_to_predict ~ projection + NEW + Rating + SCHOOL_TYPE, grade_overall = 'gradebygrade')
mean(abs(rowSums(test[,16:29],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 25) 
mean(abs(rowSums(test[,16:29],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 50) 
mean(abs(rowSums(test[,16:29],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 100) 


# Experimenting with training data

# train only with schools that change less than a certain number of students
datafinal_train_stable= datafinal_train[(apply(datafinal_train[,c("enrollment_to_predict","projection")], 1, function(x) max(x) - min(x) <= 25))|(datafinal_train$projection == 0),]

test = predictschool(data_test = datafinal, data_train = datafinal_train_stable, algorithm_of_choice = "lm", modelformula = enrollment_to_predict ~ projection*Rating + NEW + (SCHOOL_TYPE == 'Regular') + newnearby, grade_overall = 'gradebygrade')
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 25) 
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 50) 
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 100)

# exclude new schools
datafinal_train_nonew = datafinal_train[datafinal_train$projection > 0,]
datafinal_nonew = datafinal[datafinal$projection > 0,]

# neighborhood enrollment
setwd("/Volumes/appdata-3/School reports")
schools = read.csv("schools.csv", stringsAsFactors = F)
neighborhoodschools = as.matrix(subset(schools, SchoolStyle == 'Neighborhood Enrollment', select = "SchoolID"))

datafinal_train_temp = datafinal_train[(datafinal_train$projection > 0) & (datafinal_train$SCHOOL_CODE %in% neighborhoodschools),]
datafinal_temp = datafinal[(datafinal$projection > 0) & (datafinal$SCHOOL_CODE %in% neighborhoodschools),]

test = predictschool(data_test = datafinal_temp, data_train = datafinal_train_temp, algorithm_of_choice = "lm", modelformula = enrollment_to_predict ~ projection*(Rating + Teacher + Safety) + newnearby + GRADE, grade_overall = 'overall')

schoolslist = unique(datafinal_temp$SCHOOL_CODE)
test = test[test$SCHOOL_CODE %in% schoolslist,]
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 25) 
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 50) 
mean(abs(rowSums(test[,seq(16,42,2)],na.rm = T) - rowSums(test[,2:15],na.rm=T)) <= 100)
