#!/usr/bin/Rscript

#########################
####Error Diagnostics####
#########################

####CPS Team####
####08/19/2014####

####Packages####
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))

####Construct Data for Modeling####
#prediction: cohort survival prediction
#enrollment: true enrollment data
#changing_feature: school features changing every year
#static_feature: school features not changing every year
#aggregate_feature: school features aggregated from the database
#grade: the grade we want to classify
construct_data <- function(prediction, enrollment, changing_feature, static_feature, aggregate_feature, grade) {
    pred <- data.frame(SchoolID=prediction$SchoolID, Prediction=prediction[,grade])
    enroll <- data.frame(SchoolID=enrollment$SchoolID, PreviousEnrollment=enrollment[,grade])
    group <- with(static_feature, data.frame(SchoolID, SchoolType, SchoolStyle, SchoolGradeGroup))
    data <- merge(pred, enroll, by="SchoolID", all.x=T)
    data <- merge(data, group, by="SchoolID", all.x=T)
    data <- merge(data, changing_feature, by="SchoolID", all.x=T)
    data <- merge(data, aggregate_feature, by="SchoolID", all.x=T)
    data <- within(data, {
        PreviousEnrollment[is.na(PreviousEnrollment)] <- 0
        SchoolType <- factor(SchoolType, exclude=NULL)
        SchoolStyle <- factor(SchoolStyle, exclude=NULL)
        SchoolGradeGroup <- factor(SchoolGradeGroup, exclude=NULL)
        White[is.na(White)] <- median(White, na.rm=T)
        Black[is.na(Black)] <- median(Black, na.rm=T)
        Hispanic[is.na(Hispanic)] <- median(Hispanic, na.rm=T)
        AUSL <- factor(AUSL, exclude=NULL)
        Mobility[is.na(Mobility)] <- median(Mobility, na.rm=T)
        Rating <- factor(Rating, exclude=NULL)
        Probation <- factor(Probation, exclude=NULL)
        Health <- factor(Health, exclude=NULL)
        Safety <- factor(Safety, exclude=NULL)
        Family <- factor(Family, exclude=NULL)
        Environment <- factor(Environment, exclude=NULL)
        Instruction <- factor(Instruction, exclude=NULL)
        Leader <- factor(Leader, exclude=NULL)
        Teacher <- factor(Teacher, exclude=NULL)
        SchoolFood[is.na(SchoolFood)] <- median(SchoolFood, na.rm=T)
        SchoolESL[is.na(SchoolESL)] <- median(SchoolESL, na.rm=T)
        SchoolMale[is.na(SchoolMale)] <- median(SchoolMale, na.rm=T)
        SchoolHomeless[is.na(SchoolHomeless)] <- median(SchoolHomeless, na.rm=T)
        SchoolGPA[is.na(SchoolGPA)] <- median(SchoolGPA, na.rm=T)
        SchoolAttendanceRate[is.na(SchoolAttendanceRate)] <- median(SchoolAttendanceRate, na.rm=T)
    })
    return(data)
}

####Train Decision Tree Model####
#location: data source
#year: year to train
#threshold: >= threshold -> 1; otherwise 0
diagnostic <- function(location, year, threshold, grade) {
    prediction_train <- read.csv(paste0(location, "csp", year, ".csv"))
    enrollment_train <- read.csv(paste0(location, "true", year-1, ".csv"))
    change_train <- read.csv(paste0(location, "schools_changing_features_", year, ".csv"), stringsAsFactors=F)
    aggregate_train <- read.csv(paste0(location, "schools_aggregate_features_", year, ".csv"), stringsAsFactors=F)
    static_train <- read.csv(paste0(location, "schools_static_features.csv"), stringsAsFactors=F)
    true_enrollment <- read.csv(paste0(location, "true", year, ".csv"))
    data_train_temp <- construct_data(prediction_train, enrollment_train, change_train, static_train, aggregate_train, grade)
    enroll <- data.frame(SchoolID=true_enrollment$SchoolID, CurrentEnrollment=true_enrollment[,grade])
    data_train <- merge(data_train_temp, enroll, by="SchoolID")
    data_train <- within(data_train, {
        Label <- rep(0, nrow(data_train))
        Label[abs(CurrentEnrollment-Prediction)>=threshold] <- 1
        Label <- factor(Label)
    })
    if (length(unique(data_train$Label))==1) {
        cat("Threshold is not appropriate!\n")
        return()
    }
    model <- rpart(formula=Label~Prediction+PreviousEnrollment+
                       SchoolType+SchoolStyle+SchoolGradeGroup+White+
                       Black+Hispanic+Mobility+Rating+Probation+
                       SchoolESL+SchoolMale+SchoolAttendanceRate,
                   data=data_train)
    print(prp(x=model, type=3))
    return(model)
}

####Make Prediction####
#location: data source
#year: year to test
#threshold: >= threshold -> 1; otherwise 0
#model: decision tree model
prediction <- function(location, year, threshold, grade, model) {
    prediction_test <- read.csv(paste0(location, "csp", year, ".csv"))
    enrollment_test <- read.csv(paste0(location, "true", year-1, ".csv"))
    change_test <- read.csv(paste0(location, "schools_changing_features_", year, ".csv"), stringsAsFactors=F)
    aggregate_test <- read.csv(paste0(location, "schools_aggregate_features_", year, ".csv"), stringsAsFactors=F)
    static_test <- read.csv(paste0(location, "schools_static_features.csv"), stringsAsFactors=F)
    data_test <- construct_data(prediction_test, enrollment_test, change_test, static_test, aggregate_test, grade)
    result <- as.data.frame(predict(object=model, newdata=data_test, type="prob"))
    output <- data.frame(SchoolID=data_test$SchoolID, Probability=result[,"1"])
    write.csv(x=output, file=paste0("misprojection_", year, ".csv"), row.names=F)
    return(output)
}





