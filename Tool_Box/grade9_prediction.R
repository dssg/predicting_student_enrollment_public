#!/usr/bin/Rscript

##########################
####Grade 9 Prediction####
##########################

####CPS Team####
####08/17/2014####

####Packages####
suppressMessages(library(dplyr))
suppressMessages(library(fields))
suppressMessages(library(reshape2))
suppressMessages(library(clogitL1))
suppressMessages(library(survival))
suppressMessages(library(stringr))

####Compute Distance on Map into a Vector####
#modified from fields::rdist.earth (see rdist.earth for more details)
#now x1 and x2 can be vectors
rdist_earth_vec <- function(x1, x2, miles=T, R=NULL) {
    if (is.null(R)) {
        if (miles) {
            R <- 3963.34
        }
        else {
            R <- 6378.388
        }
    }
    coslat1 <- cos((x1[,2]*pi)/180)
    sinlat1 <- sin((x1[,2]*pi)/180)
    coslon1 <- cos((x1[,1]*pi)/180)
    sinlon1 <- sin((x1[,1]*pi)/180)
    coslat2 <- cos((x2[,2]*pi)/180)
    sinlat2 <- sin((x2[,2]*pi)/180)
    coslon2 <- cos((x2[,1]*pi)/180)
    sinlon2 <- sin((x2[,1]*pi)/180)
    pp <- cbind(coslat1*coslon1, coslat1*sinlon1, sinlat1)*cbind(coslat2*coslon2, coslat2*sinlon2, sinlat2)
    pp <- rowSums(pp)
    return(R*acos(ifelse(abs(pp)>1, 1*sign(pp), pp)))
}

###Construct Data Set for Conditional Logistic Regression###
#location: the path to the data
#year: the year we want make prediction
#num: Inf for all schools; otherwise only "num" schools are used
construct_data <- function(location, year, num) {
    #Process Data
    students <- read.csv(paste0(location, "students", year, ".csv"), stringsAsFactors=F) #student information
    students <- subset(students, !(EducationType%in%c("21 - 60%", "61 - 100%"))) #exclude LRE2/LRE3 students
    names(students)[names(students)=="Longitude"] <- "LongitudeHome"
    names(students)[names(students)=="Latitude"] <- "LatitudeHome"
    schools_static <- read.csv(paste0(location, "schools_static_features.csv"), stringsAsFactors=F) #school static information
    schools_progress <- read.csv(paste0(location, "schools_changing_features_", year, ".csv"), stringsAsFactors=F) #school progress feature
    schools_aggregate <- read.csv(paste0(location, "schools_aggregate_features_", year, ".csv"), stringsAsFactors=F) #school aggregate feature
    schools_change <- merge(schools_progress, schools_aggregate, by="SchoolID", all.x=T) #school changing feature
    schools_change$SchoolKey <- NULL
    schools_change[is.na(schools_change$White),]$White <- median(schools_change$White, na.rm=T) #impute race
    schools_change[is.na(schools_change$Black),]$Black <- median(schools_change$Black, na.rm=T) #impute race
    schools_change[is.na(schools_change$Hispanic),]$Hispanic <- median(schools_change$Hispanic, na.rm=T) #impute racex
    schools_change$Other <- pmax(0, 100-rowSums(schools_change[,c("White", "Black", "Hispanic")])) #other race
    enroll <- read.csv(paste0(location, "true", year-1, ".csv"), stringsAsFactors=F)
    enroll9 <- subset(enroll, select=c("SchoolID", "Grade9"))
    colnames(enroll9) <- c("SchoolID", "PreviousEnrollment")
    cat("Finish Reading Data...\n")
    #Inner Join to Obtain Final Student and School Data
    schools <- subset(schools_static, select=c(SchoolKey, SchoolID, SchoolType, SchoolSubType, SchoolStyle, Longitude, Latitude))
    colnames(schools) <- c("SchoolKey", "SchoolIDES", "SchoolTypeES", "SchoolSubTypeES", "SchoolStyleES", "LongitudeES", "LatitudeES") #elementary schools
    students_schools <- merge(students, schools, all.x=T, by.x="ThisGradeSchoolKey", by.y="SchoolKey")
    students_final <- students_schools #final students data
    colnames(schools) <- c("SchoolKey", "SchoolIDHS", "SchoolTypeHS", "SchoolSubTypeHS", "SchoolStyleHS", "LongitudeHS", "LatitudeHS") #high schools
    students_schools <- merge(students_schools, schools, all.x=T, by.x="NextGradeSchoolKey", by.y="SchoolKey")
    students_schools$DistanceHomeHS <- with(students_schools, rdist_earth_vec(cbind(LongitudeHome, LatitudeHome), cbind(LongitudeHS, LatitudeHS)))
    possible_schools <- tbl_df(students_schools) %>% 
        group_by(NextGradeSchoolKey) %>% 
        summarize(n=n(), AvgDist=mean(DistanceHomeHS, na.rm=T), n_dist=sum(!is.na(DistanceHomeHS))) %>%
        filter(n_dist>1) %>%
        arrange(desc(n_dist)) #remove NA distance schools
    if (is.finite(num)) {
        possible_schools <- possible_schools[1:num,] #sub-sampling
    }
    students_final <- subset(students_final, NextGradeSchoolKey%in%possible_schools$NextGradeSchoolKey)
    schools_final <- subset(schools, SchoolKey%in%possible_schools$NextGradeSchoolKey)
    schools_final <- merge(schools_final, enroll9, by.x="SchoolIDHS", by.y="SchoolID", all.x=T)
    schools_final <- merge(schools_final, schools_change, by.x="SchoolIDHS", by.y="SchoolID", all.x=T)
    schools_final <- within(schools_final, {
        PreviousEnrollment[is.na(PreviousEnrollment)] <- 0
        NewSchool <- as.numeric(PreviousEnrollment == 0)
    })
    #Outer Join to Obtain Data for Conditional Logistic Regression
    students_schools_final <- merge(unique(students_final), unique(schools_final), by=NULL, all=T)
    cat("Finish Constructing Table...\n")
    #Feature Computation and Missing Value
    #1. Distance
    students_schools_final$DistanceHomeHS <- with(students_schools_final, rdist_earth_vec(cbind(LongitudeHome, LatitudeHome), cbind(LongitudeHS, LatitudeHS)))
    median_distance <- median(students_schools_final$DistanceHomeHS, na.rm=T)
    students_schools_final[is.na(students_schools_final$DistanceHomeHS),]$DistanceHomeHS <- median_distance
    students_schools_final$DistanceESHS <- with(students_schools_final, rdist_earth_vec(cbind(LongitudeES, LatitudeES), cbind(LongitudeHS, LatitudeHS)))
    median_distance <- median(students_schools_final$DistanceESHS, na.rm=T)
    students_schools_final[is.na(students_schools_final$DistanceESHS),]$DistanceESHS <- median_distance
    #2. School Feature
    students_schools_final <- within(students_schools_final, {
        SchoolTypeES[is.na(SchoolTypeES)] <- "Unknown"
        SchoolSubTypeES[is.na(SchoolSubTypeES)] <- "Unknown"
        SchoolStyleES[is.na(SchoolStyleES)] <- "Unknown"
        SchoolTypeHS[is.na(SchoolTypeHS)] <- "Unknown"
        SchoolSubTypeHS[is.na(SchoolSubTypeHS)] <- "Unknown"
        SchoolStyleHS[is.na(SchoolStyleHS)] <- "Unknown"
        SchoolTypeMatch <- as.numeric(SchoolTypeES==SchoolTypeHS)
        SchoolSubTypeMatch <- as.numeric(SchoolSubTypeES==SchoolSubTypeHS)
        SchoolStyleMatch <- as.numeric(SchoolStyleES==SchoolStyleHS)
        SchoolTypeHS <- factor(SchoolTypeHS)
        SchoolSubTypeHS <- factor(SchoolSubTypeHS)
        SchoolStyleHS <- factor(SchoolStyleHS)
        SelectiveEnrollment <- as.numeric(SchoolStyleHS=="Selective Enrollment")
        Race[Race %in% c("American Indian", "Hawaiian or Pacific Islander", "Multi", "N/A")] <- "Other"
        Race <- factor(Race)
        Language[!(Language %in% c("English", "Spanish", "Polish"))] <- "Other"
        Language <- factor(Language)
        BirthCountry[!(BirthCountry %in% c("United States", "Mexico", "China"))] <- "Other"
        BirthCountry <- factor(BirthCountry)
        Food <- factor(Food)
        ESL[is.na(ESL)] <- "Unknown"
        ESL <- factor(ESL)
        SchoolChange[is.na(SchoolChange)] <- 0
        SchoolGPA[is.na(SchoolGPA)] <- median(SchoolGPA, na.rm=T)
    })
    #3. ISAT
    students_schools_final$AvgScore <- (students_schools_final$EighthMathISAT+students_schools_final$EighthReadingISAT)/2
    median_ISAT <- median(students_schools_final$AvgScore, na.rm=T)
    students_schools_final[is.na(students_schools_final$AvgScore),]$AvgScore <- median_ISAT
    #4. Progress Report
    students_schools_final <- within(students_schools_final, {
        Rating[is.na(Rating)] <- "Unknown"
        Safety[is.na(Safety)] <- "Unknown"
        Probation[is.na(Probation)] <- "Unknown"
        Rating <- factor(Rating)
        RatingLevel1 <- as.numeric(Rating=="level 1")
        Safety <- factor(Safety)
        Probation <- factor(Probation)
        Mobility[is.na(Mobility)] <- median(Mobility, na.rm=T)
    })
    #5. Other Features
    students_schools_final <- within(students_schools_final, {
        SchoolSelected <- as.numeric(NextGradeSchoolKey==SchoolKey)
        SameAsPreviousSchool <- ifelse(is.na(ThisGradeSchoolKey), 0, as.numeric(ThisGradeSchoolKey==SchoolKey))
        CatchmentSchool <- as.numeric(CatchmentSchoolKey==SchoolKey)
        SchoolGPA[is.na(SchoolGPA)] <- median(GPA, na.rm=T)
        SchoolAttendanceRate[is.na(SchoolAttendanceRate)] <- median(SchoolAttendanceRate, na.rm=T)
        AttendanceRate[is.na(AttendanceRate)] <- median(AttendanceRate, na.rm=T)
        PercentSameRace <- NA
        PercentSameRace[Race=="Black, Non-Hispanic"] <- Black[Race=="Black, Non-Hispanic"]
        PercentSameRace[Race=="Hispanic"] <- Hispanic[Race=="Hispanic"]
        PercentSameRace[Race=="White, Non-Hispanic"] <- White["White, Non-Hispanic"]
        PercentSameRace[is.na(PercentSameRace)] <- Other[is.na(PercentSameRace)]
        PercentSameFood <- NA
        PercentSameFood[Food=="Yes"] <- SchoolFood[Food=="Yes"]
        PercentSameFood[Food!="Yes"] <- 1-SchoolFood[Food!="Yes"]
        PercentSameESL <- NA
        PercentSameESL[ESL=="Yes"] <- SchoolESL[ESL=="Yes"]
        PercentSameESL[ESL!="Yes"] <- 1-SchoolESL[ESL!="Yes"]
        PercentSameMale <- NA
        PercentSameMale[Gender=="Male"] <- SchoolMale[Gender=="Male"]
        PercentSameMale[Gender!="Male"] <- 1-SchoolMale[Gender!="Male"]
        PercentSameHomeless <- NA
        PercentSameHomeless[Homeless=="Yes"] <- SchoolHomeless[Homeless=="Yes"]
        PercentSameHomeless[Homeless!="Yes"] <- 1-SchoolHomeless[Homeless!="Yes"]
    })
    cat("Finish Data Cleaning...\n")
    return(students_schools_final)
}

####Make Prediction####
#convert the prediction to probability
#prediction: the prediction value from condition logistic regression
#test_data: data for test
make_prediction <- function(prediction, test_data) {
    test <- data.frame(StudentKey=test_data$StudentKey,
                       SchoolKey=test_data$SchoolKey,
                       Pred=exp(as.numeric(prediction)))
    pred_mat <- dcast(test, StudentKey~SchoolKey, mean, value.var="Pred")
    pred_mat[,-1] <- pred_mat[,-1]/rowSums(pred_mat[,-1])
    return(pred_mat)
}

####Reshape Probability Into Enrollment####
#prob: probability from conditional logistic regression
reshape_enroll <- function(prob) {
    data <- round(colSums(prob[,-1]))
    school_key <- as.numeric(str_replace(names(data), "X", ""))
    enroll <- as.numeric(data)
    school_enroll <- data.frame(SchoolKey=school_key, Enrollment=enroll)
    return(school_enroll)
}

####Train Conditional Logistic Regression####
clogit <- function(regression_formula, data_last_year, data_this_year) {
    cat(paste0("Formula: ", regression_formula, "\n"))
    feature <- model.matrix(as.formula(regression_formula), data_last_year)[,-1]
    feature_test <- model.matrix(as.formula(regression_formula), data_this_year)[,-1]
    feature <- feature[, colnames(feature)%in%colnames(feature_test)] #make sure the features are matched
    feature <- feature[, apply(feature, 2, sd)>0]
    feature_means <- colMeans(feature)
    feature_sds <- apply(feature, 2, sd)
    feature <- scale(feature, center=feature_means, scale=feature_sds)
    feature_test <- scale(feature_test, center=feature_means, scale=feature_sds)
    label <- data_last_year$SchoolSelected
    start <- proc.time()
    model <- clogitL1(x=feature, y=label, strata=data_last_year$StudentKey, alpha=1)
    model_cv <- cv.clogitL1(model, numFolds=5)
    end <- proc.time()
    time <- end-start
    cat(paste0("Training Takes ", as.numeric(time["elapsed"])/3600), "Hours...\n")
    #Predict
    coefficient <- summary(model_cv)$beta_minCV
    if (is.nan(sum(coefficient))) {
        coefficient <- summary(model_cv)$beta_minCV1se
        if (is.nan(sum(coefficient))) {
            cat("We Cannot Make Prediction!\n")
            return()
        }
    }
    prediction <- feature_test%*%coefficient 
    prob <- make_prediction(prediction, data_this_year)
    return(prob)
}

####Construct Baseline: Copy Last Year Enrollment####
#data_last_year: last year training data
#new_school: new school keys
copy_last_year <- function(last_enroll, this_enroll, new_school) {
    baseline <- data.frame(SchoolKey=this_enroll$SchoolKey, Enrollment=NA)
    for (i in 1:nrow(baseline)) {
        index <- which(last_enroll$SchoolKey==baseline[i,"SchoolKey"])
        if (length(index)>0) {
            baseline[i,"Enrollment"] <- last_enroll[index,"Enrollment"]
        }
    }
    baseline[is.na(baseline$Enrollment),"Enrollment"] <- max(0, round((sum(this_enroll$Enrollment)-sum(baseline$Enrollment, na.rm=T))/sum(is.na(baseline$Enrollment))))
    return(baseline)
}

####Conditional Logistic Regression####
grade9_prediction <- function(location, year) {
    cat("Process Last Year Data...\n")
    data_last_year <- construct_data(location, year-1, Inf)
    cat("Process This Year Data...\n")
    data_this_year <- construct_data(location, year, Inf)
    #Construct Design Matrix
    cat("Creating Model Matrices...\n")
    #Train/Predict
    #1. Distance
    regression_formula <- "SchoolSelected~ns(DistanceHomeHS,2)"
    prob_d <- clogit(regression_formula, data_last_year, data_this_year)
    enrollment_d <- reshape_enroll(prob_d)
    #2. Distance+Catchment
    regression_formula <- "SchoolSelected~ns(DistanceHomeHS,2)+CatchmentSchool"
    prob_dc <- clogit(regression_formula, data_last_year, data_this_year)
    enrollment_dc <- reshape_enroll(prob_dc)
    #3. Distance+Catchment+Rating
    regression_formula <- "SchoolSelected~Rating*ns(DistanceHomeHS,2)+CatchmentSchool"
    prob_dcr <- clogit(regression_formula, data_last_year, data_this_year)
    enrollment_dcr <- reshape_enroll(prob_dcr)
    #4. All
    regression_formula <- "SchoolSelected~Rating*ns(DistanceHomeHS,2)+ns(PercentSameRace,2)+ns(PercentSameESL,2)+ns(PercentSameMale,2)+ns(Mobility,2)+ns(SchoolAttendanceRate,2)+CatchmentSchool+SchoolTypeMatch+PreviousEnrollment"
    prob_all <- clogit(regression_formula, data_last_year, data_this_year)
    enrollment_all <- reshape_enroll(prob_all)
    cat("Finish Conditional Logit Prediction ^_^Y\n")
    #Baseline
    new_school <- setdiff(unique(data_this_year$SchoolKey), unique(data_last_year$SchoolKey))
    this_year <- dcast(data_this_year, StudentKey~SchoolKey, mean, value.var="SchoolSelected")[,-1]
    this_enroll <- data.frame(SchoolKey=as.numeric(names(this_year)), Enrollment=round(colSums(this_year)))
    last_year <- dcast(data_last_year, StudentKey~SchoolKey, mean, value.var="SchoolSelected")[,-1]
    last_enroll <- data.frame(SchoolKey=as.numeric(names(last_year)), Enrollment=round(colSums(last_year)))
    baseline <- copy_last_year(last_enroll, this_enroll, new_school)
    cat("Finish Baseline Prediction ^_^Y\n")
    #Ensemble
    total_new_this_year <- with(enrollment_all, sum(Enrollment[SchoolKey%in%new_school]))
    total_old_this_year <- sum(this_enroll$Enrollment)-total_new_this_year
    proportion <- with(baseline, {Enrollment[!SchoolKey%in%new_school]/sum(Enrollment[!SchoolKey%in%new_school])})
    ensemble <- within(enrollment_all, {Enrollment[!SchoolKey%in%new_school]=round(total_old_this_year*proportion)})
    cat("Finish Ensemble Prediction ^_^Y\n")
    #Save Files
    save(data_last_year, file=paste0("clogit_", year, "_data_", year-1, ".RData")) #last year data
    save(data_this_year, file=paste0("clogit_", year, "_data_", year, ".RData")) #this year data
    write.csv(x=prob_d, file=paste0("clogit_", year, "_prob_d.csv"), row.names=F) #distance
    write.csv(x=prob_dc, file=paste0("clogit_", year, "_prob_dc.csv"), row.names=F) #distance+catchment+rating
    write.csv(x=prob_dcr, file=paste0("clogit_", year, "_prob_dcr.csv"), row.names=F) #distance+catchment+rating
    write.csv(x=prob_all, file=paste0("clogit_", year, "_prob_all.csv"), row.names=F) #all features
    write.csv(x=enrollment_d, file=paste0("clogit_", year, "_prediction_d.csv"), row.names=F) #distance
    write.csv(x=enrollment_dc, file=paste0("clogit_", year, "_prediction_dc.csv"), row.names=F) #distance+catchment
    write.csv(x=enrollment_dcr, file=paste0("clogit_", year, "_prediction_dcr.csv"), row.names=F) #distance+catchment+rating
    write.csv(x=enrollment_all, file=paste0("clogit_", year, "_prediction_all.csv"), row.names=F) #all features
    write.csv(x=baseline, file=paste0("baseline_", year, "_prediction_all.csv"), row.names=F) #baseline
    write.csv(x=ensemble, file=paste0("ensemble_", year, "_prediction_all.csv"), row.names=F) #final model
    cat("Finish Saving Files ^_^Y\n")
    return(prob_all)
}





