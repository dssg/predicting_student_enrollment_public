#!/usr/bin/Rscript

#############################
####Cohort Survival Model####
#############################

####CPS Team####
####08/19/2014####

####Moving Average####
#data: history 
#num: years of moving average
ma <- function(data, num) {
    pred <- vector(length=length(data)-num)
    for (i in 1:(length(data)-num)) {
        pred[i] <- mean(data[i:(i+1)])
    }
    return(pred)
}

####Exponential Smoothing####
#data: history
#num: percentage of exponential smoothing
es <- function(data, num) {
    pred <- vector(length=length(data)-1)
    for (i in 1:(length(data)-1)) {
        if (i==1) {
            pred[i] <- data[i]
        }
        else {
            pred[i] <- num*data[i]+(1-num)*pred[i-1]
        }
    }
    return(pred)
}

####All Models####
#enroll_vector: historical enrollment
#this will provide the best prediction based on six models
model <- function(enroll_vector) {
    data <- enroll_vector[enroll_vector!=0]
    if (length(data)==1) {
        cat(paste0("Single"), "\n")
        return(data)
    }
    else if (length(data)==2) {
        cat(paste0("Double"), "\n")
        return(mean(data))
    }
    else {
        candidate <- vector(length=6)
        value <- vector(length=6)
        #MA1
        pred <- ma(data, 1)
        real <- data[-1]
        candidate[1] <- mean((real-pred)^2)
        value[1] <- pred[length(pred)]
        #MA2
        pred <- ma(data, 2)
        real <- data[-c(1,2)]
        candidate[2] <- mean((real-pred)^2)
        value[2] <- pred[length(pred)]
        #ES2
        pred <- es(data, 0.2)
        real <- data[-1]
        candidate[3] <- mean((real-pred)^2)
        value[3] <- pred[length(pred)]
        #ES3
        pred <- es(data, 0.3)
        real <- data[-1]
        candidate[4] <- mean((real-pred)^2)
        value[4] <- pred[length(pred)]
        #ES4
        pred <- es(data, 0.4)
        real <- data[-1]
        candidate[5] <- mean((real-pred)^2)
        value[5] <- pred[length(pred)]
        #ES5
        pred <- es(data, 0.5)
        real <- data[-1]
        candidate[6] <- mean((real-pred)^2)
        value[6] <- pred[length(pred)]
        all_model <- c("MA1", "MA2", "ES2", "ES3", "ES4", "ES5")
        cat(paste0(all_model[which.min(candidate)], "\n"))
        return(value[which.min(candidate)])
    }
}

####Cohort Survival Model####
#last_grade: name of last grade
#this_grade: name of this grade
#city_csr: city-level cohort survival rate
csm <- function(last_grade, this_grade, city_csr) {
    ratio <- unlist(this_grade)[-1]/unlist(last_grade)[-length(unlist(last_grade))]
    ratio[is.nan(ratio)] <- 0
    ratio[is.infinite(ratio)] <- 0
    if (sum(ratio)!=0) {
        temp <- model(ratio)
        if (temp!=0) {
            return(round(unlist(last_grade)[length(unlist(last_grade))]*temp))
        }
        else {
            return(round(unlist(last_grade)[length(unlist(last_grade))]*mean(ratio[ratio!=0])))
        }
    }
    else {
        return(round(unlist(last_grade)[length(unlist(last_grade))]*city_csr[,names(this_grade)]))
    }
}

####Entry Level Model####
#this_grade: name of this grade
#city_el: city-level enrollment
el <- function(this_grade,  city_el) {
    if (sum(this_grade)!=0) {
        temp <- model(unlist(this_grade))
        if (temp!=0) {
            return(round(temp))
        }
        else {
            return(round(city_el[,names(this_grade)]))
        }
    }
    else {
        return(round(city_el[,names(this_grade)]))
    }
}

####Predict Next Year Enrollment####
#history: enrollment history
#grade: the grade we want to make prediction
#city_csr: city-level cohort survival rate
#city_el: city-level enrollment
pred_enroll <- function(history, grade, city_csr, city_el) {
    pred <- data.frame(matrix(NA, 1, ncol(history)))
    name <- colnames(history)
    colnames(pred) <- name
    pred$SchoolID <- unique(history$SchoolID)
    for (i in 2:(ncol(grade)-1)) {
        if (grade[i]!=0) { #active grade
            cat(paste0(name[i], "\n"))
            if (name[i]=="K" | name[i]=="Grade1" | name[i]=="Grade9" | name[i]=="LRE") {
                pred[i] <- el(history[i], city_el) #entry level model
            }
            else {
                if (history[nrow(history),i-1]!=0) { #check if last grade last year has enrollment
                    pred[i] <- csm(history[i-1], history[i], city_csr) #cohort survival model
                }
                else {
                    pred[i] <- el(history[i], city_el) #entry level model
                }
            }
        }
        else {
            pred[i] <- 0
        }
    }
    pred$ATOT <- sum(pred[2:(length(pred)-1)])
    return(pred)
}

####Main Function For Cohort Survival Model####
#location: data source
#year: year to predict
cohort_survival <- function(location, year) {
    #Read Data
    active <- read.csv(paste0(location, "active", year, ".csv")) #active schools and grades
    school1 <- read.csv(paste0(location, "true", year-1, ".csv")) #enrollment -1 year
    school2 <- read.csv(paste0(location, "true", year-2, ".csv")) #enrollment -2 year
    school3 <- read.csv(paste0(location, "true", year-3, ".csv")) #enrollment -3 year
    school4 <- read.csv(paste0(location, "true", year-4, ".csv")) #enrollment -4 year
    school5 <- read.csv(paste0(location, "true", year-5, ".csv")) #enrollment -5 year
    school_list <- list(school5, school4, school3, school2, school1)
    #Set Up
    all_id <- unique(active$SchoolID)
    all_school <- do.call("rbind", school_list)
    all_school$SchoolID <- NULL
    all_school$ATOT <- NULL
    #City Level Cohort Survival Rate
    city_csr <- data.frame(Grade2=0.98, Grade3=1.03, Grade4=0.93, Grade5=0.98, Grade6=1, Grade7=0.97, Grade8=0.98, Grade10=1.04, Grade11=0.84, Grade12=0.91)
    city_el <- data.frame(matrix(apply(all_school, 2, function(x) {mean(x[x!=0])}), 1, ncol(all_school)))
    colnames(city_el) <- colnames(all_school)
    #Make Prediction
    name <- c("SchoolID", "K", "Grade1", "Grade2", "Grade3", "Grade4", "Grade5", "Grade6", "Grade7", "Grade8", "Grade9", "Grade10", "Grade11", "Grade12", "LRE", "ATOT")
    result <- data.frame(matrix(NA, length(all_id), length(name)))
    colnames(result) <- name
    for (i in 1:length(all_id)) {
        id = all_id[i]
        cat(paste0(id), "\n")
        active_grade <- subset(active, active$SchoolID==id)
        history <- data.frame(matrix(NA, length(school_list), length(name)))
        colnames(history) <- name
        for (j in 1:length(school_list)) {
            school <- school_list[[j]]
            index <- which(school$SchoolID==id)
            if (length(index)!=0) {
                history[j,] <- school[index,]
            }
            else {
                history[j,] <- c(id, rep(0, length(name)-1))
            }
        }
        result[i,] <- pred_enroll(history, active_grade, city_csr, city_el)
    }
    write.csv(x=result, file=paste0("csm_prediction_", year, ".csv"), row.names=F)
    return(result)
}