#!/usr/bin/Rscript

###########################
###Evaluation New School###
###########################

###Zhou Ye###
###08/06/2014###

rm(list=ls())
setwd("/home/zye/Model")
year <- 2013
suppressMessages(library(stringr))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))
result <- read.csv(paste0("clogit_", year, "_prob_all.csv"))
load(paste0("clogit_", year, "_data_", year-1, ".RData"))
load(paste0("clogit_", year, "_data_", year, ".RData"))
new_school <- setdiff(unique(data_this_year$SchoolKey), unique(data_last_year$SchoolKey))

###Reshape###
reshape_enroll <- function(data) {
    school_key <- as.numeric(str_replace(names(data), "X", ""))
    enroll <- as.numeric(data)
    school_enroll <- data.frame(SchoolKey=school_key, Enrollment=enroll)
    return(school_enroll)
}

###Distance###
rdist_earth_vec <- function(x1, x2, miles=T, R=NULL) {
    #modified from fields::rdist.earth
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

###Prediction###
prediction <- round(colSums(result[,-1]))
pred_enroll <- reshape_enroll(prediction)

###True Enrollment###
real <- dcast(data_this_year, StudentKey~SchoolKey, mean, value.var="SchoolSelected")
true_enroll <- reshape_enroll(colSums(real[,-1]))

###School Location###
school_location <- unique(with(data_this_year, data.frame(SchoolKey, LongitudeHS, LatitudeHS)))
old_school_location <- subset(school_location, !SchoolKey%in%new_school)
old_school_location$Distance <- NA

###Baseline###
last_year <- dcast(data_last_year, StudentKey~SchoolKey, mean, value.var="SchoolSelected")
last_enroll <- reshape_enroll(colSums(last_year[,-1]))
#Last Year Mean
baseline1 <- data.frame(SchoolKey=true_enroll$SchoolKey, Enrollment=NA)
for (i in 1:nrow(baseline1)) {
    index <- which(last_enroll$SchoolKey==baseline1[i,"SchoolKey"])
    if (length(index)>0) {
        baseline1[i,"Enrollment"] <- last_enroll[index,"Enrollment"]
    }
}
baseline1[is.na(baseline1$Enrollment),"Enrollment"] <- round(mean(baseline1$Enrollment, na.rm=T))
#Last Year Median
baseline2 <- data.frame(SchoolKey=true_enroll$SchoolKey, Enrollment=NA)
for (i in 1:nrow(baseline1)) {
    index <- which(last_enroll$SchoolKey==baseline2[i,"SchoolKey"])
    if (length(index)>0) {
        baseline2[i,"Enrollment"] <- last_enroll[index,"Enrollment"]
    }
}
baseline2[is.na(baseline2$Enrollment),"Enrollment"] <- round(median(baseline2$Enrollment, na.rm=T))
#KNN Mean
k <- 5
baseline3 <- data.frame(SchoolKey=true_enroll$SchoolKey, Enrollment=NA)
for (i in 1:nrow(baseline3)) {
    index <- which(last_enroll$SchoolKey==baseline3[i,"SchoolKey"])
    if (length(index)>0) {
        baseline3[i,"Enrollment"] <- last_enroll[index,"Enrollment"]
    }
    else {
        self_location <- subset(school_location, SchoolKey==baseline3[i,"SchoolKey"])
        self_coordinate <- with(self_location, t(replicate(nrow(old_school_location), c(LongitudeHS, LatitudeHS))))
        old_school_location <- within(old_school_location, {
            Distance <- rdist_earth_vec(cbind(LongitudeHS, LatitudeHS), self_coordinate)
        })
        close_school_location <- old_school_location[order(old_school_location$Distance),][1:k,]
        baseline3[i,"Enrollment"] <- round(mean(subset(last_enroll, SchoolKey%in%close_school_location$SchoolKey)$Enrollment))
    }
}
#KNN Median
k <- 5
baseline4 <- data.frame(SchoolKey=true_enroll$SchoolKey, Enrollment=NA)
for (i in 1:nrow(baseline4)) {
    index <- which(last_enroll$SchoolKey==baseline4[i,"SchoolKey"])
    if (length(index)>0) {
        baseline4[i,"Enrollment"] <- last_enroll[index,"Enrollment"]
    }
    else {
        self_location <- subset(school_location, SchoolKey==baseline3[i,"SchoolKey"])
        self_coordinate <- with(self_location, t(replicate(nrow(old_school_location), c(LongitudeHS, LatitudeHS))))
        old_school_location <- within(old_school_location, {
            Distance <- rdist_earth_vec(cbind(LongitudeHS, LatitudeHS), self_coordinate)
        })
        close_school_location <- old_school_location[order(old_school_location$Distance),][1:k,]
        baseline4[i,"Enrollment"] <- round(median(subset(last_enroll, SchoolKey%in%close_school_location$SchoolKey)$Enrollment))
    }
}

###Evaluation###
pred <- subset(merge(pred_enroll, true_enroll, by="SchoolKey"), SchoolKey%in%new_school)
base1 <- subset(merge(baseline1, true_enroll, by="SchoolKey"), SchoolKey%in%new_school)
base2 <- subset(merge(baseline2, true_enroll, by="SchoolKey"), SchoolKey%in%new_school)
base3 <- subset(merge(baseline3, true_enroll, by="SchoolKey"), SchoolKey%in%new_school)
base4 <- subset(merge(baseline4, true_enroll, by="SchoolKey"), SchoolKey%in%new_school)
pred_mae <- mean(abs(pred$Enrollment.x-pred$Enrollment.y))
base1_mae <- mean(abs(base1$Enrollment.x-base1$Enrollment.y))
base2_mae <- mean(abs(base2$Enrollment.x-base2$Enrollment.y))
base3_mae <- mean(abs(base3$Enrollment.x-base3$Enrollment.y))
base4_mae <- mean(abs(base4$Enrollment.x-base4$Enrollment.y))
mae_graph <- data.frame(Model=factor(c("baseline1", "baseline2", "baseline3", "baseline4", "clogit"), levels=c("baseline1", "baseline2", "baseline3", "baseline4", "clogit")), MAE=c(base1_mae, base2_mae, base3_mae, base4_mae, pred_mae))
ggplot(mae_graph, aes(x=Model, y=MAE))+geom_bar(stat="identity", fill="midnightblue")




