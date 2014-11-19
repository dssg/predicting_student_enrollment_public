#!/usr/bin/Rscript

################################################
####Evaluate Conditional Logistic Regression####
################################################

####CPS Team####
####08/18/2014####

rm(list=ls())
setwd("/home/zye/Model/")
year <- 2013

####Packages####
suppressMessages(library(stringr))
suppressMessages(library(reshape2))
suppressMessages(library(ggplot2))

####Accuracy####
accuracy <- function(prob, true_school, len) {
    acc <- vector(length=len)
    temp <- t(apply(-prob[,-1], 1, rank, ties.method="random"))
    for (k in 1:len) {
        top_k <- (temp<=k)*1
        acc[k] <- sum(true_school*top_k)/nrow(top_k)
    }
    return(acc)
}

####Evaluation####
#True Enrollment This Year
load(paste0("clogit_", year, "_data_", year, ".RData"))
true_school <- dcast(data_this_year, StudentKey~SchoolKey, mean, value.var="SchoolSelected")[,-1]
true_enrollment <- data.frame(SchoolKey=as.numeric(names(true_school)), Enrollment=round(colSums(true_school)))
#Predictive Probability
d <- read.csv(paste0("clogit_", year, "_prob_d.csv")) #distance
dc <- read.csv(paste0("clogit_", year, "_prob_dc.csv")) #distance+catchment
dcr <- read.csv(paste0("clogit_", year, "_prob_dcr.csv")) #distance+catchment+rating
all <- read.csv(paste0("clogit_", year, "_prob_all.csv")) #all features
#Predictive Enrollment
d_prediction <- read.csv(paste0("clogit_", year, "_prediction_d.csv"))
dc_prediction <- read.csv(paste0("clogit_", year, "_prediction_dc.csv"))
dcr_prediction <- read.csv(paste0("clogit_", year, "_prediction_dcr.csv"))
all_prediction <- read.csv(paste0("clogit_", year, "_prediction_all.csv"))
baseline_prediction <- read.csv(paste0("baseline_", year, "_prediction_all.csv"))
ensemble_prediction <- read.csv(paste0("ensemble_", year, "_prediction_all.csv"))
#Compute Accuracy From Probability
num <- 10
d_acc <- data.frame(Model=rep("D", num), Accuracy=accuracy(d, true_school, num))
dc_acc <- data.frame(Model=rep("D+C", num), Accuracy=accuracy(dc, true_school, num))
dcr_acc <- data.frame(Model=rep("D+C+R", num), Accuracy=accuracy(dcr, true_school, num))
all_acc <- data.frame(Model=rep("ALL", num), Accuracy=accuracy(all, true_school, num))
acc_graph <- do.call("rbind", list(d_acc, dc_acc, dcr_acc, all_acc))
acc_graph$Rank <- factor(1:num, levels=1:num)
ggplot(acc_graph, aes(x=Rank, y=Accuracy, group=Model, colour=Model))+geom_line()
#Compare Enrollment
d_compare <- merge(d_prediction, true_enrollment, by="SchoolKey")
dc_compare <- merge(dc_prediction, true_enrollment, by="SchoolKey")
dcr_compare <- merge(dcr_prediction, true_enrollment, by="SchoolKey")
all_compare <- merge(all_prediction, true_enrollment, by="SchoolKey")
baseline_compare <- merge(baseline_prediction, true_enrollment, by="SchoolKey")
ensemble_compare <- merge(ensemble_prediction, true_enrollment, by="SchoolKey")
d_mae <- mean(abs(d_compare$Enrollment.x-d_compare$Enrollment.y))
dc_mae <- mean(abs(dc_compare$Enrollment.x-dc_compare$Enrollment.y))
dcr_mae <- mean(abs(dcr_compare$Enrollment.x-dcr_compare$Enrollment.y))
all_mae <- mean(abs(all_compare$Enrollment.x-all_compare$Enrollment.y))
baseline_mae <- mean(abs(baseline_compare$Enrollment.x-baseline_compare$Enrollment.y))
ensemble_mae <- mean(abs(ensemble_compare$Enrollment.x-ensemble_compare$Enrollment.y))
mae_graph <- data.frame(Model=factor(c("clogit", "baseline", "ensemble"), levels=c("clogit", "baseline", "ensemble")), MAE=c(all_mae, baseline_mae, ensemble_mae))
ggplot(mae_graph, aes(x=Model, y=MAE))+geom_bar(stat="identity", fill="midnightblue")






