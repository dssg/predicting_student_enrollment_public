#!/usr/bin/Rscript

#################################
####Main Script For Modelling####
#################################

####CPS Team####
####08/17/2014####

rm(list=ls())
setwd("/home/zye/Model/")

####Cohort Survival Model####
source("cohort_survival.R")
location <- "/mnt/data/cps/Data/"
year_to_predict <- 2013
result <- cohort_survival(location, year_to_predict)

####Model Diagnostics####
source("model_diagnostics.R")
location <- "/mnt/data/cps/Data/"
year_to_train <- 2012
threshold <- 50
grade <- "Grade9"
model <- diagnostic(location, year, threshold, grade)
year_to_predict <- 2013
result <- prediction(location, year, threshold, grade, model)

####Conditional Logistic Regression####
source("grade9_prediction.R")
location <- "/mnt/data/cps/Data/"
year_to_predict <- 2012
result <- grade9_prediction(location, year_to_predict)






