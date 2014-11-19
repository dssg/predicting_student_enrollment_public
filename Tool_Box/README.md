##CPS Tool Box##

###Description###
Models For CPS:

1. Cohort Survival Model: **cohort_survival.R**.

2. Decision Tree: **model_diagnostics.R**.

3. Conditional Logistic Regression: **grade9_prediction.R**.

The model code is designed to run on **RStudio**. We recommend RStudio since it is the best way to edit and run R for people without much experience before. 

In order to run three models, you should use the script *main.R*. There are three sections in the code that correspond to the three models. 

For all parts, you need to source the function scripts, set the parameters and run the function to obtain the results. The details of three models are introduced below. 

**We expect CPS to use the above four scripts.** There are also evaluation scripts which are used to evaluate the model: *eval_grade9_prediction.R* and *eval_new_grade9_prediction.R*. The first one is to evaluate on all schools and the second one is on new schools. These codes are used by ourselves to see how the model will perform. 


###Cohort Survival Model###
This is essentially the replication of CPS's work on predicting the next year enrollment for each grade of each school given five-year history. For example, if we want to predict the enrollment on 2013, we need to use the enrollment data from 2008 to 2012. Our replication improves on the original VBA to code: the replication needs 3 seconds to run while the original VBA requires 1 hour and 25 minutes. 

This is the code to run the script:

```
source("cohort_survival.R")
location <- "/mnt/data/cps/Data/"
year_to_predict <- 2013
result <- cohort_survival(location, year_to_predict)
```

The variable "location" is the directory to store all data files. The variable "year_to_predict" is the year you want to make the prediction. For example, if you want to make predictions on 2013, then type 2013. The code will automatically find the previous five-year enrollment data for you. 

The input data for the cohort survival model is one active grade file and five enrollment history files. We stress the usage of the active grade file. The column names of this file are SchoolID and all grades. Before making predictions, we suggest knowing, for each school and each grade, what the predictions are on. If the grades at a certain school needs prediction, then put 1 there; otherwise 0. As a default, set all entries as 1. 

The output is a csv file called "csm_prediction_year.csv". The year corresponds on the year you input into the model. 


###Decision Tree###
This part is to help CPS diagnose their predictions. Specifically, it will help CPS find the set of schools with the highest likelihood to be mis-projected given the threshold.

This is the code to run the script:

```
source("model_diagnostics.R")
location <- "/mnt/data/cps/Data/"
year_to_train <- 2012
threshold <- 50
grade <- "Grade9"
model <- diagnostic(location, year, threshold, grade)
year_to_predict <- 2013
result <- prediction(location, year, threshold, grade, model)
```

The variable "location" is the directory to store all data files. The variable "year_to_train" is the year you want to analyze the misprojection. The variable "threshold" shows the tolerance for the misprojection: if the misprojection is larger than the threshold, it is bad; otherwise, good. The variable "grade" is the grade you want to make diagnostics on. 

In the code above, we will make diagnostics on the year 2012 and grade 9 with a threshold of 50. The output of the "diagnostic" function is the R object from the decision tree and a visualization of the decision tree. You will find the probability of making a bad projection from the leaves of the tree to see which types of schools are likely to be misprojected. 

The decision tree is very sensitive to the threshold. If you set threshold 50 or 25, the two trees might be very different. Thus you need to know what threshold you are interested in before using this script. 

Another function is "prediction". This function is used to predict the probability of making a bad projection for the future. The hope here is to help CPS adjust predictions more accurately. We suggest CPS use this model when you do not have other solid information when adjusting predictions. The input arguments are similar with "diagnostic" but requires as input the model trained from "diagnostic" into the model. 

If you want to make a prediction, the output will be "misprojection_year.csv". The year depends on your inputs. 


###Conditional Logistic Regression###
This is our main work of this summer -- a student-level model to make predictions for the 8th to 9th grade. The model we use is a conditional logistic regression. It can provide the probability of student i going to school j for all students and all schools. You can also obtain the predictive enrollment from these probability.


This is the code to run the script:

```
source("grade9_prediction.R")
location <- "/mnt/data/cps/Data/"
year_to_predict <- 2012
result <- grade9_prediction(location, year_to_predict)
```

The input is exactly the same as the cohort survival model. 

For output, there are two RData files. They are the data frames for training and testing conditonal logit model. Since the data size has over 4,000,000 rows and 80 columns, they are saved as RData files. Use "load" function in R to read the data. The variable names are "data_last_year" and "data_this_year". The code to generate the Rdata files is (variable "year" is the year to make prediction on): 

```
save(data_last_year, file=paste0("clogit_", year, "_data_", year-1, ".RData")) #last year data
save(data_this_year, file=paste0("clogit_", year, "_data_", year, ".RData")) #this year data
```

There are also four csv files associated with the probability of student i going to school j. The code to generate the probability files is:

```
write.csv(x=prob_d, file=paste0("clogit_", year, "_prob_d.csv"), row.names=F) #distance
write.csv(x=prob_dc, file=paste0("clogit_", year, "_prob_dc.csv"), row.names=F) #distance+catchment+rating
write.csv(x=prob_dcr, file=paste0("clogit_", year, "_prob_dcr.csv"), row.names=F) #distance+catchment+rating
write.csv(x=prob_all, file=paste0("clogit_", year, "_prob_all.csv"), row.names=F) #all features
```

There are four models which are associated with what features we want to put in:

1. d: we only use distance from home to high school as features.

2. dc: we use distance and if the school is a catchment school as features.

3. dcr: we use distance, catchment school and school rating as features.

4. all: we use all the features including the above and also previous enrollment, school attendance rate, etc. 

Another four csv files associated with four models above are the enrollment prediction files. The code to generate the enrollment files is:

```
write.csv(x=enrollment_d, file=paste0("clogit_", year, "_prediction_d.csv"), row.names=F) #distance
write.csv(x=enrollment_dc, file=paste0("clogit_", year, "_prediction_dc.csv"), row.names=F) #distance+catchment
write.csv(x=enrollment_dcr, file=paste0("clogit_", year, "_prediction_dcr.csv"), row.names=F) #distance+catchment+rating
write.csv(x=enrollment_all, file=paste0("clogit_", year, "_prediction_all.csv"), row.names=F) #all features
```

The predictive enrollment is obtained by summing up the probability of all students for a specific school. 

Finally, another two files are the baseline and our final ensemble enrollment prediction. CPS should use the ensemble file as their prediction of enrollment of 9th grade in practice. The code to generate the two enrollment files is:

```
write.csv(x=baseline, file=paste0("baseline_", year, "_prediction_all.csv"), row.names=F) #baseline
write.csv(x=ensemble, file=paste0("ensemble_", year, "_prediction_all.csv"), row.names=F) #final model
```

The baseline model is copy the enrollment last year for old schools and for the new schools, we use (total students-total students predicted in old schools)/(number of total new schools). This will gurantee the total students match. 

We recommend that CPS needs to use **ensemble_year_prediction_all.csv** for the final prediction. The year depends on the year you want to make prediction. It uses clogit to predict new schools and proportionally scale the students on old schools based on the enrollment last year so that the total number of students are correct. 



