# ------------------------------------------------- #
#   predicting catchment high schools - fall 2013   #
# ------------------------------------------------- #

# run CPS_gradebygrade_dataprep.R

### greg's predictions
setwd("/Volumes/appdata-3/School Reports")
gregdata = read.csv("CSVfileforR_excerptof_FY14SBBALLOCATIONS_FINALSUMMARY_asof_032414.csv", stringsAsFactors = FALSE)
gregdata_chs = subset(gregdata, SchoolID %in% schools_with1314boundaries)

summary(gregdata_chs$Projected.Total - gregdata_chs$Actual.Total)
mean(abs(gregdata_chs$Projected.Total - gregdata_chs$Actual.Total)) # 66
mean(abs(gregdata_chs$Projected.Total - gregdata_chs$Actual.Total) <= 50) # 0.51
mean(abs(gregdata_chs$Projected.Total - gregdata_chs$Actual.Total) <= 100) # 0.73

### prepare data 
datafinal12_chs = subset(datafinal12, SCHOOL_CODE %in% schools_with1314boundaries)
datafinal13_chs = subset(datafinal13, SCHOOL_CODE %in% schools_with1314boundaries)

apply(datafinal12_chs, 2, function(x) sum(is.na(x)))
apply(datafinal13_chs, 2, function(x) sum(is.na(x)))
datafinal12_chs[is.na(datafinal12_chs)] = 'Unknown'
datafinal13_chs[is.na(datafinal13_chs)] = 'Unknown'

### compare to rolling over 2012 or Zhou's cohort survival projection
library("plyr")
baseline_comparison = ddply(datafinal13_chs, .(SCHOOL_CODE), summarize, cstotal = sum(cs_projection), lastyeartotal = sum(projection), truetotal = sum(enrollment_to_predict))
mean(abs(baseline_comparison$truetotal - baseline_comparison$cstotal)) # cohort survival: 77 student mean absolute error
mean(abs(baseline_comparison$truetotal - baseline_comparison$cstotal) <= 50) # 0.49
mean(abs(baseline_comparison$truetotal - baseline_comparison$cstotal) <= 100) # 0.75

mean(abs(baseline_comparison$lastyeartotal - baseline_comparison$truetotal)) # roll over 2012: 87 student mean absolute error
mean(abs(baseline_comparison$lastyeartotal - baseline_comparison$truetotal) <= 50) # 0.39
mean(abs(baseline_comparison$lastyeartotal - baseline_comparison$truetotal) <= 100) # 0.61

### train regression model on 2012 data
m1 = lm(enrollment_to_predict ~ (projection + cs_projection)*Rating + newnearby, data = datafinal12_chs)
summary(m1) # summarize coefficients
plot(m1) # residual plots for model diagnostic

### test model on 2013 data
m1_test = predict(m1, newdata = datafinal13_chs, interval = "prediction", level = 0.95)
m1_test = as.data.frame(m1_test)
m1_test_var = ((m1_test$upr - m1_test$lwr)/4)^2

datafinal13_chs$testpredictions = m1_test$fit
datafinal13_chs$testvar = m1_test_var

test_summary = ddply(datafinal13_chs, .(SCHOOL_CODE), summarize, truetotal = sum(enrollment_to_predict), predictedtotal = sum(testpredictions), predictedvar = sum(testvar))
mean(abs(test_summary$truetotal - test_summary$predictedtotal)) # mean absolute error
mean(abs(test_summary$truetotal - test_summary$predictedtotal) <= 50) 
mean(abs(test_summary$truetotal - test_summary$predictedtotal) <= 100) # 0.80 

# calculate prediction interval lower and upper limits
test_summary$predictedlower = test_summary$predictedtotal - 1.96*sqrt(test_summary$predictedvar)
test_summary$predictedupper = test_summary$predictedtotal + 1.96*sqrt(test_summary$predictedvar)

# how many 95% intervals cover the true school total (should be about 95%)
mean((test_summary$predictedlower <= test_summary$truetotal) & (test_summary$truetotal <= test_summary$predictedupper))

# summarize the widths of the confidence intervals
summary(test_summary$predictedupper - test_summary$predictedlower)

setwd("/Volumes/appdata-3/School Reports")
schools = read.csv("schools.csv", stringsAsFactors = F)

test_summary = merge(test_summary, schools[,c("SchoolID","SchoolShortName")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SchoolID")
test_summary = merge(test_summary, gregdata_chs[,c("SchoolID","Projected.Total")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SchoolID")

# plot our estimates and CPS's estimates

plot(test_summary$truetotal, test_summary$predictedtotal, pch = 16, xlim = c(0,3300), ylim = c(0,3300),xlab = "True enrollment 2013", ylab = "Predicted enrollment 2013")
segments(x0 = test_summary$truetotal, y0 = test_summary$predictedtotal - 1.96*sqrt(test_summary$predictedvar), x1 = test_summary$truetotal, y1 = test_summary$predictedtotal + 1.96*sqrt(test_summary$predictedvar))
segments(x0=0,y0=0,x1=4000,y1=4000, col=2)
points(x = test_summary$truetotal, y = test_summary$Projected.Total, pch = 2, col = 4)
legend("bottomright", c("DSSG", "CPS"),col = c(1,4), pch = c(16,2), bty = "n", pt.cex = 2, title = "Prediction Model:")

# plot school estimates and intervals
library(ggplot2)
test_summary$SCHOOL_CODE = with(test_summary, reorder(SCHOOL_CODE, -predictedtotal))

ggplot(data=test_summary, aes(x=SCHOOL_CODE, y=predictedtotal)) + 
  geom_bar(stat="identity", fill = "#E69F00") + 
geom_errorbar(aes(ymin=predictedtotal - 1.96*sqrt(predictedvar), ymax=predictedtotal + 1.96*sqrt(predictedvar)),
              width=.2,                    # Width of the error bars
              position=position_dodge(.9)) +
  xlab("Catchment high schools") + ylab("Predicted enrollment") + 
theme(legend.position = "none", axis.ticks = element_blank(), 
      axis.text.x = element_blank())
