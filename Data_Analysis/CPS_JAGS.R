# --------------------------------------- #
#   prepare data for hierarchical model   #
# --------------------------------------- #

# variables: SCHOOL_CODE, GRADE, GROUP (charter / neighborhood), 
# for progressive grades: RATIO = [2012 grade k]/[2011 grade k-1]

setwd("/Volumes/appdata-3/Count Data")
countdata = read.csv("enrollment_byschool_byyear_bygrade.csv", stringsAsFactors = F)

setwd("/Volumes/appdata-3/School Reports")
schools = read.csv("schools.csv", stringsAsFactors = F)

data1 = subset(countdata, YEAR == 2012 & ENROLLMENT > 0 & GRADE %in% c("X2","X3","X4","X5","X6","X7","X8","X10","X11","X12"), select = c("SCHOOL_CODE","GRADE","ENROLLMENT"))

gradeorder = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")

data1$PREV_GRADE = lapply(data1[,"GRADE"], function(x) gradeorder[which(gradeorder == x)-1])
data1$PREV_GRADE = as.character(data1$PREV_GRADE)
table(data1$GRADE, data1$PREV_GRADE)

data11 = subset(countdata, YEAR == 2011)

data1= merge(data1, data11[,c("SCHOOL_CODE","GRADE","ENROLLMENT")], all.x = T, by.x = c("SCHOOL_CODE","PREV_GRADE"), by.y = c("SCHOOL_CODE","GRADE"), suffixes = c(".THIS_GRADE",".PREV_GRADE"))

data1$CSRATIO = data1$ENROLLMENT.THIS_GRADE/data1$ENROLLMENT.PREV_GRADE
hist(data1$CSRATIO)
data1 = subset(data1, is.finite(CSRATIO))

data1 = merge(data1, schools[,c("SchoolID","SchoolType")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SchoolID")

data2 = subset(countdata, YEAR == 2011 & ENROLLMENT > 0 & GRADE %in% c("X2","X3","X4","X5","X6","X7","X8","X10","X11","X12"), select = c("SCHOOL_CODE","GRADE","ENROLLMENT"))

gradeorder = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")

data2$PREV_GRADE = lapply(data2[,"GRADE"], function(x) gradeorder[which(gradeorder == x)-1])
data2$PREV_GRADE = as.character(data2$PREV_GRADE)
table(data2$GRADE, data2$PREV_GRADE)

data10 = subset(countdata, YEAR == 2010)

data2= merge(data2, data10[,c("SCHOOL_CODE","GRADE","ENROLLMENT")], all.x = T, by.x = c("SCHOOL_CODE","PREV_GRADE"), by.y = c("SCHOOL_CODE","GRADE"), suffixes = c(".THIS_GRADE",".PREV_GRADE"))

data2$CSRATIO = data2$ENROLLMENT.THIS_GRADE/data2$ENROLLMENT.PREV_GRADE
hist(data2$CSRATIO)
data2 = subset(data2, is.finite(CSRATIO))

data2 = merge(data2, schools[,c("SchoolID","SchoolType")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SchoolID")

data = rbind(data1, data2)

data = subset(data, CSRATIO <= 5)

data$SCHOOL_GROUP = as.numeric(as.factor(data$SchoolType))
data$SCHOOL = data$SCHOOL_CODE
data$SCHOOL = as.numeric(as.factor(data$SCHOOL))
data$GRADE_GROUP = data$GRADE
data$GRADE_GROUP = as.numeric(as.factor(data$GRADE_GROUP))


#install.packages("R2jags")
library("R2jags")

model<-function(){
  
  # ratios
  for (i in 1:n.obs){
    y[i] ~ dnorm(gamma[SCHOOL[i]] + delta[GRADE[i]], tau)
  }

  tau <- pow(sigma, -2)
  sigma ~ dunif(0,100)
  
  # grades
  for (j in 1:n.grades){
    delta[j] ~ dnorm(0, tau.delta)
  }
  
  tau.delta <- pow(sigma.delta, -2)
  sigma.delta ~ dunif(0,100)
  
  # schools
  for (k in 1:n.schools){
    gamma[k] ~ dnorm(mu.gamma[SCHOOL_GROUP[k]], tau.gamma[SCHOOL_GROUP[k]])
  }
    
  # groups
  for (l in 1:n.groups){
    mu.gamma[l] ~ dnorm(mu.star, tau.mu)
    tau.gamma[l] <- pow(sigma.gamma[l], -2)
    sigma.gamma[l] ~ dunif(0, 100)
  }
  
 mu.star ~ dnorm(0, 1/10000)
 tau.mu<- pow(sigma.mu, -2)
 sigma.mu ~ dunif(0, 100)
}

schoolgroupmat = unique(data[,c("SCHOOL_CODE","SCHOOL_GROUP")])

data_jags=list(y=data$CSRATIO, SCHOOL = data$SCHOOL, GRADE = data$GRADE_GROUP, SCHOOL_GROUP = schoolgroupmat$SCHOOL_GROUP, n.obs = 7546, n.schools = 671, n.grades = 10, n.groups = 5)
inits=function(){list("sigma" = 1, "delta" = c(rep(0, length(unique(data$GRADE)))), "sigma.delta" = 1, "mu.star" = 1, "sigma.gamma" = c(1,1,1,1,1), "sigma.mu" = 1)}
parameters.to.save<-c("delta","sigma", "sigma.delta", "mu.gamma","mu.star", "sigma.gamma", "sigma.mu","gamma")

sim = jags(data_jags, inits, parameters.to.save, model.file=model, n.chains=3, n.iter=10000)
print(sim)

tracemat = sim$BUGSoutput$sims.matrix
tracemat = as.data.frame(tracemat)
names(tracemat)

# predictions and credible intervals for 2013/2012
test = unique(data[,c("SCHOOL_CODE","GRADE","SCHOOL","SCHOOL_GROUP","GRADE_GROUP")])
test$postpredmean = NA
test$postpred025 = NA
test$postpred975 = NA

for (i in 1:nrow(test)){
  gammatemp = tracemat[,paste('gamma[',test[i,"SCHOOL"],']', sep = "")]
  deltatemp = tracemat[,paste('delta[',test[i,"GRADE_GROUP"],']', sep = "")]
  sigmatemp = tracemat$sigma
  
  ytemp = rnorm(3000, gammatemp + deltatemp, sd = sqrt(sigmatemp))
  test[i,"postpredmean"] = mean(ytemp)
  test[i,"postpred025"] = quantile(ytemp, .025)
  test[i,"postpred975"] = quantile(ytemp,.975)
}


data3 = subset(countdata, YEAR == 2013 & ENROLLMENT > 0 & GRADE %in% c("X2","X3","X4","X5","X6","X7","X8","X10","X11","X12"), select = c("SCHOOL_CODE","GRADE","ENROLLMENT"))

gradeorder = c("X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12")

data3$PREV_GRADE = lapply(data3[,"GRADE"], function(x) gradeorder[which(gradeorder == x)-1])
data3$PREV_GRADE = as.character(data3$PREV_GRADE)
table(data3$GRADE, data3$PREV_GRADE)

data12 = subset(countdata, YEAR == 2012)

data3= merge(data3, data12[,c("SCHOOL_CODE","GRADE","ENROLLMENT")], all.x = T, by.x = c("SCHOOL_CODE","PREV_GRADE"), by.y = c("SCHOOL_CODE","GRADE"), suffixes = c(".THIS_GRADE",".PREV_GRADE"))

data3$CSRATIO = data3$ENROLLMENT.THIS_GRADE/data3$ENROLLMENT.PREV_GRADE
hist(data3$CSRATIO)
data3 = subset(data3, is.finite(CSRATIO))

data3 = merge(data3, schools[,c("SchoolID","SchoolType")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SchoolID")

test = merge(test, data3[,c("SCHOOL_CODE","GRADE","CSRATIO")], all.x = T, by.x = c("SCHOOL_CODE","GRADE"), by.y = c("SCHOOL_CODE","GRADE"))

test = subset(test, CSRATIO <= 2)
plot(test$CSRATIO, test$postpredmean)


plot(1:3000, tracemat$'mu.gamma[5]', type ="l")
lines(1:3000, tracemat$'mu.gamma[3]', col = colorlist[4])
lines(1:3000, tracemat$'mu.gamma[2]', col = colorlist[3])
lines(1:3000, tracemat$'mu.gamma[4]', col = colorlist[5])
lines(1:3000, tracemat$'mu.gamma[5]', col = colorlist[7])

unique(data[,c("SchoolType","SCHOOL_GROUP")])
legend("topright",c("Alternative","Charter","Contract","Performance","Regular"), col = colorlist[c(1,3,4,5,7)], pch = 16)

plot(1:3000, tracemat$'delta[1]', type ="l",ylim=c(-.2,.2), col = colorlist[1])
lines(1:3000, tracemat$'delta[2]', col = colorlist[2])
lines(1:3000, tracemat$'delta[3]', col = colorlist[3])
lines(1:3000, tracemat$'delta[4]', col = colorlist[4])
lines(1:3000, tracemat$'delta[5]', col = colorlist[5])
lines(1:3000, tracemat$'delta[6]', col = colorlist[6])
lines(1:3000, tracemat$'delta[7]', col = colorlist[7])
lines(1:3000, tracemat$'delta[8]', col = colorlist[8])
lines(1:3000, tracemat$'delta[9]', col = colorlist[9])
lines(1:3000, tracemat$'delta[10]', col = colorlist[10])

legend("topright",c("2nd grade", "3rd grade", "4th grade", "5th grade", "6th grade", "7th grade", "8th grade", "10th grade", "11th grade", "12th grade"), col = colorlist[c(4,5,6,7,8,9,10,1,2,3)], pch = 16)

unique(data[,c("GRADE","GRADE_GROUP")])
colorlist = brewer.pal(10, "Set3")
