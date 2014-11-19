#!/usr/bin/Rscript

#######################################
####Create School Changing Features####
#######################################

####CPS Team####
####08/19/2014####

rm(list=ls())
setwd("/Volumes/DSSG/Zhou_Classification/")
elem2013 <- read.csv("CPS_Elementary_Report_20122013.csv", stringsAsFactors=F)
high2013 <- read.csv("CPS_HighSchool_Report_20122013.csv", stringsAsFactors=F)
elem2014 <- read.csv("CPS_Elementary_Report_20132014.csv", stringsAsFactors=F)
high2014 <- read.csv("CPS_HighSchool_Report_20132014.csv", stringsAsFactors=F)
prog2012 <- read.csv("Progress_2011_2012.csv", stringsAsFactors=F)
name <- c("SchoolID", "SchoolName", "SchoolType", "ZipCode", "Rating", "Probation", "Health", "Safety", "Family", "Environment", "Instruction", "Leader", "Teacher")

####2011 - 2012###
progress_2011_2012 <- with(prog2012, {
    data.frame(School.ID, Name.of.School, Elementary..Middle..or.High.School, ZIP.Code,
               CPS.Performance.Policy.Level, CPS.Performance.Policy.Status,
               Healthy.Schools.Certified., Safety.Icon, Family.Involvement.Icon,
               Environment.Icon, Instruction.Icon, Leaders.Icon, Teachers.Icon, stringsAsFactors=F)
})
colnames(progress_2011_2012) <- name
progress_2011_2012 <- within(progress_2011_2012, {
    SchoolType[SchoolType=="MS"] <- "ES";
    Rating[Rating=="Not Enough Data" | Rating=="NDA"] <- NA;
    Probation[Probation=="Not on Probation"] <- "No";
    Probation[Probation=="Probation"] <- "Yes";
    Probation[Probation=="Not Applicable" | Probation=="NDA"] <- NA;
    Safety[Safety=="NDA"] <- NA;
    Family[Family=="NDA"] <- NA;
    Environment[Environment=="NDA"] <- NA;
    Instruction[Instruction=="NDA"] <- NA;
    Leader[Leader=="NDA"] <- NA;
    Teacher[Teacher=="NDA"] <- NA;
    Rating <- tolower(Rating);
    Probation <- tolower(Probation);
    Health <- tolower(Health);
    Safety <- tolower(Safety);
    Family <- tolower(Family);
    Environment <- tolower(Environment);
    Instruction <- tolower(Instruction);
    Leader <- tolower(Leader);
    Teacher <- tolower(Teacher);
})
write.csv(x=progress_2011_2012, file="/Users/zhouye/Dropbox/DSSG/Edit_Prediction/progress_2011_2012.csv", row.names=F)

###2012 - 2013###
progress_2012_2013_es <- with(elem2013, {
    data.frame(School.ID, School.Name, "ES", ZIP, Overall.Rating,
               On.Probation, Healthy.School.Certified, Safety,
               Involved.Families, Supportive.Environment, 
               Ambitious.Instruction, Effective.Leaders, Collaborative.Teachers, stringsAsFactors=F)
})
colnames(progress_2012_2013_es) <- name
progress_2012_2013_es <- within(progress_2012_2013_es, {
    Rating[Rating=="Not Enough Data"] <- NA;
    Safety[Safety=="Not Enough Data"] <- NA;
    Safety[Safety=="Neutral"] <- "Average";
    Family[Family=="Not Enough Data"] <- NA;
    Family[Family=="Neutral"] <- "Average";
    Environment[Environment=="Not Enough Data"] <- NA;
    Environment[Environment=="Neutral"] <- "Average";
    Instruction[Instruction=="Not Enough Data"] <- NA;
    Instruction[Instruction=="Neutral"] <- "Average";
    Leader[Leader=="Not Enough Data"] <- NA;
    Leader[Leader=="Neutral"] <- "Average";
    Teacher[Teacher=="Not Enough Data"] <- NA;
    Teacher[Teacher=="Neutral"] <- "Average";
    Rating <- tolower(Rating);
    Probation <- tolower(Probation);
    Health <- tolower(Health);
    Safety <- tolower(Safety);
    Family <- tolower(Family);
    Environment <- tolower(Environment);
    Instruction <- tolower(Instruction);
    Leader <- tolower(Leader);
    Teacher <- tolower(Teacher);
})
progress_2012_2013_hs <- with(high2013, {
    data.frame(School.ID, School.Name, "HS", ZIP, Overall.Rating,
               On.Probation, Healthy.School.Certified, Safety,
               Involved.Families, Supportive.Environment, 
               Ambitious.Instruction, Effective.Leaders, Collaborative.Teachers, stringsAsFactors=F)
})
colnames(progress_2012_2013_hs) <- name
progress_2012_2013_hs <- within(progress_2012_2013_hs, {
    Rating[Rating=="Not Enough Data"] <- NA;
    Safety[Safety=="Not Enough Data"] <- NA;
    Safety[Safety=="Neutral"] <- "Average";
    Family[Family=="Not Enough Data"] <- NA;
    Family[Family=="Neutral"] <- "Average";
    Environment[Environment=="Not Enough Data"] <- NA;
    Environment[Environment=="Neutral"] <- "Average";
    Instruction[Instruction=="Not Enough Data"] <- NA;
    Instruction[Instruction=="Neutral"] <- "Average";
    Leader[Leader=="Not Enough Data"] <- NA;
    Leader[Leader=="Neutral"] <- "Average";
    Teacher[Teacher=="Not Enough Data"] <- NA;
    Teacher[Teacher=="Neutral"] <- "Average";
    Rating <- tolower(Rating);
    Probation <- tolower(Probation);
    Health <- tolower(Health);
    Safety <- tolower(Safety);
    Family <- tolower(Family);
    Environment <- tolower(Environment);
    Instruction <- tolower(Instruction);
    Leader <- tolower(Leader);
    Teacher <- tolower(Teacher);
})
progress_2012_2013 <- rbind(progress_2012_2013_es, progress_2012_2013_hs)
write.csv(x=progress_2012_2013, file="/Users/zhouye/Dropbox/DSSG/Edit_Prediction/progress_2012_2013.csv", row.names=F)

###2013 - 2014###
progress_2013_2014_es <- with(elem2014, {
    data.frame(School.ID, Name.of.School, "ES", ZIP.Code, CPS.Performance.Policy.Level,
               CPS.Performance.Policy.Status, Healthy.Schools.Certification, Safe,
               Involved.Family, Supportive.Environment, 
               Ambitious.Instruction, Effective.Leaders, Collaborative.Teachers, stringsAsFactors=F)
})
colnames(progress_2013_2014_es) <- name
progress_2013_2014_es <- within(progress_2013_2014_es, {
    Rating[Rating=="NOT ENOUGH DATA" | Rating==""] <- NA;
    Probation[Probation=="NOT APPLICABLE" | Probation==""] <- NA
    Probation[Probation=="ON PROBATION"] <- "Yes"
    Probation[Probation=="NOT ON PROBATION"] <- "No"
    Health[Health=="PENDING CERTIFICATION" | Health==""] <- NA
    Health[Health=="HEALTHY SCHOOLS CERTIFIED"] <- "Yes"
    Health[Health=="NOT CERTIFIED"] <- "No"
    Safety[Safety=="NOT ENOUGH DATA" | Safety==""] <- NA;
    Safety[Safety=="NEUTRAL"] <- "Average";
    Family[Family=="NOT ENOUGH DATA" | Family==""] <- NA;
    Family[Family=="NEUTRAL"] <- "Average";
    Environment[Environment=="NOT ENOUGH DATA" | Environment==""] <- NA;
    Environment[Environment=="NEUTRAL"] <- "Average";
    Instruction[Instruction=="NOT ENOUGH DATA" | Instruction==""] <- NA;
    Instruction[Instruction=="NEUTRAL"] <- "Average";
    Leader[Leader=="NOT ENOUGH DATA" | Leader==""] <- NA;
    Leader[Leader=="NEUTRAL"] <- "Average";
    Teacher[Teacher=="NOT ENOUGH DATA"] <- NA;
    Teacher[Teacher=="NEUTRAL"] <- "Average";
    Rating <- tolower(Rating);
    Probation <- tolower(Probation);
    Health <- tolower(Health);
    Safety <- tolower(Safety);
    Family <- tolower(Family);
    Environment <- tolower(Environment);
    Instruction <- tolower(Instruction);
    Leader <- tolower(Leader);
    Teacher <- tolower(Teacher);
})
progress_2013_2014_hs <- with(high2014, {
    data.frame(School.ID, Name.of.School, "ES", ZIP.Code, CPS.Performance.Policy.Level,
               CPS.Performance.Policy.Status, Healthy.Schools.Certification, Safe,
               Involved.Family, Supportive.Environment, 
               Ambitious.Instruction, Effective.Leaders, Collaborative.Teachers, stringsAsFactors=F)
})
colnames(progress_2013_2014_hs) <- name
progress_2013_2014_hs <- within(progress_2013_2014_hs, {
    Rating[Rating=="NOT ENOUGH DATA" | Rating==""] <- NA;
    Probation[Probation=="NOT APPLICABLE" | Probation==""] <- NA
    Probation[Probation=="ON PROBATION"] <- "Yes"
    Probation[Probation=="NOT ON PROBATION"] <- "No"
    Health[Health=="PENDING CERTIFICATION" | Health==""] <- NA
    Health[Health=="HEALTHY SCHOOLS CERTIFIED"] <- "Yes"
    Health[Health=="NOT CERTIFIED"] <- "No"
    Safety[Safety=="NOT ENOUGH DATA" | Safety==""] <- NA;
    Safety[Safety=="NEUTRAL"] <- "Average";
    Family[Family=="NOT ENOUGH DATA" | Family==""] <- NA;
    Family[Family=="NEUTRAL"] <- "Average";
    Environment[Environment=="NOT ENOUGH DATA" | Environment==""] <- NA;
    Environment[Environment=="NEUTRAL"] <- "Average";
    Instruction[Instruction=="NOT ENOUGH DATA" | Instruction==""] <- NA;
    Instruction[Instruction=="NEUTRAL"] <- "Average";
    Leader[Leader=="NOT ENOUGH DATA" | Leader==""] <- NA;
    Leader[Leader=="NEUTRAL"] <- "Average";
    Teacher[Teacher=="NOT ENOUGH DATA"] <- NA;
    Teacher[Teacher=="NEUTRAL"] <- "Average";
    Rating <- tolower(Rating);
    Probation <- tolower(Probation);
    Health <- tolower(Health);
    Safety <- tolower(Safety);
    Family <- tolower(Family);
    Environment <- tolower(Environment);
    Instruction <- tolower(Instruction);
    Leader <- tolower(Leader);
    Teacher <- tolower(Teacher);
})
progress_2013_2014 <- rbind(progress_2013_2014_es, progress_2013_2014_hs)
write.csv(x=progress_2013_2014, file="/Users/zhouye/Dropbox/DSSG/Edit_Prediction/progress_2013_2014.csv", row.names=F)

setwd("/Volumes/appdata-3/Volan data/01 - FY 06 to FY 14 Enrollment History/csvfiles")

# csv files of Greg's 20th day excel files
enrollment2013mat = read.csv("FY2014_20th_DAY_ENROLLMENT_allgrades.csv")
enrollment2012mat = read.csv("FY2013_20th_DAY_ENROLLMENT_allgrades.csv")
enrollment2011mat = read.csv("FY2012_20th_DAY_ENROLLMENT_allgrades.csv")
enrollment2010mat = read.csv("FY2011_20th_DAY_ENROLLMENT_allgrades.csv")
enrollment2009mat = read.csv("FY2010_20th_DAY_ENROLLMENT_allgrades.csv")

# sum up SPED and LRE
enrollment2013mat$LRE = rowSums(enrollment2013mat[,15:20], na.rm = TRUE)
enrollment2012mat$LRE = rowSums(enrollment2012mat[,15:16], na.rm = TRUE)
enrollment2011mat$LRE = rowSums(enrollment2011mat[,15:17], na.rm = TRUE)
enrollment2010mat$LRE = rowSums(enrollment2010mat[,15:17], na.rm = TRUE)
enrollment2009mat$LRE = rowSums(enrollment2009mat[,15:17], na.rm = TRUE)

# columns to include: school ID, K - 12, LRE total
enrollment2013mat = enrollment2013mat[,c(1:14,21)]
enrollment2012mat = enrollment2012mat[,c(1:14,17)]
enrollment2011mat = enrollment2011mat[,c(1:14,18)]
enrollment2010mat = enrollment2010mat[,c(1:14,18)]
enrollment2009mat = enrollment2009mat[,c(1:14,18)]

# column names
colnames(enrollment2009mat) = c("SCHOOL_CODE", "K","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","LRE")
colnames(enrollment2010mat) = c("SCHOOL_CODE", "K","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","LRE")
colnames(enrollment2011mat) = c("SCHOOL_CODE", "K","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","LRE")
colnames(enrollment2012mat) = c("SCHOOL_CODE", "K","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","LRE")
colnames(enrollment2013mat) = c("SCHOOL_CODE", "K","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","LRE")

# remove NA school codes
enrollment2013mat = enrollment2013mat[!is.na(enrollment2013mat$SCHOOL_CODE),]
enrollment2012mat = enrollment2012mat[!is.na(enrollment2012mat$SCHOOL_CODE),]
enrollment2011mat = enrollment2011mat[!is.na(enrollment2011mat$SCHOOL_CODE),]
enrollment2010mat = enrollment2010mat[!is.na(enrollment2010mat$SCHOOL_CODE),]
enrollment2009mat = enrollment2009mat[!is.na(enrollment2009mat$SCHOOL_CODE),]

# if enrollment is NA, fill in 0
enrollment2013mat[(is.na(enrollment2013mat))] = 0
enrollment2012mat[(is.na(enrollment2012mat))] = 0
enrollment2011mat[(is.na(enrollment2011mat))] = 0
enrollment2010mat[(is.na(enrollment2010mat))] = 0
enrollment2009mat[(is.na(enrollment2009mat))] = 0

# enrollment matrix
temp2009 = reshape(enrollment2009mat, idvar = "SCHOOL_CODE", times = names(enrollment2009mat)[2:15], timevar = "GRADE", varying = list(names(enrollment2009mat)[2:15]), direction = "long", v.names = "ENROLLMENT")
temp2009$YEAR = 2009

temp2010 = reshape(enrollment2010mat, idvar = "SCHOOL_CODE", times = names(enrollment2010mat)[2:15], timevar = "GRADE", varying = list(names(enrollment2010mat)[2:15]), direction = "long", v.names = "ENROLLMENT")
temp2010$YEAR = 2010

temp2011 = reshape(enrollment2011mat, idvar = "SCHOOL_CODE", times = names(enrollment2011mat)[2:15], timevar = "GRADE", varying = list(names(enrollment2011mat)[2:15]), direction = "long", v.names = "ENROLLMENT")
temp2011$YEAR = 2011

temp2012 = reshape(enrollment2012mat, idvar = "SCHOOL_CODE", times = names(enrollment2012mat)[2:15], timevar = "GRADE", varying = list(names(enrollment2012mat)[2:15]), direction = "long", v.names = "ENROLLMENT")
temp2012$YEAR = 2012

temp2013 = reshape(enrollment2013mat, idvar = "SCHOOL_CODE", times = names(enrollment2013mat)[2:15], timevar = "GRADE", varying = list(names(enrollment2013mat)[2:15]), direction = "long", v.names = "ENROLLMENT")
temp2013$YEAR = 2013

enrollmentmatrix = rbind(temp2009, temp2010, temp2011, temp2012, temp2013)

# write to csv
setwd("/Volumes/appdata-3/Count Data")
write.csv(enrollmentmatrix, file = "enrollment_byschool_byyear_bygrade.csv", row.names = FALSE)

# DONE


# ========================================================== #
#   creating school matrix of features that don't change     #
# ========================================================== #

# list of schools for each year
schools2009 = unique(enrollment2009mat$SCHOOL_CODE)
schools2010 = unique(enrollment2010mat$SCHOOL_CODE)
schools2011 = unique(enrollment2011mat$SCHOOL_CODE)
schools2012 = unique(enrollment2012mat$SCHOOL_CODE)
schools2013 = unique(enrollment2013mat$SCHOOL_CODE)

# all schools from fall 2009 - 2013
allschools = union(schools2009, union(schools2010, union(schools2011, union(schools2012, schools2013))))
allschools = allschools[!is.na(allschools)]

# start making all schools matrix
allschoolsmat = as.data.frame(allschools)
colnames(allschoolsmat) = "SCHOOL_CODE"

# all schools in database
setwd("/Volumes/appdata-3/School Reports")
schools_table = read.csv("schools.csv", stringsAsFactors = FALSE)

allschoolsmat = merge(allschoolsmat, schools_table[,c("SCHOOL_CODE","SCHOOL_NAME","SCHOOL_TYPE","SCHOOL_GRADES_GROUP","lon","lat")], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

# use school report from 2011 to try to fill in missing lat/lon
schools_report2011 = read.csv("Chicago_Public_Schools_-_Progress_Report_Cards__2011-2012_.csv", stringsAsFactors = FALSE)

for (i in 1:nrow(allschoolsmat)){
    if (is.na(allschoolsmat[i,"lon"]) & nrow(schools_report2011[schools_report2011$School.ID == allschoolsmat[i,"SCHOOL_CODE"],]) > 0)
    {allschoolsmat[i,"lon"] = schools_report2011[schools_report2011$School.ID == allschoolsmat[i,"SCHOOL_CODE"],"Longitude"] ;
     allschoolsmat[i,"lat"] = schools_report2011[schools_report2011$School.ID == allschoolsmat[i,"SCHOOL_CODE"],"Latitude"]}  
}

library(ggmap)

garfieldparklocation = geocode('3250 W Monroe St, Chicago, IL 60612')
allschoolsmat[(allschoolsmat$SCHOOL_CODE == 400095),"lon"] = garfieldparklocation$lon
allschoolsmat[(allschoolsmat$SCHOOL_CODE == 400095),"lat"] = garfieldparklocation$lat

# crane achievement has same address as crane tech
allschoolsmat[allschoolsmat$SCHOOL_CODE == 610378,"lon"] = allschoolsmat[allschoolsmat$SCHOOL_CODE == 609702,"lon"]
allschoolsmat[allschoolsmat$SCHOOL_CODE == 610378,"lat"] = allschoolsmat[allschoolsmat$SCHOOL_CODE == 609702,"lat"]

# end of filling in lat/lon

# indicator for years when school is active
allschoolsmat$active2009 = NA
allschoolsmat$active2010 = NA
allschoolsmat$active2011 = NA
allschoolsmat$active2012 = NA
allschoolsmat$active2013 = NA

for (i in 1:nrow(allschoolsmat)){
    allschoolsmat[i,"active2009"] = length(intersect(allschoolsmat[i,"SCHOOL_CODE"], schools2009))
    allschoolsmat[i,"active2010"] = length(intersect(allschoolsmat[i,"SCHOOL_CODE"], schools2010))
    allschoolsmat[i,"active2011"] = length(intersect(allschoolsmat[i,"SCHOOL_CODE"], schools2011))
    allschoolsmat[i,"active2012"] = length(intersect(allschoolsmat[i,"SCHOOL_CODE"], schools2012))
    allschoolsmat[i,"active2013"] = length(intersect(allschoolsmat[i,"SCHOOL_CODE"], schools2013))
}

allschoolsmat = allschoolsmat[(allschoolsmat$SCHOOL_CODE != 400152),] # this school doesn't have any enrollment

sum(is.na(allschoolsmat)) # 0

setwd("/Volumes/appdata-3/School Reports")
write.csv(allschoolsmat, file = "schools_staticfeatures.csv", row.names = FALSE)

# DONE


# =============================================== #
#   school features that do change year to year   #
# =============================================== #

# list of schools
setwd("/Volumes/appdata-3/Count Data")
countdata = read.csv("enrollment_byschool_byyear_bygrade.csv")

# make one data frame for each year to start

schools_whatyouknowforfall11 = as.data.frame(unique(countdata[(countdata$YEAR == 2011),"SCHOOL_CODE"]))
colnames(schools_whatyouknowforfall11) = "SCHOOL_CODE"
schools_whatyouknowforfall11$year_topredict = 2011

schools_whatyouknowforfall12 = as.data.frame(unique(countdata[(countdata$YEAR == 2012),"SCHOOL_CODE"]))
colnames(schools_whatyouknowforfall12) = "SCHOOL_CODE"
schools_whatyouknowforfall12$year_topredict = 2012

schools_whatyouknowforfall13 = as.data.frame(unique(countdata[(countdata$YEAR == 2013),"SCHOOL_CODE"]))
colnames(schools_whatyouknowforfall13) = "SCHOOL_CODE"
schools_whatyouknowforfall13$year_topredict = 2013


# race
setwd("/Volumes/appdata-3/School Reports/Ethnicity")
race1011 = read.csv("FY2011_race.csv")
race1112 = read.csv("FY2012_race.csv")
race1213 = read.csv("FY2013_race.csv")

schools_whatyouknowforfall11 = merge(schools_whatyouknowforfall11, race1011, all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "School.ID")
schools_whatyouknowforfall12 = merge(schools_whatyouknowforfall12, race1112, all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "School.ID")
schools_whatyouknowforfall13 = merge(schools_whatyouknowforfall13, race1213, all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "School.ID")


# AUSL turnarounds (source = AUSL site)

# starting fall 2010 or before: bradwell, chicago academy, chicago academy HS,
# collins, curtis, deneen, dodge, dulles, harvard, howe, johnson, morton,
# national teachers academy, orr, phillips, sherman, solorio, tarkington
AUSL_priorfall2010 = c(609806, 610248, 610340, 610499, 609900, 609883, 609888, 
                       610263, 609971, 610000, 610274, 610257, 610231, 610389, 
                       609727, 610172, 610543, 610396)
# starting fall 2011: none

schools_whatyouknowforfall11$AUSL = (lapply(schools_whatyouknowforfall11$SCHOOL_CODE, function(x) (x %in% AUSL_priorfall2010)) == TRUE)


# starting fall 2012: casals, fuller, herzl, marquette, piccolo, stagg
AUSL_fall2012 = c(610021, 609928, 609991, 610053, 610106, 610339)
schools_whatyouknowforfall12$AUSL = (lapply(schools_whatyouknowforfall12$SCHOOL_CODE, function(x) (x %in% AUSL_fall2012)) == TRUE)

# starting fall 2013: carter, chalmers, dewey, lewis, o'keeffe
AUSL_fall2013 = c(609844, 609851, 609885, 610036, 610103)
schools_whatyouknowforfall13$AUSL = (lapply(schools_whatyouknowforfall13$SCHOOL_CODE, function(x) (x %in% AUSL_fall2013)) == TRUE)

# starting fall 2014: dvorak, gresham, mcnair
#AUSL_fall2014 = c(610254, 609955, 610282)

# other turnarounds (by OSI): marshall HS, vocational career academy, tilden career HS, wendell smith elem, woodson south elem



# mobility rate
setwd("/Volumes/appdata-3/School Reports")
mobilitydata = read.csv("mobilityrates.csv", na.strings = c(" ",""))
# I think the year is the year the school year ended 
# (i.e. the rate for 2009 is for the 2008-09 school year)

currentnames = names(schools_whatyouknowforfall12)

schools_whatyouknowforfall11 = merge(schools_whatyouknowforfall11, mobilitydata[,c("School.ID","X2011")], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "School.ID")
colnames(schools_whatyouknowforfall11) = c(currentnames, "mobility_rate")

schools_whatyouknowforfall12 = merge(schools_whatyouknowforfall12, mobilitydata[,c("School.ID","X2012")], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "School.ID")
colnames(schools_whatyouknowforfall12) = c(currentnames, "mobility_rate")

schools_whatyouknowforfall13 = merge(schools_whatyouknowforfall13, mobilitydata[,c("School.ID","X2013")], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "School.ID")
colnames(schools_whatyouknowforfall13) = c(currentnames, "mobility_rate")


# report data
setwd("/Volumes/appdata-3/Master_Data")
progress1112 = read.csv("progress_2011_2012.csv", stringsAsFactors = FALSE)
progress1213 = read.csv("progress_2012_2013.csv", stringsAsFactors = FALSE)
progress1314 = read.csv("progress_2013_2014.csv", stringsAsFactors = FALSE)

schools_whatyouknowforfall11 = merge(schools_whatyouknowforfall11, progress1112[,c(1,5:13)], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SchoolID")
schools_whatyouknowforfall12 = merge(schools_whatyouknowforfall12, progress1213[,c(1,5:13)], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SchoolID")
schools_whatyouknowforfall13 = merge(schools_whatyouknowforfall13, progress1314[,c(1,5:13)], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SchoolID")

schools_whatyouknowforfallxx = rbind(schools_whatyouknowforfall11, schools_whatyouknowforfall12, schools_whatyouknowforfall13)

setwd("/Volumes/appdata-3/School Reports")
write.csv(schools_whatyouknowforfallxx, file = "schools_changingfeatures.csv", row.names = FALSE)

rm(list=ls())
setwd("/Volumes/DSSG/School Reports")
data <- read.csv("schools_changingfeatures.csv")
colnames(data) <- c("SchoolID", "YearToPredict", "White", "Black", "Hispanic", "AUSL", "Mobility", "Rating", "Probation", "Health", "Safety", "Family", "Environment", "Instruction", "Leader", "Teacher")

data2011 <- subset(data, data$YearToPredict==2011)
data2011$YearToPredict <- NULL
write.csv(x=data2011, file="schools_changing_features_2011.csv", row.names=F)

data2012 <- subset(data, data$YearToPredict==2012)
data2012$YearToPredict <- NULL
write.csv(x=data2012, file="schools_changing_features_2012.csv", row.names=F)

data2013 <- subset(data, data$YearToPredict==2013)
data2013$YearToPredict <- NULL
write.csv(x=data2013, file="schools_changing_features_2013.csv", row.names=F)

rm(list=ls())
setwd("/Volumes/DSSG/Master_Data/")
enroll <- read.csv("enrollment.csv", stringsAsFactors=T)
all_year <- sort(unique(enroll$YEAR))
name <- c("SCHOOL_CODE", "Year", "K", "G1", "G2", "G3", "G4", "G5", "G6",
          "G7", "G8", "G9", "G10", "G11", "G12")
old_name <- c("K", "X1", "X2", "X3", "X4", "X5", "X6",
              "X7", "X8", "X9", "X10", "X11", "X12")

active_grade <- function(year) {
    temp <- subset(enroll, enroll$YEAR==year)
    result <- data.frame(matrix(0, nrow(temp), length(name)))
    colnames(result) <- name
    result$SCHOOL_CODE <- temp$SCHOOL_CODE
    result$Year <- year
    for (i in 1:nrow(result)) {
        for (j in 1:length(old_name)) {
            flag <- subset(temp, temp$SCHOOL_CODE==result[i,]$SCHOOL_CODE & temp$GRADE==old_name[j])$ENROLLMENT
            result[i,j+2] <- ifelse(flag>0, 1, 0)
        }
    }
    return(result)
}

data <- list()
for (i in 1:length(all_year)) {
    print(i)
    data[[i]] <- active_grade(all_year[i])
}
final_data <- do.call("rbind", data)
write.csv(x=final_data, file="/Users/zhouye/Dropbox/DSSG/avtive_grade.csv", row.names=F)

rm(list=ls())
setwd("/Volumes/DSSG/Master_Data/")
tract <- read.csv("../Other Datasets/percent_race_by_tract.csv")
static <- read.csv("schools_static_features.csv")
change2011 <- read.csv("schools_changing_features_2011.csv")
change2012 <- read.csv("schools_changing_features_2012.csv")
change2013 <- read.csv("schools_changing_features_2013.csv")
fill_miss <- function(data) {
    index <- which(is.na(rowSums(data[,c("White", "Black", "Hispanic")])))
    for (i in index) {
        print(i)
        id <- data[i,]$SchoolID
        j <- which(static$SchoolID==id)
        fips <- static[j,]$FIPSCode
        code <- as.numeric(substring(fips, 3, 13))
        k <- which(tract$Tract==code)
        if (length(k)!=0) {
            data[i,c("White", "Black", "Hispanic")] <- tract[k,c("PercentWhite", "PercentBlack", "PercentHispanic")]
        }
    }
    return(data)
}
new2011 <- fill_miss(change2011)
new2012 <- fill_miss(change2012)
new2013 <- fill_miss(change2013)
write.csv(x=new2011, file="schools_changing_features_2011.csv", row.names=F)
write.csv(x=new2012, file="schools_changing_features_2012.csv", row.names=F)
write.csv(x=new2013, file="schools_changing_features_2013.csv", row.names=F)



