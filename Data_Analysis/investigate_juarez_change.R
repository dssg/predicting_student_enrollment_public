# source('C:/Users/Andrew/github/predicting_student_enrollment/estimation-sandbox/feeder_school_prediction_89.R')
setwd("//admin/appdata/DataWarehouse/DSSG")
load("//admin/appdata/DataWarehouse/DSSG/Student_Data/students_89_snap.RData")
schools = read.csv("//admin/appdata/DataWarehouse/DSSG/School Reports/schools.csv")
library(fields)

students = subset(students, !(STUDENT_EDUCATIONAL_EXCEPT_TYP %in% c("21 - 60%", "61 - 100%")))
students$THIS_GRADE_SCHOOL_KEY[is.na(students$THIS_GRADE_SCHOOL_KEY)] = 0
students$NEXT_GRADE_SCHOOL_KEY[is.na(students$NEXT_GRADE_SCHOOL_KEY)] = 0

tbl2012 = with(subset(students, SCHOOL_YEAR == 2011), table(THIS_GRADE_SCHOOL_KEY, NEXT_GRADE_SCHOOL_KEY))
tbl2013 = with(subset(students, SCHOOL_YEAR == 2012), table(THIS_GRADE_SCHOOL_KEY, NEXT_GRADE_SCHOOL_KEY))

sc_code = 609713
sc_code = 609764
sc_code = 610563
key = subset(schools, SCHOOL_CODE == sc_code, SCHOOL_KEY)[1,1]
location = subset(schools, SCHOOL_CODE == sc_code, c(lon,lat))
el_schools2012 = tbl2012[,colnames(tbl2012)==key]
el_schools2013 = tbl2013[,colnames(tbl2013)==key]

el_schools2012df = data.frame(SCHOOL_KEY = names(el_schools2012), Num2012 = el_schools2012)
el_schools2013df = data.frame(SCHOOL_KEY = names(el_schools2013), Num2013 = el_schools2013)

el_schoolsdf = merge(el_schools2012df, el_schools2013df, all = TRUE)
el_schoolsdf = el_schools2013df
el_schoolsdf$Num2012[is.na(el_schoolsdf$Num2012)] = 0
el_schoolsdf$Num2013[is.na(el_schoolsdf$Num2013)] = 0
el_schoolsdf = subset(el_schoolsdf, Num2012>0 | Num2013>0)
el_schoolsdf = merge(el_schoolsdf, subset(schools, , c(SCHOOL_KEY, SCHOOL_CODE, SCHOOL_SHORT_NAME, lon, lat)), all.x = TRUE)
dist.mat = rdist.earth(subset(el_schoolsdf, , c(lon, lat)), location)
el_schoolsdf = cbind(el_schoolsdf, Distance = dist.mat[,1])
el_schoolsdf$Change = el_schoolsdf$Num2013 - el_schoolsdf$Num2012

head(el_schoolsdf[order(-abs(el_schoolsdf$Num2012-el_schoolsdf$Num2013)),], 10)
head(el_schoolsdf[order(-abs(el_schoolsdf$Num2013)),], 10)

qplot(Distance,Change,data = el_schoolsdf)

sc_code = 610329
key = subset(schools, SCHOOL_CODE == sc_code, SCHOOL_KEY)[1,1]
location = subset(schools, SCHOOL_CODE == sc_code, c(lon,lat))

high_schools2012 = tbl2012[rownames(tbl2012)==key,]
high_schools2013 = tbl2013[rownames(tbl2013)==key,]

high_schools2012df = data.frame(SCHOOL_KEY = names(high_schools2012), Num2012 = high_schools2012)
high_schools2013df = data.frame(SCHOOL_KEY = names(high_schools2013), Num2013 = high_schools2013)

high_schoolsdf = merge(high_schools2012df, high_schools2013df, all = TRUE)
high_schoolsdf$Num2012[is.na(high_schoolsdf$Num2012)] = 0
high_schoolsdf$Num2013[is.na(high_schoolsdf$Num2013)] = 0
high_schoolsdf = subset(high_schoolsdf, Num2012>0 | Num2013>0)
high_schoolsdf = merge(high_schoolsdf, subset(schools, , c(SCHOOL_KEY, SCHOOL_CODE, SCHOOL_SHORT_NAME, lon, lat)), all.x = TRUE)
dist.mat = rdist.earth(subset(high_schoolsdf, , c(lon, lat)), location)
high_schoolsdf = cbind(high_schoolsdf, Distance = dist.mat[,1])
high_schoolsdf$Change = high_schoolsdf$Num2013 - high_schoolsdf$Num2012

head(high_schoolsdf[order(-abs(high_schoolsdf$Num2012-high_schoolsdf$Num2013)),], 10)
tail(high_schoolsdf[order(high_schoolsdf$Num2012-high_schoolsdf$Num2013),], 10)
head(high_schoolsdf[order(-abs(high_schoolsdf$Num2012)),], 10)

colSums(high_schoolsdf[,2:3])
