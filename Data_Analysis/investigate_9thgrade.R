# source('C:/Users/Andrew/github/predicting_student_enrollment/estimation-sandbox/feeder_school_prediction_89.R')
setwd("/Volumes/appdata-3")
load("/Volumes/appdata-3/Student_Data/students_89_snap.RData")
schools = read.csv("/Volumes/appdata-3/School Reports/schools.csv")

library(fields)

students = subset(students, !(STUDENT_EDUCATIONAL_EXCEPT_TYP %in% c("21 - 60%", "61 - 100%")))
students$THIS_GRADE_SCHOOL_KEY[is.na(students$THIS_GRADE_SCHOOL_KEY)] = 0
students$NEXT_GRADE_SCHOOL_KEY[is.na(students$NEXT_GRADE_SCHOOL_KEY)] = 0

tbl2011_0 = as.data.frame(with(subset(students, SCHOOL_YEAR == 2010 & THIS_GRADE_SCHOOL_KEY == 0), table(NEXT_GRADE_SCHOOL_KEY)))
tbl2011_0$studentsource = 'outside_CPS'
#colnames(tbl2011_0) = c("SCHOOL_KEY", "Num_0")

tbl2011_catchment = as.data.frame(with(subset(students, SCHOOL_YEAR == 2010 & NEXT_GRADE_SCHOOL_KEY == CATCHMENT_SCHOOL_KEY), table(NEXT_GRADE_SCHOOL_KEY)))
tbl2011_catchment$studentsource = 'catchment'
#colnames(tbl2011_catchment) = c("SCHOOL_KEY", "Num_catchment")

tbl2011_other = as.data.frame(with(subset(students, SCHOOL_YEAR == 2010 & THIS_GRADE_SCHOOL_KEY > 0 & NEXT_GRADE_SCHOOL_KEY != CATCHMENT_SCHOOL_KEY), table(NEXT_GRADE_SCHOOL_KEY)))
tbl2011_other$studentsource = 'other'
#colnames(tbl2011_other) = c("SCHOOL_KEY", "Num_other")

tbl2011 = rbind(tbl2011_0, tbl2011_catchment, tbl2011_other)
tbl2011$year = 2011
#tbl2011 = merge(tbl2011_0, tbl2011_catchment, all = TRUE)
#tbl2011 = merge(tbl2011, tbl2011_other, all = TRUE)


tbl2012_0 = as.data.frame(with(subset(students, SCHOOL_YEAR == 2011 & THIS_GRADE_SCHOOL_KEY == 0), table(NEXT_GRADE_SCHOOL_KEY)))
tbl2012_0$studentsource = 'outside_CPS'
#colnames(tbl2012_0) = c("SCHOOL_KEY", "Num_0")
tbl2012_catchment = as.data.frame(with(subset(students, SCHOOL_YEAR == 2011 & NEXT_GRADE_SCHOOL_KEY == CATCHMENT_SCHOOL_KEY), table(NEXT_GRADE_SCHOOL_KEY)))
tbl2012_catchment$studentsource = 'catchment'
#colnames(tbl2012_catchment) = c("SCHOOL_KEY", "Num_catchment")
tbl2012_other = as.data.frame(with(subset(students, SCHOOL_YEAR == 2011 & THIS_GRADE_SCHOOL_KEY > 0 & NEXT_GRADE_SCHOOL_KEY != CATCHMENT_SCHOOL_KEY), table(NEXT_GRADE_SCHOOL_KEY)))
tbl2012_other$studentsource = 'other'
#colnames(tbl2012_other) = c("SCHOOL_KEY", "Num_other")

tbl2012 = rbind(tbl2012_0, tbl2012_catchment, tbl2012_other)
tbl2012$year = 2012
#tbl2012 = merge(tbl2012_0, tbl2012_catchment, all = TRUE)
#tbl2012 = merge(tbl2012, tbl2012_other, all = TRUE)

tbl2013_0 = as.data.frame(with(subset(students, SCHOOL_YEAR == 2012 & THIS_GRADE_SCHOOL_KEY == 0), table(NEXT_GRADE_SCHOOL_KEY)))
tbl2013_0$studentsource = 'outside_CPS'
#colnames(tbl2013_0) = c("SCHOOL_KEY", "Num_0")
tbl2013_catchment = as.data.frame(with(subset(students, SCHOOL_YEAR == 2012 & NEXT_GRADE_SCHOOL_KEY == CATCHMENT_SCHOOL_KEY), table(NEXT_GRADE_SCHOOL_KEY)))
tbl2013_catchment$studentsource = 'catchment'
#colnames(tbl2013_catchment) = c("SCHOOL_KEY", "Num_catchment")
tbl2013_other = as.data.frame(with(subset(students, SCHOOL_YEAR == 2012 & THIS_GRADE_SCHOOL_KEY > 0 & NEXT_GRADE_SCHOOL_KEY != CATCHMENT_SCHOOL_KEY), table(NEXT_GRADE_SCHOOL_KEY)))
tbl2013_other$studentsource = 'other'
#colnames(tbl2013_other) = c("SCHOOL_KEY", "Num_other")

tbl2013 = rbind(tbl2013_0, tbl2013_catchment, tbl2013_other)
tbl2013$year = 2013
#tbl2013 = merge(tbl2013_0, tbl2013_catchment, all = TRUE)
#tbl2013 = merge(tbl2013, tbl2013_other, all = TRUE)

#tbl2011$Year = 2011
#tbl2012$Year = 2012
#tbl2013$Year = 2013

tbl9thgradesources = rbind(tbl2011, tbl2012, tbl2013)
setwd("/Volumes/appdata-3/Student_Data")
write.csv(tbl9thgradesources, file = "tbl9thgradesources_tracy.csv", row.names = FALSE)


# all to-and-from data
tbl2011_all = as.data.frame(with(subset(students, SCHOOL_YEAR == 2010), table(CATCHMENT_SCHOOL_KEY, NEXT_GRADE_SCHOOL_KEY)))
tbl2011_all$year = 2011

tbl2012_all = as.data.frame(with(subset(students, SCHOOL_YEAR == 2011), table(CATCHMENT_SCHOOL_KEY, NEXT_GRADE_SCHOOL_KEY)))
tbl2012_all$year = 2012

tbl2013_all = as.data.frame(with(subset(students, SCHOOL_YEAR == 2012), table(CATCHMENT_SCHOOL_KEY, NEXT_GRADE_SCHOOL_KEY)))
tbl2013_all$year = 2013

tblall = rbind(tbl2011_all, tbl2012_all, tbl2013_all)

tblall = merge(tblall, schools[,c("SCHOOL_KEY","SCHOOL_SHORT_NAME")], all.x = TRUE, by.x = "CATCHMENT_SCHOOL_KEY", by.y = "SCHOOL_KEY")
colnames(tblall)[5] = 'CATCHMENT_SCHOOL_NAME'

tblall = merge(tblall, schools[,c("SCHOOL_KEY","SCHOOL_SHORT_NAME")], all.x = TRUE, by.x = "NEXT_GRADE_SCHOOL_KEY", by.y = "SCHOOL_KEY")
colnames(tblall)[6] = 'NEXT_GRADE_SCHOOL_NAME'

setwd("/Volumes/appdata-3/School Reports")
write.csv(tblall, file = "tbl_9thgrade_toandfrom_tracy.csv", row.names = FALSE)








setwd("/Volumes/appdata-3/Count Data")
countdata = read.csv("enrollment_byschool_byyear_bygrade.csv")

# plot by school
countdata_wide = reshape(countdata, v.names = "ENROLLMENT", timevar = "YEAR", idvar = c("SCHOOL_CODE","GRADE"), direction = "wide")
countdata_wide[is.na(countdata_wide)] = 0

grade9change = subset(countdata_wide, GRADE == 'X9' & ENROLLMENT.2012 > 0 & ENROLLMENT.2013 > 0)
head(grade9change[order(abs(grade9change$ENROLLMENT.2013 - grade9change$ENROLLMENT.2012), decreasing = T),c("SCHOOL_CODE")])

plotschool(609707)

plotschool = function(sc_code){
key = subset(schools, SCHOOL_CODE == sc_code, SCHOOL_KEY)[1,1]
name = subset(schools, SCHOOL_CODE == sc_code, SCHOOL_NAME)[1,1]

test = matrix(nrow = 3, ncol = 3)
test = as.data.frame(test)
rownames(test) = c("Catchment","Other","Outside district")
colnames(test) = c("2011", "2012", "2013")
test[1,1] = subset(tbl2011, SCHOOL_KEY == key, select = "Num_catchment")
test[2,1] = subset(tbl2011, SCHOOL_KEY == key, select = "Num_other")
test[3,1] = subset(tbl2011, SCHOOL_KEY == key, select = "Num_0")

test[1,2] = subset(tbl2012, SCHOOL_KEY == key, select = "Num_catchment")
test[2,2] = subset(tbl2012, SCHOOL_KEY == key, select = "Num_other")
test[3,2] = subset(tbl2012, SCHOOL_KEY == key, select = "Num_0")

test[1,3] = subset(tbl2013, SCHOOL_KEY == key, select = "Num_catchment")
test[2,3] = subset(tbl2013, SCHOOL_KEY == key, select = "Num_other")
test[3,3] = subset(tbl2013, SCHOOL_KEY == key, select = "Num_0")
test = as.matrix(test)
test[is.na(test)]=0

barplot(test, beside = F, legend.text = T, args.legend = list(x = 'topleft'),main =paste(name),ylab="Grade 9 enrollment",col=c(2,4,5))
}







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
