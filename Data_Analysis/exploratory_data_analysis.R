# ---------------------- #
#   Exploring CPS data   #
# ---------------------- #

# ======================================================= #
#   exploring schools that are far off from projections   #
# ======================================================= #

# greg's data
setwd("/Volumes/appdata-3/School Reports") 
greg_data = read.csv("CSVfileforR_excerptof_FY14SBBALLOCATIONS_FINALSUMMARY_asof_032414.csv", header = TRUE)

# how far off are projections in aggregate for different types of schools?
greg_data$dif = greg_data$Projected.Total - greg_data$Actual.Total

# look at elementary schools off by more than 50
elem_50off = greg_data[(abs(greg_data$Projected.Total - greg_data$Actual.Total) > 50)&(greg_data$School.Type == "Elementary"),]
elem_50off$dif = elem_50off$Projected.Total - elem_50off$Actual.Total
elem_50off$propbudget = elem_50off$dif/elem_50off$Projected.Total
elem_50off[order(elem_50off$propbudget),c(1,2,13,22,23,24)]

# high schools off by more than 50
hs_50off = greg_data[(abs(greg_data$Projected.Total - greg_data$Actual.Total) > 50)&(greg_data$School.Type == "High School"),]
hs_50off$dif = hs_50off$Projected.Total - hs_50off$Actual.Total
hs_50off$propbudget = hs_50off$dif/hs_50off$Projected.Total
hs_50off[order(hs_50off$propbudget),c(1,2,13,22,23,24)]

# charter schools off by more than 50
charter_50off = greg_data[(abs(greg_data$Projected.Total - greg_data$Actual.Total) > 50)&(greg_data$School.Type == "Charter"),]
charter_50off$dif = charter_50off$Projected.Total - charter_50off$Actual.Total
charter_50off$propbudget = charter_50off$dif/charter_50off$Projected.Total
charter_50off[order(charter_50off$propbudget),c(1,2,13,22,23,24)]


# ============================================================ #
#   exploring 2012 enrollments (less extreme year than 2013)   #
# ============================================================ #

library(lubridate)
library(plyr)

# read count data
setwd("/Volumes/appdata-3/Count Data")

counts = read.csv("Enrollment_Counts_Month_Current.csv", stringsAsFactors = FALSE)
counts$Date = as.Date(counts$Date)
counts$STUDENT_ANNUAL_GRADE_CODE = factor(counts$STUDENT_ANNUAL_GRADE_CODE, c('PE','PK','K','1','2','3','4','5','6','7','8','9','10','11','12'))

counts$Year = as.numeric(substring(counts$SCHOOL_YEAR,6,9))
counts$Month = month(counts$Date)
names(counts)[names(counts)=="STUDENT_ANNUAL_GRADE_CODE"] = "Grade"

# read school data
setwd("/Volumes/appdata-3/School Reports")

report_elem = read.csv("CPS_Elementary_Report_20122013.csv")
report_high = read.csv("CPS_HighSchool_Report_20122013.csv")
levels(report_elem$Student.Performance.Level) <- c("Far Below Average", "Below Average", "Average", "Above Average", "Far Above Average")
report_high$Student.Performance.Level = factor(report_high$Student.Performance.Level, c("Far Below Average", "Below Average", "Average", "Above Average", "Far Above Average"))
report_elem$Overall.Rating = factor(report_elem$Overall.Rating, levels = c("Level 1", "Level 2", "Level 3"))
report_high$Overall.Rating = factor(report_high$Overall.Rating, levels = c("Level 1", "Level 2", "Level 3"))

school_reports <- rbind(subset(report_elem, select = c(School.ID, Student.Performance.Level, Overall.Rating)), subset(report_high, select = c(School.ID, Student.Performance.Level, Overall.Rating)))

# function for adding grades
advance_grade <- function(current_grade, years = 1) {
  locs = apply(matrix(current_grade),1,function(x) which(x == levels(counts$Grade))) + years
  is.na(locs[locs<1 | locs>length(levels(counts$Grade))]) <- TRUE
  levels(counts$Grade)[locs]
}

# Enrollments we want to predict
pred_enrolls = subset(counts, Month == 10, select = c(Year, SCHOOL_KEY, SCHOOL_NAME, SCHOOL_CODE, Grade, Enrollments))

# Last year 20th day enrollments
prev_20_enrolls = subset(counts, Month == 10, select = c(Year, SCHOOL_KEY, SCHOOL_NAME, SCHOOL_CODE, Grade, Enrollments))
prev_20_enrolls$Year = prev_20_enrolls$Year + 1
names(prev_20_enrolls)[names(prev_20_enrolls)=="Enrollments"] <- "Prev_20_Enrollments"

# Last year 20th day enrollments for previous grade
prev_20_enrolls_grade = prev_20_enrolls
prev_20_enrolls_grade$Grade = advance_grade(prev_20_enrolls_grade$Grade, 1)
names(prev_20_enrolls_grade)[names(prev_20_enrolls_grade)=="Prev_20_Enrollments"] <- "Prev_Grade_20_Enrollments"

# Last year end of year enrollments
prev_end_enrolls = subset(counts, Month == 6, select = c(Year, SCHOOL_KEY, SCHOOL_NAME, SCHOOL_CODE, Grade, Enrollments))
prev_end_enrolls$Year = prev_end_enrolls$Year + 1
names(prev_end_enrolls)[names(prev_end_enrolls)=="Enrollments"] <- "Prev_End_Enrollments"

# Last year end of year enrollments for previous grade
prev_end_enrolls_grade = prev_end_enrolls
prev_end_enrolls_grade$Grade = advance_grade(prev_end_enrolls_grade$Grade, 1)
names(prev_end_enrolls_grade)[names(prev_end_enrolls_grade)=="Prev_End_Enrollments"] <- "Prev_Grade_End_Enrollments"

# combine to one dataset
enrolls <- merge(pred_enrolls, prev_20_enrolls, all.x=TRUE)
enrolls <- merge(enrolls, prev_20_enrolls_grade, all.x=TRUE)
enrolls <- merge(enrolls, prev_end_enrolls, all.x=TRUE)
enrolls <- merge(enrolls, prev_end_enrolls_grade, all.x=TRUE)

# add in some school information
enrolls <- merge(enrolls, school_reports, all.x=TRUE, by.x = "SCHOOL_CODE", by.y = "School.ID")

schools_basicdata = read.csv("schools.csv")

enrolls <- merge(enrolls, schools_basicdata, all.x=TRUE, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

# look at 2011 to 2012 enrollments
schools_fall11to12 = ddply(enrolls, .(SCHOOL_CODE), summarize, fall2011enrollment = sum(Enrollments[Year == 2012]), fall2012enrollment = sum(Enrollments[Year == 2013]))
schools_fall11to12 = subset(schools_fall11to12, (fall2011enrollment > 0)|(fall2012enrollment > 0))

offby50 = schools_fall11to12[(abs(schools_fall11to12$fall2012enrollment - schools_fall11to12$fall2011enrollment) >= 100)&(schools_fall11to12$fall2011enrollment > 0)&(schools_fall11to12$fall2012enrollment > 0),]

school_info = ddply(enrolls, .(SCHOOL_CODE), summarize, Min_Year = min(Year, na.rm=TRUE), Max_Year = max(Year, na.rm=TRUE))

newschools_fall2012 = school_info[(school_info$Min_Year == 2013),]
closingschools_fall2012 = school_info[(school_info$Max_Year == 2012),]

# ------------------------- #
#   new schools fall 2013   #
# ------------------------- #

newschools_fall2013 = school_info[(school_info$Min_Year == 2014),]

# pick 1 new school from fall 2013 to analyze

newschool1_fall2013 = newschools_fall2013[13,]$SCHOOL_CODE
newschool1_type = schools_basicdata[(schools_basicdata$SchoolID == newschool1_fall2013),]$SchoolGradeGroup
newschool1_location = schools_basicdata[(schools_basicdata$SchoolID == newschool1_fall2013),c("Longitude","Latitude")]
newschool1_name = schools_basicdata[(schools_basicdata$SchoolID == newschool1_fall2013),]$SchoolName

dist_school1 = rdist.earth(newschool1_location, schools_basicdata[,c("Longitude","Latitude")], miles = TRUE, R = NULL)
schools_within1mile = schools_basicdata[(dist_school1 <= 1),]
schools_within1mile = schools_within1mile[(!is.na(schools_within1mile$SchoolID)),]

test = merge(schools_within1mile, enrolls, all.x = TRUE, by.x = "SchoolID", by.y = "SCHOOL_CODE")
test = test[(test$SchoolGradeGroup == newschool1_type),]

table(test[(test$SchoolID == newschool1_fall2013),]$Grade)

test2 = ddply(test, .(SchoolID, Year, Longitude, Latitude), summarize, total_enr = sum(Enrollments, na.rm = TRUE))
test2 = test2[(!is.na(test2$Year)),]

scaleparam = scale_size(limits=c(0,max(test2$total_enr)), range = c(1,15))

enr = as.data.frame(test2[(test2$Year == "2012"),])
qm3 = qmap(location = c(newschool1_location$Longitude, newschool1_location$Latitude) , zoom = 14) + labs(title = '2011-2012', size = "enrollment") +geom_point(data = enr[(enr$SchoolID != newschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr), show_guide = FALSE) + scaleparam
print(qm3)

enr = as.data.frame(test2[(test2$Year == "2013"),])
qm4 = qmap(location = c(newschool1_location$Longitude, newschool1_location$Latitude) , zoom = 14) +labs(title = '2012-2013', size = "enrollment")+ geom_point(data = enr[(enr$SchoolID != newschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr), show_guide = FALSE) + scaleparam
print(qm4)

enr = as.data.frame(test2[(test2$Year == "2014"),])
#qm5 = qmap(location = c(newschool1_location$lon, newschool1_location$lat) , zoom = 14) + labs(title = '2013-2014: newschool', size = "enrollment")+geom_point(data = enr[(enr$SCHOOL_CODE == newschool1_fall2013),], aes(x = lon.x, y = lat.x, size = total_enr), position = position_jitter(w = .0005, h = .0005), color = 'red', show_guide = FALSE) + geom_point(data = enr[(enr$SCHOOL_CODE != newschool1_fall2013),], aes(x = lon.x, y = lat.x, size = total_enr), show_guide = FALSE) + scaleparam
qm5 = qmap(location = c(newschool1_location$Longitude, newschool1_location$Latitude) , zoom = 14) + labs(title = '2013-2014: Back of the Yards IB HS', size = "enrollment")+geom_point(data = enr[(enr$SchoolID == newschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr), color = 'red', show_guide = FALSE) + geom_point(data = enr[(enr$SchoolID != newschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr), show_guide = FALSE) + scaleparam
print(qm5)


# school closings
closeschools_fall2013 = school_info[(school_info$Max_Year == 2013),]

# pick 1 school that closed in spring 2013 to analyze
closeschool1_fall2013 = sample(unique(closeschools_fall2013$SCHOOL_CODE), size = 1)
closeschool1_type = schools_basicdata[(schools_basicdata$SchoolID == closeschool1_fall2013),]$SchoolGradeGroup
closeschool1_location = report_elem[(report_elem$School.ID == closeschool1_fall2013),c("Longitude","Latitude")]
closeschool1_name = schools_basicdata[(schools_basicdata$SchoolID == closeschool1_fall2013),]$SchoolName

dist_school1 = rdist.earth(closeschool1_location[c(2,1)], schools_basicdata[,c("Longitude","Latitude")], miles = TRUE, R = NULL)

schools_within1mile = schools_basicdata[((dist_school1 <= 1)|(schools_basicdata$SchoolID == closeschool1_fall2013)),]
schools_within1mile = schools_within1mile[(!is.na(schools_within1mile$SchoolID)),]

schools_within1mile[(schools_within1mile$SchoolID == closeschool1_fall2013),]$Longitude = closeschool1_location$Latitude
schools_within1mile[(schools_within1mile$SchoolID == closeschool1_fall2013),]$Latitude = closeschool1_location$Longitude

test = merge(schools_within1mile, enrolls, all.x = TRUE, by.x = "SchoolID", by.y = "SCHOOL_CODE")
test = test[(test$SchoolGradeGroup == closeschool1_type),]

table(test[(test$SchoolID == closeschool1_fall2013),]$Grade) # K - 8
test = test[-(test$Grade == 'PE'),]
test = test[-(test$Grade == 'PK'),]

test2 = ddply(test, .(SchoolID, Year, Longitude, Latitude), summarize, total_enr = sum(Enrollments, na.rm = TRUE))
#test2 = test2[(!is.na(test2$Year)),]

enr = test2[(test2$Year == "2010"),]
qm1 = qmap(location = c(closeschool1_location$Latitude, closeschool1_location$Longitude), zoom = 14) + labs(title = '2010') + geom_point(data = enr[(enr$SchoolID == closeschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr), position = position_jitter(w = .0005, h = .0005), color = 'cyan') + geom_point(data = enr[(enr$SchoolID != closeschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr)) + scale_size(breaks = c(200,400,600,800,1000), limits=c(0,1000), range = c(1,15))

enr = test2[(test2$Year == "2011"),]
qm2 = qmap(location = c(closeschool1_location$Latitude, closeschool1_location$Longitude) , zoom = 14) + labs(title = '2011')+geom_point(data = enr[(enr$SchoolID == closeschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr), position = position_jitter(w = .0005, h = .0005), color = 'cyan')+ geom_point(data = enr[(enr$SchoolID != closeschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr)) + scale_size(breaks = c(200,400,600,800,1000), limits=c(0,1000), range = c(1,15))

enr = test2[(test2$Year == "2012"),]
qm3 = qmap(location = c(closeschool1_location$Latitude, closeschool1_location$Longitude) , zoom = 14) + labs(title = '2012') +geom_point(data = enr[(enr$SchoolID == closeschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr), position = position_jitter(w = .0005, h = .0005), color = 'cyan') +geom_point(data = enr[(enr$SchoolID != closeschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr)) + scale_size(breaks = c(200,400,600,800,1000), limits=c(0,1000), range = c(1,15))

enr = test2[(test2$Year == "2013"),]
qm4 = qmap(location = c(closeschool1_location$Latitude, closeschool1_location$Longitude) , zoom = 14) +labs(title = '2013')+ geom_point(data = enr[(enr$SchoolID == closeschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr), position = position_jitter(w = .0005, h = .0005), color = 'cyan') +geom_point(data = enr[(enr$SchoolID != closeschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr)) + scale_size(breaks = c(200,400,600,800,1000), limits=c(0,1000), range = c(1,15))

enr = test2[(test2$Year == "2014"),]
qm5 = qmap(location = c(closeschool1_location$Latitude, closeschool1_location$Longitude) , zoom = 14) + labs(title = '2014') + geom_point(data = enr[(enr$SchoolID != closeschool1_fall2013),], aes(x = Longitude, y = Latitude, size = total_enr)) + scale_size(breaks = c(200,400,600,800,1000), limits=c(0,1000), range = c(1,15))

pushViewport(viewport(layout = grid.layout(2, 3)))
print(qm1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(qm2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(qm3, vp = viewport(layout.pos.row = 1, layout.pos.col = 3))
print(qm4, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(qm5, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
