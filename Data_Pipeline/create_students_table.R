#!/usr/bin/Rscript

###########################
####Create Student data####
###########################

####CPS Team####
####08/20/2014####

setwd("//admin/appdata/DataWarehouse/DSSG/Student_Data")

library(RODBC)

fileName1 <- '//admin/appdata/DataWarehouse/DSSG/Student_Data/student_table_with_ninth_snapshot_vw.sql'
student_query <- readChar(fileName1, file.info(fileName1)$size)
fileName2 <- '//admin/appdata/DataWarehouse/DSSG/Student_Data/student_ISAT_scores_vw.sql'
tests_query <- readChar(fileName2, file.info(fileName2)$size)
fileName3 <- '//admin/appdata/DataWarehouse/DSSG/Student_Data/student_attendance_vw.sql'
attendance_query <- readChar(fileName3, file.info(fileName3)$size)

# an ODBC called "DW_QA" which accesses the K12intel_qa database
channel <- odbcConnect("DW_QA")
students = sqlQuery(channel, student_query, stringsAsFactors = TRUE)
tests = sqlQuery(channel, tests_query)
attendance = sqlQuery(channel, attendance_query)
odbcCloseAll()

# indicators of whether the student left the system after 8th
# or entered the system in 9th grade
students$Left <- as.numeric(is.na(students$NextGradeSchoolKey))
students$Entered <- as.numericis.na(students$ThisGradeSchoolKey))

# combine with students testing and attendance information
students = merge(students, tests, all.x = TRUE)
students = merge(students, attendance, all.x = TRUE)

# add in geocodes
student_geo = read.csv("student_geo_correct.csv")
students = merge(students, student_geo[,-2], all.x = TRUE)

# split up by year and save
for (year in 2011:2013) {
  students_year = subset(students, SchoolYear == (year-1))
  students_year = students_year[,-which(colnames(students_year)=="SchoolYear")]
  write.table(students_year, paste0("//admin/appdata/DataWarehouse/DSSG/Master_Data/students",year,".csv"), sep = ",", row.names = FALSE)
}
