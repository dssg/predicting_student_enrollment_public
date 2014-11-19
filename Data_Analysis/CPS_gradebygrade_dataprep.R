# ====================================================== #
#   grade-by-grade data prep for "predictschool" script  #
# ====================================================== #

# data should have columns named "SCHOOL_CODE", "enrollment_to_predict", "projection" and "GRADE"
# "projection" can be something like last year's enrollment or the cohort survival projection
# all other columns are main effect predictors

# things to remember:
#
# (1) Starting fall 2012, YCCS charter school went from code 609686 to these codes, 
# according to Greg's FY2013 spreadsheet: c(400123:400137,400139,400141:400145,400150)
#
# (2) It looks like James Shields Elementary school had been K-8 and split in fall 2012 to
# code 610174 (elementary school, now K-4) and code 610559 (the middle school grades 5-8)
#
# (3) It looks like 400067 Calumet MS combined with 400061 Calumet HS in fall 2012
#

# --------------------------------------------------------- #
# here I prepare data to predict 2013 enrollments by grade: #
# --------------------------------------------------------- #

setwd("/Volumes/appdata-3/Count Data")
countdata = read.csv("enrollment_byschool_byyear_bygrade.csv")

setwd("/Volumes/appdata-3/School Reports")
schoolfeatures = read.csv("schools_staticfeatures.csv", stringsAsFactors = F)

# add school features for each grade
data = merge(countdata, schoolfeatures, all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

# pull out 2013
data13 = subset(data, YEAR == 2013)

# main features
data13 = data13[,c(1,2,3,6)]
colnames(data13) = c("SCHOOL_CODE", "GRADE", "enrollment_to_predict","SCHOOL_TYPE")

# REMOVE YCCS
data13 = data13[(data13$SCHOOL_CODE %in% c(400123:400137,400139,400141:400145,400150)) == "FALSE",]
data13 = data13[(data13$SCHOOL_CODE %in% 609686) == 'FALSE',]



# previous year
data12 = subset(data, YEAR == 2012)

# main features
data12 = data12[,c(1,2,3)]
colnames(data12) = c("SCHOOL_CODE", "GRADE", "projection")
projection_long = data12

# REMOVE YCCS
projection_long = projection_long[(projection_long$SCHOOL_CODE %in% c(400123:400137,400139,400141:400145,400150, 609686)) == 'FALSE',]

data13_final = merge(data13, projection_long, all.x = TRUE, by.x = c("SCHOOL_CODE","GRADE"), by.y = c("SCHOOL_CODE","GRADE"))

# for new schools, fill in 0 for projection
new13 = schoolfeatures[(schoolfeatures$active2012 == 0)&(schoolfeatures$active2013 == 1),"SCHOOL_CODE"]
data13_final[(data13_final$SCHOOL_CODE %in% new13)&(is.na(data13_final$projection)),"projection"] = 0

# which schools are missing projection
schoolsNA = data13_final[(is.na(data13_final$projection)),"SCHOOL_CODE"]
schoolfeatures[(schoolfeatures$SCHOOL_CODE %in% schoolsNA)==TRUE,]

# make an indicator for whether a grade at a school is new
data13_final$NEW = ((data13_final$enrollment_to_predict > 0)&(data13_final$projection == 0))

setwd("/Volumes/appdata-3/Master_Data")
progress1314 = read.csv("schools_changing_features_2013.csv", stringsAsFactors = F)

datafinal13 = merge(data13_final, progress1314[,!names(progress1314) %in% c("SchoolName","SchoolType","ZipCode")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SchoolID")
datafinal13 = subset(datafinal13, enrollment_to_predict > 0)

# new grades nearby
for (grade in levels(datafinal13$GRADE)){
  grademat = subset(datafinal13, GRADE == grade)
  grademat = merge(grademat, schoolfeatures[,c("SCHOOL_CODE","lon","lat")], all.x = TRUE, by.x = "SCHOOL_CODE",by.y = "SCHOOL_CODE")
  
  newgrades = subset(datafinal13, (enrollment_to_predict >= 10)&(projection == 0)&(GRADE == grade))
  newlocations = schoolfeatures[(schoolfeatures$SCHOOL_CODE %in% newgrades$SCHOOL_CODE), c("lon","lat")]
  
  grademat$new_nearby = 0
  
  library("fields")
  for (i in 1:nrow(grademat)){
    dist_to_school = rdist.earth(grademat[i,c("lon","lat")], newlocations, miles = TRUE, R = NULL)
    
    # how many new schools within 2 miles?
    grademat[i,"new_nearby"] = sum(dist_to_school <=2)  
  }
  
  currentnames = names(datafinal13)
  datafinal13 = merge(datafinal13, grademat[,c("SCHOOL_CODE","GRADE","new_nearby")], all.x = TRUE, by.x = c("SCHOOL_CODE","GRADE"), by.y = c("SCHOOL_CODE","GRADE"))
  names(datafinal13) = c(currentnames, paste('new',grade))
}

datafinal13$newnearby = rowSums(datafinal13[,c("new K","new X1","new X2","new X3","new X4","new X5","new X6","new X7","new X8","new X9","new X10","new X11","new X12","new LRE")], na.rm = T)
datafinal13 = datafinal13[,!names(datafinal13) %in% c("new K","new X1","new X2","new X3","new X4","new X5","new X6","new X7","new X8","new X9","new X10","new X11","new X12","new LRE")]

setwd("/Volumes/appdata-3/School reports")
schools = read.csv("schools.csv", stringsAsFactors = F)

datafinal13 = merge(datafinal13, schools[,c("SchoolID","SchoolType","SchoolSubType","SchoolStyle")], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SchoolID")

# add public data
library("plyr")

setwd("/Volumes/appdata-3/Other Datasets/public neighborhood data")
crimedata = read.csv("crime_by_HScatchment_SY1213.csv",stringsAsFactors = F)
crimedata$crimetotal = rowSums(crimedata[,4:33])
crimedata = ddply(crimedata, .(schoolIDs), summarize, crimecount = sum(crimetotal))

schools_with1314boundaries = crimedata[,"schoolIDs"]
  
businessdata = read.csv("business_by_HScatchment_SY1213.csv",stringsAsFactors = F)
businessdata = ddply(businessdata, .(schoolIDs), summarize, licensecount = sum(licensecount))

datafinal13 = merge(datafinal13, crimedata, all.x = T, by.x = "SCHOOL_CODE", by.y = "schoolIDs")
datafinal13 = merge(datafinal13, businessdata, all.x = T, by.x = "SCHOOL_CODE", by.y = "schoolIDs")

rm(countdata, data, data12, data13, data13_final, progress1314, projection_long, schoolfeatures, new13, schoolsNA, dist_to_school, grademat, newgrades, newlocations,schools,currentnames, grade, i, crimedata, businessdata)



# --------------------------------------------------------- #
# here I prepare data to predict 2012 enrollments by grade: #
# --------------------------------------------------------- #

setwd("/Volumes/appdata-3/Count Data")
countdata = read.csv("enrollment_byschool_byyear_bygrade.csv")

setwd("/Volumes/appdata-3/School Reports")
schoolfeatures = read.csv("schools_staticfeatures.csv", stringsAsFactors = F)
  
# add school features for each grade
data = merge(countdata, schoolfeatures, all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

# pull out 2012
data12 = subset(data, YEAR == 2012)

# main features
data12 = data12[,c(1,2,3,6)]
colnames(data12) = c("SCHOOL_CODE", "GRADE", "enrollment_to_predict","SCHOOL_TYPE")

# # YCCS: add up all new codes to old code 
# # (from Greg's spreadsheet FY2013 I found which schools he added to get 609686 total)
# # then remove the new school codes
# for (j in levels(data12$GRADE)){
#   data12[(data12$SCHOOL_CODE == 609686)&(data12$GRADE == j),"enrollment_to_predict"] = data12[(data12$SCHOOL_CODE == 609686)&(data12$GRADE == j),"enrollment_to_predict"] + sum(data12[(data12$SCHOOL_CODE %in% c(400123:400137,400139,400141:400145,400150))&(data12$GRADE == j),"enrollment_to_predict"])
# }
# 
# data12 = data12[(data12$SCHOOL_CODE %in% c(400123:400137,400139,400141:400145,400150)) == "FALSE",]

# REMOVE YCCS
data12 = data12[(data12$SCHOOL_CODE %in% c(400123:400137,400139,400141:400145,400150)) == "FALSE",]
data12 = data12[(data12$SCHOOL_CODE %in% 609686) == 'FALSE',]

# previous year
data11 = subset(data, YEAR == 2011)

# main features
data11 = data11[,c(1,2,3)]
colnames(data11) = c("SCHOOL_CODE", "GRADE", "projection")
projection_long = data11
# end previous year

# REMOVE YCCS
projection_long = projection_long[(projection_long$SCHOOL_CODE %in% c(400123:400137,400139,400141:400145,400150)) == 'FALSE',] # none
projection_long = projection_long[(projection_long$SCHOOL_CODE == 609686) == 'FALSE',]

# # it is accurate to use countdata from greg's file
# # because greg's file does not have any schools that are not in the "true" sheet
# # and the only schools the "true" has that greg's file doesn't have are the YCCS codes
# setwd("/Volumes/appdata-3/Master_Data")
# true12 = read.csv("true2012.csv")
# greg_not_true = setdiff(unique(data12$SCHOOL_CODE), true12$SchoolID)
# true_not_greg = setdiff(true12$SchoolID, unique(data12$SCHOOL_CODE))
#  
# schoolfeatures[(schoolfeatures$SCHOOL_CODE %in% greg_not_true),] 
# schoolfeatures[(schoolfeatures$SCHOOL_CODE %in% true_not_greg),]


# James Shields middle school (grades 5 - 8) 610559 split from elementary 610174 (I think)
# data12 contains the two codes, but the csp file doesn't
projection_long[(projection_long$SCHOOL_CODE == 610559),]

shieldsmiddle = matrix(nrow = 14, ncol = 3)
shieldsmiddle = as.data.frame(shieldsmiddle)
colnames(shieldsmiddle) = c("SCHOOL_CODE", "GRADE", "projection")
shieldsmiddle$SCHOOL_CODE = 610559
shieldsmiddle$GRADE = c('K', 'X1', 'X2', 'X3', 'X4','X5','X6','X7','X8','X9','X10','X11','X12','LRE')
shieldsmiddle[(shieldsmiddle$GRADE == 'X5'),"projection"] = projection_long[(projection_long$SCHOOL_CODE == 610174)&(projection_long$GRADE == 'X5'),"projection"]
shieldsmiddle[(shieldsmiddle$GRADE == 'X6'),"projection"] = projection_long[(projection_long$SCHOOL_CODE == 610174)&(projection_long$GRADE == 'X6'),"projection"]
shieldsmiddle[(shieldsmiddle$GRADE == 'X7'),"projection"] = projection_long[(projection_long$SCHOOL_CODE == 610174)&(projection_long$GRADE == 'X7'),"projection"]
shieldsmiddle[(shieldsmiddle$GRADE == 'X8'),"projection"] = projection_long[(projection_long$SCHOOL_CODE == 610174)&(projection_long$GRADE == 'X8'),"projection"]

shieldsmiddle[is.na(shieldsmiddle$projection),'projection'] = 0

projection_long = rbind(projection_long, shieldsmiddle)
 
projection_long[(projection_long$SCHOOL_CODE == 610174)&(projection_long$GRADE == 'X5'),"projection"] = 0
projection_long[(projection_long$SCHOOL_CODE == 610174)&(projection_long$GRADE == 'X6'),"projection"] = 0
projection_long[(projection_long$SCHOOL_CODE == 610174)&(projection_long$GRADE == 'X7'),"projection"] = 0
projection_long[(projection_long$SCHOOL_CODE == 610174)&(projection_long$GRADE == 'X8'),"projection"] = 0


# calumet MS (grade 6 - 8) 400067 to calumet HS 400061 (I think)
projection_long[(projection_long$SCHOOL_CODE == 400067),]
projection_long[(projection_long$SCHOOL_CODE == 400061)&(projection_long$GRADE == 'X6'),"projection"] = projection_long[(projection_long$SCHOOL_CODE == 400067)&(projection_long$GRADE == 'X6'),"projection"]
projection_long[(projection_long$SCHOOL_CODE == 400061)&(projection_long$GRADE == 'X7'),"projection"] = projection_long[(projection_long$SCHOOL_CODE == 400067)&(projection_long$GRADE == 'X7'),"projection"]
projection_long[(projection_long$SCHOOL_CODE == 400061)&(projection_long$GRADE == 'X8'),"projection"] = projection_long[(projection_long$SCHOOL_CODE == 400067)&(projection_long$GRADE == 'X8'),"projection"]


data12_final = merge(data12, projection_long, all.x = TRUE, by.x = c("SCHOOL_CODE","GRADE"), by.y = c("SCHOOL_CODE","GRADE"))

# for new schools, fill in 0 for projection
new12 = schoolfeatures[(schoolfeatures$active2011 == 0)&(schoolfeatures$active2012 == 1),"SCHOOL_CODE"]
new12 = setdiff(new12, c(400123:400137,400139,400141:400145,400150))
data12_final[(data12_final$SCHOOL_CODE %in% new12)&(is.na(data12_final$projection)),"projection"] = 0

# which schools are missing projection
schoolsNA = data12_final[(is.na(data12_final$projection)),"SCHOOL_CODE"]
schoolfeatures[(schoolfeatures$SCHOOL_CODE %in% schoolsNA)==TRUE,]

# make an indicator for whether a grade at a school is new
data12_final$NEW = ((data12_final$enrollment_to_predict > 0)&(data12_final$projection == 0))

setwd("/Volumes/appdata-3/Master_Data")
progress1213 = read.csv("schools_changing_features_2012.csv", stringsAsFactors = F)

datafinal12 = merge(data12_final, progress1213[,!names(progress1213) %in% c("SchoolName","SchoolType","ZipCode")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SchoolID")

# new grades nearby
for (grade in levels(datafinal12$GRADE)){
  grademat = subset(datafinal12, GRADE == grade)
  grademat = merge(grademat, schoolfeatures[,c("SCHOOL_CODE","lon","lat")], all.x = TRUE, by.x = "SCHOOL_CODE",by.y = "SCHOOL_CODE")
  
  newgrades = subset(datafinal12, (enrollment_to_predict >= 10)&(projection == 0)&(GRADE == grade))
  newlocations = schoolfeatures[(schoolfeatures$SCHOOL_CODE %in% newgrades$SCHOOL_CODE), c("lon","lat")]
  
  grademat$new_nearby = 0
  
  library("fields")
  for (i in 1:nrow(grademat)){
    dist_to_school = rdist.earth(grademat[i,c("lon","lat")], newlocations, miles = TRUE, R = NULL)
    
    # how many new schools within 2 miles?
    grademat[i,"new_nearby"] = sum(dist_to_school <=2)  
  }
  
  currentnames = names(datafinal12)
  datafinal12 = merge(datafinal12, grademat[,c("SCHOOL_CODE","GRADE","new_nearby")], all.x = TRUE, by.x = c("SCHOOL_CODE","GRADE"), by.y = c("SCHOOL_CODE","GRADE"))
  names(datafinal12) = c(currentnames, paste('new',grade))
}

datafinal12$newnearby = rowSums(datafinal12[,c("new K","new X1","new X2","new X3","new X4","new X5","new X6","new X7","new X8","new X9","new X10","new X11","new X12","new LRE")], na.rm = T)
datafinal12 = datafinal12[,!names(datafinal12) %in% c("new K","new X1","new X2","new X3","new X4","new X5","new X6","new X7","new X8","new X9","new X10","new X11","new X12","new LRE")]

# schools
setwd("/Volumes/appdata-3/School reports")
schools = read.csv("schools.csv", stringsAsFactors = F)

datafinal12 = merge(datafinal12, schools[,c("SchoolID","SchoolType","SchoolSubType","SchoolStyle")], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SchoolID")

datafinal12 = subset(datafinal12, enrollment_to_predict > 0)

# add public data
library("plyr")

setwd("/Volumes/appdata-3/Other Datasets/public neighborhood data")
crimedata = read.csv("crime_by_HScatchment_SY1112.csv",stringsAsFactors = F)
crimedata$crimetotal = rowSums(crimedata[,4:ncol(crimedata)])
crimedata = ddply(crimedata, .(schoolIDs), summarize, crimecount = sum(crimetotal))

businessdata = read.csv("business_by_HScatchment_SY1112.csv",stringsAsFactors = F)
businessdata = ddply(businessdata, .(schoolIDs), summarize, licensecount = sum(licensecount))

datafinal12 = merge(datafinal12, crimedata, all.x = T, by.x = "SCHOOL_CODE", by.y = "schoolIDs")
datafinal12 = merge(datafinal12, businessdata, all.x = T, by.x = "SCHOOL_CODE", by.y = "schoolIDs")


rm(countdata, data, data12, data11, data12_final, progress1213, projection_long, schoolfeatures, new12, schoolsNA, dist_to_school, grademat, newgrades, newlocations,schools,currentnames, grade, i, shieldsmiddle, crimedata, businessdata)


# --------------------------------------------------------- #
# here I prepare data to predict 2011 enrollments by grade: #
# --------------------------------------------------------- #

setwd("/Volumes/appdata-3/Count Data")
countdata = read.csv("enrollment_byschool_byyear_bygrade.csv")

setwd("/Volumes/appdata-3/School Reports")
schoolfeatures = read.csv("schools_staticfeatures.csv", stringsAsFactors = F)

# add school features for each grade
data = merge(countdata, schoolfeatures, all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

# pull out 2011
data11 = subset(data, YEAR == 2011)

# main features
data11 = data11[,c(1,2,3,6)]
colnames(data11) = c("SCHOOL_CODE", "GRADE", "enrollment_to_predict","SCHOOL_TYPE")

# YCCS not a problem 
data11[(data11$SCHOOL_CODE %in% c(400123:400137,400139,400141:400145,400150)),]

# remove YCCS
data11 = data11[(data11$SCHOOL_CODE != 609686),]

# previous year
data10 = subset(data, YEAR == 2010)

# main features
data10 = data10[,c(1,2,3)]
colnames(data10) = c("SCHOOL_CODE", "GRADE", "projection")
projection_long = data10
# end previous year

# YCCS check
projection_long[projection_long$SCHOOL_CODE %in% c(400123:400137,400139,400141:400145,400150),] # none

# REMOVE YCCS
projection_long = projection_long[(projection_long$SCHOOL_CODE != 609686),]

# # it is accurate to use countdata from greg's file
# # because greg's file does not have any schools that are not in the "true" sheet
# # and no schools in "true" sheet that are not in greg's sheet
# setwd("/Volumes/appdata-3/Master_Data")
# true11 = read.csv("true2011.csv")
# greg_not_true = setdiff(unique(data11$SCHOOL_CODE), true11$SchoolID)
# true_not_greg = setdiff(true11$SchoolID, unique(data11$SCHOOL_CODE))
# 
# schoolfeatures[(schoolfeatures$SCHOOL_CODE %in% greg_not_true),]
# schoolfeatures[(schoolfeatures$SCHOOL_CODE %in% true_not_greg),]

# James Shields middle school not an issue for 2011
# calumet MS not an issue for 2011

data11_final = merge(data11, projection_long, all.x = TRUE, by.x = c("SCHOOL_CODE","GRADE"), by.y = c("SCHOOL_CODE","GRADE"))

# for new schools, fill in 0 for projection
new11 = schoolfeatures[(schoolfeatures$active2011 == 1)&(schoolfeatures$active2010 == 0),"SCHOOL_CODE"]
data11_final[(data11_final$SCHOOL_CODE %in% new11)&(is.na(data11_final$projection)),"projection"] = 0

# which schools are missing projection
schoolsNA = data11_final[(is.na(data11_final$projection)),"SCHOOL_CODE"]
schoolfeatures[(schoolfeatures$SCHOOL_CODE %in% schoolsNA)==TRUE,] # none

# make an indicator for whether a grade at a school is new
data11_final$NEW = ((data11_final$enrollment_to_predict > 0)&(data11_final$projection == 0))

setwd("/Volumes/appdata-3/Master_Data")
progress1112 = read.csv("schools_changing_features_2011.csv", stringsAsFactors = F)
datafinal11 = merge(data11_final, progress1112[,!names(progress1112) %in% c("SchoolName","SchoolType","ZipCode")], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SchoolID")

datafinal11 = subset(datafinal11, enrollment_to_predict > 0)

# new grades nearby
for (grade in levels(datafinal11$GRADE)){
  grademat = subset(datafinal11, GRADE == grade)
  grademat = merge(grademat, schoolfeatures[,c("SCHOOL_CODE","lon","lat")], all.x = TRUE, by.x = "SCHOOL_CODE",by.y = "SCHOOL_CODE")
  
  newgrades = subset(datafinal11, (enrollment_to_predict >= 10)&(projection == 0)&(GRADE == grade))
  newlocations = schoolfeatures[(schoolfeatures$SCHOOL_CODE %in% newgrades$SCHOOL_CODE), c("lon","lat")]
  
  grademat$new_nearby = 0
  
  library("fields")
  for (i in 1:nrow(grademat)){
    dist_to_school = rdist.earth(grademat[i,c("lon","lat")], newlocations, miles = TRUE, R = NULL)
    
    # how many new schools within 2 miles?
    grademat[i,"new_nearby"] = sum(dist_to_school <=2)  
  }
  
  currentnames = names(datafinal11)
  datafinal11 = merge(datafinal11, grademat[,c("SCHOOL_CODE","GRADE","new_nearby")], all.x = TRUE, by.x = c("SCHOOL_CODE","GRADE"), by.y = c("SCHOOL_CODE","GRADE"))
  names(datafinal11) = c(currentnames, paste('new',grade))
}

datafinal11$newnearby = rowSums(datafinal11[,c("new K","new X1","new X2","new X3","new X4","new X5","new X6","new X7","new X8","new X9","new X10","new X11","new X12","new LRE")], na.rm = T)
datafinal11 = datafinal11[,!names(datafinal11) %in% c("new K","new X1","new X2","new X3","new X4","new X5","new X6","new X7","new X8","new X9","new X10","new X11","new X12","new LRE")]

setwd("/Volumes/appdata-3/School reports")
schools = read.csv("schools.csv", stringsAsFactors = F)

datafinal11 = merge(datafinal11, schools[,c("SchoolID","SchoolType","SchoolSubType","SchoolStyle")], all.x = TRUE, by.x = "SCHOOL_CODE", by.y = "SchoolID")

# add public data
library("plyr")

setwd("/Volumes/appdata-3/Other Datasets/public neighborhood data")
crimedata = read.csv("crime_by_HScatchment_SY1011.csv",stringsAsFactors = F)
crimedata$crimetotal = rowSums(crimedata[,4:ncol(crimedata)])
crimedata = ddply(crimedata, .(schoolIDs), summarize, crimecount = sum(crimetotal))

businessdata = read.csv("business_by_HScatchment_SY1011.csv",stringsAsFactors = F)
businessdata = ddply(businessdata, .(schoolIDs), summarize, licensecount = sum(licensecount))

datafinal11 = merge(datafinal11, crimedata, all.x = T, by.x = "SCHOOL_CODE", by.y = "schoolIDs")
datafinal11 = merge(datafinal11, businessdata, all.x = T, by.x = "SCHOOL_CODE", by.y = "schoolIDs")

rm(countdata, data, data11, data10, data11_final, progress1112, projection_long, schoolfeatures, new11, schoolsNA, dist_to_school, grademat, newgrades, newlocations,schools,currentnames, grade, i, crimedata, businessdata)


# Zhou's approximate cohort projection
setwd("/Volumes/appdata-3/Prediction")
cohort2012 = read.csv("csm_prediction_2012.csv",stringsAsFactors = F)
cohort2012 = cohort2012[,1:15]
cohort2013 = read.csv("csm_prediction_2013.csv",stringsAsFactors = F)
cohort2013 = cohort2013[,1:15]
colnames(cohort2012) = c("SchoolID","K","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","LRE")
colnames(cohort2013) = c("SchoolID","K","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","LRE")
 
cohort2012_long = reshape(cohort2012, varying = list(names(cohort2012[,2:15])), v.names = "cs_projection", times = names(cohort2012[,2:15]), idvar = "SchoolID", timevar = "GRADE", ids = "SchoolID", direction = 'long')
cohort2013_long = reshape(cohort2013, varying = list(names(cohort2013[,2:15])), v.names = "cs_projection", times = names(cohort2013[,2:15]), idvar = "SchoolID", timevar = "GRADE", ids = "SchoolID", direction = 'long')

datafinal12 = merge(datafinal12, cohort2012_long, all.x = TRUE, by.x = c("SCHOOL_CODE","GRADE"), by.y = c("SchoolID", "GRADE"))

datafinal13 = merge(datafinal13, cohort2013_long, all.x = TRUE, by.x = c("SCHOOL_CODE","GRADE"), by.y = c("SchoolID", "GRADE"))
 
rm(cohort2012, cohort2013, cohort2012_long, cohort2013_long)


# add delta variables

add_numeric_delta_1112 = function(columnname, datafinal12, datafinal11){
  deltamat = merge(unique(datafinal11[,c("SCHOOL_CODE",columnname)]), unique(datafinal12[,c("SCHOOL_CODE",columnname)]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
  deltamat[,paste(columnname, 'delta', sep = "_")] = deltamat[,paste(columnname,".2012", sep = "")] - deltamat[,paste(columnname,".2011", sep = "")]
  datafinal12 = merge(datafinal12, deltamat[,c("SCHOOL_CODE",paste(columnname, 'delta', sep = "_"))], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")
  datafinal12
}

datafinal12 = add_numeric_delta_1112('Mobility',datafinal12, datafinal11)
datafinal12 = add_numeric_delta_1112('Black', datafinal12, datafinal11)
datafinal12 = add_numeric_delta_1112('Hispanic', datafinal12, datafinal11)

add_numeric_delta_1213 = function(columnname, datafinal13, datafinal12){
  deltamat = merge(unique(datafinal12[,c("SCHOOL_CODE",columnname)]), unique(datafinal13[,c("SCHOOL_CODE",columnname)]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
  deltamat[,paste(columnname, 'delta', sep = "_")] = deltamat[,paste(columnname,".2013", sep = "")] - deltamat[,paste(columnname,".2012", sep = "")]
  datafinal13 = merge(datafinal13, deltamat[,c("SCHOOL_CODE",paste(columnname, 'delta', sep = "_"))], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")
  datafinal13
}

datafinal13 = add_numeric_delta_1213('Mobility',datafinal13, datafinal12)
datafinal13 = add_numeric_delta_1213('Black', datafinal13, datafinal12)
datafinal13 = add_numeric_delta_1213('Hispanic', datafinal13, datafinal12)


# change in crime counts
### 11-12
delta_crimecount = merge(unique(datafinal11[,c("SCHOOL_CODE","crimecount")]), unique(datafinal12[,c("SCHOOL_CODE","crimecount")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
delta_crimecount$crimecount_percentdelta = delta_crimecount$crimecount.2012/delta_crimecount$crimecount.2011 - 1
datafinal12 = merge(datafinal12, delta_crimecount[,c("SCHOOL_CODE","crimecount_percentdelta")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

### 12-13
delta_crimecount = merge(unique(datafinal12[,c("SCHOOL_CODE","crimecount")]), unique(datafinal13[,c("SCHOOL_CODE","crimecount")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
delta_crimecount$crimecount_percentdelta = delta_crimecount$crimecount.2013/delta_crimecount$crimecount.2012 - 1
datafinal13 = merge(datafinal13, delta_crimecount[,c("SCHOOL_CODE","crimecount_percentdelta")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

rm(delta_crimecount)

# change in business license counts
### 11-12
delta_licensecount = merge(unique(datafinal11[,c("SCHOOL_CODE","licensecount")]), unique(datafinal12[,c("SCHOOL_CODE","licensecount")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
delta_licensecount$licensecount_percentdelta = delta_licensecount$licensecount.2012/delta_licensecount$licensecount.2011 - 1
datafinal12 = merge(datafinal12, delta_licensecount[,c("SCHOOL_CODE","licensecount_percentdelta")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

### 12-13
delta_licensecount = merge(unique(datafinal12[,c("SCHOOL_CODE","licensecount")]), unique(datafinal13[,c("SCHOOL_CODE","licensecount")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
delta_licensecount$licensecount_percentdelta = delta_licensecount$licensecount.2013/delta_licensecount$licensecount.2012 - 1
datafinal13 = merge(datafinal13, delta_licensecount[,c("SCHOOL_CODE","licensecount_percentdelta")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

rm(delta_licensecount)










# change in probation
### 11-12
delta_Probation = merge(unique(datafinal11[,c("SCHOOL_CODE","Probation")]), unique(datafinal12[,c("SCHOOL_CODE","Probation")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
delta_Probation$to_Probation = (delta_Probation$Probation.2012 == 'yes' & delta_Probation$Probation.2011 == 'no')
delta_Probation$from_Probation = (delta_Probation$Probation.2012 == 'no' & delta_Probation$Probation.2011 == 'yes')
datafinal12 = merge(datafinal12, delta_Probation[,c("SCHOOL_CODE","to_Probation","from_Probation")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")
datafinal12[is.na(datafinal12$to_Probation),"to_Probation"] = 'FALSE'
datafinal12[is.na(datafinal12$from_Probation),"from_Probation"] = 'FALSE'

### 12-13
delta_Probation = merge(unique(datafinal12[,c("SCHOOL_CODE","Probation")]), unique(datafinal13[,c("SCHOOL_CODE","Probation")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
delta_Probation$to_Probation = (delta_Probation$Probation.2013 == 'yes' & delta_Probation$Probation.2012 == 'no')
delta_Probation$from_Probation = (delta_Probation$Probation.2013 == 'no' & delta_Probation$Probation.2012 == 'yes')
datafinal13 = merge(datafinal13, delta_Probation[,c("SCHOOL_CODE","to_Probation","from_Probation")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")
datafinal13[is.na(datafinal13$to_Probation),"to_Probation"] = 'FALSE'
datafinal13[is.na(datafinal13$from_Probation),"from_Probation"] = 'FALSE'

rm(delta_Probation)

# change in safety
### 11-12
delta_Safety = merge(unique(datafinal11[,c("SCHOOL_CODE","Safety")]), unique(datafinal12[,c("SCHOOL_CODE","Safety")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
delta_Safety$Safety.2011 = as.factor(delta_Safety$Safety.2011)
table(delta_Safety$Safety.2011)
levels(delta_Safety$Safety.2011) = c(3,4,5,1,2)
delta_Safety$Safety.2012 = as.factor(delta_Safety$Safety.2012)
table(delta_Safety$Safety.2012)
levels(delta_Safety$Safety.2012) = c(3,4,5,1,2)

delta_Safety$Safety.2011 = as.numeric(as.character(delta_Safety$Safety.2011))
delta_Safety$Safety.2012 = as.numeric(as.character(delta_Safety$Safety.2012))
delta_Safety$delta_Safety = delta_Safety$Safety.2012 - delta_Safety$Safety.2011

datafinal12 = merge(datafinal12, delta_Safety[,c("SCHOOL_CODE","delta_Safety")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

### 12-13
delta_Safety = merge(unique(datafinal12[,c("SCHOOL_CODE","Safety")]), unique(datafinal13[,c("SCHOOL_CODE","Safety")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
delta_Safety$Safety.2012 = as.factor(delta_Safety$Safety.2012)
table(delta_Safety$Safety.2012)
levels(delta_Safety$Safety.2012) = c(3,4,5,1,2)
delta_Safety$Safety.2013 = as.factor(delta_Safety$Safety.2013)
table(delta_Safety$Safety.2013)
levels(delta_Safety$Safety.2013) = c(3,4,5,1,2)

delta_Safety$Safety.2012 = as.numeric(as.character(delta_Safety$Safety.2012))
delta_Safety$Safety.2013 = as.numeric(as.character(delta_Safety$Safety.2013))
delta_Safety$delta_Safety = delta_Safety$Safety.2013 - delta_Safety$Safety.2012

datafinal13 = merge(datafinal13, delta_Safety[,c("SCHOOL_CODE","delta_Safety")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

rm(delta_Safety)

# change in family
### 11-12
delta_Family = merge(unique(datafinal11[,c("SCHOOL_CODE","Family")]), unique(datafinal12[,c("SCHOOL_CODE","Family")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
delta_Family$Family.2011 = as.factor(delta_Family$Family.2011)
table(delta_Family$Family.2011)
levels(delta_Family$Family.2011) = c(3,4,5,1,2)
delta_Family$Family.2012 = as.factor(delta_Family$Family.2012)
table(delta_Family$Family.2012)
levels(delta_Family$Family.2012) = c(3,4,5,1,2)

delta_Family$Family.2011 = as.numeric(as.character(delta_Family$Family.2011))
delta_Family$Family.2012 = as.numeric(as.character(delta_Family$Family.2012))
delta_Family$delta_Family = delta_Family$Family.2012 - delta_Family$Family.2011

datafinal12 = merge(datafinal12, delta_Family[,c("SCHOOL_CODE","delta_Family")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

### 12-13
delta_Family = merge(unique(datafinal12[,c("SCHOOL_CODE","Family")]), unique(datafinal13[,c("SCHOOL_CODE","Family")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
delta_Family$Family.2012 = as.factor(delta_Family$Family.2012)
table(delta_Family$Family.2012)
levels(delta_Family$Family.2012) = c(3,4,5,1,2)
delta_Family$Family.2013 = as.factor(delta_Family$Family.2013)
table(delta_Family$Family.2013)
levels(delta_Family$Family.2013) = c(3,4,5,1,2)

delta_Family$Family.2012 = as.numeric(as.character(delta_Family$Family.2012))
delta_Family$Family.2013 = as.numeric(as.character(delta_Family$Family.2013))
delta_Family$delta_Family = delta_Family$Family.2013 - delta_Family$Family.2012

datafinal13 = merge(datafinal13, delta_Family[,c("SCHOOL_CODE","delta_Family")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

rm(delta_Family)

# change in environment
### 11-12
delta_Environment = merge(unique(datafinal11[,c("SCHOOL_CODE","Environment")]), unique(datafinal12[,c("SCHOOL_CODE","Environment")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
delta_Environment$Environment.2011 = as.factor(delta_Environment$Environment.2011)
table(delta_Environment$Environment.2011)
levels(delta_Environment$Environment.2011) = c(3,4,5,1,2)
delta_Environment$Environment.2012 = as.factor(delta_Environment$Environment.2012)
table(delta_Environment$Environment.2012)
levels(delta_Environment$Environment.2012) = c(3,4,5,1,2)

delta_Environment$Environment.2011 = as.numeric(as.character(delta_Environment$Environment.2011))
delta_Environment$Environment.2012 = as.numeric(as.character(delta_Environment$Environment.2012))
delta_Environment$delta_Environment = delta_Environment$Environment.2012 - delta_Environment$Environment.2011

datafinal12 = merge(datafinal12, delta_Environment[,c("SCHOOL_CODE","delta_Environment")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

### 12-13
delta_Environment = merge(unique(datafinal12[,c("SCHOOL_CODE","Environment")]), unique(datafinal13[,c("SCHOOL_CODE","Environment")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
delta_Environment$Environment.2012 = as.factor(delta_Environment$Environment.2012)
table(delta_Environment$Environment.2012)
levels(delta_Environment$Environment.2012) = c(3,4,5,1,2)
delta_Environment$Environment.2013 = as.factor(delta_Environment$Environment.2013)
table(delta_Environment$Environment.2013)
levels(delta_Environment$Environment.2013) = c(3,4,5,1,2)

delta_Environment$Environment.2012 = as.numeric(as.character(delta_Environment$Environment.2012))
delta_Environment$Environment.2013 = as.numeric(as.character(delta_Environment$Environment.2013))
delta_Environment$delta_Environment = delta_Environment$Environment.2013 - delta_Environment$Environment.2012

datafinal13 = merge(datafinal13, delta_Environment[,c("SCHOOL_CODE","delta_Environment")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

rm(delta_Environment)

# change in instruction
### 11-12
delta_Instruction = merge(unique(datafinal11[,c("SCHOOL_CODE","Instruction")]), unique(datafinal12[,c("SCHOOL_CODE","Instruction")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
delta_Instruction$Instruction.2011 = as.factor(delta_Instruction$Instruction.2011)
table(delta_Instruction$Instruction.2011)
levels(delta_Instruction$Instruction.2011) = c(3,4,5,1,2)
delta_Instruction$Instruction.2012 = as.factor(delta_Instruction$Instruction.2012)
table(delta_Instruction$Instruction.2012)
levels(delta_Instruction$Instruction.2012) = c(3,4,5,1,2)

delta_Instruction$Instruction.2011 = as.numeric(as.character(delta_Instruction$Instruction.2011))
delta_Instruction$Instruction.2012 = as.numeric(as.character(delta_Instruction$Instruction.2012))
delta_Instruction$delta_Instruction = delta_Instruction$Instruction.2012 - delta_Instruction$Instruction.2011

datafinal12 = merge(datafinal12, delta_Instruction[,c("SCHOOL_CODE","delta_Instruction")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")


### 12-13
delta_Instruction = merge(unique(datafinal12[,c("SCHOOL_CODE","Instruction")]), unique(datafinal13[,c("SCHOOL_CODE","Instruction")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
delta_Instruction$Instruction.2012 = as.factor(delta_Instruction$Instruction.2012)
table(delta_Instruction$Instruction.2012)
levels(delta_Instruction$Instruction.2012) = c(3,4,5,1,2)
delta_Instruction$Instruction.2013 = as.factor(delta_Instruction$Instruction.2013)
table(delta_Instruction$Instruction.2013)
levels(delta_Instruction$Instruction.2013) = c(3,4,5,1,2)

delta_Instruction$Instruction.2012 = as.numeric(as.character(delta_Instruction$Instruction.2012))
delta_Instruction$Instruction.2013 = as.numeric(as.character(delta_Instruction$Instruction.2013))
delta_Instruction$delta_Instruction = delta_Instruction$Instruction.2013 - delta_Instruction$Instruction.2012

datafinal13 = merge(datafinal13, delta_Instruction[,c("SCHOOL_CODE","delta_Instruction")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

rm(delta_Instruction)

# change in rating
### 11-12
delta_Rating = merge(unique(datafinal11[,c("SCHOOL_CODE","Rating")]), unique(datafinal12[,c("SCHOOL_CODE","Rating")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
delta_Rating$Rating.2011 = as.factor(delta_Rating$Rating.2011)
table(delta_Rating$Rating.2011)
levels(delta_Rating$Rating.2011) = c(3,2,1)
delta_Rating$Rating.2012 = as.factor(delta_Rating$Rating.2012)
table(delta_Rating$Rating.2012)
levels(delta_Rating$Rating.2012) = c(3,2,1)

delta_Rating$Rating.2011 = as.numeric(as.character(delta_Rating$Rating.2011))
delta_Rating$Rating.2012 = as.numeric(as.character(delta_Rating$Rating.2012))
delta_Rating$delta_Rating = delta_Rating$Rating.2012 - delta_Rating$Rating.2011

datafinal12 = merge(datafinal12, delta_Rating[,c("SCHOOL_CODE","delta_Rating")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

### 12-13
delta_Rating = merge(unique(datafinal12[,c("SCHOOL_CODE","Rating")]), unique(datafinal13[,c("SCHOOL_CODE","Rating")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
delta_Rating$Rating.2012 = as.factor(delta_Rating$Rating.2012)
table(delta_Rating$Rating.2012)
levels(delta_Rating$Rating.2012) = c(3,2,1)
delta_Rating$Rating.2013 = as.factor(delta_Rating$Rating.2013)
table(delta_Rating$Rating.2013)
levels(delta_Rating$Rating.2013) = c(3,2,1)

delta_Rating$Rating.2012 = as.numeric(as.character(delta_Rating$Rating.2012))
delta_Rating$Rating.2013 = as.numeric(as.character(delta_Rating$Rating.2013))
delta_Rating$delta_Rating = delta_Rating$Rating.2013 - delta_Rating$Rating.2012

datafinal13 = merge(datafinal13, delta_Rating[,c("SCHOOL_CODE","delta_Rating")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

rm(delta_Rating)


# change in leader
### 11-12
delta_Leader = merge(unique(datafinal11[,c("SCHOOL_CODE","Leader")]), unique(datafinal12[,c("SCHOOL_CODE","Leader")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
delta_Leader$Leader.2011 = as.factor(delta_Leader$Leader.2011)
table(delta_Leader$Leader.2011)
levels(delta_Leader$Leader.2011) = c(3,4,5,1,2)
delta_Leader$Leader.2012 = as.factor(delta_Leader$Leader.2012)
table(delta_Leader$Leader.2012)
levels(delta_Leader$Leader.2012) = c(3,4,5,1,2)

delta_Leader$Leader.2011 = as.numeric(as.character(delta_Leader$Leader.2011))
delta_Leader$Leader.2012 = as.numeric(as.character(delta_Leader$Leader.2012))
delta_Leader$delta_Leader = delta_Leader$Leader.2012 - delta_Leader$Leader.2011

datafinal12 = merge(datafinal12, delta_Leader[,c("SCHOOL_CODE","delta_Leader")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

### 12-13
delta_Leader = merge(unique(datafinal12[,c("SCHOOL_CODE","Leader")]), unique(datafinal13[,c("SCHOOL_CODE","Leader")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
delta_Leader$Leader.2012 = as.factor(delta_Leader$Leader.2012)
table(delta_Leader$Leader.2012)
levels(delta_Leader$Leader.2012) = c(3,4,5,1,2)
delta_Leader$Leader.2013 = as.factor(delta_Leader$Leader.2013)
table(delta_Leader$Leader.2013)
levels(delta_Leader$Leader.2013) = c(3,4,5,1,2)

delta_Leader$Leader.2012 = as.numeric(as.character(delta_Leader$Leader.2012))
delta_Leader$Leader.2013 = as.numeric(as.character(delta_Leader$Leader.2013))
delta_Leader$delta_Leader = delta_Leader$Leader.2013 - delta_Leader$Leader.2012

datafinal13 = merge(datafinal13, delta_Leader[,c("SCHOOL_CODE","delta_Leader")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

rm(delta_Leader)

# change in teacher
### 11-12
delta_Teacher = merge(unique(datafinal11[,c("SCHOOL_CODE","Teacher")]), unique(datafinal12[,c("SCHOOL_CODE","Teacher")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2011",".2012"))
delta_Teacher$Teacher.2011 = as.factor(delta_Teacher$Teacher.2011)
table(delta_Teacher$Teacher.2011)
levels(delta_Teacher$Teacher.2011) = c(3,4,5,1,2)
delta_Teacher$Teacher.2012 = as.factor(delta_Teacher$Teacher.2012)
table(delta_Teacher$Teacher.2012)
levels(delta_Teacher$Teacher.2012) = c(3,4,5,1,2)

delta_Teacher$Teacher.2011 = as.numeric(as.character(delta_Teacher$Teacher.2011))
delta_Teacher$Teacher.2012 = as.numeric(as.character(delta_Teacher$Teacher.2012))
delta_Teacher$delta_Teacher = delta_Teacher$Teacher.2012 - delta_Teacher$Teacher.2011

datafinal12 = merge(datafinal12, delta_Teacher[,c("SCHOOL_CODE","delta_Teacher")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

### 12-13
delta_Teacher = merge(unique(datafinal12[,c("SCHOOL_CODE","Teacher")]), unique(datafinal13[,c("SCHOOL_CODE","Teacher")]), all = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE", suffixes = c(".2012",".2013"))
delta_Teacher$Teacher.2012 = as.factor(delta_Teacher$Teacher.2012)
table(delta_Teacher$Teacher.2012)
levels(delta_Teacher$Teacher.2012) = c(3,4,5,1,2)

delta_Teacher[!is.na(delta_Teacher$Teacher.2013)&(delta_Teacher$Teacher.2013 == ""),"Teacher.2013"] = NA
delta_Teacher$Teacher.2013 = as.factor(delta_Teacher$Teacher.2013)
table(delta_Teacher$Teacher.2013)
levels(delta_Teacher$Teacher.2013) = c(3,4,5,1,2)

delta_Teacher$Teacher.2012 = as.numeric(as.character(delta_Teacher$Teacher.2012))
delta_Teacher$Teacher.2013 = as.numeric(as.character(delta_Teacher$Teacher.2013))
delta_Teacher$delta_Teacher = delta_Teacher$Teacher.2013 - delta_Teacher$Teacher.2012

datafinal13 = merge(datafinal13, delta_Teacher[,c("SCHOOL_CODE","delta_Teacher")], all.x = T, by.x = "SCHOOL_CODE", by.y = "SCHOOL_CODE")

rm(delta_Teacher)

for (j in c("Mobility_delta","Black_delta","Hispanic_delta","crimecount_percentdelta","licensecount_percentdelta","to_Probation","from_Probation","delta_Safety","delta_Family","delta_Environment","delta_Instruction","delta_Rating","delta_Leader","delta_Teacher")){
  datafinal12[is.na(datafinal12[,j]),j] = 0
  datafinal13[is.na(datafinal13[,j]),j] = 0
}

rm(j)
