# ------------------------------------------ #
#   create data file to read in to tableau   #
# ------------------------------------------ #

setwd("/Volumes/appdata-3/Other Datasets/public neighborhood data/CPS High School Attendance Boundaries SY13 14")

library("GISTools")
boundaries = readShapePoly(fn = "geo_aeud-d3nt-1") # load boundaries
boundaries = boundaries[boundaries@data$BoundaryGr %in% c("9, 10", "9, 10, 11, 12"),] # only 9th grade boundaries
boundaries_polygons = slot(boundaries, "polygons")
boundaries_data = slot(boundaries, "data")

boundaries_csv = NULL

for (i in 1:57){
  
  temp = boundaries_polygons[[i]] # pull out polygon i
  tempcoords = slot(slot(temp, "Polygons")[[1]],"coords") # extract coordinates
  tempid = slot(temp, "ID") # extract polygon ID
  tempname = as.character(boundaries_data[i,"SchoolID"]) # extract school ID
  
  tempmat = cbind(c(0:(nrow(tempcoords)-1)),tempcoords, rep(tempid, nrow(tempcoords)), rep(tempname, nrow(tempcoords)))
  tempmat = as.data.frame(tempmat)
  colnames(tempmat) = c("PointOrder","Longitude","Latitude","PolygonID","SchoolID")
  boundaries_csv = rbind(boundaries_csv, tempmat) # save information about this polygon
  
}

setwd("/Volumes/appdata-3/School Reports")
schoolstable = read.csv("schools.csv") # schools table from database
# for each catchment school ID, match up the school key and school name
boundaries_csv = merge(boundaries_csv, schoolstable[,c("SchoolKey","SchoolID","SchoolShortName")], all.x = T, by.x = "SchoolID", by.y = "SchoolID")

setwd("/Volumes/appdata-3/School Reports")
data = read.csv("tbl_9thgrade_toandfrom_tracy.csv", stringsAsFactors = F) # matrix of where students are from and where they go to 9th grade
data2013 = subset(data, year == 2013) # only look at 2013

# match up each catchment school key with the school ID
data2013 = merge(data2013, schoolstable[,c("SchoolID","SchoolKey")], all.x = TRUE, by.x = "CATCHMENT_SCHOOL_KEY", by.y = "SchoolKey")

# for each 'next grade' school key, match up the latitude and longitude of that school
data2013 = merge(data2013, schoolstable[,c("SchoolKey","Latitude","Longitude")], all.x = T, by.x = "NEXT_GRADE_SCHOOL_KEY", by.y = "SchoolKey")

data2013 = subset(data2013, Freq > 0) # only need combinations of catchment area and school where there are actually students
data2013 = subset(data2013, !is.na(Latitude)) # remove if the locoation is missing

catchment1314 = unique(boundaries_data$SchoolID) # find out school IDs of catchment areas

data2013 = subset(data2013, SchoolID %in% catchment1314) # only need to consider catchment areas that have associated geographic area
# there are some students where the catchment area is still their 8th grade school, we remove these

# prepare data to read in to tableau: create 'type' variable
boundaries_csv$type = 'polygon' 
data2013$type = 'school_point'

boundaries_csv$NEXT_GRADE_SCHOOL_NAME = NA
boundaries_csv$Freq = 0

names(boundaries_csv)[7] = "CATCHMENT_SCHOOL_NAME"
boundaries_csv = boundaries_csv[,c("type","SchoolID","PointOrder","PolygonID","Longitude","Latitude","CATCHMENT_SCHOOL_NAME","NEXT_GRADE_SCHOOL_NAME","Freq")]

data2013$PointOrder = NA
data2013$PolygonID = NA
data2013 = data2013[,c("type","SchoolID","PointOrder","PolygonID","Longitude","Latitude","CATCHMENT_SCHOOL_NAME","NEXT_GRADE_SCHOOL_NAME","Freq")]

setwd("/Volumes/appdata-3/Other Datasets/public neighborhood data")

boundaries_csv$Longitude = as.numeric(as.character(boundaries_csv$Longitude))
boundaries_csv$Latitude = as.numeric(as.character(boundaries_csv$Latitude))

data_for_tableau = rbind(boundaries_csv, data2013)
write.csv(data_for_tableau, file = "data_for_tableau_FINAL.csv", row.names = F)

# for reverse, run above except do not remove 0 Freq's
locations = unique(data2013[,c("NEXT_GRADE_SCHOOL_NAME","Latitude","Longitude")])
locations$type = 'locations'
locations$PolygonID = NA
locations$PointOrder = NA
locations$SchoolID = NA
locations$Freq = 0
locations$CATCHMENT_SCHOOL_NAME = NA

locations = locations[,names(data2013)]

data_for_tableau = rbind(boundaries_csv, locations)
data_for_tableau = data_for_tableau[,names(data_for_tableau) != 'Freq']
write.csv(data_for_tableau, file = "data_for_tableau_FINAL_reverse.csv", row.names = F)
write.csv(data2013, file = "data_for_tableau_FINAL_reverse2.csv", row.names = F)
