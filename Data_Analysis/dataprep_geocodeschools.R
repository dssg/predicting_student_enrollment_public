# =================================== #
#   geocode + prep data for tableau   #
# =================================== #

# geocode school locations from street address --> lat long
library("ggmap")

setwd("/Volumes/appdata-3/School Reports")
data = read.csv("CSVfileforR_excerptof_FY14SBBALLOCATIONS_FINALSUMMARY_asof_032414.csv", stringsAsFactors = F)
school_locations = read.csv("CPS_SchoolLocations_SY13_14.csv", stringsAsFactors = F)

schoolmat = matrix(nrow = 623, ncol = 6)
schoolmat = as.data.frame(schoolmat)
colnames(schoolmat) = c("School ID", "Lon", "Lat", "Actual Total", "Projected Total", "Difference")

for (i in 1:623){
  schoolmat[i,1] = data[i,1] # school ID
  
  findlocation = school_locations[(school_locations$SchoolID == data[i,1]),]
  geocodeschool = geocode(findlocation$Address)
  schoolmat[i,2] = geocodeschool$lon # longitude
  schoolmat[i,3] = geocodeschool$lat # latitude
  schoolmat[i,4] = data[i,22] # actual total
  schoolmat[i,5] = data[i,13] # projected total
  schoolmat[i,6] = schoolmat[i,5] - schoolmat[i,4] # projected - actual
}

# add more variables
schoolmat_extra = matrix(nrow = 623, ncol = 5)
schoolmat_extra = as.data.frame(schoolmat_extra)
colnames(schoolmat_extra) = c("School ID", "Category", "Type", "Geo network", "Geographic region")

for (i in 1:623){
  schoolmat_extra[i,1] = data[i,1] # school ID
  
  findlocation = school_locations[(school_locations$SchoolID == data[i,1]),]
  if (nrow(findlocation) > 0){
  schoolmat_extra[i,2] = findlocation$SchoolCate
  schoolmat_extra[i,3] = findlocation$SchoolType
  schoolmat_extra[i,4] = findlocation$Geographic
  schoolmat_extra[i,5] = findlocation$Geograph_1
  }
}

write.csv(schoolmat, file = "school_projections_geocode_fall13.csv")
write.csv(schoolmat_extra, file = "school_projections_geocode_fall13_EXTRA.csv")