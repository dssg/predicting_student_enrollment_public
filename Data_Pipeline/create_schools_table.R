#!/usr/bin/Rscript

#####################################
####Create School Static Features####
#####################################

####CPS Team####
####08/20/2014####

rm(list = ls())
setwd("//admin/appdata/DataWarehouse/DSSG/School Reports/")

# Pull in data from CPS data warehouse ####
library(RODBC)
# an ODBC called "DW_QA" which accesses the K12intel_qa database
channel <- odbcConnect("DW_QA")

school_query <- "SELECT * 
  FROM K12INTEL_DW.dbo.DSSG_School_Query
  WHERE SchoolKey>1"
odbcClose(channel)

schools = sqlQuery(channel, school_query, stringsAsFactors = FALSE)

# Geocode school addresses ####
library("ggmap")
addresses = with(schools, paste(Address, City, State, ZipCode, sep = ", "))
good_addresses = !is.na(schools$Address)
geocodeschools = geocode(addresses[good_addresses])

schools$Longitude[good_addresses] = geocodeschools$lon
schools$Latitude[good_addresses] = geocodeschools$lat

# plot(geocodeschools) # should look like chicago

# Get census block group and tract information based on the geocode ####
get_FIPS <- function(latlons) {
  library(RJSONIO)
  # latlons is a n X 2 matrix with latitude in first column and longitude in second column
  # returns an n-dimensional character vector of FIPS codes
  urls = paste0("http://data.fcc.gov/api/block/2010/find?format=json&latitude=",
                latlons[,1], "&longitude=", latlons[,2])
  
  FIPSs = rep(NA, nrow(latlons))
  index = which(!apply(is.na(latlons), 1, any))
  for (i in index) {
    bk = fromJSON(urls[i])
    
    if (bk$status == "OK") {
      FIPSs[i] = bk$Block
    }
  }
  
  FIPSs[index] = paste0("US", FIPSs[index])
  return(FIPSs)
}

FIPS = get_FIPS(subset(schools, select = c("Latitude", "Longitude")))
schools$FIPSCode = FIPS

write.table(schools, "//admin/appdata/DataWarehouse/DSSG/Master_Data/schools_static_features.csv", row.names = FALSE, sep = ",")


## School Aggregate Features ####
setwd("//admin/appdata/DataWarehouse/DSSG/School Reports/")

fileName1 <- '//admin/appdata/DataWarehouse/DSSG/School Reports/school_aggregate_features_vw.sql'
aggr_query <- readChar(fileName1, file.info(fileName1)$size)

library(RODBC)
channel <- odbcConnect("DW_QA")
school_aggr = sqlQuery(channel, aggr_query, stringsAsFactors = FALSE)
odbcClose(channel)

for (year in 2011:2013) {
  school_aggr_year = subset(school_aggr, SchoolYear == year)
  school_aggr_year = school_aggr_year[,-which(colnames(school_aggr)=="SchoolYear")]
  write.table(school_aggr_year, paste0("//admin/appdata/DataWarehouse/DSSG/Master_Data/schools_aggregate_features_",year,".csv"), row.names = FALSE, sep = ",")
}

