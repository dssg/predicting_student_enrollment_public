#!/usr/bin/Rscript

#######################
####Create Geo Code####
#######################

####CPS Team####
####08/20/2014####

library('ggmap')
library('plyr')
library('RDSTK')

setwd("//admin/appdata/DataWarehouse/DSSG/")

year=2011
students <- read.csv(paste0("students",year,".csv"), header=T)

students$Address <- do.call(paste, c(students[c("StreetNumber", "StreetDirection", "StreetName", "StreetType", "City", "State", "ZipCode")], sep="+"))
#fit student addresses into a data frame of text string
#subset to create new dataset
names(students)
student_addresses <- students[,c(5,42)]

temp <- data.frame(matrix(NA, nrow(students), 13))
for (i in 1:nrow(students))
{
  tmp <- tryCatch(
    street2coordinates(student_addresses[i,3], session=getCurlHandle())
  )
  temp[i,] <- tmp[1,]
  
  print(i)
}

#add latitude and longitudes to original dataset
student_addresses$latitude <- newset[,3]
student_addresses$longitude <- newset[,5]

#write to csv
write.csv(student_addresses, file="student_address_table.csv", row.names=F)
