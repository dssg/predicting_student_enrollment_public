setwd("//admin/appdata/DataWarehouse/DSSG/Master_Data/")
library(reshape2)
library(ggplot2)
rm(list=ls())

year = 2013
students = read.csv(paste0("student",year,".csv"))
students = subset(students, !(EducationType %in% c("21 - 60%", "61 - 100%")))
schools = read.csv("schools_static_features.csv")

# students$NextGradeSchoolKey = ifelse(is.na(students$NextGradeSchoolKey),
#                                      0, as.character(students$NextGradeSchoolKey))
students = subset(students, !is.na(students$NextGradeSchoolKey))
students$CatchmentSchoolKey = ifelse(is.na(students$CatchmentSchoolKey),
                                     0, students$CatchmentSchoolKey)

trans_table = with(students, table(CatchmentSchoolKey, NextGradeSchoolKey))
trans_table_melt <- melt(trans_table)
trans_table_melt = merge(trans_table_melt, subset(schools,,c(SchoolKey, SchoolID)), 
                         by.x = "CatchmentSchoolKey", by.y = "SchoolKey", all.x = TRUE)
colnames(trans_table_melt)[4] = "CatchmentSchoolID"
trans_table_melt = merge(trans_table_melt, subset(schools,,c(SchoolKey, SchoolID)), 
                         by.x = "NextGradeSchoolKey", by.y = "SchoolKey", all.x = TRUE)
colnames(trans_table_melt)[5] = "NextGradeSchoolID"

library(dplyr)
PercentSame = tbl_df(trans_table_melt) %>% 
  mutate(SameSchool = (CatchmentSchoolKey==NextGradeSchoolKey)) %>%
  group_by(CatchmentSchoolID) %>% 
  summarise(
    Total = sum(value),
    Catchment = sum(ifelse(SameSchool,value,0))
    ) %>%
  mutate(PercentCatchment = Catchment/Total)

# Neighborhood boundaries
setwd("//admin/appdata/DataWarehouse/DSSG/Other Datasets/public neighborhood data/CPS High School Attendance Boundaries SY13 14")
# http://geocommons.com/overlays/305198
library(rgdal)
library(maptools)
library(rgeos)

boundaries = readOGR(dsn=".", layer="geo_aeud-d3nt-1")
boundaries@data$id = rownames(boundaries@data)
boundaries = gBuffer(boundaries, width=0, byid=TRUE)
boundaries.points = fortify(boundaries, region="id")
boundaries.df = plyr::join(boundaries.points, boundaries@data, by="id")

boundaries.df = merge(boundaries.df, PercentSame, by.x = "SchoolID", by.y = "CatchmentSchoolID", all.x=TRUE)
boundaries.df = boundaries.df[order(boundaries.df$order),]
boundaries.df = subset(boundaries.df, !is.na(boundaries.df$Catchment))

library(ggmap)
plot_base = qmap(location = c(-87.70831, 41.84566), zoom = 10, maptype = "terrain") 

# id = 609708 # 609679
# boundaries_school_counts = merge(bourdaries.df, subset(trans_table_melt, CatchmentSchoolID == id, c(NextGradeSchoolID, value)),
#                                  by.x = "SchoolID", by.y = "NextGradeSchoolID", all.x = TRUE)
# # boundaries_school_counts$value[is.na(boundaries_school_counts$value)] <- 0
# colnames(boundaries_school_counts)[ncol(boundaries_school_counts)] <- "To"
# boundaries_school_counts = merge(boundaries_school_counts, subset(trans_table_melt, NextGradeSchoolID == id, c(CatchmentSchoolID, value)),
#                                  by.x = "SchoolID", by.y = "CatchmentSchoolID", all.x = TRUE)
# # boundaries_school_counts$value[is.na(boundaries_school_counts$value)] <- 0
# colnames(boundaries_school_counts)[ncol(boundaries_school_counts)] <- "From"
# boundaries_school_counts = boundaries_school_counts[order(boundaries_school_counts$order),]
# 
# school = subset(schools, SchoolID == id)
# 
# school_mark = geom_point(data=school, aes(x = Longitude, y = Latitude), col = 'red', shape = 4, size = 4)
# school_path = geom_path(data = boundaries_school_counts, aes(long,lat,group=group))
# school_poly = geom_polygon(data = boundaries_school_counts, aes(long,lat,group=group, fill = To), alpha = 1)
# p <- ggplot() + school_poly + school_mark + school_path + ggtitle(paste0("School = ", school$SchoolName)) +
#   scale_fill_continuous(low = "white", high = "steelblue")
# print(p)

# school_path = geom_path(data = boundaries.df, aes(long,lat,group=group), colour = "black", alpha = .65, size = 0.5)
# school_poly = geom_polygon(data = boundaries.df, aes(long,lat,group=group, fill = PercentCatchment*100), alpha = .6)
# p <- plot_base + school_poly + school_path + scale_fill_continuous(low = "white", high = "#000066") +
#   ggtitle("Percent of Students that Go to their Catchment School") + labs(fill=NULL) + 
#   theme(legend.justification=c(1,1), legend.position=c(1,1), legend.background = element_rect(fill="transparent"))
# print(p)

school_poly = geom_polygon(data = boundaries.df, aes(long,lat,group=group, fill = cut(round(PercentCatchment*100), seq(0,75,15))), alpha = .6)
p <- plot_base + school_poly + school_path + scale_fill_brewer(palette="Purples", labels = paste0(seq(0,60,15),"% to ",c(seq(15,60,15),65),"%")) +
  labs(fill=NULL) + theme(legend.text = element_text(size = 18), legend.justification=c(1,1), legend.position=c(1,1), legend.background = element_rect(fill="transparent"))
print(p)
ggsave(plot = p, filename = "//admin/appdata/DataWarehouse/DSSG/Visualizations/R plots/PercentCatchmentSchool.pdf", width=8, height=8)
# ggtitle("Percent of Students that Go to their Catchment School")
