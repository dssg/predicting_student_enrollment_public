library("plyr")
library("GISTools")
library("ggplot2")

setwd("/Volumes/appdata-3/School Reports")
data = read.csv("tbl_9thgrade_toandfrom_tracy.csv", stringsAsFactors = F)
data2013 = subset(data, year == 2013)
schoolstable = read.csv("schools.csv")

setwd("/Volumes/appdata-3/Other Datasets/public neighborhood data/CPS High School Attendance Boundaries SY13 14")
HS_boundaries = readShapePoly(fn = "geo_aeud-d3nt-1")
HS_boundaries_data = slot(HS_boundaries, "data")
HS_boundaries_data = subset(HS_boundaries_data, BoundaryGr %in% c("9, 10", "9, 10, 11, 12"))
HS_catchment1314 = unique(HS_boundaries_data$SchoolID)
HS_catchment1314_keys = subset(schoolstable, SchoolID %in% HS_catchment1314, select = SchoolKey)

catchmentsum = ddply(data2013, .(CATCHMENT_SCHOOL_KEY), summarize, catchmentsum = sum(Freq))

data2013 = merge(data2013, catchmentsum, all.x = T, by.x = "CATCHMENT_SCHOOL_KEY", by.y = "CATCHMENT_SCHOOL_KEY")

data2013 = subset(data2013, CATCHMENT_SCHOOL_KEY %in% as.matrix(HS_catchment1314_keys))

# merge lat long
data2013 = merge(data2013, schoolstable[,c("SchoolKey","Longitude","Latitude")], all.x = T, by.x = "NEXT_GRADE_SCHOOL_KEY", by.y = "SchoolKey")
data2013 = subset(data2013, !is.na(CATCHMENT_SCHOOL_NAME))
data2013 = subset(data2013, !is.na(NEXT_GRADE_SCHOOL_NAME))

data2013$catchmentprop = data2013$Freq/data2013$catchmentsum
data2013$iscatchment = 1*(data2013$NEXT_GRADE_SCHOOL_KEY  == data2013$CATCHMENT_SCHOOL_KEY)

# all together sorted N to S
data2013$NEXT_GRADE_SCHOOL_NAME = with(data2013, reorder(NEXT_GRADE_SCHOOL_NAME, Latitude))
data2013$CATCHMENT_SCHOOL_NAME = factor(data2013$CATCHMENT_SCHOOL_NAME, levels = intersect(levels(data2013$NEXT_GRADE_SCHOOL_NAME), unique(data2013$CATCHMENT_SCHOOL_NAME)))

p <- ggplot(data2013, aes(NEXT_GRADE_SCHOOL_NAME, CATCHMENT_SCHOOL_NAME)) + geom_tile(aes(fill = catchmentprop), colour = "white") + scale_fill_gradient(low = "white", high = "#000066", trans = "sqrt", limits=c(0,1)) + scale_y_discrete(limits = rev(levels(data2013$CATCHMENT_SCHOOL_NAME)))
p + theme(legend.position = "none", axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())


# catchment schools only
data2013_catch = subset(data2013, CATCHMENT_SCHOOL_KEY %in% as.matrix(HS_catchment1314_keys))
data2013_catch = subset(data2013_catch, NEXT_GRADE_SCHOOL_KEY %in% as.matrix(HS_catchment1314_keys))

data2013_catch$NEXT_GRADE_SCHOOL_NAME = with(data2013_catch, reorder(NEXT_GRADE_SCHOOL_NAME, Latitude))
data2013_catch$CATCHMENT_SCHOOL_NAME = factor(data2013_catch$CATCHMENT_SCHOOL_NAME, levels = levels(data2013_catch$NEXT_GRADE_SCHOOL_NAME))
#data2013$NEXT_GRADE_SCHOOL_NAME = factor(data2013$NEXT_GRADE_SCHOOL_NAME, levels = c(levels(data2013$CATCHMENT_SCHOOL_NAME), setdiff(data2013$NEXT_GRADE_SCHOOL_NAME, data2013$CATCHMENT_SCHOOL_NAME)))

p <- ggplot(data2013_catch, aes(NEXT_GRADE_SCHOOL_NAME, CATCHMENT_SCHOOL_NAME)) + geom_tile(aes(fill = catchmentprop), colour = "white") + scale_fill_gradient(low = "white", high = "#000066", trans = "sqrt", limits=c(0,1)) + scale_x_discrete(limits = rev(levels(data2013_catch$NEXT_GRADE_SCHOOL_NAME)))
# with names
p + theme(legend.position = "none", axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size = 8, angle = 270, hjust = 0, colour = "grey50"))
# without names
p + theme(legend.position = "none", axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())


p2 <- ggplot(data2013_catch, aes(NEXT_GRADE_SCHOOL_NAME, CATCHMENT_SCHOOL_NAME)) + geom_tile(aes(fill = iscatchment), colour = "white") + scale_fill_gradient(low = "white", high = "#000066", trans = "sqrt", limits=c(0,1)) + scale_x_discrete(limits = rev(levels(data2013_catch$NEXT_GRADE_SCHOOL_NAME)))
# with names
p2 + theme(legend.position = "none", axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size = 8, angle = 270, hjust = 0, colour = "grey50"))
# without names
p2 + theme(legend.position = "none", axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())


# non-catchment schools only
data2013_noncatch = subset(data2013, CATCHMENT_SCHOOL_KEY %in% as.matrix(HS_catchment1314_keys))
data2013_noncatch = subset(data2013_noncatch, ! NEXT_GRADE_SCHOOL_KEY %in% as.matrix(HS_catchment1314_keys))

data2013_noncatch$NEXT_GRADE_SCHOOL_NAME = with(data2013_noncatch, reorder(NEXT_GRADE_SCHOOL_NAME, Latitude))
data2013_noncatch$CATCHMENT_SCHOOL_NAME = factor(data2013_noncatch$CATCHMENT_SCHOOL_NAME, levels = levels(data2013_catch$CATCHMENT_SCHOOL_NAME))
#data2013$NEXT_GRADE_SCHOOL_NAME = factor(data2013$NEXT_GRADE_SCHOOL_NAME, levels = c(levels(data2013$CATCHMENT_SCHOOL_NAME), setdiff(data2013$NEXT_GRADE_SCHOOL_NAME, data2013$CATCHMENT_SCHOOL_NAME)))


p <- ggplot(data2013_noncatch, aes(NEXT_GRADE_SCHOOL_NAME, CATCHMENT_SCHOOL_NAME)) + geom_tile(aes(fill = catchmentprop), colour = "white") + scale_fill_gradient(low = "white", high = "#000066", trans = "sqrt", limits=c(0,1)) + scale_x_discrete(limits = rev(levels(data2013_noncatch$NEXT_GRADE_SCHOOL_NAME)))
# with names
p + theme(legend.position = "none", axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_text(size = 8, angle = 270, hjust = 0, colour = "grey50"))
# without names
p + theme(legend.position = "none", axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())


# all schools, by rating
setwd("/Volumes/appdata-3/Master_Data")
schoolfeatures = read.csv("schools_changing_features_2013.csv", stringsAsFactors = F)

data2013 = merge(data2013, schoolstable[,c("SchoolKey", "SchoolID")], all.x = T, by.x = "CATCHMENT_SCHOOL_KEY", by.y = "SchoolKey")
data2013 = merge(data2013, schoolfeatures[,c("SchoolID","Rating","Safety","Mobility")], all.x = T, by.x = "SchoolID", by.y = "SchoolID")
table(data2013$Rating)
data2013$Rating2 = as.numeric(as.factor(data2013$Rating))

# catchment schools only
data2013_catch = subset(data2013, CATCHMENT_SCHOOL_KEY %in% as.matrix(HS_catchment1314_keys))
data2013_catch = subset(data2013_catch, NEXT_GRADE_SCHOOL_KEY %in% as.matrix(HS_catchment1314_keys))

data2013_catch$CATCHMENT_SCHOOL_NAME = with(data2013_catch, reorder(CATCHMENT_SCHOOL_NAME, Rating2))
data2013_catch$NEXT_GRADE_SCHOOL_NAME = factor(data2013_catch$NEXT_GRADE_SCHOOL_NAME, levels = levels(data2013_catch$CATCHMENT_SCHOOL_NAME))
#data2013$NEXT_GRADE_SCHOOL_NAME = factor(data2013$NEXT_GRADE_SCHOOL_NAME, levels = c(levels(data2013$CATCHMENT_SCHOOL_NAME), setdiff(data2013$NEXT_GRADE_SCHOOL_NAME, data2013$CATCHMENT_SCHOOL_NAME)))

p <- ggplot(data2013_catch, aes(NEXT_GRADE_SCHOOL_NAME, CATCHMENT_SCHOOL_NAME)) + geom_tile(aes(fill = catchmentprop), colour = "white") + scale_fill_gradient(low = "white", high = "#000066", trans = "sqrt", limits=c(0,1)) + scale_x_discrete(limits = rev(levels(data2013_catch$NEXT_GRADE_SCHOOL_NAME)))
# with names
p + theme(legend.position = "none", axis.ticks = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), 
          axis.text.x = element_text(size = 8, angle = 270, hjust = 0, 
                                     colour = "grey50")) + 
  geom_rect(aes(xmin = 0.5, xmax = 47.5, ymin = 5.5, ymax = 5.5),
            fill = "transparent", color = "black", size = 1) + 
  geom_rect(aes(xmin = 0.5, xmax = 47.5, ymin = 30.5, ymax = 30.5),
            fill = "transparent", color = "black", size = 1) + 
    geom_rect(aes(ymin = 0.5, ymax = 47.5, xmin = 47.5-5, xmax = 47.5-5),
            fill = "transparent", color = "black", size = 1) + 
  geom_rect(aes(ymin = 0.5, ymax = 47.5, xmin = 47.5-30, xmax = 47.5-30),
            fill = "transparent", color = "black", size = 1)

# non-catchment
data2013_noncatch = subset(data2013, CATCHMENT_SCHOOL_KEY %in% as.matrix(HS_catchment1314_keys))
data2013_noncatch = subset(data2013_noncatch, ! NEXT_GRADE_SCHOOL_KEY %in% as.matrix(HS_catchment1314_keys))

data2013_noncatch$CATCHMENT_SCHOOL_NAME = factor(data2013_noncatch$CATCHMENT_SCHOOL_NAME, levels = levels(data2013_catch$CATCHMENT_SCHOOL_NAME))
#data2013$NEXT_GRADE_SCHOOL_NAME = factor(data2013$NEXT_GRADE_SCHOOL_NAME, levels = c(levels(data2013$CATCHMENT_SCHOOL_NAME), setdiff(data2013$NEXT_GRADE_SCHOOL_NAME, data2013$CATCHMENT_SCHOOL_NAME)))


p2 <- ggplot(data2013_noncatch, aes(NEXT_GRADE_SCHOOL_NAME, CATCHMENT_SCHOOL_NAME)) + geom_tile(aes(fill = catchmentprop), colour = "white") + scale_fill_gradient(low = "white", high = "#000066", trans = "sqrt", limits=c(0,1)) + scale_x_discrete(limits = rev(levels(data2013_noncatch$NEXT_GRADE_SCHOOL_NAME)))
# with names
p2 + theme(legend.position = "none", axis.ticks = element_blank(), 
          axis.title.x = element_blank(), axis.title.y = element_blank(), 
          axis.text.x = element_text(size = 8, angle = 270, hjust = 0, 
                                     colour = "grey50")) + 
  geom_rect(aes(xmin = 0.5, xmax = 103.5, ymin = 5.5, ymax = 5.5),
            fill = "transparent", color = "black", size = 1) + 
  geom_rect(aes(xmin = 0.5, xmax = 103.5, ymin = 30.5, ymax = 30.5),
            fill = "transparent", color = "black", size = 1)
  

# without names
p + theme(legend.position = "none", axis.ticks = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
