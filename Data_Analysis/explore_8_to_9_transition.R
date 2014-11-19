setwd("//admin/appdata/DataWarehouse/DSSG")
load("Student_Data/students.RData")
library(reshape2)
library(ggplot2)

students$High_School = ifelse(is.na(students$NEXT_GRADE_STUDENT_ANNUAL_SCHOOL),
                                  "None", as.character(students$NEXT_GRADE_STUDENT_ANNUAL_SCHOOL))
students$High_School_Code = ifelse(is.na(students$NEXT_GRADE_STUDENT_ANNUAL_SCHOOL_CODE),
                                       0, students$NEXT_GRADE_STUDENT_ANNUAL_SCHOOL_CODE)

students2010 = subset(students, SCHOOL_YEAR == "2010-2011")

transition_codes = table(students2010$STUDENT_ANNUAL_SCHOOL_CODE, 
                         students2010$High_School_Code)
transition_codes_years = table(students$STUDENT_ANNUAL_SCHOOL_CODE, 
                               students$High_School_Code,
                               students$SCHOOL_YEAR)



trans_codes_m = melt(transition_codes_years, varnames = c("ES_code", "HS_code", "SCHOOL_YEAR"))
trans_codes_m = trans_codes_m[trans_codes_m$value>0,]
head(trans_codes_m)

schools = read.csv("School Reports/schools.csv")
schools_loc = subset(schools, select = c("SCHOOL_CODE", "lat", "lon", "SCHOOL_TYPE"))

trans_codes_m = merge(trans_codes_m, schools_loc[, -4], by.x = "ES_code", by.y = "SCHOOL_CODE", all.x = TRUE)
names(trans_codes_m)[5:6] <- paste("ES", c("lat","lon"), sep = "_")

trans_codes_m = merge(trans_codes_m, schools_loc, by.x = "HS_code", by.y = "SCHOOL_CODE", all.x = TRUE)
names(trans_codes_m)[7:8] <- paste("HS", c("lat","lon"), sep = "_")

# plot on map for some schools ####
library(ggmap)
pdf("Visualizations/R plots/explore_8_to_9.pdf")
source_school_codes = sample(unique(trans_codes_m$ES_code),20)
# map = get_googlemap('chicago', zoom = 11)
for (sc in source_school_codes) {
  es_trans = as.data.frame(trans_codes_m[trans_codes_m$ES_code == sc & trans_codes_m$SCHOOL_YEAR == "2010-2011",])
  if (!is.na(es_trans$ES_lat[1])) {
    school_name = as.character(schools$SCHOOL_SHORT_NAME[schools$SCHOOL_CODE == es_trans$ES_code[1]])
    qm <- qmap("Chicago", zoom = 10) + labs(size = "# Students", title = school_name) + 
      geom_point(data = es_trans[1,], aes(x = ES_lon, y = ES_lat), shape = "x", colour = "black", size = 10) + 
      geom_point(data = es_trans, aes(x = HS_lon, y = HS_lat, size = value), colour = "red")
    print(qm)
  }
}
dev.off()

# plot with "background singers" ####
high_schools = as.data.frame(unique(trans_codes_m[!is.na(trans_codes_m$HS_lat), 7:9]))
plot_base = ggplot(high_schools, aes(HS_lon, HS_lat)) + geom_point(aes(colour = SCHOOL_TYPE), shape = 1) + coord_map()

source_school_codes = sample(unique(trans_codes_m$ES_code),20)
for (sc in source_school_codes) {
  es_trans = as.data.frame(trans_codes_m[trans_codes_m$ES_code == sc, ])
  es_trans[es_trans$HS_code == 0, 7:8] = rep(c(41.9, -87.55), each = sum(es_trans$HS_code == 0))
  if (!is.na(es_trans$ES_lat[1])) {
    school_name = as.character(schools$SCHOOL_SHORT_NAME[schools$SCHOOL_CODE == es_trans$ES_code[1]])
    school_type = as.character(schools$SCHOOL_TYPE[schools$SCHOOL_CODE == es_trans$ES_code[1]])
    p <- plot_base + geom_point(data = es_trans, aes(size = value, colour = SCHOOL_TYPE)) +
      geom_point(data = es_trans, aes(x = ES_lon, y = ES_lat, colour = school_type), shape = "x", size = 10) +
      labs(size = "# Students", title = paste0(school_name, " (", school_type, ")"), x = "Latitude", y = "Longitude") + 
      facet_wrap( ~ SCHOOL_YEAR, nrow = 1)
    print(p)
  }
}

# distance between elem school and high schools ####
rdist.earth.vec <- function(x1, x2, miles = TRUE, R = NULL) {
  # modified from fields::rdist.earth
  if (is.null(R)) {
    if (miles) 
      R <- 3963.34
    else R <- 6378.388
  }
  coslat1 <- cos((x1[, 2] * pi)/180)
  sinlat1 <- sin((x1[, 2] * pi)/180)
  coslon1 <- cos((x1[, 1] * pi)/180)
  sinlon1 <- sin((x1[, 1] * pi)/180)
  
  coslat2 <- cos((x2[, 2] * pi)/180)
  sinlat2 <- sin((x2[, 2] * pi)/180)
  coslon2 <- cos((x2[, 1] * pi)/180)
  sinlon2 <- sin((x2[, 1] * pi)/180)

  pp <- cbind(coslat1 * coslon1, coslat1 * sinlon1, sinlat1) *
    cbind(coslat2 * coslon2, coslat2 * sinlon2, sinlat2)
  pp = rowSums(pp)
  return(R * acos(ifelse(abs(pp) > 1, 1 * sign(pp), pp)))
}

trans_codes_m$Distance = with(trans_codes_m, rdist.earth.vec(cbind(ES_lon, ES_lat), cbind(HS_lon, HS_lat)))

trans_codes_m2010 = subset(trans_codes_m, SCHOOL_YEAR == "2010-2011" & !is.na(Distance))
ggplot(trans_codes_m2010, aes(Distance, weight = value)) + geom_histogram(binwidth=1) + facet_wrap( ~ SCHOOL_TYPE, scales = "free_y")

wquantile <- function(v,w=rep(1,length(v)),p=.5) {
  # from https://stat.ethz.ch/pipermail/r-help/2009-February/188762.html
  if (!is.numeric(v) || !is.numeric(w) || length(v) != length(w))
    stop("Values and weights must be equal-length numeric vectors")
  if ( !is.numeric(p) || any( p<0 | p>1 ) )
    stop("Quantiles must be 0<=p<=1")
  ranking <- order(v)
  sumw <- cumsum(w[ranking])
  if ( is.na(w[1]) || w[1]<0 ) stop("Weights must be non-negative numbers")
  plist <- sumw/sumw[length(sumw)]
  sapply(p, function(p) v [ ranking [ which.max( plist >= p ) ] ])
}

with(subset(trans_codes_m2010, SCHOOL_TYPE == "Regular"),
     wquantile(Distance, value, p = .9))
with(subset(trans_codes_m2010, SCHOOL_TYPE == "Charter"),
     wquantile(Distance, value, p = .9))
with(subset(trans_codes_m2010, SCHOOL_TYPE == "Performance"),
     wquantile(Distance, value, p = .9))
with(subset(trans_codes_m2010, SCHOOL_TYPE == "Contract"),
     wquantile(Distance, value, p = .9))
with(subset(trans_codes_m2010, SCHOOL_TYPE == "Alternative"),
     wquantile(Distance, value, p = .9))
with(subset(trans_codes_m2010),
     wquantile(Distance, value, p = 0.8569962))

with(subset(trans_codes_m2010, SCHOOL_TYPE == "Regular"),
  sum(value[Distance<6.27])/sum(value))


# open Illinois zip code shape files ####
setwd("//admin/appdata/DataWarehouse/DSSG/Other Datasets/IllinoisZipCodes")
# http://geocommons.com/overlays/305198
library(rgdal) 
library(maptools)
library(rgeos)

ilzips = readOGR(dsn=".", layer="Zip_Codes")
ilzips@data$id = rownames(ilzips@data)
ilzips = gBuffer(ilzips, width=0, byid=TRUE)
ilzips.points = fortify(ilzips, region="id")
ilzips.df = plyr::join(ilzips.points, ilzips@data, by="id")


# look at zip code source to high school ####
setwd("//admin/appdata/DataWarehouse/DSSG")
transition_zips_years = table(students$STUDENT_POSTAL_CODE, 
                               students$High_School_Code,
                               students$SCHOOL_YEAR)

trans_codes_m = melt(transition_zips_years, varnames = c("ZipCode", "HS_code", "SCHOOL_YEAR"))
trans_codes_m = trans_codes_m[trans_codes_m$value>0,]
head(trans_codes_m)

schools = read.csv("School Reports/schools.csv")
schools_loc = subset(schools, select = c("SCHOOL_CODE", "lat", "lon", "SCHOOL_TYPE"))

library(zipcode)
data(zipcode)

trans_codes_m = merge(trans_codes_m, zipcode[, c(1, 4, 5)], by.x = "ZipCode", by.y = "zip", all.x = TRUE)
names(trans_codes_m)[5:6] <- paste("zip", c("lat","lon"), sep = "_")

trans_codes_m = merge(trans_codes_m, schools_loc, by.x = "HS_code", by.y = "SCHOOL_CODE", all.x = TRUE)
names(trans_codes_m)[7:8] <- paste("HS", c("lat","lon"), sep = "_")

library(dplyr)
big_zips = tbl_df(trans_codes_m) %>%
  group_by(ZipCode) %>%
  summarise(Students = sum(value)) %>%
  filter(Students >= 1000) %>%
  arrange(desc(Students))

total_per_zip_year = tbl_df(trans_codes_m) %>%
  group_by(ZipCode, SCHOOL_YEAR) %>%
  summarise(TotalStudents = sum(value))
trans_codes_m = merge(trans_codes_m, total_per_zip_year)
trans_codes_m$Percent = trans_codes_m$value / trans_codes_m$TotalStudents * 100
# trans_codes_m$Distance = with(trans_codes_m, rdist.earth.vec(cbind(zip_lon, zip_lat), cbind(HS_lon, HS_lat)))


# plot
# source_zips = sort(sample(as.character(unique(trans_codes_m$ZipCode))[-1],20))
library(maps)
high_schools = as.data.frame(unique(subset(trans_codes_m, !is.na(HS_lat), 
                                           c("SCHOOL_YEAR", "HS_lat", "HS_lon", "SCHOOL_TYPE"))))

source_zips = big_zips[1:10, 1]
z = source_zips[2]
size_range = range(trans_codes_m[trans_codes_m$ZipCode %in% source_zips, "Percent"])


plot_base = ggplot(high_schools, aes(HS_lon, HS_lat)) + 
  labs(size = "% of Students", y = "Latitude", x = "Longitude", colour = "High School Type") + 
  scale_size_continuous(breaks = seq(10, floor(size_range[2]/10)*10, by = 10), limit = size_range, range = c(2, 6)) + 
  facet_wrap( ~ SCHOOL_YEAR, nrow = 1) + geom_path( data=map_data("state"), aes(x=long, y=lat, group = group),colour="black") + 
  coord_map(xlim = range(high_schools$HS_lon), ylim = range(high_schools$HS_lat)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
pdf("Visualizations/R plots/explore_zip_to_9.pdf", hei=7, wid = 11)
for (z in source_zips) {
  cat(which(z == source_zips), " ")
  zip_trans = as.data.frame(trans_codes_m[trans_codes_m$ZipCode == z, ])
  if (sum(zip_trans$HS_code == 0)>0)
    zip_trans[zip_trans$HS_code == 0, 7:8] = rep(c(41.9, -87.55), each = sum(zip_trans$HS_code == 0))
  if (!is.na(zip_trans$zip_lat[1])) {
    zip_code = as.character(z)
    zip_poly = geom_polygon(data = subset(ilzips.df, ZCTA5CE10 == z), aes(long,lat,group=group), fill="grey", alpha = .8)
    # zip_poly = geom_path(data = subset(ilzips.df, ZCTA5CE10 == z), aes(long,lat,group=group), colour="black")
    p <- plot_base + zip_poly + geom_point(aes(colour = SCHOOL_TYPE), shape = 1) + 
      geom_point(data = zip_trans, aes(size = Percent, colour = SCHOOL_TYPE)) +
      ggtitle(paste0("Zip Code = ", zip_code))
      # geom_point(data = zip_trans, aes(x = zip_lon, y = zip_lat), shape = "x", size = 10)
    print(p)
  }
}
dev.off()


# same as above with google map in background
# library(ggmap)
# plot_base = qmap("Chicago", zoom = 10) + 
#   labs(size = "% of Students", x = "Latitude", y = "Longitude", colour = "High School Type") + 
#   scale_size_continuous(breaks = seq(10, floor(size_range[2]/10)*10, by = 10), limit = size_range, range = c(3, 10)) + 
#   facet_wrap( ~ SCHOOL_YEAR, nrow = 1) # + xlim(range(high_schools$HS_lat)) + ylim(range(high_schools$HS_lon))
# for (z in source_zips) {
#   zip_trans = as.data.frame(trans_codes_m[trans_codes_m$ZipCode == z, ])
#   if (sum(zip_trans$HS_code == 0)>0)
#     zip_trans[zip_trans$HS_code == 0, 7:8] = rep(c(41.9, -87.55), each = sum(zip_trans$HS_code == 0))
#   if (!is.na(zip_trans$zip_lat[1])) {
#     zip_code = as.character(z)
#     zip_poly = geom_path(data = subset(ilzips.df, ZCTA5CE10 == z), aes(long,lat,group=group), colour="black")
#     p <- plot_base + zip_poly + 
#       geom_point(data = high_schools, aes(HS_lon, HS_lat, colour = SCHOOL_TYPE), shape = 1, size = 3) + 
#       geom_point(data = zip_trans, aes(HS_lon, HS_lat, size = Percent, colour = SCHOOL_TYPE)) +
#       ggtitle(paste0("Zip Code = ", zip_code))
#     # geom_point(data = zip_trans, aes(x = zip_lon, y = zip_lat), shape = "x", size = 10)
#     print(p)
#   }
# }

# plot for OpenGov Hack Night
trans_codes_m = subset(trans_codes_m, SCHOOL_YEAR == "2012-2013")

high_schools = as.data.frame(unique(subset(trans_codes_m, !is.na(HS_lat), 
                                           c("SCHOOL_YEAR", "HS_lat", "HS_lon", "SCHOOL_TYPE"))))

source_zips = big_zips[1:10, 1]
size_range = range(trans_codes_m[trans_codes_m$ZipCode %in% source_zips, "Percent"])


plot_base = ggplot(high_schools, aes(HS_lon, HS_lat)) + 
  labs(size = "# of Students", y = "Latitude", x = "Longitude") + 
  scale_size_continuous(range = c(2, 6)) + # breaks = seq(5, 25, by = 5), limit = size_range, 
  geom_path( data=map_data("state"), aes(x=long, y=lat, group = group),colour="black") + 
  coord_map(xlim = range(high_schools$HS_lon), ylim = range(high_schools$HS_lat)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

z = source_zips[2]
zip_trans = as.data.frame(trans_codes_m[trans_codes_m$ZipCode == z, ])
if (sum(zip_trans$HS_code == 0)>0)
  zip_trans[zip_trans$HS_code == 0, 7:8] = rep(c(41.9, -87.55), each = sum(zip_trans$HS_code == 0))
if (!is.na(zip_trans$zip_lat[1])) {
  zip_code = as.character(z)
  zip_poly = geom_polygon(data = subset(ilzips.df, ZCTA5CE10 == z), aes(long,lat,group=group), fill="grey", alpha = .8)
  # zip_poly = geom_path(data = subset(ilzips.df, ZCTA5CE10 == z), aes(long,lat,group=group), colour="black")
  p <- plot_base + zip_poly + geom_point(shape = 1) + 
    geom_point(data = zip_trans, aes(size = value)) +
    ggtitle(paste0("Zip Code = ", zip_code))
  # geom_point(data = zip_trans, aes(x = zip_lon, y = zip_lat), shape = "x", size = 10)
  print(p)
}


library(ggmap)

# plot_base = qmap("Chicago", zoom = 10, maptype = "roadmap") + 
# colMeans(high_schools[,c(3,2)])
plot_base = qmap(location = c(-87.70831, 41.84566), zoom = 11, maptype = "terrain") + 
  labs(size = "Students", x = "Latitude", y = "Longitude")
  # + xlim(range(high_schools$HS_lat)) + ylim(range(high_schools$HS_lon))

z = big_zips[2,1]
pdf("Visualizations/R plots/zip_to_HS_maps.pdf")
for (z in big_zips[,1]) {
  print(z)
  zip_trans = as.data.frame(trans_codes_m[trans_codes_m$ZipCode == z, ])
  if (sum(zip_trans$HS_code == 0)>0)
    zip_trans[zip_trans$HS_code == 0, 7:8] = rep(c(41.9, -87.55), each = sum(zip_trans$HS_code == 0))
  
  zip_code = as.character(z)
  # zip_poly = geom_path(data = subset(ilzips.df, ZCTA5CE10 == z), aes(long,lat,group=group), colour="black")
  zip_poly = geom_polygon(data = subset(ilzips.df, ZCTA5CE10 == z), aes(long,lat,group=group), fill="blue", alpha = .5)
  p <- plot_base + zip_poly + 
    geom_point(data = high_schools, aes(HS_lon, HS_lat), shape = 1, size = 3) + 
    geom_point(data = zip_trans, aes(HS_lon, HS_lat, size = value)) +
    scale_size_continuous(range = c(3, 9)) + 
    ggtitle(paste0("Zip Code = ", zip_code)) + theme(legend.text = element_text(size = 16), legend.title = element_text(size = 16))
  print(p)
}
dev.off()
