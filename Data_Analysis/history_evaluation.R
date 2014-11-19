######################################
###Historical Projection Evaluation###
######################################

###CPS Team###
###07/21/2014###

rm(list=ls())
setwd("/Volumes/DSSG/Master_Data/")
suppressMessages(library(ggplot2))
suppressMessages(library(reshape2))
suppressMessages(library(gridExtra))
pred2009 <- read.csv("csp2009.csv")
pred2010 <- read.csv("csp2010.csv")
pred2011 <- read.csv("csp2011.csv")
pred2012 <- read.csv("csp2012.csv")
pred2013 <- read.csv("csp2013.csv")
true2009 <- read.csv("true2009.csv")
true2010 <- read.csv("true2010.csv")
true2011 <- read.csv("true2011.csv")
true2012 <- read.csv("true2012.csv")
true2013 <- read.csv("true2013.csv")
edit <- read.csv("cps_adjust.csv")

###Process Data###
mis_projection <- function(pred, real) {
    id <- intersect(pred$SchoolID, real$SchoolID)
    pred <- subset(pred, pred$SchoolID%in%id)
    real <- subset(real, real$SchoolID%in%id)
    pred <- pred[order(pred$SchoolID),]
    real <- real[order(real$SchoolID),]
    data <- pred-real #projection-actual
    data$SchoolID <- sort(id)
    return(data)
}
data2009 <- mis_projection(pred2009, true2009)
data2010 <- mis_projection(pred2010, true2010)
data2011 <- mis_projection(pred2011, true2011)
data2012 <- mis_projection(pred2012, true2012)
data2013 <- mis_projection(pred2013, true2013)
data2009$year <- 2009
data2010$year <- 2010
data2011$year <- 2011
data2012$year <- 2012
data2013$year <- 2013

###Evaluation###
data_plot <- function(data, name, threshold) {
    temp <- data.frame(year=data$year, miss=unlist(data[name]))
    temp$indicator <- NA
    temp[temp$miss>=threshold,]$indicator <- "Over-projected By 100"
    temp[temp$miss>25 & temp$miss<threshold,]$indicator <- "Over-projected By 25"
    temp[temp$miss>= -25 & temp$miss<=25,]$indicator <- "Tolerably Mis-projected"
    temp[temp$miss< -25 & temp$miss>-threshold,]$indicator <- "Under-projected By 25"
    temp[temp$miss<=-threshold,]$indicator <- "Under-projected By 100"
    temp$indicator <- factor(temp$indicator, levels=c("Under-projected By 100", "Under-projected By 25", "Tolerably Mis-projected", "Over-projected By 25", "Over-projected By 100"))
    return(temp)
}
data_error <- function(data, threshold) {
    err <- data.frame(Year=c(2009, 2010, 2011, 2012, 2013), Total=NA, Error=NA)
    for (i in 1:nrow(err)) {
        temp <- subset(data, data$year==err[i,]$Year)
        err[i,]$Total <- nrow(temp)
        err[i,]$Error <- length(which(abs(temp$miss)>=threshold))
    }
    return(err)
}
#school
data2009_new <- data_plot(data2009, "ATOT", 100)
data2010_new <- data_plot(data2010, "ATOT", 100)
data2011_new <- data_plot(data2011, "ATOT", 100)
data2012_new <- data_plot(data2012, "ATOT", 100)
data2013_new <- data_plot(data2013, "ATOT", 100)
data_all <- do.call("rbind", list(data2009_new, data2010_new, data2011_new, data2012_new, data2013_new))
data_err <- data_error(data_all, 100)
ggplot()+
    geom_bar(data=data_all, aes(x=factor(year), fill=indicator), binwidth=0.5, position="stack")+
    geom_text(data=data_err, aes(x=factor(Year), y=Total+10, label=Error), vjust=0)+
    xlab("Year")+ylab("Number of Schools")

###Serious Mis-projection By Year###
name <- c("K", "Grade1", "Grade2", "Grade3", "Grade4", "Grade5", "Grade6", "Grade7", "Grade8", "Grade9", "Grade10", "Grade11", "Grade12")
grade <- data.frame(matrix(NA, 5, length(name)+1))
colnames(grade) <- c("Year", name)
grade$Year <- 2009:2013
for (i in 1:length(name)) {
    grade[1,i+1] <- length(which(abs(unlist(data2009[name[i]]))>=25))
    grade[2,i+1] <- length(which(abs(unlist(data2010[name[i]]))>=25))
    grade[3,i+1] <- length(which(abs(unlist(data2011[name[i]]))>=25))
    grade[4,i+1] <- length(which(abs(unlist(data2012[name[i]]))>=25))
    grade[5,i+1] <- length(which(abs(unlist(data2013[name[i]]))>=25))
}
grade_melted <- melt(grade, id=1)
ggplot(data=grade_melted, aes(x=Year, y=value, colour=variable))+geom_line()+xlab("Year")+ylab("Number of Schools")

###Compare CSM And Edit###
real <- data.frame(SchoolID=true2013$SchoolID, Total=true2013$ATOT)
csmp <- data.frame(SchoolID=pred2013$SchoolID, ATOT=pred2013$ATOT)
before <- merge(real, csmp)
after <- merge(real, edit)
before_plot <- ggplot(before, aes(x=ATOT-Total))+geom_histogram(binwidth=5)+xlab("Mis-project")+ylab("Number of Schools")+xlim(-500, 500)+ggtitle("Before Edit")
after_plot <- ggplot(after, aes(x=ATOT-Total))+geom_histogram(binwidth=5)+xlab("Mis-project")+ylab("Number of Schools")+xlim(-500, 500)+ggtitle("After Edit")
grid.arrange(before_plot, after_plot, ncol=2)

####Last Year Prediction Error####
name <- c("K", paste0("Grade", 1:12))
temp <- data2013[,names(data2013)%in%name]
grade_graph <- data.frame(Grade=c("K", as.character(1:12)), Misprojection=colSums(abs(temp)>=30))
grade_graph$Grade <- factor(grade_graph$Grade, levels=c("K", as.character(1:12)))
ggplot(grade_graph, aes(x=Grade, y=Misprojection))+geom_bar(stat="identity", fill="slategray4")+theme_bw(base_size=20)+labs(x=NULL, y="# Schools with Large Error")





