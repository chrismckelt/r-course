# Title: run_analysis.R
# Version: 1.0
# Author: Chris McKelt
###############################################################################

#The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
#The goal is to prepare tidy data that can be used for later analysis. 
#You will be graded by your peers on a series of yes/no questions related to the project. 
#You will be required to submit: 
#1) a tidy data set as described below, 
#2) a link to a Github repository with your script for performing the analysis, and 
#3) a code book that describes the variables, the data, and any transformations or work that you performed 
#to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. 
#This repo explains how all of the scripts work and how they are connected. 

#One of the most exciting areas in all of data science right now is wearable computing - see for example this article . 
#Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
#http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#Here are the data for the project:
#https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#You should create one R script called run_analysis.R that does the following. 
#1) Merges the training and the test sets to create one data set.
#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#3) Uses descriptive activity names to name the activities in the data set
#4) Appropriately labels the data set with descriptive variable names. 
#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

#Good luck!

###############################################################################
# packages and setup
###############################################################################
rm(list = ls()) # clear vars
packages <- c("data.table", "reshape2", "plyr", "assertthat")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

setwd("C:/dev/r-course/course-2/project")
path <- getwd()
path

pathIn <- file.path(path, "UCI HAR Dataset") # may not be created yet

# create results folder
resultsfolder <- "results"
if (!file.exists(resultsfolder)) {
    print("create results folder")
    dir.create(resultsfolder)
}

###############################################################################
# functions 
###############################################################################
# download zip and extract to folder
downloadProjectFile <- function() {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    filename <- "Dataset.zip"
    if (!file.exists(path)) {
        dir.create(path)
    }
    zipFile <- file.path(path, filename)
    download.file(url, zipFile)
    unzip(zipFile, exdir = path)
}

# read file from given folder and filename
getFile <- function(folder, filename) {
    res <- file.path(pathIn, folder, filename)
    class(res)
    fread(res)
}

#save to results folder
saveresults <- function(data, name) {
    print(paste("saving results", name))
    file <- paste(resultsfolder, "/", name, ".csv", sep = "")
    write.csv(data, file)
}
###############################################################################

# download if not already downloaded
if (!file.exists("./UCI HAR Dataset"))
    downloadProjectFile()

# 1.Merges the training and the test sets to create one data set.

# subjects
subject_train <- getFile("train", "subject_train.txt")
subject_test <- getFile("test", "subject_test.txt")
subject <- rbind(subject_train, subject_test) #merge
setnames(subject, "V1", "subject")
print("data table added : subjects")
# activity
activity_train <- getFile("train", "Y_train.txt")
activity_test <- getFile("test", "Y_test.txt")
activity <- rbind(activity_train, activity_test)
setnames(activity, "V1", "activityId")
print("data table added : activity")
#training
train_train <- getFile("train", "X_train.txt")
train_test <- getFile("test", "X_test.txt")
train <- rbind(train_train, train_test) #merge
print("data table added : train")
# merge all 3 into 1 data table called dt
dt <- cbind(subject, activity, train)
dt <- arrange(dt, subject, activity)
colnames(dt)
print("data tables merged to dt")
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- fread(file.path(pathIn, "features.txt"))
setnames(features, names(features), c("featureId", "featureName"))
features <- features[grepl("mean\\(\\)|std\\(\\)", featureName)]
row_count <- nrow(features)
assert_that(row_count == 66)
print("data table added : features")

# append V to featureId column and convert to variable names for column filtering (include pre-named)
filt <- sapply(list(features$featureId), FUN = function(x) {
    c("subject", "activityId", paste0("V", x))
})
select_expression <- c(key(dt), filt)
print(nrow(dt))
filtered <- dt[colnames(dt) %in% filt]

print(nrow(dt))
print("dt filtered for columns with mean or standard deviation")
saveresults(dt, "mean_and_standard_deviation")
setkey(dt,subject,activityId)
stop("stopping...")

# 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(activity_labels, names(activity_labels), c("activityId", "activityName"))
dt <- merge(dt, activity_labels, all.x = TRUE)

 
#4) Appropriately labels the data set with descriptive variable names. 
dt$activity <- factor(dt$V1)
dt$feature <- factor(dt$V2)
dt <- arrange(dt, feature,activity)