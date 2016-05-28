
# Title: run_analysis.R
# Version: 2.0
# Author: Chris McKelt


####packages and setup

    rm(list = ls()) # clear vars
    setwd("C:/dev/r-course/course-2/project")

####install missing packages and reference
    list.of.packages <- c("dplyr", "tidyr", "knitr", "markdown")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
    if (length(new.packages))
        install.packages(new.packages)
    sapply(new.packages, require, character.only = TRUE, quietly = FALSE)

####set working directory
    path <- getwd()
    path
    data_folder <- file.path(path, "UCI HAR Dataset") # may not be created yet

####create results folder for output if required
    results_folder <- "results"
    if (!file.exists(results_folder)) {
        print("create results folder")
        dir.create(results_folder)
    }

##----------------------   functions start ----------------------
 
####download zip and extract to folder
    download_project_files <- function() {
        url <- "https://d396qusza40orc.cloudfront.net/extract_data%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        filename <- "Dataset.zip"
        if (!file.exists(path)) {
            dir.create(path)
        }
        zip_file <- file.path(path, filename)
        download.file(url, zip_file)
        unzip(zip_file, exdir = path)
    }

####read extract and convert it to a data.frame
    get_tables <- function(filename, cols = NULL) {
        print(paste("Getting table:", filename))
        f <- paste(data_folder, filename, sep = "/")
        data <- data.frame()
        if (is.null(cols)) {
            data <- read.table(f, sep = "", stringsAsFactors = F)
        } else {
            data <- read.table(f, sep = "", stringsAsFactors = F, col.names = cols)
        }
        data
    }

####for the type of data (test/test) read the relevant columns in each file per folder  
    extract_data <- function(type, features) {
        print(paste("Getting data", type))
        subject_data <- get_tables(paste(type, "/", "subject_", type, ".txt", sep = ""), "subject")
        y_data <- get_tables(paste(type, "/", "y_", type, ".txt", sep = ""), "activity")
        x_data <- get_tables(paste(type, "/", "X_", type, ".txt", sep = ""), features$V2)
        return(cbind(subject_data, y_data, x_data))
    }

####save to results folder ./results
    save_results <- function(data, name) {
        print(paste("saving results", name))
        file <- paste(results_folder, "/", name, ".csv", sep = "")
        write.csv(data, file)
    }

##----------------------   functions end ----------------------

####download if not already downloaded
    if (!file.exists("./UCI HAR Dataset"))
        download_project_files()


# Step 1 - Merges the training and the test sets to create one data set.


####metadata
    features <- get_tables("features.txt")
    activity_labels <- get_tables("activity_labels.txt")

####training data
    test <- extract_data("test", features)
    train <- extract_data("train", features)
####merge both train and test data
    combined_set <- rbind(train, test)
    combined_set


# Step 2 - Extracts only the measurements on the mean and standard deviation for each measurement.


    clean_set <- combined_set[, grepl("[Mm]ean|std|subject|activity", names(combined_set)) & !grepl("meanFreq", names(combined_set))]


# Step 3 - Uses descriptive activity names to name the activities in the data set


    clean_names <- tolower(names(clean_set))
    clean_names <- sub("^t", "time", clean_names)
    clean_names <- sub("^f", "frequency", clean_names)
    clean_names <- gsub("\\.", "", clean_names)
    clean_names <- sub("acc", "acceleration", clean_names)
    clean_names <- sub("mag", "magnitude", clean_names)
    clean_names <- sub("std", "standarddeviation", clean_names)
    clean_names <- sub("gyro", "gyroscope", clean_names)
    clean_names <- sub("tbody", "timebody", clean_names)


# Step 4 - Appropriately labels the data set with descriptive variable names. 


    names(clean_set) <- clean_names
    save_results(clean_set, "tidy_fitness_data")


# Step 5 -  Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


####activity and subject are the first 2 columns
    tidy_set <- aggregate(clean_set[, 3:75], list(subject = clean_set$subject, activity = clean_set$activity), mean, use.names = TRUE)
 
    save_results(tidy_set, "tidy_fitness_data_averages_per_subject_activity")

####output files required for assessment results.txt
    file <- paste(results_folder, "/", "tidy_set", ".txt", sep = "")
    write.table(tidy_set, file, row.names = FALSE)
####output codebook.md
    library(knitr)
    library(markdown)
    rmd <- file.path(getwd(), "run_analysis.r")
    knit(rmd, output = "codebook.md", encoding = "ISO8859-1", quiet = FALSE)
    markdownToHTML("codebook.md", "codebook.html")
