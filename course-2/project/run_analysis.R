packages <- c("data.table", "reshape2")
sapply(packages, require, character.only = TRUE, quietly = TRUE)

path <- getwd()
path


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

downloadProjectFile()

# 1.Merges the training and the test sets to create one data set.
# read in all the files
pathIn <- file.path(path, "UCI HAR Dataset")
list.files(pathIn, recursive = TRUE)

# subjects
dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(pathIn, "test", "subject_test.txt"))
dtSubject <- rbind(dtSubjectTrain, dtSubjectTest) #merge
setnames(dtSubject, "V1", "subject")

# activity
dtActivitySet <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtTestActivitySet <- fread(file.path(pathIn, "test", "Y_test.txt"))
dtActivity <- rbind(dtActivitySet, dtTestActivitySet)
setnames(dtActivity, "V1", "activity")

#training
dtTrainingSet <- fread(file.path(pathIn, "train", "X_train.txt"))
dtTestTrainingSet <- fread(file.path(pathIn, "test", "X_test.txt"))
dtTraining <- rbind(dtTrainingSet, dtTestTrainingSet) #merge

# merge all 3 into 1 data table
dt <- cbind(dtSubject,dtTraining,dtActivity)
setkey(dt, subject, activity)
colnames(dt)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("id", "featureName"))
nrow(dtFeatures)
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
nrow(dtFeatures) # ensure rows have been filterd
dtFeatures$featureCode <- dtFeatures[, paste0("V", id)]
head(dtFeatures)
dtFeatures$featureCode



# 3. Uses descriptive activity names to name the activities in the data set
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]