library(data.table)
library(reshape2)
#Class Project
#Merges the training and the test sets to create one data set.
#Extracts only the measurements on the mean and standard deviation for each measurement. 
#Uses descriptive activity names to name the activities in the data set
#Appropriately labels the data set with descriptive variable names. 
#From the data set in step 4, creates a second, independent tidy data set with the 
#average of each variable for each activity and each subject.

## Step-by-step analysis of the data
##1.download data from web
url <-"http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "dataset.zip")

##2. unzip the data to work directory
unzip("dataset.zip")

##2.2 set the file path to the data file
path <-getwd()
pathIn <- file.path(path, "UCI HAR Dataset") 

##3. read the feature and activity lable tables
dtActivityTrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
dtActivityTest <- fread(file.path(pathIn, "test", "Y_test.txt"))

##4. read data set from training and test
dtSubjectTrain <- fread(file.path(pathIn, "train", "subject_train.txt"))
dtSubjectTest <- fread(file.path(pathIn, "test", "subject_test.txt"))

fileToDataTable <- function(f) {
  df <- read.table(f)
  dt <- data.table(df)
}
dtTrain <- fileToDataTable(file.path(pathIn, "train", "X_train.txt"))
dtTest <- fileToDataTable(file.path(pathIn, "test", "X_test.txt"))

#5 Merge training and test data
dt <- rbind(dtTrain, dtTest)


#6 Change subject and activity col names and merge
dtSubject <- rbind(dtSubjectTest,dtSubjectTrain)
setnames(dtSubject, "V1", "subject")

dtActivity <- rbind(dtActivityTrain, dtActivityTest)
setnames(dtActivity, "V1", "activityNum")
dtSubject <- cbind(dtSubject, dtActivity)

dt <- cbind(dtSubject, dt) #merge data set with activity

#7 set key for data extraction
setkey(dt, subject, activityNum)

#8. Read from feature txt files
dtFeatures <- fread(file.path(pathIn, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))

#9. Subset data with measurements only containing mean and std
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
head(dtFeatures)

#10. Add feature code with V1 ... V9
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]

#11 Subset with feature names on the dt data set
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]

#12. Read the descriptive file
dtActivityNames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

dt <- merge(dt, dtActivityNames, by="activityNum", all.x=TRUE)
setkey(dt, subject, activityNum, activityName)
dt <- data.table(melt(dt, key(dt), variable.name="featureCode"))
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by="featureCode", all.x=TRUE)

#13. Create activity colm with activity and feature names
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

#14. Seperate feature names: referene from chan
grepthis <- function (regex) {
  grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol=nrow(y))
dt$featDomain <- factor(x %*% y, labels=c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol=nrow(y))
dt$featInstrument <- factor(x %*% y, labels=c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol=nrow(y))
dt$featAcceleration <- factor(x %*% y, labels=c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol=nrow(y))
dt$featVariable <- factor(x %*% y, labels=c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels=c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels=c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow=n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol=nrow(y))
dt$featAxis <- factor(x %*% y, labels=c(NA, "X", "Y", "Z"))


#15. Create a data set with the average of each variable for each activity and each subject.

setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]

write.table(dtTidy, "tidy_data.txt")
