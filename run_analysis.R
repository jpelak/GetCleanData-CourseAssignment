#
#
# John Pelak - Getting and Cleaning Data - Course Project
#
#

# preconditions: requires dplyr and reshape2 packages to have been installed
#                expects Samsung data file tree in same folder as this script

run_analysis <- function() {
  #
  # Step 0 : load packages, libraries, and tables
  #
  
  # Step 0.1 : load dplyr and reshape2 libraries
  library(dplyr)
  library(reshape2)
  
  # Step 0.1 : load common tables
  activityLabels <- read.table("activity_labels.txt")
  features <- read.table("features.txt")

  # Step 0.2 : load test tables
  subjectTest <- read.table("test/subject_test.txt")
  xTest <- read.table("test/X_test.txt")
  yTest <- read.table("test/y_test.txt")

  # Step 0.3 : load training tables
  subjectTrain <- read.table("train/subject_train.txt")
  xTrain <- read.table("train/X_train.txt")
  yTrain <- read.table("train/y_train.txt")
  
  # Step 0.4 : set column names
  colnames(activityLabels) <- c("activityCode", "activity")
  colnames(subjectTest) <- "subject"
  colnames(subjectTrain) <- "subject"
  colnames(yTest) <- "activityCode"
  colnames(yTrain) <- "activityCode"
  colnames(xTest) <- features[,2]
  colnames(xTrain) <- features[,2]
  
  #
  # Step 1 : merge the training and test sets to create one data set
  #
  
  # Step 1.1 : bind columns for test and training tables
  subjectActivityTest <- cbind(subjectTest, yTest, xTest)
  subjectActivityTrain <- cbind(subjectTrain, yTrain, xTrain)
  
  # Step 1.2 : bing test and training data rows
  subjectActivityData <- rbind(subjectActivityTest, subjectActivityTrain)
  
  #
  # Step 2 : extract only the measurements on the mean and standard deviation for each measurement
  #
  
  # Step 2.1 : get a vector of mean() columns
  meanCols <- grep("mean\\(\\)", features[,2])
  meanCols <- meanCols + 2 ## offset by 2 for subject and activity columns
  
  # Step 2.2 : get a vector of std() columns
  stdCols <- grep("std\\(\\)", features[,2])
  stdCols <- stdCols + 2 ## offset by 2 for subject and activity columns
  
  # Step 2.3 : extract subject, activityCode, mean(), and std() columns
  subjectActivityMeanStdData <- subjectActivityData[, c(1, 2, meanCols, stdCols)]
  
  #
  # Step 3 : use descriptive activity names to name the activites in the data set
  #
  
  # Step 3.1 : convert activityCode column into a factor, using appropriate labels
  activityFactor <- factor(subjectActivityMeanStdData$activityCode,levels=activityLabels[,1],labels=activityLabels[,2],ordered=FALSE)

  # Step 3.2 : bind the factor column
  subjectActivityFactorData <- cbind(activityFactor,subjectActivityMeanStdData)
  
  #
  # Step 4 : appropriately label the data set with descriptive variable names
  #
  
  # Step 4.1 : rename the first column to "activity"
  names(subjectActivityFactorData)[1] <- "activity"
  subjectActivityFactorData <<- subjectActivityFactorData
  
  #
  # Step 5 : create an independent tidy data set with the average of each variable for each activity and each subject
  #
  
  # Step 5.1 : compute averages by subject and activity using the melt function
  averageSubjectActivity <<- melt(subjectActivityFactorData, id=c(1, 2, 3))
  
  # Step 5.2 : write to table
  write.table(averageSubjectActivity, file="averageSubjectActivity.txt", row.name=FALSE)

}