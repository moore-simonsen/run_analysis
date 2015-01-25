run_analysis <- {
##Reading in test datasets
  DFsubj_te <- read.table("/Users/sarahmoore/Documents/datasciencecoursera/UCI HAR Dataset/test/subject_test.txt", sep = "", header = FALSE)
  DFtest_y <- read.table("/Users/sarahmoore/Documents/datasciencecoursera/UCI HAR Dataset/test/y_test.txt", sep = "", header = FALSE)
  DFtest_x <- read.table("/Users/sarahmoore/Documents/datasciencecoursera/UCI HAR Dataset/test/x_test.txt", sep = "", header = FALSE)

##Reading in train datasets
  DFsubj_tr <- read.table("/Users/sarahmoore/Documents/datasciencecoursera/UCI HAR Dataset/train/subject_train.txt", sep = "", header = FALSE)
  DFtrain_x <- read.table("/Users/sarahmoore/Documents/datasciencecoursera/UCI HAR Dataset/train/x_train.txt", sep = "", header = FALSE)
  DFtrain_y <- read.table("/Users/sarahmoore/Documents/datasciencecoursera/UCI HAR Dataset/train/y_train.txt", sep = "", header = FALSE)

##Naming activity and subject data and binding all test datasets together
    name_DFteSubj <- setnames(DFsubj_te, "V1", "Subjectnames")
    name_DFteY <- setnames(DFtest_y, "V1", "Activitytypes")
    DFtest <- cbind(DFtest_x, name_DFteSubj, name_DFteY)

##Naming activity and subject data and inding training datasets together
    name_DFtrSubj <- setnames(DFsubj_tr, "V1", "Subjectnames")
    name_DFtrY <- setnames(DFtrain_y, "V1", "Activitytypes")
    DFtrain <- cbind(DFtrain_x, name_DFtrSubj, name_DFtrY)

##Merging test and training sets into one data set
  test_train <- rbind(DFtest, DFtrain)

##Extracting the columns on mean and standard deviation for each measurement of originally chosen features, incl. subjects and activities
  subs_tt <- subset(test_train, select = c(1:6, 41:46, 81:86, 121:126, 161:166, 201:202, 214:215, 227:228, 240:241, 253:254, 266:271, 345:350, 424:429, 503:504, 516:517, 529:530, 542:543, 562:563))

##Giving the activitites descriptive names
  read_actFrame <- read.table("/Users/sarahmoore/Documents/datasciencecoursera/UCI HAR Dataset/activity_labels.txt", sep = "", header = FALSE) 
  subs_tt[,68] <- read_actFrame[subs_tt[,68],2]

##Load the reshape package into the R console (library(reshape))
##Labeling the variable names descriptively
  rename_DF <- rename(subs_tt, c("V1"="tBodyAccMeanX", "V2"="tBodyAccMeanY","V3"= "tBodyAccMeanZ","V4"= "tBodyAccStdX","V5"="tBodyAccStdY","V6"="tBodyAccStdZ", "V41" = "tGravityAccMeanX","V42"="tGravityAccMeanY","V43"= "tGravityAccMeanZ","V44"= "tGravityAccStdX","V45"= "tGravityAccStdY","V46"= "tGravityAccStdZ", "V81"= "tBodyAccJerkMeanX","V82"= "tBodyAccJerkMeanY","V83"= "tBodyAccJerkMeanZ","V84"= "tBodyAccJerkStdX","V85"= "tBodyAccJerkStdY","V86"= "tBodyAccJerkStdZ", "V121"= "tBodyGyroMeanX","V122"= "tBodyGyroMeanY","V123"= "tBodyGyroMeanZ","V124"= "tBodyGyroStdX","V125"= "tBodyGyroStdY","V126"= "tBodyGyroStdZ", "V161"= "tBodyGyroJerkMeanX","V162"= "tBodyGyroJerkMeanY","V163"= "tBodyGyroJerkMeanZ","V164"= "tBodyGyroJerkStdX","V165"= "tBodyGyroJerkStdY","V166"= "tBodyGyroJerkStdZ", "V201"= "tBodyAccMagMean","V202"= "tBodyAccMagStd","V214"= "tGravityAccMagMean","V215"= "tGravityAccMagStd","V227"= "tBodyAccJerkMagMean","V228"= "tBodyAccJerkMagStd","V240"= "tBodyGyroMagMean","V241"= "tBodyGyroMagStd","V253"= "tBodyGyroJerkMagMean","V254"= "tBodyGyroJerkMagStd", "V266"= "fBodyAccMeanX","V267"= "fBodyAccMeanY","V268"= "fBodyAccMeanZ","V269"= "fBodyAccStdX","V270"= "fBodyAccStdY","V271"= "fBodyAccStdZ", "V345"= "fBodyAccJerkMeanX","V346"= "fBodyAccJerkMeanY","V347"= "fBodyAccJerkMeanZ","V348"= "fBodyAccJerkStdX","V349"= "fBodyAccJerkStdY","V350"= "fBodyAccJerkStdZ", "V424"= "fBodyGyroMeanX","V425"= "fBodyGyroMeanY","V426"= "fBodyGyroMeanZ","V427"= "fBodyGyroStdX","V428"= "fBodyGyroStdY","V429"= "fBodyGyroStdZ", "V503"= "fBodyAccMagMean","V504"= "fBodyAccMagStd","V516"= "fBodyBodyAccJerkMagMean","V517"= "fBodyBodyAccJerkMagStd","V529"= "fBodyBodyGyroMagMean","V530"= "fBodyBodyGyroMagStd","V542"= "fBodyBodyGyroJerkMagMean","V543"= "fBodyBodyGyroJerkMagStd"))
 
##Load the dplyr package into the R console (library(dplyr))
##Creating 2. Tidy dataset with average of each var. for each act. and each subj. (180 obs. and 68 var.)
  ##1. order/arrange - subj + act
  wrap_DF <- tbl_df(rename_DF) 
  order_DF <- wrap_DF[order(wrap_DF$Subjectnames, wrap_DF$Activitytypes),]

  ##Load the data.table package into the R console - library(data.table)  
  ##Finalyzing tidy data
  order_DT <- data.table(order_DF)
  variables <- head(names(order_DT), -2)
  tidyData2 <- order_DT[, lapply(.SD, mean), .SDcols=variables, by=list(Subjectnames, Activitytypes)]
  
##Convert DF to DT
  tidyData <- as.data.frame.matrix(tidyData2) 
}
  print(tidyData2)