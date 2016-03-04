# Clean up the workspace
rm(list=ls())

#################################################################################
## Download data
#################################################################################

# # Download zipped Data into /data folder
# if(!file.exists("./data")){dir.create("./data")}
# fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download.file(fileUrl, destfile="./data/Dataset.zip", method="libcurl")
# 
# # unzip the Dataset.zip file
# unzip(zipfile="./data/Dataset.zip", exdir="./data")
# # files are unzipped to the /data/UCI HAR Dataset file folder

#################################################################################
## Load Train and Test data into variables
#################################################################################

path_dataset <- file.path("./data" , "UCI HAR Dataset")


#load Activity information (Y_train and Y_test))
data_Activity_Train <- read.table(file.path(path_dataset, "train", "Y_train.txt"), header = FALSE)
data_Activity_Test  <- read.table(file.path(path_dataset, "test" , "Y_test.txt" ), header = FALSE)

activity_Labels <- read.table(file.path(path_dataset, "activity_labels.txt"), header = FALSE)

#load Subject information (subject_train and subject_test)
data_Subject_Train <- read.table(file.path(path_dataset, "train", "subject_train.txt"), header = FALSE)
data_Subject_Test  <- read.table(file.path(path_dataset, "test" , "subject_test.txt"), header = FALSE)


#load Features information (X_train and X_test)
data_Features_Train <- read.table(file.path(path_dataset, "train", "X_train.txt"), header = FALSE)
data_Features_Test  <- read.table(file.path(path_dataset, "test" , "X_test.txt" ), header = FALSE)


#################################################################################
## Create one R script called run_analysis.R that does the following
#################################################################################

# Deliverable 1 - Merges the training and the test sets to create one data set.

  #Combine data by rows using rbind
  data_Subject <- rbind(data_Subject_Train, data_Subject_Test)
  data_Activity <- rbind(data_Activity_Train, data_Activity_Test)
  data_Features <- rbind(data_Features_Train, data_Features_Test)

  #Name variables with labels
  data_Features_Names <- read.table(file.path(path_dataset, "features.txt"), head=FALSE)
  
  
  
  names(data_Subject) <- c("subject")
  names(data_Activity) <- c("activity")
  names(data_Features) <- data_Features_Names$V2
  names(activity_Labels) <- c('activity_id', 'activity_type') 
  
  #Merge into one data set using cbind
  data_ToMerge <- cbind(data_Subject, data_Activity)
  data_Merged <- cbind(data_Features, data_ToMerge)


# Deliverable 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
  
  #get the subset of Features Names that contain mean() or std()
  subset_data_Features_Names<-data_Features_Names$V2[grep("mean\\(\\)|std\\(\\)", data_Features_Names$V2)]
  
  #
  select_Names<-c(as.character(subset_data_Features_Names), "subject", "activity" )
  Data<-subset(data_Merged,select=select_Names)
  
# Deliverable 3 - Uses descriptive activity names to name the activities in the data set
  
  descriptive_Data = merge(Data, activity_Labels, by.x='activity', by.y = 'activity_id', all.x=TRUE);
  
# Deliverable 4 - Appropriately labels the data set with descriptive variable names.
  names(descriptive_Data)<-gsub("Acc", "Accelerometer", names(descriptive_Data))
  names(descriptive_Data)<-gsub("BodyBody", "Body", names(descriptive_Data))
  names(descriptive_Data)<-gsub("^f", "frequency", names(descriptive_Data))
  names(descriptive_Data)<-gsub("Gyro", "Gyroscope", names(descriptive_Data))
  names(descriptive_Data)<-gsub("Mag", "Magnitude", names(descriptive_Data))
  names(descriptive_Data)<-gsub("^t", "time", names(descriptive_Data))
  
  
# Deliverable 5 - From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

  #Remove the activity column
  descriptive_Data  = descriptive_Data[,names(descriptive_Data) != 'activity'];
  
  library(plyr);
  tidy_Data <- aggregate(. ~subject + activity_type, descriptive_Data, mean)
  tidy_Data<-tidy_Data[order(tidy_Data$subject,tidy_Data$activity_type),]
  write.table(tidy_Data, file = "tidy_Data.txt", row.name=FALSE, sep='\t')  
  


  