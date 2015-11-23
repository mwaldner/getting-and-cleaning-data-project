##Section 1
## Start by gathering the raw data

if (!file.exists(./"ProjectData"))
{dir.create("./ProjectData")}

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip " 
download.file(fileUrl, destfile = "./ProjectData/RawData.zip", mode = "wb")

## Unzip the file

unzip(zipfile = "./ProjectData/RawData.zip", exdir = "./ProjectData")

## files from the "UCI HAR Dataset"

path <- file.path("./ProjectData", "UCI HAR Dataset")
files <- list.files(path, recursive = TRUE)

#Reads all the data that we need for the final data frame

x_test <- read.table(file.path(path,"test", "X_test.txt"), header=FALSE)
y_test <- read.table(file.path(path,"test", "y_test.txt"), header=FALSE)
subject_test <- read.table(file.path(path,"test", "subject_test.txt"), header=FALSE)
x_train <- read.table(file.path(path,"train", "X_train.txt"), header=FALSE)
y_train <- read.table(file.path(path,"train", "y_train.txt"), header=FALSE)
subject_train <- read.table(file.path(path, "train", "subject_train.txt"), header = FALSE)
features <- read.table(file.path(path,"features.txt"), header = FALSE)

#Start building the data frame by organizing the variables into columns

x_column <- rbind(x_test, x_train)
y_column <- rbind(y_test, y_train)
subject_column <- rbind(subject_test,subject_train)

# Put names to vairables

names(x_column) <- features$V2
names(y_column) <- c("activity")
names(subject_column) <- c("subject")

#Combine columns into data frame

DATA <- cbind(x_column, y_column)
DATA <- cbind(DATA, subject_column)
 
##Section2
#Subset the data to only include mean and standard deviation data

subset_features <- features$V2[grep("mean\\()|std()",features$V2)]
subset_columns <- c(as.character(subset_features),"activity","subject")

DATA <- subset(DATA, select = subset_columns)

##Section 3
## Merge data frame with activity_lables to match specific activities to their corresponding integer code 

library(dplyr)

activity_names <- read.table(file.path(path, "activity_labels.txt"), header = FALSE)
DATA <- merge(DATA, activity_names, by.x = "activity", by.y = "V1" )
DATA <- DATA[,-1]
DATA <- rename(DATA, activity = V2)

##Section4
## Label variables with descriptive names

variables = colnames((DATA))

for (i in 1:length(variables)){
  if(substr(variables[i],1,1) == "t"){
    
    variables[i] = gsub("-mean\\()","Mean Time",variables[i])
    variables[i] = gsub("-std\\()", "standard Deviation Time", variables[i])
    variables[i] = gsub("^t", "", variables[i])
  }
  
  else if(substr(variables[i],1,1) == "f"){
    
    variables[i] = gsub("-mean\\()","Mean Frequency",variables[i])
    variables[i] = gsub("-std\\()", "standard Deviation Frequency", variables[i])
    variables[i] = gsub("^f", "", variables[i])
  }

} 

colnames(DATA) = variables

names(DATA) <- gsub("Acc", " Accelerometer-", names(DATA))
names(DATA) <- gsub("Gyro", " Gyroscope-", names(DATA))
names(DATA) <- gsub("Mag", "Magnitude ", names(DATA))
names(DATA) <- gsub("Jerk", "Jerk ", names(DATA))  
names(DATA) <- gsub("BodyBody", "Body", names(DATA))

##Section 5
##  Create a second independent tidy data set with the average of each variable for each activity and subject_test

TidyData <- aggregate(. ~subject + activity, DATA, mean)
TidyData <- TidyData[order(TidyData$subject,TidyData$activity),]
write.table(TidyData, file = "tidydata.txt",row.name=FALSE)
