#Getting and Cleaning Data Code Book

#Description

This file contains additional information about the variables and the run_analysis.R script. 

#Data

[The data for this pproject can be found here] (https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

#Data Set Information

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' in the UCI HAR Dataset for more details. 

For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

#Section 1: Merge training and test sets to create one data set.

After setting the source directory for the files to be downloaded into, read the relevant data to similarly named variables.

- x_test 
- y_test 
- subject_test 
- x_train 
- y_train 
- subject_train 
- features 
- activity_names 

Start building the data frame by organizing data from different sets into the same column 

- x_column 
- y_column 
- subject_column

Assign names to each variable and then combine all the columns to get one merged data frame.

- DATA

#Section 2: Extract only the measurements on the mean and standard deviation for each measuerment.

Subset the data in the features table to get the columns that only include mean and standard deviation data. Combine with the activity and subject columns in a vector named subset_columns. Subset DATA to only include information for the columns in subset_columns.

#Section 3: Assigns descriptive names to the activities in the data set.

Load the dplyr package and merge DATA with the activity_names table to include descriptive activity names in DATA. The tables are merged using the columns in both tables that holds the integer that refer to a specific activity ( "V1" and "activity").

#Section 4: Assign descriptive variable names to the data set.

Use loops and the gsub function for pattern replacement to give the variables more descriptive names.

#section 5: Creates a second data set called tidydata.txt that has the average of each variable for each activity and each subject.

Use the aggregate and order function to summarize the data set per insturctions, into a new data frame called Tidy Data. Write that data out to a file named tidydata.txt.
