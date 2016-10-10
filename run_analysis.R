##########################################################################################################

## Coursera Getting and Cleaning Data Course Project

# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##########################################################################################################

# Clean up workspace
rm(list=ls())

# 1. Merge the training and the test sets to create one data set.

# Read in the data from files
features     = read.table('./features.txt',header=FALSE); 
activity_Type = read.table('./activity_labels.txt',header=FALSE); 
subject_Train = read.table('./train/subject_train.txt',header=FALSE); 
x_Train       = read.table('./train/x_train.txt',header=FALSE); 
y_Train       = read.table('./train/y_train.txt',header=FALSE); 
subject_Test = read.table('./test/subject_test.txt',header=FALSE); 
x_Test       = read.table('./test/x_test.txt',header=FALSE); 
y_Test       = read.table('./test/y_test.txt',header=FALSE); 

# Assigin column names to the data imported above
colnames(activity_Type)  = c('activityId','activityType');
colnames(subject_Train)  = "subjectId";
colnames(x_Train)        = features[,2]; 
colnames(y_Train)        = "activityId";
colnames(subject_Test) = "subjectId";
colnames(x_Test)       = features[,2]; 
colnames(y_Test)       = "activityId";

# Create the train set by merging subjectTrain, xTrain, and yTrain
train_Data = cbind(subject_Train, x_Train,y_Train);

# Create the test set by merging the subject_Test, x_Test, and y_Test data
test_Data = cbind(subject_Test,x_Test, y_Test);

# Combine train and test data to create a final data set
Final_Data = rbind(train_Data,test_Data);

# Create a vector for the column names to locate mean and std columns
Names  = colnames(Final_Data); 

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

# Create a ID_Vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
ID_Vector = (grepl("activity..",Names) | grepl("subject..",Names) | grepl("-mean..",Names) & !grepl("-meanFreq..",Names) & !grepl("mean..-",colNames) | grepl("-std..",Names) & !grepl("-std()..-",Names));

# Create a subset of Final_Data table based on the ID_Vector to keep only desired columns
Final_Data = Final_Data[ID_Vector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the Final_Data set created above with the acitivityType table to include descriptive activity names
Final_Data = merge(Final_Data,activity_Type,by='activityId',all.x=TRUE);

# Updating the Names vector to include the new column names after merge
Names  = colnames(Final_Data); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names
for (i in 1:length(Names)) 
{
  Names[i] = gsub("\\()","",Names[i])
  Names[i] = gsub("-std$","StdDev",Names[i])
  Names[i] = gsub("-mean","Mean",Names[i])
  Names[i] = gsub("^(t)","time",Names[i])
  Names[i] = gsub("^(f)","freq",Names[i])
  Names[i] = gsub("([Gg]ravity)","Gravity",Names[i])
  Names[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",Names[i])
  Names[i] = gsub("[Gg]yro","Gyro",Names[i])
  Names[i] = gsub("AccMag","AccMagnitude",Names[i])
  Names[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",Names[i])
  Names[i] = gsub("JerkMag","JerkMagnitude",Names[i])
  Names[i] = gsub("GyroMag","GyroMagnitude",Names[i])
};

# Reassigning the new descriptive column names to the Final_Data set
colnames(Final_Data) = Names;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, NoActivityType without the activityType column
NoActivityType  = Final_Data[,names(Final_Data) != 'activity_Type'];

# Summarizing the NoActivityType table to include just the mean of each variable for each activity and each subject
Complete_Data    = aggregate(NoActivityType[,names(NoActivityType) != c('activityId','subjectId')],by=list(activityId=NoActivityType$activityId,subjectId = NoActivityType$subjectId),mean);

# Merging the Complete_Data with activity_Type to include descriptive activity names
Complete_Data    = merge(Complete_Data,activity_Type,by='activityId',all.x=TRUE);

# Export the Complete_Data set 
write.table(Complete_Data, './Complete_Data.txt',row.names=FALSE);


