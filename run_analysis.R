
## Getting and Cleaning Data Course Project 
## 
## run_analysis.R does the following: 
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
## 3. Uses descriptive activity names to name the activity in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set with the average 
##   of each variable for each activity and each subject.

require(plyr)

## help function to retrieve data
read_dataset <- function(subject_file, x_file, y_file) {
    dir = "UCI\ HAR\ Dataset"
    features = read.table(paste(dir,'features.txt',sep="/"),header=FALSE); 
    subject = read.table(paste(dir,subject_file,sep="/"),header=FALSE); 
    x_train = read.table(paste(dir,x_file,sep="/"),header=FALSE);
    y_train= read.table(paste(dir,y_file,sep="/"),header=FALSE); 
    colnames(subject) = "subject.ID";
    colnames(x_train) = features[,2];
    colnames(y_train) = "activity.ID";
    dataset = cbind(y_train,subject,x_train);
    dataset;
}

##
## 1. Read and merge the training and the test sets to create one data set.
##

training_dataset = read_dataset('train/subject_train.txt',
                              'train/x_train.txt',
                             'train/y_train.txt');

test_dataset = read_dataset('test/subject_test.txt',
                            'test/x_test.txt',
                            'test/y_test.txt');

# Combine training and test data to create a final data set
combined_dataset = rbind(training_dataset,test_dataset);

## 
## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
##
column_names = colnames(combined_dataset);
selector = (grepl("activity|subject",column_names) | 
                grepl("-mean.{2}$",column_names) | 
                grepl("-std.{2}$",column_names));
selected_data = combined_dataset[selector==TRUE];

## 
## 3. Use descriptive activity names to name the activity in the data set
##
activity = read.table(paste("UCI\ HAR\ Dataset",'activity_labels.txt',sep = "/"),header=FALSE);
colnames(activity) = c('activity.ID','activity.Type');
selected_data = merge(selected_data,activity,by='activity.ID',all.x=TRUE);

## 
## 4. Appropriately label the data set with descriptive activity names.
##
column_names = colnames(selected_data);
for (i in 1:length(column_names)) {
    column_names[i] = gsub("\\()","",column_names[i])
    column_names[i] = gsub("-std", "SD", column_names[i])
    column_names[i] = gsub("-mean", "Mean", column_names[i])
}

colnames(selected_data)=column_names

## 
## 5. From the data set in step 4, creates a second, independent tidy data set with 
##    the average of each variable for each activity and each subject.
##
tidy_data = ddply(selected_data, c("subject.ID","activity.Type"), numcolwise(mean))
write.table(tidy_data, file = "tidy_data.txt", row.name=FALSE)
