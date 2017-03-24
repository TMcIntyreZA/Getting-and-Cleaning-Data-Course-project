## NB: dataset url: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
## unzip and copy the following files to working directory prior:
## 'activity_labels.txt'; 'features.txt'; 'subject_test.txt'; 'subject_train.txt';
## 'X_train.txt'; 'y_train.txt'; 'X_test.txt'; 'y_test.txt'

# Reading training and test tables:
library(data.table)
x_train <- read.table("X_train.txt")
y_train <- read.table("y_train.txt")
subject_train <- read.table("subject_train.txt")

x_test <- read.table("X_test.txt")
y_test <- read.table("y_test.txt")
subject_test <- read.table("subject_test.txt")

# Reading additional files (features and activity labels):
features <- read.table("features.txt")
activitylabels = read.table("activity_labels.txt")

## STEP 1 (equivalent to INSTRUCTION 4)##
##Assigning column names
  
colnames(x_train) <- features[,2] 
colnames(y_train) <- "activityID"
colnames(subject_train) <- "ID"

colnames(x_test) <- features[,2] 
colnames(y_test) <- "activityID"
colnames(subject_test) <- "ID"

colnames(activitylabels) <- c('activityID','activityTYPE')

## INSTRUCTION 1 ##
##Merging all data into single dataset called 'alldata'
 
training_full <- cbind(y_train, subject_train, x_train)
test_full <- cbind(y_test, subject_test, x_test)
alldata <- rbind(training_full, test_full)

dim(alldata)

## INSTRUCTION 2 ##
##Extracting only the the mean and standard deviation for each measurement

c_names <- colnames(alldata)

##function to define IDs, mean and standard deviation:
X_SD <- (grepl("activityID" , c_names) | 
          grepl("ID" , c_names) | 
          grepl("mean.." , c_names) | 
          grepl("std.." , c_names) 
  )

## Making subset containing only means and standard deviations
mean_sd <- alldata[ , X_SD == TRUE]
dim(mean_sd)
mean_sd[1,]


## INSTRUCTION 3 ##
## Adding descriptive activity names to name the activities in the data set
summary1 <- merge(mean_sd, activitylabels,
                  by='activityID',
                  all.x=TRUE)
dim(summary1)
summary1[1,]

## INSTRUCTION 5 ##
## Creating a second, independent tidy data set with the average of each variable for each activity and each subject

summary2 <- aggregate(. ~ ID + activityTYPE, data = summary1, mean)

summary2 <- summary2[order(summary2$activityTYPE, summary2$ID),]

write.csv(summary2, "meansbyActivityandSubject.csv", row.names = F)

##########################################################################################################################
