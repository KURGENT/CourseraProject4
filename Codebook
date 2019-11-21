This is Getting and Cleaning Data Assignment to demonstrate skills to create a tidy data set from many files.

#first load necessary packages
library(dplyr)
library(tidyr)
library(stringi)
library(reshape2)

#1.Merges the training and the test sets to create one data set.
#reading all files and check the content

setwd( "C:/Users/kurgent/Documents/coursera/3. Getting and Cleaning Data/Wk 4/HW/UCI HAR Dataset")
list.files(path = "C:/Users/kurgent/Documents/coursera/3. Getting and Cleaning Data/Wk 4/HW/UCI HAR Dataset")
actnames <- read.table("activity_labels.txt",header = F)
feature <- read.table("features.txt",header = F)
sub_test <- read.table("subject_test.txt",header = F)
sub_train <- read.table("subject_train.txt",header = F)
X_test <- read.table("X_test.txt",header = F)
X_train <- read.table("X_train.txt",header = F)
y_test <- read.table("y_test.txt",header = F)
y_train <- read.table("y_train.txt",header = F)

#put subject label, test label and data(feature file) for Test data
TestData <- cbind(sub_test,y_test,X_test)
TrainData <- cbind(sub_train,y_train,X_train)

#put Train Data and Test Data together
TrainTest <- rbind(TrainData,TestData)

#4.Appropriately labels the data set with descriptive variable names.
#name variables
feature_names <- as.character(feature[,2])
vari_names <- c("subID","TrainTestLabel",feature_names)
names(TrainTest) <- vari_names

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
#select columns including the word mean() and std()
#make characters of variable names including mean() and std()

mean <- grep("mean()",names(TrainTest),value = T, fixed = T)
std <- grep("std()",names(TrainTest),value = T, fixed = T)

#subset TrainTest datafram with only columns with subID, TrainTestLavel and mean() and std()
TrainTest_Selected <- TrainTest[,c("subID","TrainTestLabel",mean,std)]

#3.Uses descriptive activity names to name the activities in the data set
TrainTest_3 <- merge(TrainTest_Selected,actnames,by.x = "TrainTestLabel",by.y = "V1",all.x = T)
names(TrainTest_3) <- sub("V2","Activity",names(TrainTest_3)) 

#5.From the data set in step #4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

##put all the variables to gether to prep for melting

melt <- melt(TrainTest_3,id=c("subID","Activity"),measure.vars=c(mean,std))

#make a table showing means for each variable, each activitity, and each subject
aggregate <- aggregate(melt$value, list(subject = melt$subID,activity = melt$Activity,variable = melt$variable),mean,na.rm=T)

#create txt file from the data.frame "aggregate"
write.table(aggregate, file = "CourseraProject4.txt",sep = "|",row.names = F,col.names = T)
read.table("CourseraProject4.txt",header = FALSE, sep = "|")
