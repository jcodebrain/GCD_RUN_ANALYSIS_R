########################################################################################
# File Name : run_analysis.R
# 
# The goal of the project : Getting,Cleaning the data set, 
# Creating a tidy data set and Uploading it.
#
# This program consists of the five parts(procedures).
#
# 1. Loading dependencies(packages).
# 2. Imports data files related to the project.
# 3. Extracts only the measurements on the mean and standard deviation & Naming them.
# 4. Merges the training and the test data set to create one data set.
# 5. Create a independent tidy data set with the average of each variable 
#    for each activity and each subject.
#
########################################################################################

########################################################################################
#
# Part 1 : Loading dependency packages.
# : data.table , reshapes2
#
########################################################################################

if (!require("data.table")) {
    install.packages("data.table")
    require("data.table")
}

if (!require("reshape2")) {
    install.packages("reshape2")
    require("reshape2")
}

#######################################################################################
#
# Part 2 : Imports data files related to the project
#
########################################################################################

##### Defines Local directories where the files are
data_dir <- "./UCI HAR Dataset"
test_dir <- paste(data_dir, "test", sep="/")
train_dir <- paste(data_dir, "train", sep="/")

##### Imports data files, stripping white
subject_test_data <- read.table(paste(test_dir,"subject_test.txt",sep="/"))
subject_train_data <- read.table(paste(train_dir,"subject_train.txt",sep="/"))

features_name <- read.table(paste(data_dir,"features.txt",sep="/"))[,2]
activity_name <- read.table(paste(data_dir,"activity_labels.txt",sep="/")) [,2]

test_x_data <- read.table(paste(test_dir,"X_test.txt",sep="/"))
train_x_data <- read.table(paste(train_dir,"X_train.txt",sep="/"))

test_y_data <- read.table(paste(test_dir,"Y_test.txt",sep="/"))
train_y_data <- read.table(paste(train_dir,"Y_train.txt",sep="/"))

#######################################################################################
#
# Part 3 : Extracts only the measurements on the mean and standard deviation & Naming them.
#
########################################################################################

##### Naming X datas
names(test_x_data) = features_name
names(train_x_data) = features_name

##### Filtering only mean and std columns
key_features <- grepl("mean|std",features_name)
test_x_data_filtered = test_x_data[,key_features]
train_x_data_filtered = train_x_data[,key_features]

##### Naming Y datas
test_y_data[,2] = activity_name[test_y_data[,1]]
names(test_y_data) = c("ACTIVITY_ID", "ACTIVITY_NAME")
train_y_data[,2] = activity_name[train_y_data[,1]]
names(train_y_data) = c("ACTIVITY_ID", "ACTIVITY_NAME")

##### Naming subject datas
names(subject_test_data) = "SUBJECT_ID"
names(subject_train_data) = "SUBJECT_ID"

##### Combines all columns altogether
#View(test_x_data_filtered)
all_test_data = cbind(as.data.table(subject_test_data),test_y_data,test_x_data_filtered)
all_train_data = cbind(as.data.table(subject_train_data),train_y_data,train_x_data_filtered)

#######################################################################################
#
# Part 4 : Merges the training and the test data set to create one data set.
#
########################################################################################

##### Merging the combined columns 
merged_data = rbind(all_test_data,all_train_data)

##### Writing the merged data to a file
write.table(merged_data, file = paste(data_dir,"merged_data.txt",sep="/"))


#######################################################################################
#
# Part 5 : Create a independent tidy data set with the average of each variable 
#          for each activity and each subject.
#
########################################################################################

##### Melting the data set
labels = c("SUBJECT_ID","ACTIVITY_ID","ACTIVITY_NAME")
molten_data = melt(merged_data, id= labels, measure.vars=setdiff(colnames(merged_data), labels ))

##### Resahping the molten data with aggregation
tidy_molten_data = dcast(molten_data, SUBJECT_ID + ACTIVITY_NAME ~ variable, mean)

##### Writing the reshaped data set to a file
write.table(tidy_molten_data, file = paste(data_dir,"tidy_molten_data.txt",sep="/"))

