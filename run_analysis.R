library(dplyr)

# Reading Training Set
x_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subj_train <- read.table("./train/subject_train.txt")

# Reading Test Set
x_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subj_test <- read.table("./test/subject_test.txt")

# Reading features heading
feature_name <- read.table("features.txt")


###############################################################
### 1. Merging training and test set to create one data set ###
###############################################################
x_all <- rbind(x_train, x_test)
y_all <- rbind(y_train, y_test)
subj_all <- rbind(subj_train, subj_test)


#################################################################
### 2. Extracting only the mean and stdev of each measurement ###
#################################################################

# Find mean and stdev features by matching "mean()" and "std()"
mean_feature_idx <- grep("mean\\(\\)", feature_name$V2)
mean_feature <- feature_name$V2[mean_feature_idx]
stdev_feature_idx <- grep("std\\(\\)", feature_name$V2)
stdev_feature <- feature_name$V2[stdev_feature_idx]

# Creating a data frame of all the mean() and std() measurements as features
new_x <- cbind(x_all[,mean_feature_idx], x_all[,stdev_feature_idx])


#################################################################
### 3. Use descriptive activity names to name the activities  ###
#################################################################

# Read activity labels
activity_label <- read.table("activity_labels.txt")

# Merge y_all with activity labels to add descriptive names for each activity 
y_all_label <- join(y_all, activity_label, by="V1", type="left")


############################################################################
### 4. Appropriately label the data set with descriptive variable names  ###
############################################################################

# Give variables of the new data set descriptive variable names
names(new_x) <- c(mean_feature, stdev_feature)

# Give the label data set descriptive names
names(y_all_label) <- c("activity_id", "activity")

# Give the subject data set descriptive names
names(subj_all) <- "subj"


##################################################################
### 5. Create a new dataset with the average of each variable  ###
##################################################################

# Bind subject_id and activity_id to the dataset in step 4
new_x_2 <- cbind(subj_all, y_all_label, new_x)

new_x_2_mean <- new_x_2 %>%
                    group_by(subj, activity_id, activity) %>%
                    summarise_each(funs(mean))
                
write.table(new_x_2_mean, file="tidydata.txt", header=FALSE)







