#download the data

if (!file.exists("getdata_dataset.zip")){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
    download.file(fileURL, "getdata_dataset.zip")
}
#unzip the data
if (!file.exists("UCI HAR Dataset")) {
    unzip("getdata_dataset.zip")
}

#read features
features <- read.table("uci har dataset/features.txt")
required_features_index <- grep(".*mean.*|.*std.*", features[,2])
required_features <- features[required_features_index,]
required_features[,2] <- gsub("-mean", "Mean", required_features[,2])
required_features[,2] <- gsub("-std", "Std", required_features[,2])
required_features[,2] <- gsub("[()]", "", required_features[,2])

#read test data
test_data <- read.table("uci har dataset/test/x_test.txt")
required_test_data <- test_data[,required_features_index]
colnames(required_test_data) <- required_features[,2]
test_volunteer_id <- read.table("uci har dataset/test/subject_test.txt")
colnames(test_volunteer_id) <- c("volunteer_id")
test_label <- read.table("uci har dataset/test/y_test.txt")
colnames(test_label) <- c("label")
#merge all the three test data
required_test_data <- data.frame(test_volunteer_id, test_label, required_test_data)


#read train data
train_data <- read.table("uci har dataset/train/x_train.txt")
required_train_data <- train_data[,required_features_index]
colnames(required_train_data) <- required_features[,2]
train_volunteer_id <- read.table("uci har dataset/train/subject_train.txt")
colnames(train_volunteer_id) <- c("volunteer_id")
train_label <- read.table("uci har dataset/train/y_train.txt")
colnames(train_label) <- c("label")
#merge all the three train data
required_train_data <- data.frame(train_volunteer_id, train_label, required_train_data)


#merge test and train data
required_data <- rbind(required_test_data,required_train_data)


#read activites
activites <- read.table("uci har dataset/activity_labels.txt")
activites[,2] <- as.character(activites[,2])

x <- function(y){
    for (i in 1:nrow(activites)) {

        if (y==activites[i,1]) {
            return(activites[i,2])
        }
    }
}

#convert lables
required_data[,2] <- sapply(required_data[,2], x)



#create mean data set
library(reshape2)
required_data_melted <- melt(required_data, id = c("volunteer_id", "label"))
required_data_mean <- dcast(required_data_melted, volunteer_id + label ~ variable, mean)



