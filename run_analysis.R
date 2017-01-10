dataUrl <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
zipFile <- './data.zip'
extData <- './data'

downloadZip <- function(url, dest){
  if (!file.exists(dest)){
    download.file(url, dest, method="curl")
  }
  else{
    message("File exists")
  }
}

unzipData <- function(zipFile, extData){
  if (!file.exists(extData)){
    unzip(zipFile, exdir=extData)
  }
  else{
    message("directory exists")
  }
  return (list.files(extData, recursive=TRUE))
    
}

# get the dataframe from unzipped files which is stored under the extracted Data(extData).
getDataFrame <- function(extData, name, unzippedFiles, test=FALSE){
  string_test <- "train"
  if (test){
    string_test <- "test"
  }
  
  if (name == "subject"){
    search <- paste("/subject", string_test, sep="_")
  }
  else if (name == "activity"){
    search <- paste("/y", string_test, sep="_")
  }
  else if (name == "features"){
    search <- paste("/X", string_test, sep="_")
  }
  # print(unzippedFiles[grep(search, unzippedFiles)])
  return (read.table(file.path(extData, unzippedFiles[grep(search, unzippedFiles)]), header=FALSE))
}

combine_train_test <- function(train, test){
  return (rbind(train, test))
}

getFeaturesName <- function(extData, unzippedFiles){
  return (read.table(file.path(extData, unzippedFiles[grep("/features.txt", unzippedFiles)]), header=FALSE))
}

getActivityName <- function(extData, unzippedFiles){
  return(read.table(file.path(extData, unzippedFiles[grep("/activity_labels.txt", unzippedFiles)]), header=FALSE))
}
# Download zip files from given url. 
downloadZip(dataUrl, zipFile)

# Unzips downloaded files to extData
unzippedFiles <- unzipData(zipFile, extData)

# get Dataframes for subject, activity and features.
train_subject <- getDataFrame(extData, "subject", unzippedFiles)
test_subject <- getDataFrame(extData, "subject", unzippedFiles, test = TRUE)

train_activity <- getDataFrame(extData, "activity", unzippedFiles)
test_activity <- getDataFrame(extData, "activity", unzippedFiles, test = TRUE)

train_features <- getDataFrame(extData, "features", unzippedFiles)
test_features <- getDataFrame(extData, "features", unzippedFiles, test = TRUE)

# row binding train and test data. to form giant long data.
# 2947 + 7352 = 10299
subject <- combine_train_test(train_subject, test_subject)
activity <- combine_train_test(train_activity, test_activity)
features <- combine_train_test(train_features, test_features)

# setting appropriate column names for the data.
colnames(subject) <- c("subject")
colnames(activity) <- c("activity")
feature_name <- getFeaturesName(extData, unzippedFiles)
colnames(features) <- feature_name$V2

# column bind all the dataframes to get the giant data. 
data <- cbind(features, cbind(subject, activity))

# extracting measurements on the mean and standard deviation.
data <- subset(data, select=c(as.character(feature_name$V2[grep("mean\\(\\)|std\\(\\)", feature_name$V2)])
, "subject", "activity"))


# labelling activity column using activity_labels.txt
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING
activity_name <- getActivityName(extData, unzippedFiles)
data$activity <- factor(data$activity, levels= activity_name$V1, labels = activity_name$V2)






