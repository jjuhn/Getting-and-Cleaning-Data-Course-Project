Getting and Cleaning Data Course Project
================

``` r
knitr::opts_chunk$set(echo = TRUE)
```

### Introduction

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

<http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>

### Variables

#### Subject

"subject": ID of 1:30 volunteers who will perform activity.

#### Activity

"activity": 1:6 activities. WALKING, WALKING\_UPSTAIRS, WALKING\_DOWNSTAIRS, SITTING, STANDING, LAYING

#### Features

"timeBodyAccelerometer-mean()-X"
"timeBodyAccelerometer-mean()-Y"
"timeBodyAccelerometer-mean()-Z"
"timeBodyAccelerometer-std()-X"
"timeBodyAccelerometer-std()-Y"
"timeBodyAccelerometer-std()-Z"
"timeGravityAccelerometer-mean()-X"
"timeGravityAccelerometer-mean()-Y"
"timeGravityAccelerometer-mean()-Z"
"timeGravityAccelerometer-std()-X"
"timeGravityAccelerometer-std()-Y"
"timeGravityAccelerometer-std()-Z"
"timeBodyAccelerometerJerk-mean()-X"
"timeBodyAccelerometerJerk-mean()-Y"
"timeBodyAccelerometerJerk-mean()-Z"
"timeBodyAccelerometerJerk-std()-X"
"timeBodyAccelerometerJerk-std()-Y"
"timeBodyAccelerometerJerk-std()-Z"
"timeBodyGyroscope-mean()-X"
"timeBodyGyroscope-mean()-Y"
"timeBodyGyroscope-mean()-Z"
"timeBodyGyroscope-std()-X"
"timeBodyGyroscope-std()-Y"
"timeBodyGyroscope-std()-Z"
"timeBodyGyroscopeJerk-mean()-X"
"timeBodyGyroscopeJerk-mean()-Y"
"timeBodyGyroscopeJerk-mean()-Z"
"timeBodyGyroscopeJerk-std()-X"
"timeBodyGyroscopeJerk-std()-Y"
"timeBodyGyroscopeJerk-std()-Z"
"timeBodyAccelerometerMagnitude-mean()"
"timeBodyAccelerometerMagnitude-std()"
"timeGravityAccelerometerMagnitude-mean()"
"timeGravityAccelerometerMagnitude-std()"
"timeBodyAccelerometerJerkMagnitude-mean()"
"timeBodyAccelerometerJerkMagnitude-std()"
"timeBodyGyroscopeMagnitude-mean()"
"timeBodyGyroscopeMagnitude-std()"
"timeBodyGyroscopeJerkMagnitude-mean()"
"timeBodyGyroscopeJerkMagnitude-std()"
"frequencyBodyAccelerometer-mean()-X"
"frequencyBodyAccelerometer-mean()-Y"
"frequencyBodyAccelerometer-mean()-Z"
"frequencyBodyAccelerometer-std()-X"
"frequencyBodyAccelerometer-std()-Y"
"frequencyBodyAccelerometer-std()-Z"
"frequencyBodyAccelerometerJerk-mean()-X"
"frequencyBodyAccelerometerJerk-mean()-Y"
"frequencyBodyAccelerometerJerk-mean()-Z"
"frequencyBodyAccelerometerJerk-std()-X"
"frequencyBodyAccelerometerJerk-std()-Y"
"frequencyBodyAccelerometerJerk-std()-Z"
"frequencyBodyGyroscope-mean()-X"
"frequencyBodyGyroscope-mean()-Y"
"frequencyBodyGyroscope-mean()-Z"
"frequencyBodyGyroscope-std()-X"
"frequencyBodyGyroscope-std()-Y"
"frequencyBodyGyroscope-std()-Z"
"frequencyBodyAccelerometerMagnitude-mean()"
"frequencyBodyAccelerometerMagnitude-std()"
"frequencyBodyAccelerometerJerkMagnitude-mean()" "frequencyBodyAccelerometerJerkMagnitude-std()" "frequencyBodyGyroscopeMagnitude-mean()"
"frequencyBodyGyroscopeMagnitude-std()"
"frequencyBodyGyroscopeJerkMagnitude-mean()"
"frequencyBodyGyroscopeJerkMagnitude-std()"

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals).

These signals were used to estimate variables of the feature vector for each pattern:
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

The set of variables that were estimated from these signals are:

mean(): Mean value std(): Standard deviation

### Structure

str(tidyData)

### Summary

summary(tidyData)
