---
title: "CodeBook"
author: "MT"
date: "April 5, 2017"
output: html_document



# Features in the original data

Feature Selection 
___________________

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

tBodyAcc-XYZ  
tGravityAcc-XYZ  
tBodyAccJerk-XYZ  
tBodyGyro-XYZ  
tBodyGyroJerk-XYZ  
tBodyAccMag  
tGravityAccMag  
tBodyAccJerkMag  
tBodyGyroMag  
tBodyGyroJerkMag  
fBodyAcc-XYZ    
fBodyAccJerk-XYZ  
fBodyGyro-XYZ  
fBodyAccMag  
fBodyAccJerkMag  
fBodyGyroMag  
fBodyGyroJerkMag  

The set of variables that were estimated from these signals are: 

mean(): Mean value  
std(): Standard deviation  
mad(): Median absolute deviation   
max(): Largest value in array  
min(): Smallest value in array  
sma(): Signal magnitude area  
energy(): Energy measure. Sum of the squares divided by the number of values.   
iqr(): Interquartile range   
entropy(): Signal entropy  
arCoeff(): Autorregresion coefficients with Burg order equal to 4  
correlation(): correlation coefficient between two signals  
maxInds(): index of the frequency component with largest magnitude  
meanFreq(): Weighted average of the frequency components to obtain a mean frequency  
skewness(): skewness of the frequency domain signal   
kurtosis(): kurtosis of the frequency domain signal   
bandsEnergy(): Energy of a frequency interval within the 64 bins of the FFT of each   window.  
angle(): Angle between to vectors.    

Additional vectors obtained by averaging the signals in a signal window sample. These are used on the angle() variable:  

gravityMean  
tBodyAccMean  
tBodyAccJerkMean  
tBodyGyroMean  
tBodyGyroJerkMean  

The complete list of variables of each feature vector is available in *features.txt*

# Features in the clean data set

The clean data set contains the features listed below. When listing *feature.XYZ*, twe mean that three separate columns *feature.X*, *feature.Y*, and *feature.Z* exist in the data. 

`subject`       contains the id (1-30) of the subject for which the measurements were taken

`activity`       contains the astivity for which the measurements were taken, the possible activities are  "WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"


The columns below contain the mean of each of the corresponding measurement from the original data set calculated for each subject/activity pair. Since all the original measurements were standaraized and therefore unit free, the obtained means have no units.


`tBodyAcc.mean.XYZ`  
`tBodyAcc.std.XYZ`  
`tGravityAcc.mean.XYZ`  
`tGravityAcc.std.XYZ`  
`tBodyAccJerk.mean.XYZ`  
`tBodyAccJerk.std.XYZ`  
`tBodyGyro.mean.XYZ`  
`tBodyGyro.std.XYZ`  
`tBodyGyroJerk.mean.XYZ`  
`tBodyGyroJerk.std.XYZ`  
`tBodyAccMag.mean`  
`tBodyAccMag.std`  
`tGravityAccMag.mean`  
`tGravityAccMag.std`  
`tBodyAccJerkMag.mean`  
`tBodyAccJerkMag.std`  
`tBodyGyroMag.mean`  
`tBodyGyroMag.std`  
`tBodyGyroJerkMag.mean`  
`tBodyGyroJerkMag.std`  
`fBodyAcc.mean.XYZ`  
`fBodyAcc.std.XYZ`  
`fBodyAccJerk.mean.XYZ`  
`fBodyAccJerk.std.XYZ`  
`fBodyGyro.mean.XYZ`  
`fBodyGyro.std.XYZ`  
`fBodyAccMag.mean`  
`fBodyAccMag.std`  
`fBodyBodyAccJerkMag.mean`  
`fBodyBodyAccJerkMag.std`  
`fBodyBodyGyroMag.mean`  
`fBodyBodyGyroMag.std`  
`fBodyBodyGyroJerkMag.mean`  
`fBodyBodyGyroJerkMag.std`  