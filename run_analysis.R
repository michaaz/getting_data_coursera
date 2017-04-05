rm(list = ls())
library("dtplyr")


#    Reading the features names
path = "UCI HAR Dataset"
features <- read.table(file=paste(path,"features.txt", sep="/"))
features <- as.character(features[,2])

#    Reading the activity labels
activity.labels <- read.table(file=paste(path,"activity_labels.txt", sep="/"))
activity.labels <- as.character(activity.labels[,2])

# combining X,y, subject from test and train sets
testpath <- paste(path, "test", sep="/")
trainpath <- paste(path, "train", sep="/")
file.names <- dir(testpath, pattern =".txt")
var.names <- sub("_test.txt","",file.names, fixed=TRUE)

for(i in 1:length(file.names)){
   temp <- var.names[i]
   file.name <- paste(testpath, var.names[i], sep="/")
   file.name <- paste(file.name, "_test.txt", sep="")
   test_data <- read.table(file.name)
   file.name <- paste(trainpath, var.names[i], sep="/")
   file.name <- paste(file.name, "_train.txt", sep="")
   train_data <- read.table(file.name)
   assign(temp, rbind(test_data, train_data))
}
rm(test_data, train_data)

# setting names using features
setnames(X, names(X), features)

# selecting features with mean and std
clean.indices <- grep("mean[(]|std[(]", names(X))
X.clean <- X[,clean.indices]


# modifying the names of features so that they follow naming conventions
new_names<-make.names(names(X.clean))
new_names<-gsub("(\\.){2,}", ".", new_names)
new_names<-gsub("(\\.)$", "", new_names)
setnames(X.clean, names(X.clean), new_names)



# adding activity and subject columns to the data
df <- cbind(X.clean, activity.labels[y[,1]])
setnames(df,names(df)[length(names(df))], "activity")
df<-cbind(df, subject)
setnames(df,names(df)[length(names(df))], "subject")

# grouping by subject and activity and calculating
# the mean of each of the feature for each subject activity pair
clean_df <- df %>%
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# exporting the clean data to a text file "clean.txt" 
write.table(clean_df, "clean.txt", row.names = FALSE)

