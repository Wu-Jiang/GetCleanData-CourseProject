#read in the training data
xtrain<-read.table("./UCI HAR Dataset/train/X_train.txt")
ytrain<-read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt")

#use descriptive activity names to 
#name the activities in the entire dataset
#I refer to http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/
#and chose revalue() function
library(plyr)
ytrain<-as.factor(ytrain[,1])
train_activity<-revalue(ytrain,c("1"="WALKING", "2"="WALKING_UPSTAIRS",
                                 "3"="WALKING_DOWNSTAIRS",
                                 "4"="SITTING","5"="STANDING",
                                 "6"="LAYING"))

#combine these three dataset into train dataframe,
#equal with "column-1 subject" is conducting "column-2 activity", which can be 
#explained by observing its "column 3:563 sensor signal data"
train_data<-cbind(subject_train,train_activity,xtrain)
names(train_data)[1:2]<-c("subject","activity")

#read in the test data 
xtest<-read.table("./UCI HAR Dataset/test/X_test.txt")
ytest<-read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt")

#use descriptive activity names to 
#name the activities in the entire dataset
ytest<-as.factor(ytest[,1])
test_activity<-revalue(ytest,c("1"="WALKING", "2"="WALKING_UPSTAIRS",
                               "3"="WALKING_DOWNSTAIRS",
                               "4"="SITTING","5"="STANDING",
                               "6"="LAYING"))
#combine these three dataset into test dataframe
test_data<-cbind(subject_test,test_activity,xtest)
names(test_data)[1:2]<-c("subject","activity")


#combine test data and training data into one giant data
test_train<-rbind(test_data,train_data)

#read in the feature data, which worked as the column names of the dataframe
feature<-read.table("./UCI HAR Dataset/features.txt")
names(test_train)[3:563]<-as.vector(feature[,2])

#at this point, "test_train" dataframe is the complete merging dataset

#Then,extract column names containing the mean and standard deviation
#for the use of grep() function, I refer to the following post on the forum:
#https://class.coursera.org/getdata-002/forum/thread?thread_id=398
patterns<-c("mean\\(\\)","std\\(\\)")
matches<-grep(paste(patterns,collapse="|"),feature$V2,value=TRUE)

#before subsetting, find out the preserved columns
preserved_col<-c("subject", "activity",matches)

#subsetting and form the second data frame
second_df<-test_train[preserved_col]

#to get the average of each variable for each
#activity and each subject
#I refer to the following three posts
#http://tgmstat.wordpress.com/2013/10/31/reshape-and-aggregate-data-with-the-r-package-reshape2/
#https://class.coursera.org/getdata-002/forum/thread?thread_id=146#comment-775
#http://stackoverflow.com/questions/10787640/ddply-summarize-for-repeating-same-statistical-function-across-large-number-of
library(reshape2)
molten<-melt(second_df,id.vars=c("subject","activity"))
tidy<-dcast(molten,subject+activity~variable,mean)

#write the tidy data into the current directory

write.table(tidy,"./tidy_data.txt", row.names=FALSE)
