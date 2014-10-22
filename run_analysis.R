#[1] - Load libraries
#========================================================================
library(utils)
library(base)
library(dplyr)

#[2] - Read data files
#========================================================================
#Read test data and test label into X.test and y.test
X.test<-read.table("UCI HAR Dataset/test/X_test.txt",header=FALSE)
y.test<-read.table("UCI HAR Dataset/test/y_test.txt",header=FALSE)

#Read training data and training label into X.train and y.train
X.train<-read.table("UCI HAR Dataset/train/X_train.txt",header=FALSE)
y.train<-read.table("UCI HAR Dataset/train/y_train.txt",header=FALSE)

#Get feature names
feaName<-read.table("UCI HAR Dataset/features.txt",col.names=c("Index","FeatureName"),header=FALSE)

#Get activity labels
act_labels<-read.table("UCI HAR Dataset/activity_labels.txt",col.names=c("Index","ActivityLabels"),header=FALSE)

#Get subject index from subject_test & subject_train
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt",header=FALSE)
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt",header=FALSE)

#[3] - Data Manipulation
#========================================================================
#Combine Test and Training data
X<-rbind(X.test,X.train)
#Rename X varialbe names with actual feature names
colnames(X)<-feaName$FeatureName

#Combine Test and Training labels
y<-rbind(y.test,y.train)

#Combine subjects in Test and Training 
subject<-rbind(subject_test,subject_train)

#Get Index for variable names that has a string "mean" or "std"
Index2mean<-grep("mean",feaName$FeatureName)
Index2std<-grep("std",feaName$FeatureName)
Index<-c(Index2mean,Index2std)
Index<-sort(Index)

#Refined dataset
Xy<-X[,Index]

#Combine data Xy and subject
Xy<-cbind(Xy,subject)

#Add to column "subject" and "activity" to data Xy
Len<-dim(Xy)[2]
colnames(Xy)[Len]<-"subject"
Xy<-cbind(Xy,y)
colnames(Xy)[Len+1]<-"activity"
#Activity label values into label names
Xy$activity<- factor(Xy$activity, levels=act_labels$Index, labels=act_labels$ActivityLabels)


#[4] - Create new tidy data
#========================================================================
#Group data by "subject" and "activity"
by_group<-group_by(Xy,subject,activity)
#compute mean for each variable 
Xy_by_group<-summarise_each(by_group,funs(mean))

#Change the name of each variable
varName<-names(Xy_by_group)
varName<-gsub("tB","TimeB",varName)
varName<-gsub("fB","FrequencyB",varName)
varName<-gsub("tG","TimeG",varName)
varName<-gsub("Acc","Acceleration",varName)

varName<-gsub("-meanFreq()","MeanFreq-avg",varName)
varName<-gsub("-mean()","Mean-avg",varName)
varName<-gsub("-std()","Std-avg",varName)

colnames(Xy_by_group)<-varName

#[5] - Write tidy data as txt file
#========================================================================
write.table(Xy_by_group,file = "TidyData.txt",row.name=FALSE)
