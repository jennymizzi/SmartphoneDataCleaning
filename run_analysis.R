
# Download and unzip files
require(dplyr)

if(!file.exists("data")){ #setup directory
  dir.create("data")
}

require(tidyr)

url<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" #file url

download.file(url,destfile = "./data/Dataset.zip")
unzip("./data/Dataset.zip", exdir = "./data")

#get names of all the test files
testfiles1<- list.files("./data/UCI HAR Dataset/test", pattern = ".txt", full.names = TRUE)
testfiles2<- list.files("./data/UCI HAR Dataset/test/Inertial Signals", full.names = TRUE)

#get names of all the train files
trainfiles1<- list.files("./data/UCI HAR Dataset/train", pattern = ".txt", full.names = TRUE)
trainfiles2<- list.files("./data/UCI HAR Dataset/train/Inertial Signals", full.names = TRUE)

featureInfo <- read.table("./data/UCI HAR Dataset/features.txt") # get column header names

testData<- read.table(testfiles1[2]) # read in x test data

y_testData<- read.table(testfiles1[3]) # read in y labels 

testData<- testData %>%
  mutate(labels = y_testData$V1) # add to testData

subjectTest<- read.table(testfiles1[1]) # read in subject_test data 

testData<- testData %>%
  mutate(subject = subjectTest$V1) # add to testData

colnames(testData)<- c(as.character(unlist(featureInfo[,2])),"labels","subject") # assign header names

# repeat for train data
trainData<- read.table(trainfiles1[2]) #data w/ 2947 obs and 561 variables

y_trainData<- read.table(trainfiles1[3])

trainData<- trainData %>%
  mutate(labels = y_trainData$V1)

subjectTrain<- read.table(trainfiles1[1])

trainData<- trainData %>%
  mutate(subject = subjectTrain$V1)

colnames(trainData)<- c(as.character(unlist(featureInfo[,2])),"labels","subject") # assign header names

fullData<- rbind(testData, trainData) #combine train and test data

meanBool<- grepl("mean", colnames(fullData)) # get column names w/ mean
stdBool <- grepl("std", colnames(fullData))  # get column names w/ std

subsetBool<- meanBool | stdBool #make
subsetBool[562:563] <- TRUE #keep labels and subject columns

# looked at variables w/ meanFreq versus just mean. Since the values were different (and not duplicative), I kept both.  The decision to use one verusus the other can be done in exploring data phase
msData<- fullData[,subsetBool] # subset full data based on columsn with only mean and std in column name


activityLabels<- read.table("./data/UCI HAR Dataset/activity_labels.txt") #read in activity labels
msData$labels<- as.factor(msData$labels) #change label values to factor
levels(msData$labels)<- activityLabels$V2 #assign factor levels based on activity_levels txt file

#create new data set
bySubject<- msData %>%
  select(-labels) %>%               
  group_by(subject) %>%             
  summarise_all(mean) %>%
  mutate(subject = paste("Subject",subject, sep = '_'))

bySubject.T<- t(bySubject[,2:ncol(bySubject)]) #transpose the data

colnames(bySubject.T)<- as.matrix(bySubject[,1]) #use original row data as the new columns
bySubject.T<- cbind(rownames(bySubject.T), bySubject.T)


byActivity<- msData %>%
  select(-subject) %>%
  group_by(labels) %>%
  summarise_all(mean) %>%
  mutate(labels = paste("Activity", labels, sep = '_')) 

byActivity.T<- t(byActivity[,2:ncol(byActivity)]) #transpose the data
colnames(byActivity.T)<- as.matrix(byActivity[,1]) #use original row data as the new columns

byActivity.T<- cbind(rownames(byActivity.T), byActivity.T)

final<- as.data.frame(cbind(bySubject.T,byActivity.T)) #combine the two data sets byActivity and bySubject for final table
final<- final %>%
  select(-V32) %>%
  rename(Feature = V1)


write.table(final,"finalTidyDataset",row.name = FALSE) #write final dataset
