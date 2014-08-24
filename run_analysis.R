## Set the correct working directory on local computer.  This will be different for each person
## running the code.
setwd("./Documents/Coursera/")

## Dowloads the file set for the assignment into a zip file.
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./dataset.zip", method = "curl")

## Records the date and time the file was downloaded for reference use.
dateDownloaded <- date()
dateDownloaded

## Unzips the downloaded file into the working directory.
unzip("./dataset.zip",)

## This will confirm what was unzipped in the working directory.
list.files()

## This command will list out all of the subfolders that were unzipped.
## The UCI HAR Dataset folder should come from the unzip process.
list.files("./UCI HAR Dataset")

## This is used to create a variable of the complete dataset.
dataset <- paste("./", "UCI HAR Dataset", sep = "/")

## These commands are used to create data tables from the tab delimited text files that were unzipped.
XTrain <- read.table(file = paste(dataset, "/train/", "X_train.txt", sep = ""), header = FALSE)
YTrain <- read.table(file = paste(dataset, "/train/", "y_train.txt", sep = ""), header = FALSE)
STrain <- read.table(file = paste(dataset, "/train/", "subject_train.txt", sep = ""), header = FALSE)

XTest <- read.table(file = paste(dataset, "/test/", "X_test.txt", sep = ""), header = FALSE)
YTest <- read.table(file = paste(dataset, "/test/", "y_test.txt", sep = ""), header = FALSE)
STest <- read.table(file = paste(dataset, "/test/", "subject_test.txt", sep = ""), header = FALSE)

features <- read.table(file = paste(dataset, "features.txt", sep = "/"), header = FALSE)

activity <- read.table(file = paste(dataset, "activity_labels.txt", sep = "/"), header = FALSE)

## Establishes common names for Activity, Y, and Subject data tables.
names(activity) <- c("move_index", "move_label")
names(YTest) <- "move_index"
names(STest) <- "user_name"
names(YTrain) <- "move_index"
names(STrain) <- "user_name"

## Below commands will begin to merge different data tables.
## Since there are common names in some data tables, then we will use the plyr function.
comboTest <- arrange(join(YTest, activity), move_index)
comboTrain <- arrange(join(YTrain, activity), move_index)

DTest <- cbind(STest, XTest, comboTest)

DTrain <- cbind(STrain, XTrain, comboTrain)

## DS is the complete data set with all tables combined.
DS <- rbind(DTest, DTrain)

## Descriptive names for the complete DS.
NameSet <- c("users", "reference", "move", "mean", "std", "mad", "max", "min", "sma", 
             "energy", "iqr", "entropy", "arCoeff", "correlation", "maxInds", 
             "meanFreq", "skewness", "kurtosis", "bandsEnergy", "angle")
names(DS) <- NameSet

## Strips relevant information to determine tidy data set with average of each variable.
select_info <- data.frame(DS$users, DS$move, DS$mean, DS$std)

## Creates an empty Tidy Data Set.
tidy_DS <- NULL

## The following functions will pull average of each variable.
tidy_mean <- ddply(DS, .(users,move), summarize, mean=(mean(DS$mean)))
tidy_std <- ddply(DS, .(users,move), summarize, mean=(mean(DS$std))) [,3]
tidy_mad <- ddply(DS, .(users,move), summarize, mean=(mean(DS$mad))) [,3]
tidy_max <- ddply(DS, .(users,move), summarize, mean=(mean(DS$max))) [,3]
tidy_min <- ddply(DS, .(users,move), summarize, mean=(mean(DS$min))) [,3]
tidy_sma <- ddply(DS, .(users,move), summarize, mean=(mean(DS$sma))) [,3]
tidy_energy <- ddply(DS, .(users,move), summarize, mean=(mean(DS$energy))) [,3]
tidy_iqr <- ddply(DS, .(users,move), summarize, mean=(mean(DS$iqr))) [,3]
tidy_entropy <- ddply(DS, .(users,move), summarize, mean=(mean(DS$entropy))) [,3]
tidy_arCoeff <- ddply(DS, .(users,move), summarize, mean=(mean(DS$arCoeff))) [,3]
tidy_correlation <- ddply(DS, .(users,move), summarize, mean=(mean(DS$correlation))) [,3]
tidy_maxInds <- ddply(DS, .(users,move), summarize, mean=(mean(DS$maxInds))) [,3]
tidy_meanFreq <- ddply(DS, .(users,move), summarize, mean=(mean(DS$meanFreq))) [,3]
tidy_skewness <- ddply(DS, .(users,move), summarize, mean=(mean(DS$skewness))) [,3]
tidy_kurtosis <- ddply(DS, .(users,move), summarize, mean=(mean(DS$kurtosis))) [,3]
tidy_bandsEnergy <- ddply(DS, .(users,move), summarize, mean=(mean(DS$bandsEnergy))) [,3]
tidy_angle <- ddply(DS, .(users,move), summarize, mean=(mean(DS$angle))) [,3]

## The following action combines all of the averages into a new data table.
tidy_ds <- cbind(tidy_mean, tidy_std, tidy_mad, tidy_max, tidy_min, tidy_sma,
                 tidy_energy, tidy_iqr, tidy_entropy, tidy_arCoeff, tidy_correlation,
                 tidy_maxInds, tidy_meanFreq, tidy_skewness, tidy_kurtosis,
                 tidy_bandsEnergy, tidy_angle)

## Removes the repeating mean numbers for all of the observations.
tidy_ds_mean <- tidy_ds[2,]

## Writes a tab delimited text file with the average of all observations.
write.table(tidy_ds_mean, file = "tidyDataSet.txt", row.name = FALSE, sep = "\t")
