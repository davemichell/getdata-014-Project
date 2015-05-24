run_analysis <- function(){

## assumes that the working directory has the UCI HAR Dataset as downloaded
## load necessary libraries
    library(plyr)
    library(reshape2)
    
## read in tables that are required for both data sets
    features <- read.table("./UCI HAR Dataset/features.txt")
    activity <- read.table("./UCI HAR Dataset/activity_labels.txt",col.names=c("Activity","Activity.Name"))
    
##load the training data
    trainData <- read.table("./UCI HAR Dataset/train/X_train.txt")
    trainLabel <- read.table("./UCI HAR Dataset/train/y_train.txt", col.names=c("Activity"))
 
    ## add in a Set variable to identify where the data came from 
    trainLabel$Set <- "Train"
    trainFeature <- read.table("./UCI HAR Dataset/train/subject_train.txt", col.names=c("User"))
    
    ##name the columns in the training Data
    names(trainData) <- features$V2 
    
    ## combine the User, Activity, Set and data into 1 table 
    trainData <- cbind(trainFeature,trainLabel,trainData)
    print("Training Data has been loaded.")


##load the test data
    testData <- read.table("./UCI HAR Dataset/test/X_test.txt")
    testLabel <- read.table("./UCI HAR Dataset/test/y_test.txt", col.names=c("Activity"))

    ## add in a Set variable to identify where the data came from 
    testLabel$Set <- "Test" 
    testFeature <- read.table("./UCI HAR Dataset/test/subject_test.txt", col.names=c("User"))
    
    names(testData) <- features$V2 ##name the columns in the test Data
    
    ## combine the User, Activity, Set and data into 1 table
    testData <- cbind(testFeature,testLabel,testData)
    print("Test Data has been loaded.")
    
    ## Put both Training and Test data together
    allData <- rbind(trainData,testData)
    
    
    ## Select only the columns required
    wantedData <- allData[1:3] ## User, Activity and Set
    wantedData2 <- allData[,grep("mean", names(allData))] ## mean columns
    wantedData2 <- select(wantedData2, -contains("Freq")) ## remove meanFreq 
    wantedData3 <- allData[,grep("std", names(allData))] ## std columns
    
    ## put them all together!
    allData <- cbind(wantedData,wantedData2,wantedData3)
    
    ##add in the activity descriptor
    allData <- join(allData,activity, type="left")
    
    ## Melt the data so that it becomes long and narrow
    ## columns 4:69 are all the variables (1:3 are descriptors, as is 70)
    allMelt <- melt(allData,id=c("User","Activity.Name"),measure.vars=4:69)

    ## produce a table summarised by User, then Activity, then all the means
    outputData  <- dcast(xMelt, User + Activity.Name ~ variable, mean)

    ## write the data to a file called Tidy_data.txt
    write.table(outputData, file="Tidy_data.txt", quote=FALSE, sep = ",",row.names=FALSE, col.names=TRUE)

    print("Data has been saved in Tidy_data.txt")
}