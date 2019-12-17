# Predictive Analytics
# Loading Sentiment Package
library(sentimentr)


# Loading in the csv file

rusdata <- read.csv("C:\\Users\\alanj\\Downloads\\rusdata_new_long.csv", stringsAsFactors=FALSE)
dim(rusdata)

# Removing parameters that wouldn't be known before the campaign runs

rusShort <- rusdata[,c(1,3,8,9,12,14,19:26,29:31,33:ncol(rusdata))]


##################################################################################################################################################
# Creating a new sentiment column and populating it with data

rusShort$sentiment <- rep(NA, nrow(rusShort))

for (i in 1:nrow(rusShort)){
	rusShort$sentiment[i] <- as.data.frame(sentiment_by(rusShort$text[i],by=NULL))[1,4]
}
	
##################################################################################################################################################
# Creating a variable to flag ads that mention Trump

rusShort$Trump_O_Meter <- rep(NA, nrow(rusShort))

for (i in 1:nrow(rusShort)){
	if (grepl('Trump', rusShort$text[i])==TRUE|grepl('TRUMP', rusShort$text[i])==TRUE|grepl('trump', rusShort$text[i])==TRUE|grepl('Donald', rusShort$text[i])==TRUE
|grepl('DONALD', rusShort$text[i])==TRUE){
		rusShort$Trump_O_Meter[i] <- 1
	}else{ 
		rusShort$Trump_O_Meter[i] <- 0
	}
}

##################################################################################################################################################
# Creating a variable that shows the ratio of capital letters vs the length of the string 

rusShort$capRatio <- rep(NA, nrow(rusShort))


for (i in 1:nrow(rusShort)){
	rusShort$capRatio[i] <- sapply(regmatches(rusShort[i,4], gregexpr("[A-Z]", rusShort[i,4], perl=TRUE)), length)/nchar(rusShort[i,4])
}

##################################################################################################################################################
# Creating a variable that flags ads with exclamation points

rusShort$exclamation <- rep(NA, nrow(rusShort))

for (i in 1:nrow(rusShort)){
	if (grepl('!', rusShort$text[i])==TRUE){
		rusShort$exclamation[i] <- 1
	}else{ 
		rusShort$exclamation[i] <- 0
	}
}


###############################################################################################################################################################################################
###############################################################################################################################################################################################
# Clustering dummy targets into categories found through DataRobot Word Cloud
columnNames <- colnames(rusShort)
# save all column names into a vector

# creating the first cluster for police targets
policeKluster <- rep(NA, ncol(rusShort))


for (i in 1:ncol(rusShort)){
	if (grepl('police', columnNames[i])==TRUE | grepl('officer', columnNames[i])==TRUE| 
		grepl('Police', columnNames[i])==TRUE|grepl('POLICE', columnNames[i])==TRUE){
	policeKluster[i]<- i
	}
}
##################################################################################################################################################
# Creating the second cluster for african american targets
	
blackKluster <- rep(NA, ncol(rusShort))


for (i in 1:ncol(rusShort)){
	if (grepl('black', columnNames[i])==TRUE | grepl('Black', columnNames[i])==TRUE| 
		grepl('BLACK', columnNames[i])==TRUE|grepl('african', columnNames[i])==TRUE|grepl('African', columnNames[i])==TRUE){
	blackKluster[i]<- i
	}
}
##################################################################################################################################################
# Creating a cluster for mexican targets

mexicanKluster <- rep(NA, ncol(rusShort))


for (i in 1:ncol(rusShort)){
	if (grepl('mexi', columnNames[i])==TRUE | grepl('MEXICAN', columnNames[i])==TRUE| 
		grepl('Mexico', columnNames[i])==TRUE|grepl('MEXICO', columnNames[i])==TRUE|grepl('LATINO', columnNames[i])==TRUE){
	mexicanKluster [i]<- i
	}
}

##################################################################################################################################################
# Creating a cluster for southern/conservative topics as there seems to be a good deal of overlap in the categories

southconservativeKluster <- rep(NA, ncol(rusShort))


for (i in 1:ncol(rusShort)){
	if (grepl('republican', columnNames[i])==TRUE | grepl('conservative', columnNames[i])==TRUE| 
		grepl('right wing', columnNames[i])==TRUE|grepl('south', columnNames[i])==TRUE|grepl('confederate', columnNames[i])==TRUE|grepl('dixie', columnNames[i])==TRUE
		|grepl('Dixie', columnNames[i])==TRUE|grepl('DIXIE', columnNames[i])==TRUE){
	southconservativeKluster [i]<- i
	}
}
##################################################################################################################################################
# removing NAs to be left with just the column index values

policeKluster <-policeKluster[!is.na(policeKluster)]

blackKluster <-blackKluster[!is.na(blackKluster)]

mexicanKluster <- mexicanKluster[!is.na(mexicanKluster)]

southconservativeKluster <- southconservativeKluster[!is.na(southconservativeKluster)]
##################################################################################################################################################
# Creating new dummy columns in the dataframe

rusShort$policeTarget <- rep(0, nrow(rusShort))
rusShort$blackTarget <- rep(0, nrow(rusShort))
rusShort$mexTarget <- rep(0, nrow(rusShort))
rusShort$scTarget <- rep(0, nrow(rusShort))

##################################################################################################################################################
# Iterating throught the clusters and data frame to populate the new dummy columns

for (i in policeKluster){
	for (L in 1:length(rusShort[,i])){
		if (rusShort[,i][L] == 1){
		rusShort$policeTarget[L] <- 1
		}
	}
} 


for (i in blackKluster){
	for (L in 1:length(rusShort[,i])){
		if (rusShort[,i][L] == 1){
		rusShort$blackTarget[L] <- 1
		}
	}
} 


for (i in mexicanKluster){
	for (L in 1:length(rusShort[,i])){
		if (rusShort[,i][L] == 1){
		rusShort$mexTarget[L] <- 1
		}
	}
} 


for (i in southconservativeKluster){
	for (L in 1:length(rusShort[,i])){
		if (rusShort[,i][L] == 1){
		rusShort$scTarget[L] <- 1
		}
	}
} 
##################################################################################################################################################
# Dropping target columns that are not in the clusters

rusShortKall <- rusShort[, -c(policeKluster, blackKluster,mexicanKluster,southconservativeKluster )]
#rusShortKL <- rusShort[, c(1:17,1434:1441)]

##################################################################################################################################################
#Writing to the CSV to be loaded into DataRobot

write.csv(rusShortKall, "C:\\Users\\alanj\\OneDrive\\Desktop\\Predictive Analytics\\rusShortKl.4.csv")
##################################################################################################################################################
##################################################################################################################################################
# Additional Testing that isn't part of the main project

testData <- rusdata <- read.csv("C:\\Users\\alanj\\OneDrive\\Desktop\\Predictive Analytics\\rusShortKl.csv", stringsAsFactors=FALSE)

testData$trumpInAd <- rep(NA, nrow(testData))

for (i in 1:nrow(testData)){
	if (grepl('Trump', testData$text[i])==TRUE|grepl('TRUMP', testData$text[i])==TRUE|grepl('trump', testData$text[i])==TRUE|grepl('Donald', testData$text[i])==TRUE
|grepl('DONALD', testData$text[i])==TRUE){
		testData$Trump_O_Meter[i] <- 1
	}else{ 
		testData$Trump_O_Meter[i] <- 0
	}
}




trumpdata <- testData[testData$Trump_O_Meter==1,]


write.csv(rusShortKL, "C:\\Users\\alanj\\OneDrive\\Desktop\\Predictive Analytics\\trumpData.csv")











