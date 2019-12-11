###################
## Quant Project ##
###################


library(ggmap)
library(ggplot2)
library(gridExtra)

register_google(key = "AIzaSyAS8G7QC_rjApW18LdW6qM_dbM8Upxq2yI", write = TRUE)
#######################################################################################

data <- read.csv("C:/Users/alanj/OneDrive/Desktop/BPD.csv", stringsAsFactors=FALSE)


# grepl("/", data$Incident_ID)

datarm <- data[!grepl("/", data$Incident_ID),]






datarm$hour <- rep(NA, nrow(datarm))

datarm$hour <- sub(".* ", "", datarm[,2])
datarm$hour <- sub(":.*", "", datarm[,6])
######################################################################

colnames(datarm) <- c("incident_id", "date", "casenumber", "location", "problem")

datarm$date <- as.Date(datarm$ResponseDate, "%m/%d/%Y")
datarm<- datarm[2:nrow(datarm),]
datarm<- datarm[1:826,]

#################
# Mapping Stuff #
#################

for (i in 1:nrow(datarm)){
	datarm$location[i] <- paste(datarm$location[i], ", Boulder Colorado", sep="")
}

geo.boulder_locations <- geocode(datarm$location)

geo.boulder_locations <- as.data.frame(geo.boulder_locations)

geodata <- cbind(datarm, geo.boulder_locations)

write.csv(geodata, "C:\\Users\\alanj\\OneDrive\\Desktop\\Quant Project\\geodata.csv")

## Test Map ##
##############

geodata <- read.csv("C:\\Users\\alanj\\OneDrive\\Desktop\\Quant Project\\geodata.csv", stringsAsFactors=FALSE)
geodata <- geodata[-c(1)]
geodata <- geodata[1:826,]


geodata$NE <- rep(NA, nrow(geodata))
geodata$NW <- rep(NA, nrow(geodata))
geodata$SE <- rep(NA, nrow(geodata))
geodata$SW <- rep(NA, nrow(geodata))

for ( i in 1:nrow(geodata)){
	if (geodata$lon[i] > -105.258508 & geodata$lat[i] > 40.02559) {
		geodata$NE[i] <- 1
	}else if (geodata$lon[i] < -105.258508 & geodata$lat[i] > 40.02559) {
		geodata$NW[i] <- 1
	}else if (geodata$lon[i] > -105.258508 & geodata$lat[i] < 40.02559) {
		geodata$SE[i] <- 1
	}else if (geodata$lon[i] < -105.258508 & geodata$lat[i] < 40.02559) {
		geodata$SW[i] <- 1
	}
}

geodata[is.na(geodata)] <- 0

geodata$region <- rep(NA, nrow(geodata))

for (i in 1:nrow(geodata)){
	if (geodata$NW[i] == 1){
		geodata$region[i] <- 'NW'
	}else if (geodata$NE[i] == 1){
		geodata$region[i] <- 'NE'
	}else if (geodata$SW[i] == 1){
		geodata$region[i] <- 'SW'
	}else if (geodata$SE[i] == 1){
		geodata$region[i] <- 'SE'
	}
}

geodata$date <- as.Date(geodata$date)

geodata$day <- weekdays(geodata$date)

FriSatdata <- geodata[(geodata$day == "Friday" | geodata$day == "Thursday"),]

restOfWeek <- geodata[(geodata$day != "Friday" & geodata$day != "Thursday"),]

# Call categories
geodata$fire <- ifelse(geodata$problem =='FISTRB-Structure Fire', 1, 0)
geodata$weapon <- ifelse(geodata$problem == 'WEAPO1B-Weapon I/P',1,0)
geodata$assault <- ifelse(geodata$problem == 'ASSAU1B-Assault I/P',1,0)
geodata$menacing <- ifelse(geodata$problem == 'MENAC1B-Menacing I/P',1,0)
geodata$accident <- ifelse(geodata$problem == 'MAACCB-Major Accident',1,0)
geodata$robbery <- ifelse(geodata$problem == 'ROBBE1B-Robbery I/P',1,0)
geodata$sexassault <- ifelse(geodata$problem == 'SASL1B-Sex Assault I/P',1,0)
geodata$shooting <- ifelse(geodata$problem == 'SHOOTB-Shooting',1,0)
geodata$stabbing <- ifelse(geodata$problem == 'STABB-Stabbing',1,0)
geodata$kidnapping <- ifelse(geodata$problem == 'KIDNA1B-Kidnapping I/P',1,0)
geodata$burglary <- ifelse(geodata$problem == 'BURGL1B - Burglary I/P',1,0)

#Broad Crime Categories
geodata$violent <- rep(NA, nrow(geodata))
geodata$violent <- ifelse((geodata$problem=="ASSAU1B-Assault I/P" | geodata$problem=="SASL1B-Sex Assault I/P" | geodata$problem=="SHOOTB-Shooting" | 
geodata$problem=="STABB-Stabbing" | geodata$problem == 'ROBBE1B-Robbery I/P'|geodata$problem == 'KIDNA1B-Kidnapping I/P'),1,0)



#################################################################################################################################

geodata$nonviolent <- rep(NA, nrow(geodata))
geodata$nonviolent <- ifelse((geodata$problem=="BURGL1B - Burglary I/P" | geodata$problem=="WEAPO1B-Weapon I/P" | geodata$problem == 'MENAC1B-Menacing I/P'),1,0)
#################################################################################################################################


geodata$hour <- as.numeric(datarm$hour[1:826])

#checkdf <- geodata[geodata$violent == 0 & geodata$nonviolent == 0,]


assaultdata <- geodata[(geodata$problem=="ASSAU1B-Assault I/P" | geodata$problem=="SASL1B-Sex Assault I/P" | geodata$problem=="SHOOTB-Shooting" | 
geodata$problem=="STABB-Stabbing" | geodata$problem == 'ROBBE1B-Robbery I/P'|geodata$problem == 'KIDNA1B-Kidnapping I/P'),]
nonviolentdata  <- geodata[(geodata$problem=="BURGL1B - Burglary I/P" | geodata$problem=="WEAPO1B-Weapon I/P" | geodata$problem == 'MENAC1B-Menacing I/P'),]
accidents <- geodata[(geodata$problem == 'MAACCB-Major Accident'),]


boulder_map <- get_map(location = "Boulder Colorado", zoom = 13)

ggmap(boulder_map,) +
    geom_point(aes(x = lon, y = lat, color = region, size= 1), data = geodata, alpha = .5)+xlab("Longitude") + ylab("Latitude")



plot1 <- ggmap(boulder_map,) +
    geom_point(aes(x = lon, y = lat, color = region, size= .05, alpha=.25), data = assaultdata , alpha = .5)+
	ggtitle("Violent Crimes in Boulder")+xlab("Longitude")+ylab("Latitdue") + theme(plot.title = element_text(hjust = .5))



plot2 <- ggmap(boulder_map,) +
    geom_point(aes(x = lon, y = lat, color = region, size= .05), data = nonviolentdata, alpha = .5)+
	ggtitle("Crimes in Boulder")+xlab("Longitude")+ylab("Latitdue")+ theme(plot.title = element_text(hjust = .5))
# grid.arrange(plot1, plot2, ncol=2)




plot3 <- ggmap(boulder_map,) +
    geom_point(aes(x = lon, y = lat, color = problem, size= .05, shape=region), data = FriSatdata, alpha = .5)+
	ggtitle("Crimes on Thursdays and Fridays")+xlab("Longitude")+ylab("Latitdue")+ theme(plot.title = element_text(hjust = .5)) 


plot4 <- ggmap(boulder_map,) +
    geom_point(aes(x = lon, y = lat, color = problem, size= .05, shape=region), data = restOfWeek, alpha = .5)+
	ggtitle("Crimes During the Rest of the Week")+xlab("Longitude")+ylab("Latitdue")+ theme(plot.title = element_text(hjust = .5))

accidentplot <- ggmap(boulder_map,) +
    geom_point(aes(x = lon, y = lat, color = region, size= .05, alpha=.25), data = accidents , alpha = .5)+
	ggtitle("Accidents in Boulder")+xlab("Longitude")+ylab("Latitdue") + theme(plot.title = element_text(hjust = .5))

hourplot <- ggmap(boulder_map,) +
    geom_point(aes(x = lon, y = lat, color = hour, size= .05, shape= region), data = assaultdata, alpha = .5)+
	ggtitle("Crimes in Boulder by Hour")+xlab("Longitude")+ylab("Latitdue")+ theme(plot.title = element_text(hjust = .5))



crimeCounts <- c(sum(geodata$burglary), sum(geodata$fire), sum(geodata$weapon), sum(geodata$assault), sum(geodata$menacing), sum(geodata$accident), sum(geodata$robbery), sum(geodata$sexassault), sum(geodata$shooting), sum(geodata$stabbing), sum(geodata$kidnapping))
callLabels <- c("Buglary", "Fire", "Weapon", "Assault", "Menacing", "Accident", "Robbery", "Sexassault", "Shooting", "Stabbing", "Kidnapping")
pie(crimeCounts, callLabels, main="Boulder Police Calls by Problem")



vcrimesbyregion <- aggregate(violent ~ region, FUN=sum, data=finaldf)
vcrimesbyday <- aggregate(violent ~ day, FUN=sum, data=finaldf)

nvcrimesbyregion <- aggregate(nonviolent ~ region, FUN=sum, data=finaldf)
nvcrimesbyday <- aggregate(nonviolent ~ day, FUN=sum, data=finaldf)


testreg1 <- summary(lm(violent ~ region, data=finaldf))
glmreg1 <- summary(glm(violent ~ region, data=finaldf, family=binomial))

testregNV <- summary(lm(nonviolent ~ region, data=finaldf))
glmregNV <- summary(glm(nonviolent ~ region, data=finaldf, family=binomial))

testreg2 <- summary(lm(violent ~ day, data=finaldf))
glmreg2 <- summary(glm(violent ~ day, data=finaldf, family=binomial))

testreg2NV <- summary(lm(nonviolent ~ day, data=finaldf))
glmreg2NV <- summary(glm(nonviolent ~ day, data=finaldf, family=binomial))

testreg3 <- summary(lm(violent ~ region + day, data=finaldf))
glmreg3 <- summary(glm(violent ~ region + day, data=finaldf, family=binomial))

testreg3NV <- summary(lm(nonviolent ~ region + day, data=finaldf))
glmreg3NV <- summary(glm(nonviolent ~ region + day, data=finaldf, family=binomial))

testregInteract <- summary(lm(violent ~ region*day + region + day, data=geodata))
#########################################################################################################


tempdata <- read.csv("C:/Users/alanj/Downloads/case07.csv", stringsAsFactors=FALSE)
tempdata <- tempdata[tempdata$V4 > -50,]
tempdata$date <- as.Date(with(tempdata, paste(V1, V2, V3, sep="-")), "%Y-%m-%d")

finaldf <- merge(geodata, tempdata, by.x = "date", by.y = "date")


tempreg1 <- summary(lm(violent ~ V4, data = finaldf))
tempglm1 <- summary(glm(violent ~ V4, data = finaldf, family=binomial))
 


tempreg2 <- summary(lm(nonviolent ~ V4, data = finaldf))
tempglm2 <- summary(glm(nonviolent ~ V4, data = finaldf, family=binomial))

accidentreg <- summary(lm(accident ~ V4, data = finaldf))
 
aggregate(violent ~ region, FUN=sum, data=finaldf)
aggregate(nonviolent ~ region, FUN=sum, data=finaldf)


## aggregate daily counts of violent and nonviolent crimes ( or by hour )
##
##
##                    count of crimes = region + day + temp

dailyviolentdata <- aggregate(violent ~ region + day + hour + date, FUN=sum, data=geodata)
dailyviolentdata$hour <- as.character(dailyviolentdata$hour)

dailyNVdata <- aggregate(nonviolent ~ region + day + hour+ date, FUN=sum, data=geodata)
dailyNVdata$hour <- as.character(dailyNVdata$hour)



mrgdaily <- mrgdaily[,-c(5,6)]

mrgdaily$weekday <- weekdays(mrgdaily$date)

tempdata <- tempdata[,c(4,9)]

mrgdaily <- merge(mrgdaily, tempdata, by.x="date", by.y="date")

colnames(mrgdaily) <- c("date","region","hour","violent","nonviolent","weekday")


dreg1 <- summary(lm(violent~ region, data=mrgdaily))

dreg2 <- summary(lm(nonviolent~ region, data=mrgdaily))

dreg3 <- summary(lm(violent~ region + day + hour, data=dailyviolentdata ))

dreg4 <- summary(lm(nonviolent~ region + day + hour, data=dailyNVdata ))

dateviolentdata <- aggregate(violent ~ region + hour + date, FUN=sum, data=geodata)
dateviolentdata$hour <- as.character(dateviolentdata$hour)

dateNVdata <- aggregate(nonviolent ~ region + hour+ date, FUN=sum, data=geodata)
dateNVdata$hour <- as.character(dateNVdata$hour)


dreg5 <- summary(lm(violent~ region + date + hour, data=dateviolentdata ))

dreg6 <- summary(lm(nonviolent~ region + day + hour, data=dateNVdata ))










