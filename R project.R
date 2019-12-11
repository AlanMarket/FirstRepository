calendar <- read.csv("C:\\Users\\alanj\\OneDrive\\Desktop\\AirBnB Project\\denver_calendar.csv")
listings <- read.csv("C:\\Users\\alanj\\OneDrive\\Desktop\\AirBnB Project\\denver_listings.csv")
reviews <- read.csv("C:\\Users\\alanj\\OneDrive\\Desktop\\AirBnB Project\\denver_reviews.csv")
library(ggmap)
library(ggplot2)
head(calendar)

register_google(key = "AIzaSyAS8G7QC_rjApW18LdW6qM_dbM8Upxq2yI", write = TRUE)


calendar$pricenum <- gsub("$", "", calendar$adjusted_price, fixed=TRUE)

calendar$pricenum <- gsub(" ", "", calendar$pricenum, fixed=TRUE)

calendar$pricenum <- gsub(",", "", calendar$pricenum, fixed=TRUE)

calendar$pricenum <- as.numeric(calendar$pricenum)


calendar <- calendar[calendar$pricenum >= 0,]


head(calendar)

new_df <- aggregate(pricenum ~ listing_id, FUN=mean, data=calendar)

new_df$lat <- rep(NA, nrow(new_df))
new_df$long <- rep(NA, nrow(new_df))



head(new_df)

for (i in 1:length(listings$id)){
	if (listings$id[i] == new_df$listing_id[i]){
		new_df$lat[i] <- listings$latitude[i]
		new_df$long[i] <- listings$longitude[i]
	}
}
		
new_df2 <- new_df[new_df$lat>0,]
head(new_df2)

for (i in 1:length(new_df2$listing_id)){
	if (listings$id[i] == new_df2$listing_id[i]){
		new_df2$neighborhood[i] <- listings$neighbourhood_cleansed[i]
	}
}

df3 <- data.frame( listings$id, listings$price, listings$neighbourhood, listings$latitude, listings$longitude)

colnames(df3) = c('id', 'price', 'neighbourhood', 'latitude', 'longitude')

df3$pricenum <- gsub("$", "", df3$price, fixed=TRUE)

df3$pricenum <- gsub(" ", "", df3$pricenum, fixed=TRUE)

df3$pricenum <- gsub(",", "", df3$pricenum, fixed=TRUE)

df3$pricenum <- as.numeric(df3$pricenum)

neighbors <- aggregate(latitude ~ neighbourhood, FUN=mean, data=df3)
neighbors1 <- aggregate(longitude ~ neighbourhood, FUN=mean, data=df3)
neighbors$longitude <- neighbors1$longitude

boulder_map <- get_map(location = "denver Colorado", zoom = 12)

df4 <- df3[(df3$pricenum<2000), ]

ggmap(boulder_map,) +
    geom_point(aes(x = longitude, y = latitude, color = pricenum, size = pricenum), data = df4, alpha = .5)+
	scale_color_gradient(low="blue", high="red")


neighborhoodprice <- aggregate(pricenum ~ neighbourhood, FUN=mean, data=df3)

test <- merge(neighborhoodprice, neighbors, by.x="neighbourhood", by.y="neighbourhood")

ggmap(boulder_map,) +
    geom_point(aes(x = longitude, y = latitude, color = pricenum, size = pricenum, label=neighbourhood), data = test, alpha = .5)+
	scale_color_gradient(low="blue", high="red")

#+geom_text(aes(label=neighbourhood),hjust=0, vjust=0)


test2<- tail((test[order(test$pricenum),]), 10)

barchart <- ggplot(data=test2, aes(x=neighbourhood, y=pricenum)) +
	geom_bar(stat= "identity")

##################
### Question 2 ###
##################
		

library("RSentiment")




##################
### Question 3 ###
##################

## MAKE A NEW DF WITH JUST THE COLUMNS USED IN THE REG BELLOW ##

regdf <- data.frame(listings$price, listings$neighbourhood_cleansed, listings$accommodates, listings$bathrooms, listings$bedrooms)
names(regdf) <- c("price", "neighbourhood_cleansed", "accommodates", "bathrooms", "bedrooms")


regdf$pricenum <- gsub("$", "", regdf$price, fixed=TRUE)

regdf$pricenum <- gsub(" ", "", regdf$pricenum, fixed=TRUE)

regdf$pricenum <- gsub(",", "", regdf$pricenum, fixed=TRUE)

regdf$pricenum <- as.numeric(regdf$pricenum)

regdf$neighbourhood_cleansed <- as.character(regdf$neighbourhood_cleansed)


 summary(lm(pricenum ~ neighbourhood_cleansed + accommodates + bathrooms + bedrooms, data=regdf))












