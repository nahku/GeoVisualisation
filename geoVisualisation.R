library(sf)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggmap)
library(osmdata)
library(readxl)

#import map
map <- st_read("data/2500_NUTS1.shp", stringsAsFactors=FALSE)

#transform coordinate system to common cordinate system
map = st_transform(map,3857)

#plot the shape-file
ggplot(map) + geom_sf()

#get a map of germany from openstreetmap.org
germany_map <- get_map(getbb("Deutschland", base_url = "https://nominatim.openstreetmap.org", featuretype = "country"),maptype = "toner-background")
#plot openstreetmap
ggmap(germany_map)

domestic_migration_data <- read_excel("data/domestic_migration.xlsx")
#get the german states names from the shape-file map
states <- map$NUTS_NAME
#create a new dataframe which will contain the domestic migration data from 2003-2017
movement_per_state <- data.frame(matrix(ncol = 16, nrow = 16))

#name data frame columns
data_frame_column_names <- c("NUTS_NAME",sprintf("%s",2003:2017))
colnames(movement_per_state) <- data_frame_column_names

#create a new data frame which will contain the mean of domestic migration per state
movement_per_state_mean <- data.frame(NUTS_NAME = c(1:16), Mean_Migration = c(1:16))
#create a new data frame which will contain the sum of domestic migration per state
movement_per_state_sum <- data.frame(NUTS_NAME = c(1:16), Sum_Migration = c(1:16))

#fill the data frames with data
for(i in 1:length(states)){
  #get the index of the row containing the domestic migration data for each state
  index <- which(domestic_migration_data == states[i], arr.ind = TRUE)[1]+2
  #get the relevant data for the indexed state
  row_data = domestic_migration_data[index,-1:-3]
  #concatenate the state name and domestic migration data
  data_frame_row = c(c(states[i]), row_data)
  #name the columns of the row that is to be appended to the data frame
  names(data_frame_row) <- data_frame_column_names
  
  movement_per_state[i,] = data_frame_row
  
  movement_per_state_mean$NUTS_NAME[i] = states[i]
  #add mean domestic migration from 2003 to 2017 per state to data frame
  movement_per_state_mean$Mean_Migration[i] = mean(as.numeric(as.character(row_data)))
  
  movement_per_state_sum$NUTS_NAME[i] = states[i]
  #add sum of domestic migration from 2003 to 2017 per state to data frame
  movement_per_state_sum$Sum_Migration[i] = sum(as.numeric(as.character(row_data)))
}

#show the structure of the created data frames

str(movement_per_state)
str(movement_per_state_mean)
str(movement_per_state_sum)

new_states = c("Brandenburg","Mecklenburg-Vorpommern","Sachsen","Sachsen-Anhalt","Thüringen")
#get domestic migration data only for the new german states
new_states_movement = movement_per_state[movement_per_state$NUTS_NAME %in% new_states,]
#remove douplicates (there are 5 new states excluding Berlin)
new_states_movement = new_states_movement[1:5,]
#comupte sum of domestic migration in the new german states for each year
data_of_new_states = colSums(new_states_movement[,-1])

#create a notin operator
`%notin%` <- Negate(`%in%`)

#get domestic migration data only for the old german states (analogue to the procedure above)
old_states_movement = movement_per_state[movement_per_state$NUTS_NAME %notin% new_states,]
#there are 11 old states including berlin
old_states_movement = old_states_movement[1:11,]
data_of_old_states = colSums(old_states_movement[,-1])

plot(data_of_old_states,type = "o",col = "blue",xaxt="n",ylim=c(-55000,55000),xlab="Jahr",ylab="Saldo [Personen]",main="Saldo der Binnenwanderung zwischen neuen und alten Bundesländern",sub="(Berlin wird hier zu den alten Bundesländern gezählt)")
lines(data_of_new_states,type = "o",col = "red")
axis(1,at=1:15,lab=c(2003:2017))
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")
legend(10,-35000,legend=c("Alte Bundesländer","Neue Bundesländer"),col =c("blue","red"),lty=1,cex=1)

#function to convert a map from the 4326 coordinate reference system (crs) to the 3857 crs
ggmap_bbox <- function(map) {
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))
  
  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
  
  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

#convert the openstreetmap to 3857 crs
#otherwise the plots wouldn't be a precise overlay
germany_map <- ggmap_bbox(germany_map)
#merge shape-file map and data to display by state name
merged_movement_mean <- left_join(map,movement_per_state_mean,by="NUTS_NAME")
ggmap(germany_map) + coord_sf(crs=st_crs(3857)) + geom_sf(data=merged_movement_mean, aes(fill= Mean_Migration), inherit.aes = FALSE,alpha=0.5)+
  scale_fill_gradient2("Anzahl Personen",low= "#CC0033", mid="white",high = "#006633") + ggtitle("Durchschnitt des Saldos der Binnenwanderung in Deutschland")

#merge shape-file map and data to display by state name
merged_movement_sum<- left_join(map,movement_per_state_sum,by="NUTS_NAME")
ggmap(germany_map) + coord_sf(crs=st_crs(3857)) + geom_sf(data=merged_movement_sum, aes(fill= Sum_Migration), inherit.aes = FALSE,alpha=0.7)+
  scale_fill_gradient2("Anzahl Personen",low= "#CC0033", mid="white",high = "#006633") + ggtitle("Summe des Saldos der Binnenwanderung in Deutschland")

#import election data from the german election of 2017
votes <- read.csv(file="data/btw17_kerg.csv", sep=";")
#create a data frame for total votes and AfD votes per state
extracted_votes <- data.frame(Votes = votes$X.16, VotesAfD = votes$X.44)

#convert to numeric type
extracted_votes$Votes <- as.numeric(as.character(extracted_votes$Votes))
extracted_votes$VotesAfD <- as.numeric(as.character(extracted_votes$VotesAfD))

#compute the percentage of AfD votes per state
extracted_votes$PercentageAfD <- (extracted_votes$VotesAfD/extracted_votes$Votes)*100

#create a dataframe containing the percentage of AfD votes per state
votes_per_state <- data.frame(NUTS_NAME = c(1:16), Votes_AfD = c(1:16))
for(i in 1:length(states)){
  votes_per_state$NUTS_NAME[i] = states[i]
  votes_per_state$Votes_AfD[i] <- extracted_votes$PercentageAfD[which(votes == states[i], arr.ind = TRUE)]
}

#import income statistics and create data frame
income <- read.csv(file="data/income.csv", sep=";", colClasses=c("NULL", NA))
income <- data.frame(NUTS_NAME = map$NUTS_NAME, Income = income)

merged_votes <- left_join(map,votes_per_state,by="NUTS_NAME")

merged_income <- left_join(map,income,by="NUTS_NAME")

ggmap(germany_map) + coord_sf(crs=st_crs(3857)) + geom_sf(data=merged_votes, aes(fill= Votes_AfD), inherit.aes = FALSE,alpha=0.7)+
  scale_fill_gradient2("Stimmenanteil der AfD in %",low= "white", high = "#00008B")  + ggtitle("Stimmenanteil der AfD")

ggmap(germany_map) + coord_sf(crs=st_crs(3857)) + geom_sf(data=merged_income, aes(fill= Income), inherit.aes = FALSE,alpha=0.7)+
  scale_fill_gradient2("Haushaltsjahreseinkommen\npro Person in €",low= "white", high = "#00008B") + ggtitle("Durchschnittliches Einkommen")

x <- income[,2]
y <- votes_per_state[,2]
plot(x,y,ylab = "Stimmen für die AFD in %", xlab = "Durchschnittliches Hauhaltsjahreseinkommen pro Person in €",main ="Zusammenhang zwischen Nettohaushaltseinkommen\n und Wahlergebnis der AfD")

cor.test(x,y,method="pearson")

