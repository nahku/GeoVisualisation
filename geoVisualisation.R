library(sf)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggmap)
library(osmdata)
library(readxl)
library(DataCombine)
germany_map <- get_map(getbb("Deutschland", base_url = "https://nominatim.openstreetmap.org", featuretype = "country"),maptype = "watercolor")
ggmap(germany_map)
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
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

# Use the function:
germany_map <- ggmap_bbox(germany_map)

ggmap(germany_map)
my_data <- read_excel("binnenwanderung.xlsx")
states <- map$NUTS_NAME
#votes_per_state2 <- data.frame(NUTS_NAME = c(1:16), Data = c(1:16))
votes_per_state2 <- data.frame()
votes_per_state_mean <- data.frame(NUTS_NAME = c(1:16), Mean_Migration = c(1:16))

for(i in 1:length(states)){
  #votes_per_state2$NUTS_NAME[i] = states[i]
  tmp <- which(my_data == states[i], arr.ind = TRUE)[1]
  data = my_data[tmp,]
  data2 = data[-2:-3]
  
  votes_per_state2 <- rbind(votes_per_state2,data2)
  votes_per_state_mean$NUTS_NAME[i] = states[i]
  votes_per_state_mean$Mean_Migration[i] = mean(as.numeric(as.character(data[4:18])))
}
names(votes_per_state2)[names(votes_per_state2) == 'Bundesland'] <- 'NUTS_NAME'

ost = c("Brandenburg","Mecklenburg-Vorpommern","Sachsen","Sachsen-Anhalt","Thüringen")
test = votes_per_state2[ost,]
ost_votes = votes_per_state2[votes_per_state2$Bundesland %in% ost,]
ost_votes = ost_votes[1:5,]
a = colSums(ost_votes[,-1])

`%notin%` <- Negate(`%in%`)

west_votes = votes_per_state2[votes_per_state2$Bundesland %notin% ost,]
west_votes = west_votes[1:11,]
b = colSums(west_votes[,-1])
#Plot new and old States
plot(b,type = "o",col = "blue",xaxt="n",ylim=c(-50000,50000),xlab="Jahr",ylab="Saldo",main="Saldo der Binnenwanderung zwischen neuen und alten Bundesländern",sub="(Berlin wird hier zu den alten Bundesländern gezählt)")
lines(a,type = "o",col = "red")
axis(1,at=1:15,lab=c(2003:2017))
grid(nx = NULL, ny = NULL, col = "black", lty = "dotted")
legend(10,-35000,legend=c("Alte Bundesländer","Neue Bundesländer"),col =c("blue","red"),lty=1,cex=1)

a= which(my_data == "Bayern", arr.ind = TRUE)
#b = a[1]+2
#bayern = my_data[b,]
#bayern$ 
map <- st_read("data/2500_NUTS1.shp", stringsAsFactors=FALSE)
map = st_transform(map,3857)
str(map)
ggplot(map) + geom_sf()
#mydata = read.csv("Studium/Semester 5/Datenanalyse mit R/Abgabe/Stimmenanzahl2.csv", sep=";")
#mydata <- read.csv(file="Studium/Semester 5/Datenanalyse mit R/Abgabe/Stimmenanzahl2.csv", sep=";", colClasses=c(NA, "NULL", "NULL", "NULL"))
#stimmen <- read.csv(file="Studium/Semester 5/Datenanalyse mit R/Abgabe/Stimmenanzahl2.csv", sep=";", colClasses=c("NULL", NA, "NULL", "NULL"))
stimmen <- read.csv(file="btw17_kerg.csv", sep=";")

#umlaute <- function(variable) { 
#  variable <- gsub("ü","?",variable) 
#  variable <- gsub("ß","?",variable) 
#  variable <- gsub("ör","?",variable) 
#  variable <- gsub("ä","?",variable) 
#  return(variable) }
#stimmen$X <- umlaute(stimmen$X)



extracted_votes <- data.frame(Votes = stimmen$X.16, VotesAfD = stimmen$X.44)

extracted_votes$Votes <- as.numeric(as.character(extracted_votes$Votes))
extracted_votes$VotesAfD <- as.numeric(as.character(extracted_votes$VotesAfD))
extracted_votes$PercentageAfD <- (extracted_votes$VotesAfD/extracted_votes$Votes)*100

votes_per_state <- data.frame(NUTS_NAME = c(1:16), Votes_AfD = c(1:16))
for(i in 1:length(states)){
  votes_per_state$NUTS_NAME[i] = states[i]
  votes_per_state$Votes_AfD[i] <- extracted_votes$PercentageAfD[which(stimmen == states[i], arr.ind = TRUE)]
}

arbeitslos <- read.csv(file="Stimmenanzahl2.csv", sep=";", colClasses=c("NULL", "NULL", NA, "NULL"))
einkommen <- read.csv(file="Stimmenanzahl2.csv", sep=";", colClasses=c("NULL", "NULL", "NULL", NA))

#stimmen <- data.frame(NUTS_NAME = map$NUTS_NAME, STIMMENANZAHL = stimmen)
arbeitslos <- data.frame(NUTS_NAME = map$NUTS_NAME, ARBEITSLOSENQUOTE = arbeitslos)
einkommen <- data.frame(NUTS_NAME = map$NUTS_NAME, NETTOEINKOMMEN = einkommen)

#merged_stimmen <- left_join(map,votes_per_state,by="NUTS_NAME")
#plot_stimmen <- ggplot(merged_stimmen) + geom_sf(aes(fill= Votes_AfD))+
#  scale_fill_gradient(low= "#E0E0E0",mid="#FFFFFF", high = "#FF3300")

#binnenwanderung <- left_join(map,votes_per_state_mean,by="NUTS_NAME")
binnenwanderung <- left_join(map,votes_per_state2,by="NUTS_NAME")
#ggplot(binnenwanderung) + geom_sf(aes(fill= binnenwanderung$'2013'))+
 # scale_fill_gradient2(low= "#00008B", mid="white",high = "#FF3300")

ggmap(germany_map) + coord_sf(crs=st_crs(3857)) + geom_sf(data=binnenwanderung, aes(fill= binnenwanderung$'2013'), inherit.aes = FALSE,alpha=0.5)+
  scale_fill_gradient2(low= "#00008B", mid="white",high = "#FF3300") 

plot(st_transform(map,crs = 3857)[1], bgMap = germany_map)
merged_arbeitslos <- left_join(map,arbeitslos,by="NUTS_NAME")
plot_arbeitslos <- ggplot(merged_arbeitslos) + geom_sf(aes(fill= Arbeitslosenquote))+
  scale_fill_gradient(low= "#E0E0E0", high = "#000080")


grid.arrange(plot_stimmen, plot_arbeitslos, ncol=2)

merged_einkommen <- left_join(map,einkommen,by="NUTS_NAME")
ggplot(merged_einkommen) + geom_sf(aes(fill= Nettoeinkommen))+
  scale_fill_gradient(low= "#E0E0E0", high = "#FF3300")

x <- votes_per_state[,2]
y <- einkommen[,2]
cor.test(x,y,method="pearson")

#https://de.statista.com/statistik/daten/studie/5758/umfrage/verfuegbares-nettoeinkommen-nach-bundeslaendern/
#https://de.statista.com/statistik/daten/studie/36651/umfrage/arbeitslosenquote-in-deutschland-nach-bundeslaendern/
#https://www.bundeswahlleiter.de/bundestagswahlen/2017/ergebnisse/bund-99/land-12.html

