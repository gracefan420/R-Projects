
library(ggplot2)

library(ggmap)
library(maps)
library(mapdata)
# Here we get a USA map from maps:
usa <- map_data("usa")
dim(usa)
head(usa)
#Here is the high-res world map centered on the Pacific Ocean from mapdata
w2hr <- map_data("world2Hires")
head(w2hr)
dim(w2hr)

#By default, geom_polygon() draws with no line color, but with a black fill:

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

states <- map_data("state")
west_coast <- subset(states, region %in% c("california", "oregon", "washington"))

ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat), fill = "palegreen", color = "black") 

ggplot(data = west_coast) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") + 
  coord_fixed(1.3)


ca_df <- subset(states, region == "ohio")

counties <- map_data("county")
head(counties)

ca_county <- subset(counties, region == "ohio")
head(ca_county)

#base:  ditch the axes gridlines
ca_base <- ggplot(data = ca_df, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
ca_base + theme_nothing()

#Now plot the county boundaries in white:
ca_base + theme_nothing() + 
  geom_polygon(data = ca_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top



library(stringr)
library(dplyr)

# make a data frame
x <- readLines("data/ca-counties-wikipedia.txt")
pop_and_area <- str_match(x, "^([a-zA-Z ]+)County\t.*\t([0-9,]{2,10})\t([0-9,]{2,10}) sq mi$")[, -1] %>%
  na.omit() %>%
  str_replace_all(",", "") %>% 
  str_trim() %>%
  tolower() %>%
  as.data.frame(stringsAsFactors = FALSE)

# give names and make population and area numeric
names(pop_and_area) <- c("subregion", "population", "area")
pop_and_area$population <- as.numeric(pop_and_area$population)
pop_and_area$area <- as.numeric(pop_and_area$area)

head(pop_and_area)
