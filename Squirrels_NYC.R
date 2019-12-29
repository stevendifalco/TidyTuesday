library(tidyverse)
library(spdep)
library(rgdal)
library(KernSmooth)
library(leaflet)
library(raster)
library(htmlwidgets)

nyc_squirrels <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-29/nyc_squirrels.csv")

a <- ggplot(nyc_squirrels, aes(x=eating))+
  geom_bar()
a

##code used to group data into number in each primary fur color 
tt <- nyc_squirrels %>% 
  group_by(unique_squirrel_id) %>% 
  summarise(primary_fur_color = primary_fur_color[1], age = age[1]) %>% 
  group_by(primary_fur_color, age) %>% 
  summarise(count=n()) %>% 
  arrange(count) %>%
  drop_na() %>%
  as.data.frame()


ggplot(tt)+
  geom_bar(aes(x=primary_fur_color, y=count, fill=age), stat="identity")

#location of squirrels
x= nyc_squirrels$long
y= nyc_squirrels$lat

#dataframe using the x,y coordinates of squirrel locationas
map <- data.frame(x, y, eating=nyc_squirrels$eating)

#basic plot of where squirrels were located
plot(x = map$x, y= map$y)

#ggplot adding shapes and colors for false and true
map2 <- ggplot(map, aes(x=x, y=y)) +
  geom_point(aes(shape=eating, color=eating))

map2


##kernel density of squirrels
kernal <- bkde2D(as.matrix(nyc_squirrels[, c("long", "lat")]), bandwidth = c(.0045, .0068), gridsize = c(100,100))

#creating raster for kernel density estimate
sqrl_raster = raster(list(x=kernal$x1,y=kernal$x2,z=kernal$fhat))

#adding projection to raster layer for spatial information
projection(sqrl_raster) <- CRS("+init=epsg:4326")

#pulling kernal density at each point out
raster_data <- sqrl_raster@data@values

#adding NA for lower values so they come up as transparent in display
raster_data[which(raster_data < 250)] <- NA


#coloring raster 
raster_colors <- colorNumeric("YlOrRd", domain = raster_data, na.color = "transparent")

#Visualization of density of squirrels across central park
leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addRasterImage(sqrl_raster,
                 colors = raster_colors,
                 opacity = .8) %>%
  addLegend(pal = raster_colors,
            values = raster_data,
            title = "Squirrel Density") %>%
  setView(lng = mean(nyc_squirrels$long), lat = mean(nyc_squirrels$lat), zoom = 13.5)


#
#filtering squirrels that are feeding only
feeding <- nyc_squirrels %>%
  filter(kuks==TRUE)

kernal <- bkde2D(as.matrix(feeding[, c("long", "lat")]), bandwidth = c(.0045, .0068), gridsize = c(100,100))

#creating raster for kernel density estimate
sqrl_raster = raster(list(x=kernal$x1,y=kernal$x2,z=kernal$fhat))

#adding projection to raster layer for spatial information
projection(sqrl_raster) <- CRS("+init=epsg:4326")

#pulling kernal density at each point out
raster_data <- sqrl_raster@data@values

#adding NA for lower values so they come up as transparent in display
raster_data[which(raster_data < 250)] <- NA


#coloring raster 
raster_colors <- colorNumeric("YlOrRd", domain = raster_data, na.color = "transparent")

#Visualization of density of squirrels across central park
leaflet(options = leafletOptions(zoomControl = TRUE)) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addRasterImage(sqrl_raster,
                 colors = raster_colors,
                 opacity = .8) %>%
  addLegend(pal = raster_colors,
            values = raster_data,
            title = "Squirrels Kuks") %>%
  setView(lng = mean(nyc_squirrels$long), lat = mean(nyc_squirrels$lat), zoom = 13.5)

