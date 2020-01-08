#bushfires in Australia Map

library(tidyverse)
library(ggplot2)
library(sf)
library(ggmap)
library(gganimate)

#data imported 
nasa_fire_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/MODIS_C6_Australia_and_New_Zealand_7d.csv')

#clean up data and filter by date
nasa_fire_clean <- nasa_fire_raw %>%
  mutate(id = row_number(), 
         datetime = ymd(paste(acq_date)))

timerange <- nasa_fire_clean %>% pull(datetime) %>% range()

#get map of Australia
myMap <- get_map(location = "Australia", zoom = 4)

#mapping points
ggmap(myMap)+
  geom_point(data = nasa_fire_clean[, c("longitude", "latitude", "acq_date")], mapping = aes(x=longitude, y =latitude, colour= acq_date))


#subset to zoom into NSW
NSW <- ggmap(get_googlemap(center = c(lon =  146.836432, lat = -33.517549),
                    zoom = 6, scale = 2,
                    maptype ='terrain',
                    color = 'color'))

#map of NSW with all fire points
NSW +
  geom_point(data = nasa_fire_clean[, c("longitude", "latitude", "id")], 
             mapping = aes(x=longitude, y =latitude, group= id),
             color = "red",
             shape = ".")
