library(tidyverse)
library(maps)
library(viridis)
library(grid)

#Modes of travling biking and walking ACS data
commute_mode <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-05/commute.csv")


commute_mode$state <- recode(commute_mode$state, 
                       "Ca"= "California",
                       "Massachusett" = "Massachusetts")


#summarise data each state for percent biking and walking
commute_summary <- commute_mode %>%
  mutate(state = tolower(state)) %>%
  group_by(state, mode) %>%
  summarise(Percent = mean(percent))

#retrieve state geo data
states_map <- map_data("state")

#filter by Northeastern States
NE_states <- subset(states_map, region %in% c("connecticut", "massachusetts","maine", "new hampshire", 
                                          "new york", "rhode island", "vermont"))
#plot by new england states
commute_summary %>%
  ggplot(aes(map_id = state)) +
  geom_map(aes(fill=Percent), map = NE_states)+ #sets up map
  facet_wrap(vars(mode)) + #displays both bike and walk on same figure
  expand_limits(x= NE_states$long, y=NE_states$lat)+ #sets limits based on lat/long of states file
  coord_map("polyconic") +
  scale_fill_viridis(option = "D") +
  theme_void()+ #gets rid of xy grid
  labs(fill = "Percent of commuters", title= "The Northeast Loves Walking")+
  theme(legend.position="bottom", plot.title = element_text(hjust =0.5), 
        strip.text.x = element_text(size = 12))#changes text of mode

#filter by southeastern states
SE_states <- subset(states_map, region %in% c("alabama", "florida", "georgia", "kentucky", "mississippi", 
                                              "north carolina", "south carolina", "tennessee", "virginia"))

commute_summary %>%
  ggplot(aes(map_id = state)) +
  geom_map(aes(fill=Percent), map = SE_states)+
  facet_wrap(vars(mode)) +
  expand_limits(x= SE_states$long, y=SE_states$lat)+
  coord_map("polyconic") +
  scale_fill_viridis(option = "D") +
  theme_void()+
  labs(fill = "Percent of commuters", title= "The Southeast Hates Walking")+
  theme(legend.position="bottom", plot.title = element_text(hjust =0.5), 
        strip.text.x = element_text(size = 12))


#group dataset by % groups
commute_summary$Group[commute_summary$Percent <= 2] = "1"
commute_summary$Group[commute_summary$Percent >= 2 & commute_summary$Percent <= 3] = "2"
commute_summary$Group[commute_summary$Percent >= 3 & commute_summary$Percent <= 4] = "3"
commute_summary$Group[commute_summary$Percent >= 4 & commute_summary$Percent <= 5] = "4"
commute_summary$Group[commute_summary$Percent >= 5 & commute_summary$Percent <= 6] = "5"
commute_summary$Group[commute_summary$Percent >= 6] = "6"

#change group category to Numeric so map can be colored with group #
commute_summary$Group <- as.numeric(commute_summary$Group)

#seperate 'mode' into bike and walk
commute_Walk <- dplyr::filter(commute_summary, mode == "Walk")

#Map of groups for % walking
commute_Walk %>%
  ggplot(aes(map_id = state)) +
  geom_map(aes(fill=Group), color="black", map = states_map)+
  expand_limits(x= states_map$long, y=states_map$lat)+
  coord_map("polyconic") +
  scale_fill_viridis(option = "D") +
  theme_void()+
  labs(fill = "Percent of commuters", title= "Walking to Work by State")+
  theme(legend.position="bottom", plot.title = element_text(hjust =0.5))

