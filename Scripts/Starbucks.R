#TidyTuesday 2021, Week 52

library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(plotly)
library(htmlwidgets)

#read in data
tuesdata <- tidytuesdayR::tt_load('2021-12-21')

#load data to starbucks variable
starbucks <- tuesdata$starbucks

#group by product name and create a max_cal to calories:
starbucks <- starbucks %>%
  group_by(product_name) %>%
  mutate(max_serv_size = max(serv_size_m_l), max_cal = max(calories))

#create dateframe with simplified starbucks data.
plot_df2 <- starbucks %>%
  filter(serv_size_m_l == max_serv_size & calories == max_cal) %>%
  group_by(product_name) %>%
  slice(1) %>%
  filter(size != "1 scoop")

#create plot for caffeine vs calories
plot = ggplot(plot_df, aes(x=max_cal, y=caffeine_mg))+
  geom_point(aes(color = product_name))+
  labs(x= "Calories", y= "Caffeine(mg)", title= "Calories vs Caffeine content in Starbucks Drinks; Hover over point for details")+
  theme(legend.position = "none")

#create interactive plot
plotly = ggplotly(plot)

#save as html to view widget
saveWidget(as_widget(plotly), "plotly.html",selfcontained = F )
