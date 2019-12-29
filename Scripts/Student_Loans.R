library(raster)
library(tidyverse)
library(tidyr)

#tidytuesday dataset from Dept of Education (https://github.com/jwatzek/tidytuesday/blob/master/scripts/2019-48_loans.R)
data <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

#mutating data to be summarized by each quarter for each year 
loans = data %>% 
  mutate_at(vars(starting:wage_garnishments), function(.) ./1e9) %>% #mutates variables from starting to wage, moving decimal point
  mutate_at(vars(starting:wage_garnishments), replace_na, 0) %>% 
  mutate(inventory = starting + added, yearQ = str_c('20', year, ' Q', quarter)) %>% #creates new column with year and quarter together
  pivot_longer(total:inventory, names_to = 'method', values_to = 'amount_B') %>% #this function creates rows for each of the loan types (reformtes so each is a row) based on the name of coloumn
  mutate(method = str_to_title(str_replace_all(method, '_', ' '))) %>% #removes underscore
  group_by(yearQ, year, method) %>%  #groups information for each year and method type to organize data
  summarise(amount_B = sum(amount_B)) #cominbes together rows of similar year and method by amount

d3b = filter(loans, !(method %in% c('Inventory', 'Total'))) #removes inventory and total from table

plot <- ggplot(d3b, aes(yearQ, amount_B, fill = year, group = method)) + #amount of loan by each quarter 
  geom_bar(fill = 'white', stat = 'identity', width = .8) + #sets up white bars based on total value and fills them white
  geom_bar(aes(alpha = method), stat = 'identity', col = 'black', width = .8) + #adds stacked bars for amount of each method
  scale_fill_viridis_c() #sets colors to this palette automatically
plot

Total = filter(loans, (method %in% c('Inventory')))

p1 = ggplot(Total, aes(yearQ, amount_B, group =1)) + 
  geom_line()+ geom_point(aes(fill = year), shape = 21, size = 3, show.legend = F) 
  
p1
plot
