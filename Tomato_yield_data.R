#load libraries
library(here)
library(tidyverse)

#read all tomato yield csv file
tomato_df <- read.csv("CA_tomato_yield_by_county.csv", header = TRUE)

#filter for 2014-2024 years
tomato_df_2014to2024 <- tomato_df %>% 
  filter(between(Year,2014,2024))

#create figure
#note this is for all available counties, we may filter out to target
#certain counties
fig1 <- ggplot(tomato_df_2014to2024,
               aes(x = Year,
                   y = Value,
                   color = County))+
  geom_line()+
  labs(title = "Tomato Yields in CA per county from 2014-2024")
fig1

#test commit and push