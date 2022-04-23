library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
#loads up Lamar Jackson seasons from 2018-2021
lj_1 <- load_pbp(1999:2021) %>%
  #filters out all passers besides Lamar, NA data entries in air yards and epa,passes less than 20 yards
  filter(passer_player_name == "L.Jackson" & air_yards >= 20 & !is.na(air_yards) & !is.na(epa)) %>%
  ## only shows air_yards, epa, and season in a table
  select(air_yards, epa, season, posteam)
# code to plot the graph
lj_graph <- ggplot(data = lj_1) +
  geom_smooth(mapping = aes(x = air_yards, y = epa, color = "season"), size = 3) +
  facet_wrap(~season, nrow = 3) +
  labs(title = 'Lamar Jackson EPA and Air Yards 2018-2021'
       ,y = 'EPA'
       ,x = 'Air Yards'
  )
# the graph
lj_graph
ggsave('Lamar Air Yard.png', width = 14, height = 10, dpi = "retina")
view(lj_1)
