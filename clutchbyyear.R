library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(nflplotR)
library(dplyr)
#loads up brady, big ben, rivers, brees, mannings brothers drives when losing and less than 2 minutes to go
clutch_year <- load_pbp(1999:2021) %>%
  filter(passer_player_name %in% c('T.Brady','B.Roethlisberger','D.Brees','P.Manning','E.Manning','P.Rivers','A.Rodgers') 
         & game_seconds_remaining <= 120 & wp > 0.05 & wp < 0.95 & posteam_score <= defteam_score & defteam_score - posteam_score <= 8) %>%
  select(passer_player_name, passer_player_id, qb_epa, season, desc) %>%
clutch_yearf <- clutch_year %>% 
  mutate(
    qb_nepa = ifelse(str_detect(desc, "(SPIKED)|(spiked)|(spikes)|(SPIKES)"), NA, qb_epa)) %>%
  filter(!is.na(qb_nepa)) %>%
  group_by(passer_player_id, passer_player_name, season) %>%
  summarise_at(vars(qb_epa), list(mean_epa = mean))%>%
  arrange(-mean_epa)
view(clutch_yearf)
