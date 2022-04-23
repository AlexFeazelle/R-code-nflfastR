library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(nflplotR)
library(dplyr)
#loads up brady, big ben, rivers, brees, mannings brothers drives when losing and less than 2 minutes to go
tb_12 <- load_pbp(1999:2021) %>%
  # filters out the qb, time with 2 minutes or less, and only when they are down and needd to score
  filter(passer_player_name %in% c('T.Brady','B.Roethlisberger','D.Brees','P.Manning','E.Manning','P.Rivers') 
         & game_seconds_remaining <= 120 & wp > 0.05 & wp < 0.95, qb_spike == 0) %>%
  select(passer_player_name, passer_player_id, qb_epa, season) %>%
  group_by(passer_player_id) %>%
  by(tb_12$qb_epa, tb_12$season) %>%
  arrange(-mean_epa)
gw_g <- ggplot(data = tb_12, aes(x = reorder(passer_player_id,-mean_epa), y = mean_epa)) +
  geom_col(width = 0.5) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    axis.title.x = element_blank(),
    axis.text.x = element_nfl_headshot(size = 3)) +
  labs(
    title = 'EPA on drives with 2 mins or less and TD to win',
    y = 'EPA',
    subtitle = str_c("By. Alex Feazelle"))
ggpreview()
?element_text
  ?axis.text.y
gw_g
ggsave('Game-Winning9.png',bg = "white", width = 14, height = 10, dpi = "retina")
view(tb_12)
