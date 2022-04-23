library(tidyverse)
library(nflverse)
library(ggimage)
library(ggrepel)
library(editData)


#dataframe for Russell Wilson
pbp_rw <- load_pbp(2012:2021) |>  
  filter(season_type == "REG") |> 
  calculate_player_stats() |> 
  filter(player_name == "R.Wilson") |> 
  mutate(
    pypg = passing_yards / games ,
    avgpyps = passing_yards/ 10 ,
    Total_yards = passing_yards + rushing_yards,
    ptdspy = passing_tds / 10 ,
    Total_tds = passing_tds + rushing_tds
    
  )
#Dataframe for Tom Brady
pbp_tb <- load_pbp(2001:2011) %> |>  
  filter(season_type == "REG") %>%
  calculate_player_stats() %>%
  filter(player_name == "T.Brady") |> 
  mutate(
    pypg = passing_yards / games ,
    avgpyps = passing_yards/ 10 ,
    Total_yards = passing_yards + rushing_yards,
    ptdspy = passing_tds / 10 ,
    Total_tds = passing_tds + rushing_tds
    
  )
#dataframe for Drew Brees
pbp_db <- load_pbp(2002:2011) %>% 
  filter(season_type == "REG") %>%
  calculate_player_stats() %>%
  filter(player_name == "D.Brees") |> 
  mutate(
    pypg = passing_yards / games ,
    avgpyps = passing_yards/ 10 ,
    Total_yards = passing_yards + rushing_yards,
    ptdspy = passing_tds / 10 ,
    Total_tds = passing_tds + rushing_tds
    
  )
#dataframe for Eli Manning
pbp_em <- load_pbp(2005:2014) %>% 
  filter(season_type == "REG") %>%
  calculate_player_stats() %>%
  filter(player_name == "E.Manning") |> 
  mutate(
    pypg = passing_yards / games ,
    avgpyps = passing_yards/ 10 ,
    Total_yards = passing_yards + rushing_yards,
    ptdspy = passing_tds / 10 ,
    Total_tds = passing_tds + rushing_tds
    
  )
#Dataframe for Peyton Manning
pbp_pm <- load_pbp(1999:2008) %>% 
  filter(season_type == "REG") %>%
  calculate_player_stats() %>%
  filter(player_name == "P.Manning") |> 
  mutate(
    pypg = passing_yards / games ,
    avgpyps = passing_yards/ 10 ,
    Total_yards = passing_yards + rushing_yards,
    ptdspy = passing_tds / 10 ,
    Total_tds = passing_tds + rushing_tds
    
  )

#dataframe for Ben Roethlisberger
pbp_br <- load_pbp(2004:2013) %>% 
  filter(season_type == "REG") %>%
  calculate_player_stats() %>%
  filter(player_name == "B.Roethlisberger") |> 
  mutate(
    pypg = passing_yards / games ,
    avgpyps = passing_yards/ 10 ,
    Total_yards = passing_yards + rushing_yards,
    ptdspy = passing_tds / 10 ,
    Total_tds = passing_tds + rushing_tds
    
  )
#dataframe for Phillip Rivers
pbp_pr <- load_pbp(2006:2015) %>% 
  filter(season_type == "REG") %>%
  calculate_player_stats() %>%
  filter(player_name == "P.Rivers") |> 
  mutate(
    pypg = passing_yards / games ,
    avgpyps = passing_yards/ 10 ,
    Total_yards = passing_yards + rushing_yards,
    ptdspy = passing_tds / 10 ,
    Total_tds = passing_tds + rushing_tds
    
  )
#dataframe for Aaron Rodgers
pbp_ar <- load_pbp(2008:2017) %>% 
  filter(season_type == "REG") %>%
  calculate_player_stats() %>%
  filter(player_name == "A.Rodgers") |> 
  mutate(
    pypg = passing_yards / games ,
    avgpyps = passing_yards/ 10 ,
    Total_yards = passing_yards + rushing_yards,
    ptdspy = passing_tds / 10 ,
    Total_tds = passing_tds + rushing_tds
    
  )
#import the csv with additonal accolades data
accolades <- read.csv('/Users/cprcellphonerepair/Desktop/NflfastR/accolades2.csv', header = TRUE) |> 
  arrange(player_name2)
#combines all QB dataframes into one
comp_df <- bind_rows(pbp_ar, pbp_br, pbp_db, pbp_em, pbp_pm, pbp_pr, pbp_rw, pbp_tb)
#combines combined QB dataframe with the accolades dataframe and selects appopriate variables. 
f_df <- cbind(comp_df, accolades) |> 
  select(player_id, player_name, games, attempts, passing_yards, passing_tds, interceptions,
         rushing_yards, rushing_tds, pypg, avgpyps, Total_yards, ptdspy, Total_tds, SB_app,
         SB_win, MVPs, SB_MVPs, Pro_Bowls, All_pro)
headshots <- nflfastR::fast_scraper_roster(2021)

f_df2 <- right_join(headshots, f_df, by = c('gsis_id'='player_id')) 
f_df2n <- read.csv('/Users/cprcellphonerepair/Downloads/qbedit.csv')
f_ddf3 <- select(f_df2n,headshot_url, 
                 player_name, 
                #passing_yards, 
                 #passing_tds, 
                 #interceptions,
                 #rushing_yards, 
                 #rushing_tds, 
                 #pypg, 
                 #avgpyps,
                 #Total_yards, 
                 #ptdspy, 
                 #Total_tds, 
                 SB_app,
                 SB_win, 
                 MVPs, 
                 SB_MVPs, 
                 Pro_Bowls, 
                 All_pro) 

gtf_df <-gt(f_ddf3)
gtf_ddf2 <- gtf_df |> 
  tab_header(
    title = "First 10 Seasons Started Comparison",
    subtitle = "Had to start >=10 games for the season to count"
  ) |> 
gt_img_rows(columns = headshot_url) |> 
  gt_theme_538() |> 
  cols_label(
    headshot_url = "",
    #player_name = "Player Name",
    #passing_yards = "Yards",
    #passing_tds = "TDs",
    #rushing_yards = "Yards",
    #rushing_tds = "TDs",
    #pypg = "Yards/Game",
    #avgpyps = "Yards/Season",
    #Total_yards = "Yards",
    #ptdspy = "TDs/Year",
    #Total_tds = "TDs",
    SB_app = "SB App",
    SB_win = "SB Win",
    SB_MVPs = "SB MVPs",
    Pro_Bowls = "Pro Bowls",
    All_pro = "All Pro"
  ) |> 
  #cols_move_to_start(
   # columns = c(headshot_url, player_name,passing_yards,  passing_tds, 
     #interceptions,pypg,avgpyps,ptdspy)
 # ) |> 
  #tab_spanner(
    #label = md("**Passing**"),
   # columns = c( passing_yards, passing_tds, interceptions,
                 #pypg, avgpyps,ptdspy)) |> 
  #tab_spanner(
   # label = md("**Rushing**"),
    #columns = c(rushing_yards,rushing_tds)
 # ) |> 
 # tab_spanner(
    #label = md("**Total**"),
   # columns = c(Total_yards,Total_tds)
#  ) |> 
  tab_spanner(
    label = md("**Accolades**"),
    columns = c(SB_app, SB_win, MVPs, SB_MVPs, Pro_Bowls, All_pro) 
  ) |> 
 # fmt_number(
   # columns = pypg,
    #decimals = 1
 # ) |> 
#  fmt_number(
   # columns = c( passing_yards, Total_yards),
   # use_seps = TRUE,
   # sep_mark = ",",
    #decimals = 0
 # ) |> 
  tab_source_note(
    source_note = "Data: nflfastR"

  ) |> 
  tab_source_note(
    source_note = "Twitter: @Alex-Feazelle"
  )
  

  
gtf_ddf2







