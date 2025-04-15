library(tidyverse)
library(googlesheets4)

service_account_key_path <- "/Users/jacksoncurtis/Documents/Masters/static-bond-162921-854527ade866.json"

# Authenticate using the service account key
# This will NOT open a browser or require interaction
gs4_auth(path = service_account_key_path) 
latest = read_csv("/Users/jacksoncurtis/Documents/Masters/latest_masters_data.csv", col_types = "ccnnnn")
latest = latest %>% mutate(score=score-par) %>% rename(player=playerName) 
 
teams<- read_csv("/Users/jacksoncurtis/Documents/Masters/teams.csv")

current_round = latest %>%
  summarize(max(round)) %>%pull

player_scores = teams  %>%
  left_join(latest, by="player") %>%
  select(-tournament) %>%
  pivot_wider(id_cols = c(person, team, player), names_from=c(round, hole), values_from=score,
              names_prefix = "Round: ", names_sep=" Hole: ")  %>%
  arrange(person, team, player) %>%
  rowwise%>%
  mutate(score = sum(c_across(starts_with("Round")), na.rm=T), .after="player",
         pre_cut_score = sum(c_across(starts_with("Round: 1")|
                                        starts_with("Round: 2")), na.rm=T)) %>%
  select(-contains("NA"))


if(current_round>2){
  make_cut=T
  cut_score = 2
  made_cut =player_scores %>% mutate(made_cut = (pre_cut_score<=cut_score)) %>% filter(made_cut) %>%
    group_by(person, team) %>%
    summarize(n_making_cut=n())
  
}


teams_wide = teams %>% mutate(player_number = rep(paste0("player", 1:4), nrow(teams)/4)) %>%
  pivot_wider(id_cols = c(person,team), names_from = player_number, values_from=player) 

if(make_cut){
  teams = teams %>% left_join(made_cut) %>% rename(players_left=n_making_cut)
}else{
  teams = teams %>% mutate(players_left = 4)
}

team_scoreboard= teams_wide  %>% left_join(
  teams%>% left_join(latest) %>% #arrange(round, hole)
  group_by(person, team, round, hole)%>%
  summarize(hole_score_post_cut = min(c(score, 0,0,0,0)[1:first(players_left)]),
            hole_score_pre_cut = min(c(score, 0,0,0,0)[1:4])) %>%
  mutate(hole_score = if_else(round<3, hole_score_pre_cut, hole_score_post_cut),
         .keep="unused") %>%
  pivot_wider(id_cols = c(person, team), names_from=c(round, hole), values_from=hole_score,
              names_prefix = "Round: ", names_sep=" Hole: ") %>%
  mutate(score = sum(c_across(starts_with("round")), na.rm=TRUE), .after=team) 
)

individual_scoreboard = team_scoreboard %>%
  group_by(person) %>%
  summarize(score = sum(score, na.rm=T)) %>%
  arrange(score)



ss= "https://docs.google.com/spreadsheets/d/14TcfR33NvTB1oC5XTNhW_tSXoVaIMs6XK2DtpaxG0pM/edit?gid=0#gid=0"

write_sheet(team_scoreboard, ss=ss,
            sheet="team_scoreboard")

write_sheet(individual_scoreboard, ss=ss,
            sheet="individual_scoreboard" )
       


write_sheet(player_scores %>% mutate(made_cut = pre_cut_score<=cut_score, .keep="unused",
                                     .before=score), ss=ss,
            sheet="player_scoreboard")


# launch with launchctl load ~/Library/LaunchAgents/my_score_udpater.plist

