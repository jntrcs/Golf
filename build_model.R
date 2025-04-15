source("get_all_data.R")
source("functions.R")

all_tournaments <- unique(all_data$tournament_year)
#training_events = sample(all_tournaments, size=floor(length(all_tournaments)*.8))
#testing_events = setdiff(all_tournaments, training_events)

train_data= all_data %>% filter(tournament_year != "Masters Tournament 2024")

train_team_data= generate_hypothetical_team_score_data(train_data)
train_team_data


full_training_data <- prep_predictors(train_team_data, train_data, year=2024)



rf = fit_random_forest(full_training_data, first_predictor_col = "min_cor", last_predictor_col = "bogey_rate_this_year4")
full_training_data %>% mutate(pred = predict(rf)) %>%
  ggplot(aes(x=score, y =pred))+geom_point()



df = read_csv("masters_2024_prices.csv")
possibilities<-make_all_possible_teams(df)
team_preds = score_possibilities(possibilities, historic_data = train_data, rf, year=2024)




recs =find_best_two_teams(team_preds)
calculate_tournament_scores(all_data %>% filter(tournament_year == "Masters Tournament 2024"), recs)
