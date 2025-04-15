## For each tournament
#Fit the model on all data not in the tournament
# Find the team predicted to have the best score
# Compare against 10 randomly selected teams that cost $22-$24
#Report what place you took
first_predictor = "min_cor"
last_predictor = "bogey_rate_this_year4"

source("get_all_data.R")
source("functions.R")

library(parallel)
all_tournaments <- unique(all_data$tournament_year)
prices <- read_csv("masters_2024_prices.csv") # Load prices data
possibilities <- make_all_possible_teams(prices) # Make all possible teams

# Function to process a single tournament with error handling
process_tournament <- function(test_tournament) {
  tryCatch({
    print(test_tournament)
    train_data <- all_data %>% filter(tournament_year != test_tournament)
    year <- all_data %>%
      filter(tournament_year == test_tournament) %>%
      slice(1) %>%
      pull(year)
    train_team_data <- generate_hypothetical_team_score_data(train_data)
    full_training_data <- prep_predictors(train_team_data, train_data, year = year)
    test_tournament_stats <- all_data %>% filter(tournament_year == test_tournament)
    players_in_tourney <- unique(test_tournament_stats$player)
    
    possibilities_in_tournament <-
      possibilities %>%
      filter(
        player1 %in% players_in_tourney,
        player2 %in% players_in_tourney,
        player3 %in% players_in_tourney,
        player4 %in% players_in_tourney
      )
    # handles case where there are not any possible teams in the tournament
    if (nrow(possibilities_in_tournament) == 0)
      return(NA)
    
    rf <-
      fit_random_forest(
        full_training_data,
        first_predictor_col = first_predictor,
        last_predictor_col = last_predictor
      )
    
    team_preds <-
      score_possibilities(possibilities_in_tournament,
                          historic_data = train_data,
                          rf,
                          year = year)
    
    recs <- find_best_two_teams(team_preds)
    if (nrow(recs) == 0) {
      return(NA)
    } ## Handles case where there are not enough teams with unique players
    my_score <-
      calculate_tournament_scores(test_tournament_stats, recs) %>%
      pull(score) %>%
      sum()
    
    # Should force groups of teams to be distinct here but seems like too much work\
    random_teams  <-
      possibilities_in_tournament %>%
      sample_n(20)
    
    opponents <-
      calculate_tournament_scores(test_tournament_stats, random_teams) %>%
      mutate(group = rep(1:10, each = 2)) %>%
      group_by(group) %>%
      summarize(score = sum(score))
    
    ## Instead of winning on a tie, should make this random
    final_rank <- sum(my_score > opponents$score) + 1
    print(final_rank)
    return(final_rank)
  }, error = function(e) {
    # Handle errors for a specific tournament
    cat("Error in tournament:", test_tournament, "\n")
    cat("Error message:", e$message, "\n")
    return(NA) # Return NA for the failed tournament
  })
}

# Parallelize the sapply loop using mclapply
num_cores <- 2 # Number of cores to use
rank <- mclapply(all_tournaments, process_tournament, mc.cores = num_cores)
rank <- unlist(rank) # Convert the list to a vector

hist(rank, breaks = 20)
mean(rank, na.rm = T)
table(rank) / length(rank[! is.na(rank)])
save(rank, file = "scored_versions/version6.RData")

## Version 1 contained predictors PC 1-4, birdie rate 1-4, make_cut_rate 1-4, holes_played 1-4

## Version 2 adds top 25 principal components w/ min/max. Plus birdie and par rates by par, and eagle rates

## Version 3 adds year data  -- worse!

## Version 4 add year data and year specific predictors, 5.322

## Version 5 Cut out PC stuff altogether 5.047

# use min/mean/max cor