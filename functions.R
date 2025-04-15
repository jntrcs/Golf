#source("get_all_data.R")
library(rlang)
library(randomForest)


fit_random_forest <- function(
    full_training_data,
    response_col = "score",
    first_predictor_col,
    last_predictor_col,
    ntree = 500,
    nodesize = 5,
    ...
) {
  # Find the index of the first and last predictor columns
  start_index <- which(names(full_training_data) == first_predictor_col)
  end_index   <- which(names(full_training_data) == last_predictor_col)
  
  # Slice the column names from start_index to end_index
  predictor_cols <- names(full_training_data)[start_index:end_index]
  
  # Create a formula string: "response_col ~ predictor1 + predictor2 + ..."
  formula_str <- paste(response_col, "~", paste(predictor_cols, collapse = " + "))
  
  # Convert the string to a formula and fit the random forest
  rf_model <- randomForest(
    formula = as.formula(formula_str),
    data    = full_training_data,
    ntree   = ntree,
    nodesize = nodesize,
    ...
  )
  
  return(rf_model)
}


make_individual_stats<- function(data){
  hole_data =data %>%
    mutate(diff = score-par,
           type = case_when(diff< -1~"Eagle",
                            diff<=-1 ~ "Birdie",
                            diff <= 0~"Par",
                            diff <=1~ "Bogey",
                            diff >1 ~"DBogey")) 
  tournament_data = 
    hole_data %>%
    group_by(tournament_year, year, player) %>%
    summarize(rounds = max(round))
  player_data =hole_data %>% group_by(player) %>%
    summarize(birdie_rate = mean(type=="Birdie"),
              par_rate = mean(type=="Par"),
              bogey_rate = mean(type=="Bogey"),
              dbogey_rate = mean(type=="DBogey"),
              eagle_rate = mean(type=="Eagle"), 
              holes_played = n()) %>%
    left_join(by="player",
      hole_data %>% group_by(player, par) %>%
                summarize(birdie_rate = mean(type=="Birdie"),
                          par_rate = mean(type=="Par"),
                          bogey_rate = mean(type=="Bogey"),
                          dbogey_rate = mean(type=="DBogey")) %>%
                pivot_wider(id_cols=player, names_from=par, 
                            values_from=c(birdie_rate, par_rate))
              )%>%
    left_join(by="player",
      hole_data %>% filter(year==2024) %>%
        group_by(player) %>%
        summarize(birdie_rate_2024 = mean(type=="Birdie"),
                  par_rate_2024 = mean(type=="Par"),
                  bogey_rate_2024 = mean(type=="Bogey"),
                  holes_played_2024 = n())
    ) %>%
    left_join(by="player",
      hole_data %>% filter(year==2025) %>%
        group_by(player) %>%
        summarize(birdie_rate_2025 = mean(type=="Birdie"),
                  par_rate_2025 = mean(type=="Par"),
                  bogey_rate_2025 = mean(type=="Bogey"),
                  holes_played_2025 = n())
    ) %>%
    left_join(by="player",tournament_data %>%
                group_by(player) %>%
                summarize(make_cut_rate = mean(rounds==4))) %>%
    left_join(by="player",tournament_data %>% 
                filter(year==2024) %>%
                group_by(player) %>%
                summarize(make_cut_rate_2024 = mean(rounds==4))) %>%
    left_join(by="player",tournament_data %>%
                filter(year==2025) %>%
                group_by(player) %>%
                summarize(make_cut_rate_2025 = mean(rounds==4))) #%>%
    #filter(holes_played>500) %>%
    #arrange(desc(make_cut_rate))
  return(player_data)
}

#team_names = c("Scottie Scheffler", "Si Woo Kim", "Rory McIlroy", "Shane Lowry")
make_team_stats<-function(all_data, team_names){
  all_data= all_data %>%
    group_by(tournament, round, hole) %>%
    summarize(hole_difficulty=mean(score-par)) %>%
    left_join(all_data) %>% ungroup
  for (i in 1:(length(team_names)-1)){
    player_1_data = all_data %>% filter(player==team_names[i]) %>%
      mutate(player1 = player, score1 = score - par, .keep="unused")
    for(j in (i+1):length(team_names)){
      player_2_data = all_data %>% filter(player==team_names[j]) %>%
        mutate(player2 = player, score2 = score - par, .keep="unused")
      player_1_data %>%
        inner_join(player_2_data) %>%
        ggplot(aes(x=hole+18*(round-1)))+facet_wrap(~tournament)+geom_point(aes(y=score1, color="Kim"))+
        geom_point(aes(y=score2, color="Lowry"))
        summarize(player1=first(player1), player2=first(player2),
          cor=cor(score1-hole_difficulty, score2-hole_difficulty), n=n()) %>% print
      
    }
      
  }
    
}


calculate_individual_team_score <- function(tournament_data, team_names){
  if (length(unique(tournament_data$tournament))>1 | length(unique(tournament_data$year))>1){
    stop("Must subset data to a single tournament")
  }
  
  if (!all(team_names %in% unique(tournament_data$player))){
    print(team_names)
    stop("Not all players are in tournament")
  }
  
  tournament_data %>% 
    filter(player %in% team_names) %>%
    group_by(round, hole, par) %>%
    summarize(team_score = min(score) - first(par), .groups="drop") %>% 
    summarize(sum(team_score)) %>% pull
}

#tournament_data = all_data %>% filter(tournament=="Masters Tournament")
#team1=c("Scottie Scheffler", "Si Woo Kim", "Tommy Fleetwood", "Kurt Kitayama")
#calculate_team_score(tournament_data, team1)

#team2= c("Rory McIlroy", "Collin Morikawa", "Ryan Fox", "Neal Shipley")
#calculate_team_score(tournament_data, team2)

# For each tournament in the provided data set, the algorithm will randomly divide
# all the players into teams of 4, and then calculate each player's score
generate_hypothetical_team_score_data <-function(data){
  map_dfr(unique(data$tournament_year), function(ty){
    tournament_data = data %>% filter(tournament_year==ty)
    all_players = unique(tournament_data$player)
    #randomize order
    all_players <- sample(all_players, length(all_players))
    teams = floor(length(all_players)/4)
    map_dfr(1:teams, function(i){
       players = all_players[(teams)*(0:3)+i] 
       score = calculate_individual_team_score(tournament_data, players)
       data.frame(player1 = players[1], player2=players[2], player3 = players[3],
                  player4=players[4], score=score, tournament_year=tournament_data$tournament_year[1],
                  year = tournament_data$year[1]) 
    })%>%
      # The best team in the tournament is assigned a score of 1, with everyone else scaled from that
      mutate(score= score/min(score))
  })
}
#data=all_data %>% filter(tournament %in% unique(tournament)[1:2])
#training_data=generate_training_data(data)


## adds 4 columns to the df_teams df, one stat for each player, from highest to lowest
widen_stat <- function(df_teams, df_stats, stat_col = "stat_of_interest") {
  # Convert stat_col to a symbol for tidy evaluation
  stat_col_sym <- ensym(stat_col)
  # Also keep it as a string for pivot_wider
  stat_col_str <- as_string(stat_col_sym)
  
  # Identify which columns in df_teams hold player names
  player_cols <- grep("^player", names(df_teams), value = TRUE)
  # Identify the "other" columns we want to keep (besides the player columns)
  extra_cols <- setdiff(names(df_teams), player_cols)
  
  players_long <- df_teams %>%
    # Add a team_id to keep track of each row/team
    mutate(team_id = row_number()) %>%
    
    # Pivot from wide (player1..4) to long (player_col, player)
    pivot_longer(
      cols      = all_of(player_cols),
      names_to  = "player_col",
      values_to = "player"
    ) %>%
    
    # Join in the chosen stat from df_stats
    left_join(df_stats, by = "player") 
  
  missing = players_long %>% filter(!complete.cases(.))
  if(nrow(missing)>0){
    logger::log_warn("The following players were not found in the training data: {unique(missing$player)}")
  }
  
  players_long%>%
    
    # Group by team_id AND any extra columns so they stay intact after pivot
    group_by(team_id, across(all_of(extra_cols))) %>%
    
    # Sort each team's players by descending stat_col
    arrange(desc(!!stat_col_sym), .by_group = TRUE) %>%
    
    # Rank 1..4 so that rank=1 is the highest stat
    mutate(rank = row_number()) %>%
    
    # Pivot back to wide form: player1/stat_of_interest1 = highest, etc.
    pivot_wider(
      id_cols      = c("team_id", all_of(extra_cols)),
      names_from   = rank,
      values_from  = c("player", stat_col_str),
      names_sep    = ""
    ) %>%
    ungroup() %>%
    select(-team_id) %>%
    drop_na() %>% 
    select(all_of(player_cols), all_of(extra_cols), everything())
}


make_all_possible_teams  <- function(df, lowest_price=21, highest_price=23) {
  # Sort by price so that we can use bounds to prune combinations
  df <- df[order(df$price), ]
  n <- nrow(df)
  
  # This list will hold our valid team combinations.
  results <- list()
  
  # Recursive backtracking function.
  # 'start' is the index to start from,
  # 'team' is the current vector of selected players,
  # 'tot' is the current total price.
  recfun <- function(start, team, tot) {
    # If we already have 4 players...
    if (length(team) == 4) {
      # Check if the total is within the desired range.
      if (tot >= lowest_price && tot <= highest_price) {
        # Save the team (append the total price as the last element)
        results[[length(results) + 1]] <<- c(team, tot)
      }
      return()
    }
    
    # How many players still needed?
    remaining <- 4 - length(team)
    # Not enough players left? Exit.
    if (n - start + 1 < remaining) return()
    
    # Compute a lower bound: take the cheapest 'remaining' players available.
    lower_bound <- tot + sum(df$price[start:(start + remaining - 1)])
    if (lower_bound > highest_price) return()  # even the cheapest options would bust the cap
    
    # Compute an upper bound: take the most expensive 'remaining' players available.
    upper_bound <- tot + sum(df$price[(n - remaining + 1):n])
    if (upper_bound < lowest_price) return()  # even the best case would be too low
    
    # Try each player from the current start index onward.
    for (i in start:n) {
      recfun(i + 1, c(team, df$player[i]), tot + df$price[i])
    }
  }
  
  # Start the recursion.
  recfun(1, character(0), 0)
  
  # If no teams were found, return an empty data frame.
  if (length(results) == 0) return(data.frame())
  
  # Combine the list of valid teams into a data frame.
  teams_mat <- do.call(rbind, results)
  teams_df <- as.data.frame(teams_mat, stringsAsFactors = FALSE)
  
  # Name the columns appropriately.
  colnames(teams_df) <- c("player1", "player2", "player3", "player4", "team_price")
  
  # Convert the team price to numeric (it came in as character).
  teams_df$team_price <- as.numeric(teams_df$team_price)
  
  return(teams_df)
}


# Example usage
# teams <- make_all_possible_teams("/mnt/data/example_price_sheet.csv")
# print(head(teams))


score_possibilities<- function(teams, historic_data, model, year){
  full_training_data = prep_predictors(teams, historic_data, year=year)
  full_training_data %>% select(player1:player4, team_price) %>% 
    mutate(prediction = predict(model, newdata=full_training_data)) %>%
    arrange(desc(prediction))
}

prep_pca_predictors<- function(team_data, train_data){
  train_pca_stats <- get_pca_predictors(train_data)
  team_data <- team_data %>% mutate(team_id = 1:n())  
  
  team_data%>%
    select(team_id, starts_with( "player")) %>%
    pivot_longer(-team_id, values_to="player") %>% 
    left_join(train_pca_stats) %>%
    group_by(team_id) %>% 
    summarize(across(starts_with("PC"), list(min = min, max=max))) %>%
    left_join(team_data) %>%
    relocate(starts_with("player"), .before = everything()) %>%
    relocate(starts_with("PC"), .after = everything()) %>%
    select(-team_id)

    
}

calc_correlations<- function(.x, .y, correlation_matrix){
  
  .x %>% bind_cols(correlation_matrix %>% 
    filter(player %in%c(.x$player1, .x$player2, .x$player3, .x$player4) &
            player2 %in%c(.x$player1, .x$player2, .x$player3, .x$player4)  ) %>%
    summarize(min_cor = min(cor, na.rm=T),
              max_cor = max(cor, na.rm=T),
              mean_cor = mean(cor, na.rm=T)))
  
}

make_big_correlation_matrix <- function(train_data){
  correlation_matrix =train_data %>%mutate(score = score-par) %>%
    #select(player, score) %>%
    pivot_wider(names_from = player, values_from=score) %>%
    select(-any_of(c("tournament", "round", "hole", "par", "year",
                     "tournament_year"))) %>%
    #mutate(across(everything(), \(x)replace_na(x, replace=0))) %>%
    as.matrix %>%
    cor(use="pairwise.complete.obs") %>%
    as_tibble %>%
    mutate(player = names(.)) %>%
    pivot_longer(-player, names_to = "player2", values_to="cor") %>%
    filter(player<player2) ## keep half the names
}

prep_predictors<-function(team_data, train_data, year){
  cor_mat <- make_big_correlation_matrix(train_data)
  train_individual_stats <- make_individual_stats(train_data) %>%
    mutate(make_cut_rate_this_year = ifelse(year==2024, make_cut_rate_2024, make_cut_rate_2025),
           birdie_rate_this_year = ifelse(year==2024, birdie_rate_2024, birdie_rate_2025),
           par_rate_this_year = ifelse(year==2024, par_rate_2024, par_rate_2025),
           bogey_rate_this_year = ifelse(year==2024, bogey_rate_2024, bogey_rate_2025),
           holes_played_this_year = ifelse(year==2024, holes_played_2024, holes_played_2025),
           .keep="unused") 
  team_data_with_cors =team_data  %>%
    mutate(row=1:n())%>%
    group_by(row) %>%
    group_modify(calc_correlations, correlation_matrix= cor_mat) %>%
    ungroup %>%
    select(-row)
  full_training_data= team_data_with_cors %>%
    #prep_pca_predictors(train_data) %>%
    widen_stat(train_individual_stats, "birdie_rate")%>%
    widen_stat(train_individual_stats, "par_rate") %>%
    widen_stat(train_individual_stats, "eagle_rate") %>%
    widen_stat(train_individual_stats, "birdie_rate_3") %>%
    widen_stat(train_individual_stats, "birdie_rate_4") %>%
    widen_stat(train_individual_stats, "birdie_rate_5") %>%
    widen_stat(train_individual_stats, "par_rate_3") %>%
    widen_stat(train_individual_stats, "par_rate_4") %>%
    widen_stat(train_individual_stats, "par_rate_5") %>%
    widen_stat(train_individual_stats, "make_cut_rate") %>%
    widen_stat(train_individual_stats, "holes_played")  %>%
    widen_stat(train_individual_stats, "make_cut_rate_this_year") %>%
    widen_stat(train_individual_stats, "holes_played_this_year")  %>%
    widen_stat(train_individual_stats, "birdie_rate_this_year")%>%
    widen_stat(train_individual_stats, "par_rate_this_year") %>%
    widen_stat(train_individual_stats, "bogey_rate_this_year") %>%
    relocate(starts_with("player"), .before = everything())
}


find_best_two_teams <- function(df) {
  # Check that we have at least two teams
  df <- df %>% arrange(desc(prediction))
  n <- nrow(df)
  if (n < 2) return(data.frame())
  
  best_sum <- -Inf
  best_pair <- NULL
  
  # Precompute the player sets for each team.
  # Each element of team_list is a character vector of the four players.
  team_list <- vector("list", n)
  for (i in 1:n) {
    team_list[[i]] <- as.character(df[i, c("player1", "player2", "player3", "player4")])
  }
  
  # Loop over each pair of teams.
  # Because df is sorted by prediction descending, we can use early stopping.
  for (i in 1:(n-1)) {
    # For fixed i, the best candidate for j is i+1. If even that sum is not better than our current best,
    # then no later j (which have even lower predictions) can improve the sum.
    if (df$prediction[i] + df$prediction[i+1] <= best_sum) {
      break
    }
    for (j in (i+1):n) {
      current_sum <- df$prediction[i] + df$prediction[j]
      # Since rows are in descending order, if the current sum drops below our best, break out of the inner loop.
      if (current_sum <= best_sum) break
      
      # Check if teams are disjoint: ensure the two teams share no players.
      if (length(intersect(team_list[[i]], team_list[[j]])) == 0) {
        best_sum <- current_sum
        best_pair <- c(i, j)
      }
    }
  }
  
  # If no valid pair is found, return an empty data frame.
  if (is.null(best_pair)) {
    return(data.frame())
  }
  
  # Return the two rows (teams) that give the maximum sum(prediction)
  return(df[best_pair, ])
}

calculate_tournament_scores <- function(tournament_data, df){
  df %>% rowwise %>%
    mutate(score = calculate_individual_team_score(tournament_data, c(player1, player2, player3, player4))) %>%
    ungroup
}


get_pca_predictors <- function(train_data){
  train_data <- train_data %>%
    mutate(relative_score = score - par)
  
  player_hole <- train_data %>%
    group_by(player, tournament_year, hole) %>%
    summarise(avg_relative = mean(relative_score, na.rm = TRUE), .groups = "drop")
  
  player_matrix <- player_hole %>%
    unite("tournament_hole", tournament_year, hole) %>%
    pivot_wider(names_from = tournament_hole, values_from = avg_relative)
  
  data_for_pca <- player_matrix %>% select(-player)
  
  data_imputed <- data_for_pca  # Create a copy to impute
  for (j in seq_along(data_imputed)) {
    # Compute the mean for column j, excluding NAs
    col_mean <- mean(data_imputed[[j]], na.rm = TRUE)
    # Replace NAs in column j with the computed column mean
    data_imputed[[j]][is.na(data_imputed[[j]])] <- col_mean
  }
  
  # Run PCA (center and scale the variables)
  pca_fit <- prcomp(data_imputed, center = TRUE, scale. = FALSE)
  
  # Extract the loadings (i.e. the coefficients that define each principal component)
  #loadings <- pca_fit$rotation  # columns correspond to PCs; rows correspond to holes
  
  # Extract the PCA scores for each player (rows of the PCA space)
  pca_scores <- pca_fit$x
  
  
  n <- 25  # choose the number of principal components to keep
  pca_scores_top_n <- pca_scores[, 1:n]
  
  # Add the player identifier back in
  final_features <- data.frame(player = player_matrix$player, pca_scores_top_n)
  final_features
  #ggplot(final_features, aes(x=PC1, y=PC2))+geom_point()+
  #geom_text(data=~.x %>%filter(abs(PC1)>10|abs(PC2)>10), aes(label=player))
}
