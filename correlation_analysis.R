source("get_all_data.R")
source("functions.R")

data =all_data %>%
  group_by(player) %>% 
  filter(n()>500) %>%ungroup

avg_perf = data %>% mutate(diff=score-par) %>%
  group_by(player) %>%
  summarize(average_performance=mean(diff))

cor_mat = data %>% left_join(avg_perf) %>%
  mutate(normalized_performance = score - par - average_performance,
         hole_id = paste(tournament_year, round, hole))  %>%
  select(hole_id, player, normalized_performance) %>%
  pivot_wider(id_cols=player, names_from="hole_id", values_from=normalized_performance) %>%
  select(-player) %>%
  as.matrix %>%
  cor(use="pairwise.complete.obs") 

cor_mat[is.na(cor_mat)] <- mean(cor_mat[upper.tri(cor_mat)], na.rm=T)
isSymmetric(cor_mat)

cor_mat_fixed <- Matrix::nearPD(cor_mat, corr = TRUE)$mat


eig_decomp=eigen(cor_mat)
eigenvalues <- eig_decomp$values
# Extract eigenvectors (loadings or directions of principal components)
eigenvectors <- eig_decomp$vectors

# Proportion of variance explained by each component
prop_variance <- eigenvalues / sum(eigenvalues)

# Cumulative proportion
cum_prop_variance <- cumsum(prop_variance)

prop_variance
cum_prop_variance
