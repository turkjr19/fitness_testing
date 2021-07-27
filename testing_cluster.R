library(tidyverse)
library(janitor)
library(lubridate)

# read in fitness data
dataIn <- read.csv("summer_testing.csv")

# keep columns we want
results <- dataIn %>% 
  select(-c(testing_date, push_up_drop, shuttle_drop, shuttle_average)) %>%
  unite("player", first, last, sep=" ") %>% 
  mutate(id = row_number()) %>% 
  select(id, everything())

# pull bio
bio <- results %>% 
  select(id:wt)

# check consistency and reliability
library(psych)

alpha(results[, c("ht", "wt", "long_jump", "push_up1", "push_up2",
                  "left_5_10_5", "right_5_10_5", "shuttle1", "shuttle2")],
      na.rm = T, check.keys = T)

# function to range 0-1 with NA's
range01 <- function(x, na.rm = T){(x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))}

# create tibble for players, scale and range values
results_scaled <- results %>% 
  select(ht, wt, long_jump, push_up1, push_up2, left_5_10_5, right_5_10_5, shuttle1, shuttle2) %>% 
  mutate_all(scale, center = TRUE, scale = TRUE) %>% 
  mutate_all(range01) %>% 
  mutate(left_5_10_5 =  (1-left_5_10_5),
         right_5_10_5 = (1-right_5_10_5),
         shuttle1 = (1-shuttle1),
         shuttle2 = (1-shuttle2))

# Create friend (closeness)
# Create a tibble where we can see who is close
set.seed(9556)
friends_euclidean <- dist(results_scaled) #calculate euclidean distance
friends_matrix <- as.matrix(friends_euclidean) #convert list to matrix
friends_df <- as_tibble(friends_matrix) #convert matrix to tibble

# need to label each column with player id
name <- results %>% 
  select(player)

colnames(friends_df) <- name$player # set the column names
friends <- bind_cols(name, friends_df)

# Pivot
friends_long <- friends %>% 
  pivot_longer(
    cols = -c(player),
    names_to = "name",
    values_to = "closeness") %>% 
  mutate(closeness = (1-closeness)) %>% 
  arrange(-closeness)

# Filter by player
get_player <- function(x){
  player <- friends_long %>% 
    filter(player == x) %>% 
    filter(name != x)
  
  View(player)
}
get_player("Sam Alfano")

# who has the best overall score
results_overall <- results_scaled %>% 
  mutate(rank = long_jump +((push_up1 + push_up2)/2) + ((left_5_10_5 + right_5_10_5)/2) +
           ((shuttle1 + shuttle2)/2)) %>% 
  mutate(rank = range01(rank))

results_overall <- results_overall %>% bind_cols(name) %>% 
  select(player, rank, everything()) %>% 
  arrange(-rank)

# Write file to csv
write.csv(results_overall,
          file = "summer21_fitness.csv",
          row.names = F)

# K-Means Cluster

library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
df <- results %>% select(ht:shuttle2) # columns we need
df <- scale(df)
df <- as_tibble(df) %>% bind_cols(name) %>% select(player, everything())
distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB",
                                    mid = "white",
                                    high = "#FC4E07"))
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)





set.seed(12345)
fitK <- kmeans(results_scaled,3)
fitK

# Append cluster assignment
friendsK <- results %>% 
  mutate(cluster = str_c("cluster", fitK$cluster))

cluster1 <- friendsK %>% filter(cluster == "cluster1")
cluster2 <- friendsK %>% filter(cluster == "cluster2")
cluster3 <- friendsK %>% filter(cluster == "cluster3")


