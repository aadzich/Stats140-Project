# exploring actor pairs
library(tidyverse)
library(dplyr)
library(ggplot2)
library(igraph)

# import data
imdb <- read.csv("CURRENT_clean_imdbmovies_version5.csv", header = TRUE, stringsAsFactors = FALSE)
glimpse(imdb)

# turn genres into factors
imdb <- imdb %>% 
  mutate(
    Broad_Genre = as.factor(Broad_Genre)
  )

# first we want to create pairs of actors for each movie

# Create all possible pairs of actors for each row
actor_pairs <- imdb %>%
  rowwise() %>%
  mutate(
    Pair1 = paste(sort(c(Star1, Star2)), collapse = " & "),
    Pair2 = paste(sort(c(Star1, Star3)), collapse = " & "),
    Pair3 = paste(sort(c(Star1, Star4)), collapse = " & "),
    Pair4 = paste(sort(c(Star2, Star3)), collapse = " & "),
    Pair5 = paste(sort(c(Star2, Star4)), collapse = " & "),
    Pair6 = paste(sort(c(Star3, Star4)), collapse = " & ")
  ) %>%
  select(Pair1, Pair2, Pair3, Pair4, Pair5, Pair6, Revenue) %>% 
  pivot_longer(cols = starts_with("Pair"), names_to= "PairType",
               values_to = "ActorPair")
actor_pairs

# anova on top 20 actor pairs
# run ANOVA and Tukey w this filtered data
pop_pairs_imdb <- actor_pairs %>% 
  group_by(ActorPair) %>% 
  filter(n() >= 3) %>% 
  ungroup() %>% 
  mutate(ActorPair = as.factor(ActorPair)) %>% 
  select(ActorPair, Revenue)
pop_pairs_imdb
table(pop_pairs_imdb$ActorPair)

anova_pop_pair <- aov(Revenue ~ ActorPair, data = pop_pairs_imdb)
summary(anova_pop_pair)

tukey_pop_pair <- TukeyHSD(anova_pop_pair)
tukey_pop_pair$ActorPair

tukey_sig <- tukey_pop_pair$ActorPair %>% 
  as.data.frame() %>% 
  filter(`p adj` < .05)

tukey_sig

anova_data <- actor_pairs %>%
  filter(ActorPair %in% pair_revenue_10$ActorPair)
anova_data
# Perform ANOVA
anova_top_pairs <- aov(Revenue ~ ActorPair, data = anova_data)

# Display the summary of the ANOVA
summary(anova_top_pairs)
tukey_top_pairs <- TukeyHSD(anova_top_pairs)
tukey_top_pairs$ActorPair

tukey_sig <- tukey_top_pairs$ActorPair %>% 
  as.data.frame() %>% 
  filter(`p adj` < .05)

tukey_sig



# Reshape the data to a long format for easier aggregation
pair_revenue <- actor_pairs %>%
  group_by(ActorPair) %>%
  summarise(Avg_Revenue = mean(Revenue, na.rm = TRUE),
            Pairings = n()) %>%
  arrange(desc(Pairings))



pair_revenue_10 <- pair_revenue %>% head(10)
pair_revenue_10

# Plot the top 10 pairs with the most revenue
ggplot(pair_revenue_10, aes(x = reorder(ActorPair, Avg_Revenue), y = Avg_Revenue)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "10 Most Popular Actor Pairs by Average Revenue",
       x = "Actor Pair",
       y = "Total Revenue (in $)") +
  theme_minimal() + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))

#anova_result <- aov(Revenue ~ ActorPair, data = pair_revenue_10)

# Display ANOVA results
summary(anova_result)

tukey_poppair <- TukeyHSD(anova_result)
tukey_poppair$ActorPair

tukey_sig <- tukey_franchise$Franchise %>% 
  as.data.frame() %>% 
  filter(`p adj` < .05)

tukey_sig


head(pair_revenue)  # Top 10 actor pairs by average revenue

library(igraph)

actor_pairs
# Create an edge list for actor pairs
tot_edges <- actor_pairs %>%
  separate(ActorPair, into = c("Actor1", "Actor2"), sep = " & ") %>%
  group_by(Actor1, Actor2) %>%
  summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = "keep")
avg_edges <- actor_pairs %>%
  separate(ActorPair, into = c("Actor1", "Actor2"), sep = " & ") %>%
  group_by(Actor1, Actor2) %>%
  summarise(AvgRevenue = mean(Revenue, na.rm = TRUE), .groups = "keep")
pairs_edges <- actor_pairs %>% 
  separate(ActorPair, into = c("Actor1", "Actor2"), sep = " & ") %>% 
  group_by(Actor1, Actor2) %>% 
  summarize(Pairings = n(), .groups = "keep")
pairs_edges

# Filter for top 20 actor pairs by pairings
n_actor_pairs <- pairs_edges %>%
  arrange(desc(Pairings)) %>% 
  head(12)
n_actor_pairs
# Create the graph object for the top pairs
actor_graph <- graph_from_data_frame(n_actor_pairs)
actor_graph_undirected <- as.undirected(actor_graph)

# Plot the network
# Adjust the plot layout and aesthetics
# Plot the network with customized font
max_revenue <- max(edges$AvgRevenue, na.rm = TRUE)
edge_colors <- colorRampPalette(c("blue", "red"))(100)[as.integer(100 * edges$AvgRevenue / max_revenue)]

par(bg = "white")
plot(actor_graph_undirected, 
     vertex.size = 10,
     vertex.color = "lightcoral",
     edge.width = 4, 
     edge.label.font = 4,
     edge.label = n_actor_pairs$Pairings,
     edge.color = "gray",
     edge.label.family = "Helvetica",
     edge.label.cex = 1.2,
     vertex.label.font = 4,
     vertex.frame.color = "white",
     vertex.label.font = 4,      # Set font to bold
     vertex.label.cex = .8,    # Control the size of the labels
     vertex.label.color = "black", # Change label color
     vertex.label.family = "Helvetica"  # Change the font family (you can use other available fonts)
)







byactors <- imdb %>%
  pivot_longer(
    cols = c(Star1, Star2, Star3, Star4),
    names_to = "StarRank",
    values_to = "Actor"
  ) %>%
  select(Actor, Revenue)



byactors <- byactors %>%
  mutate(Franchise = case_when(
    Actor %in% c("Robert Downey Jr", "Chris Evans", "Scarlett Johansson","Joe Russo") ~ "Marvel",
    Actor %in% c("Daniel Radcliffe", "Emma Watson", "Rupert Grint") ~ "Harry Potter",
    Actor %in% c("Harrison Ford", "Carrie Fisher", "Mark Hamill") ~ "Star Wars",
    Actor %in% c("Tom Hanks", "Tim Allen") ~ "Toy Story",
    Actor %in% c("Joe Pesci", "Robert De Niro", "Al Pacino", "Diane Keaton") ~ "Other",
    TRUE ~ "N/A"
  ))
byactors


# Calculate the average revenue per franchise
franchise_avg_revenue <- byactors %>%
  group_by(Franchise) %>%
  summarize(AvgRevenue = mean(Revenue, na.rm = TRUE), Movies_Total = n(), .groups = 'drop')
franchise_avg_revenue
# View the results
popfranchises <- byactors %>% filter(Franchise != "N/A")
anova_result <- aov(Revenue ~ Franchise, data = popfranchises)

# Display ANOVA results
summary(anova_result)

tukey_franchise <- TukeyHSD(anova_result)
tukey_franchise$Franchise

tukey_sig <- tukey_franchise$Franchise %>% 
  as.data.frame() %>% 
  filter(`p adj` < .05)

tukey_sig



# most popular actor
most_popular_actor <- actor_pairs %>%
  separate(ActorPair, into = c("Actor1", "Actor2"), sep = " & ") %>%
  pivot_longer(cols = c(Actor1, Actor2), names_to = "Role", values_to = "Actor") %>%
  group_by(Actor) %>%
  summarise(Count = n(), TotalRevenue = sum(Revenue, na.rm = TRUE)) %>%
  arrange(desc(Count)) %>%
  slice(1) %>%  # Take the top actor
  pull(Actor)
most_popular_actor

# all pairings w de niro
de_niro_pairings <- actor_pairs %>%
  separate(ActorPair, into = c("Actor1", "Actor2"), sep = " & ") %>%
  filter(Actor1 == most_popular_actor | Actor2 == most_popular_actor)
de_niro_pairings

actor_pairs$InvolvesPopularActor <- grepl(most_popular_actor, actor_pairs$ActorPair)

t_test_result <- t.test(
  Revenue ~ InvolvesPopularActor,
  data = actor_pairs,
  alternative = "greater"  # Hypothesis: Movies with popular actor have higher revenue
)
t_test_result
