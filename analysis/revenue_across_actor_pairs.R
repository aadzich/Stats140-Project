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
# Reshape the data to a long format for easier aggregation
pair_revenue <- actor_pairs %>%
  group_by(ActorPair) %>%
  summarise(Avg_Revenue = mean(Revenue, na.rm = TRUE)) %>%
  arrange(desc(Avg_Revenue)) %>%
  head(10)

# Plot the top 10 pairs with the most revenue
ggplot(pair_revenue, aes(x = reorder(ActorPair, Avg_Revenue), y = Avg_Revenue)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  coord_flip() +
  labs(title = "Top 10 Actor Pairs by Avg Revenue",
       x = "Actor Pair",
       y = "Total Revenue (in $)") +
  theme_minimal()


# finding avg rev for each pair
pair_revenue <- actor_pairs %>%
  group_by(ActorPair) %>%
  summarize(
    AvgRevenue = mean(Revenue),
    MovieCount = n()
  ) %>%
  arrange(desc(AvgRevenue))

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


# Filter for top 20 actor pairs by revenue
avg_actor_pairs <- avg_edges %>%
  arrange(desc(AvgRevenue)) %>% 
  head(20)
top_actor_pairs
# Create the graph object for the top pairs
actor_graph <- graph_from_data_frame(avg_actor_pairs)
actor_graph_undirected <- as.undirected(actor_graph)

# Plot the network
# Adjust the plot layout and aesthetics
# Plot the network with customized font
max_revenue <- max(edges$TotalRevenue, na.rm = TRUE)
edge_colors <- colorRampPalette(c("blue", "red"))(100)[as.integer(100 * edges$TotalRevenue / max_revenue)]

plot(actor_graph_undirected, 
     vertex.size = 10,
     vertex.color = "lightgray",
     edge.width = 4, 
     edge.color = edge_colors,
     vertex.label.font = 4,      # Set font to bold
     vertex.label.cex = 1,    # Control the size of the labels
     vertex.label.color = "black", # Change label color
     vertex.label.family = "Helvetica"  # Change the font family (you can use other available fonts)
)




