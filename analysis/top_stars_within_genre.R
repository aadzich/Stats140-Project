# top stars within genres

imdb <- read.csv("CURRENT_clean_imdbmovies_version5.csv", header = TRUE, stringsAsFactors = FALSE)


# Step 1: Reshape the data to combine stars into one column
library(dplyr)
library(tidyr)

star_data <- imdb %>%
  pivot_longer(cols = c(Star1, Star2, Star3, Star4), 
               names_to = "StarRank", values_to = "Star") %>%
  filter(!is.na(Star)) # Remove rows where stars are missing
levels(as.factor(imdb$Broad_Genre))

# Step 1: Filter the data for Drama movies
drama_data <- star_data %>%
  filter(Genre == "Drama")

# Step 2: Identify the top three actors in Drama by average revenue
top_drama_actors <- drama_data %>%
  pivot_longer(cols = starts_with("Star"), names_to = "StarRank", values_to = "Actor") %>%
  group_by(Actor) %>%
  summarise(AverageRevenue = mean(Revenue, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(AverageRevenue)) %>%
  slice_max(order_by = AverageRevenue, n = 3)
top_drama_actors

# Step 3: Filter Drama data for the top three actors
filtered_drama_data <- drama_data %>%
  pivot_longer(cols = starts_with("Star"), names_to = "StarRank", values_to = "Actor") %>%
  filter(Actor %in% top_drama_actors$Actor) %>% 
  relocate(Actor)

filtered_drama_data

# Step 4: Perform ANOVA to compare revenues across the top actors
anova_result <- aov(Revenue ~ Actor, data = filtered_drama_data)

# Step 5: Summarize the ANOVA test
summary(anova_result)

# Step 6: Post-hoc analysis (Tukey HSD test)
tukey_result <- TukeyHSD(anova_result)

# Print Tukey results
print(tukey_result)

# Step 7: Visualize revenue comparisons for the top actors
ggplot(filtered_drama_data, aes(x = Actor, y = Revenue, fill = Actor)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Revenue Comparison for Top Drama Actors",
       x = "Actor",
       y = "Revenue") +
  theme(legend.position = "none")


