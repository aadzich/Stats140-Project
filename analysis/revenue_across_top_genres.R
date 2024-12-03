# revenue across top genres
library(tidyverse)
library(dplyr)

# import data
imdb <- read.csv("CURRENT_clean_imdbmovies_version5.csv", header = TRUE, stringsAsFactors = FALSE)
glimpse(imdb)

# turn genres into factors
imdb <- imdb %>% 
  mutate(
    Broad_Genre = as.factor(Broad_Genre)
  )

# count number of movies per genre
genre_counts <- imdb %>%
  group_by(Broad_Genre) %>%
  tally()

genre_counts

# filtering genres with at least 10 movies for best ANOVA results
pop_genre_imdb <- imdb %>%
  filter(Broad_Genre %in% genre_counts$Broad_Genre[genre_counts$n >= 10])

# run ANOVA and Tukey w this filtered data
anova_genre_pop <- aov(Revenue ~ Broad_Genre, data = pop_genre_imdb)
summary(anova_genre_pop)

tukey_genre_pop <- TukeyHSD(anova_genre_pop)
tukey_genre_pop$Broad_Genre

# visualize
# Boxplot to compare revenue across genres
ggplot(pop_genre_imdb, aes(x = Broad_Genre, y = Revenue, fill = Broad_Genre)) +
  geom_boxplot() +
  labs(title = "Revenue Distribution Across Different Genres",
       x = "Genre",
       y = "Revenue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()

