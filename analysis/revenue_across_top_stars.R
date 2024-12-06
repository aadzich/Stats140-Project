# revenue across top stars
library(tidyverse)
library(dplyr)

# import data
imdb <- read.csv("CURRENT_clean_imdbmovies_version5.csv", header = TRUE, stringsAsFactors = FALSE)

# turn genres into factors
imdb <- imdb %>% 
  mutate(
    Broad_Genre = as.factor(Broad_Genre))

# now want to encode actors, but consistent across columns
# pivot longer to be by actors, and find avg rev
actor_imdb <- imdb %>% 
  pivot_longer(cols = c(Star1, Star2, Star3, Star4),
               names_to = "StarVal",
               values_to = "Actor") %>% 
  mutate(Actor = as.factor(Actor)) 

# avg revenue across actors
avg_actor_imdb <- actor_imdb %>% 
  group_by(Actor) %>% 
  summarize(AvgRev = mean(Revenue), MovieCount = n())
glimpse(actor_imdb)

# total of 2709 unique top stars

# limit analysis to only the very top actors 
# (who have acted in at least 10 top movies)
pop_actor_imdb <- actor_imdb %>% 
  group_by(Actor) %>% 
  filter(n() >= 10) %>% 
  ungroup() %>% 
  mutate(Actor = as.factor(Actor))
pop_actor_imdb <- droplevels(pop_actor_imdb)
pop_actor_imdb

# now to do an ANOVA test to see if there's a significant diff 
# in revenue between movies featuring different actors
anova_pop_actor <- aov(Revenue ~ Actor, data = pop_actor_imdb)
summary(anova_pop_actor)
summary(anova_pop_actor)[[1]][["Pr(>F)"]][1]
# p value is significant!

table(pop_actor_imdb$Actor)

# now can do Tukey test to compare which actors differ significantly
tukey_pop_actor <- TukeyHSD(anova_pop_actor)
tukey_pop_actor$Actor

# filter only significant comparisons
tukey_sig <- tukey_pop_actor$Actor %>% 
  as.data.frame() %>% 
  filter(`p adj` < .05)

tukey_sig
# eyeballing looks like tom hanks one of the more sig diff actors


# grped anova across genres
ggplot(pop_actor_imdb, aes(x = Actor, y = Revenue, fill = Actor)) +
  geom_boxplot() +
  labs(title = "Revenue Distribution Across Different Actors",
       x = "Actor",
       y = "Revenue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))


