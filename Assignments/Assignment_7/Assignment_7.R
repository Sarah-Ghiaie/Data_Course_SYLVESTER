#load packages
library(tidyverse)

#read in data
relig_raw <- read_csv("Utah_Religions_by_County.csv")

#inspect data
glimpse(relig_raw)

#separate county info
county_info <- relig_raw %>%
  select(County, Pop_2010, Religious, `Non-Religious`)

#id religion proportions
relig_cols <- relig_raw %>%
  select(-County, -Pop_2010, -Religious, -`Non-Religious`) %>%
  names()

#format one row per county × religion
relig_tidy <- relig_raw %>%
  pivot_longer(
    cols = all_of(relig_cols),
    names_to = "Religion",
    values_to = "Proportion"
  ) %>%
  mutate(
    Religion = str_replace_all(Religion, "_", " "),
    Religion = str_replace_all(Religion, "-", " "),
    Religion = str_to_title(Religion)
  )

glimpse(relig_tidy)

#make and save figures
dir.create("figures", showWarnings = FALSE)

#histogram of population
p_pop <- ggplot(relig_raw, aes(Pop_2010)) +
  geom_histogram(bins = 15, fill = "steelblue") +
  labs(title = "Distribution of Utah County Populations",
       x = "Population (2010)",
       y = "Counties")

ggsave("figures/population_histogram.png", p_pop, width = 7, height = 5)


#mean proportion by religion
p_mean_relig <- relig_tidy %>%
  group_by(Religion) %>%
  summarise(mean_prop = mean(Proportion, na.rm = TRUE)) %>%
  arrange(desc(mean_prop)) %>%
  ggplot(aes(x = reorder(Religion, mean_prop),
             y = mean_prop)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Mean Religious Proportion Across Utah Counties",
       x = "Religion",
       y = "Mean Proportion")

ggsave("figures/mean_religion_barplot.png", p_mean_relig, width = 7, height = 6)


#address first question with figure

#scatterplot pop & religion proportion
p_pop_vs_relig <- ggplot(relig_tidy,
                         aes(x = Pop_2010, y = Proportion)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Religion, scales = "free_y") +
  labs(title = "Population vs Religion Proportions",
       x = "County Population",
       y = "Proportion")

ggsave("figures/population_vs_religion.png",
       p_pop_vs_relig, width = 12, height = 8)

#correlations
cor_pop <- relig_tidy %>%
  group_by(Religion) %>%
  summarise(
    correlation = cor(Pop_2010, Proportion,
                      use = "complete.obs", method = "pearson")
  ) %>%
  arrange(desc(abs(correlation)))

#address second question with figure
relig_non <- relig_tidy %>%
  left_join(
    relig_raw %>% select(County, `Non-Religious`),
    by = "County"
  )

#scatterplot religious vs non religious 
p_rel_vs_non <- ggplot(relig_non,
                       aes(x = Proportion, y = `Non-Religious`)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ Religion, scales = "free") +
  labs(title = "Religion vs Non-Religious Correlation",
       x = "Religion Proportion",
       y = "Non-Religious Proportion")

ggsave("figures/religion_vs_nonreligious.png",
       p_rel_vs_non, width = 12, height = 8)

#correlation
cor_non <- relig_non %>%
  group_by(Religion) %>%
  summarise(
    correlation = cor(Proportion, `Non-Religious`,
                      use = "complete.obs", method = "pearson")
  ) %>%
  arrange(desc(abs(correlation)))

