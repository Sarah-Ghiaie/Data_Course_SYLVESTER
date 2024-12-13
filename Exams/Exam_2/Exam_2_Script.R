 <- read.csv("unicef-u5mr.csv")

library(tidyverse)

#tidy data
unicef_data_tidy <- unicef_data %>%
  pivot_longer(starts_with("U5MR."), names_to = "Year", values_to = "U5MR") %>%
  mutate(Year = as.numeric(str_sub(Year, 5, 9))) %>%
  arrange(CountryName, Year)

# Plot U5MR over time by country and continent (Plot_1.png)
library(ggplot2)
ggplot(unicef_data_tidy, aes(x = Year, y = U5MR, color = Continent)) +
  geom_line(aes(linetype = CountryName)) +
  facet_wrap(~ Continent) +
  labs(title = "U5MR Over Time by Country and Continent",
       x = "Year", y = "U5MR") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("LASTNAME_Plot_1.png", width = 10, height = 6)

# Plot mean U5MR by continent over time (Plot_2.png)
ggplot(unicef_data_tidy, aes(x = Year, y = mean(U5MR), color = Continent)) +
  geom_line() +
  labs(title = "Mean U5MR by Continent Over Time",
       x = "Year", y = "Mean U5MR") +
  theme_bw()
ggsave("LASTNAME_Plot_2.png", width = 10, height = 6)

# Create three models
library(lme4)
mod1 <- lm(U5MR ~ Year, data = unicef_data_tidy)
mod2 <- lmer(U5MR ~ Year + (1|Continent), data = unicef_data_tidy)
mod3 <- lmer(U5MR ~ Year * Continent + (1|Continent), data = unicef_data_tidy)

# Compare model performance
summary(mod1)
summary(mod2)
summary(mod3)

# Plot predictions for all three models
future_data <- data.frame(Year = seq(min(unicef_data_tidy$Year), max(unicef_data_tidy$Year), by = 1),
                          Continent = unique(unicef_data_tidy$Continent))
predictions <- data.frame(
  Continent = future_data$Continent,
  Year = future_data$Year,
  pred = predict(mod1, newdata = future_data),
  stringsAsFactors = FALSE
) %>%
  rbind(
    data.frame(
      Continent = future_data$Continent,
      Year = future_data$Year,
      pred = predict(mod2, newdata = future_data),
      stringsAsFactors = FALSE
    ),
    data.frame(
      Continent = future_data$Continent,
      Year = future_data$Year,
      pred = predict(mod3, newdata = future_data),
      stringsAsFactors = FALSE
    )
  ) %>%
  mutate(Model = c(rep("mod1", nrow(future_data)), rep("mod2", nrow(future_data)), rep("mod3", nrow(future_data))))

ggplot(predictions, aes(x = Year, y = pred, fill = Model)) +
  geom_line() +
  facet_wrap(~ Continent) +
  labs(title = "Model Predictions for U5MR",
       x = "Year", y = "Predicted U5MR") +
  theme_bw()
gtable <- ggplotGrob(ggplot(predictions, aes(x = Year, y = pred, fill = Model)) +
                       geom_line() +
                       facet_wrap(~ Continent) +
                       labs(title = "Model Predictions for U5MR",
                            x = "Year", y = "Predicted U5MR") +
                       theme_bw())
grid.newpage()
grid.draw(gtable)

# Bonus: Predict U5MR for Ecuador in 2020 and compare with reality
ecuador_2020 <- data.frame(Year = 2020, Continent = "Americas")