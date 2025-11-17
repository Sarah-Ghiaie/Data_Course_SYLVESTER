#load packages
library(tidyverse)
library(broom)

#read in unicef data
unicef_raw <- read_csv("unicef-u5mr.csv")

#inspect raw data
glimpse(unicef_raw)

#make it tidy
unicef_tidy <- unicef_raw %>%
  pivot_longer(
    cols = starts_with("U5MR."),
    names_to  = "Year",
    values_to = "U5MR",
    values_drop_na = TRUE
  ) %>%
  mutate(
    Year = as.integer(str_remove(Year, "U5MR\\.")),
    Continent = as.factor(Continent),
    CountryName = as.factor(CountryName)
  )

#inspect tidy data
glimpse(unicef_tidy)

#plot each country's U5MR over time by continenet
plot1 <- ggplot(unicef_tidy,
                aes(x = Year, y = U5MR, group = CountryName)) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ Continent, scales = "free_y") +
  labs(
    title = "Under-5 Mortality Rate (U5MR) Over Time by Country",
    subtitle = "Lines show each country's U5MR over time, faceted by continent",
    x = "Year",
    y = "U5MR (deaths under 5 per 1,000 live births)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

ggsave("Sylvester_Plot_1.png", plot = plot1, width = 11, height = 7, dpi = 300)

#plot avg U5MR per continent per year
unicef_continent_mean <- unicef_tidy %>%
  group_by(Continent, Year) %>%
  summarise(
    mean_U5MR = mean(U5MR, na.rm = TRUE),
    .groups = "drop"
  )

plot2 <- ggplot(unicef_continent_mean,
                aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line(size = 1) +
  labs(
    title = "Mean Under-5 Mortality Rate (U5MR) by Continent Over Time",
    x = "Year",
    y = "Mean U5MR (deaths under 5 per 1,000 live births)",
    color = "Continent"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave("Sylvester_Plot_2.png", plot = plot2, width = 10, height = 6, dpi = 300)

#models of U5MR (U5MR~yr, U5MR~yr+continent, U5MR~yr*continent)
mod1 <- lm(U5MR ~ Year, data = unicef_tidy)
mod2 <- lm(U5MR ~ Year + Continent, data = unicef_tidy)
mod3 <- lm(U5MR ~ Year * Continent, data = unicef_tidy)

#compare w/AIC+ANOVA
model_compare <- tibble(
  Model = c("mod1_Year", "mod2_Year+Continent", "mod3_Year*Continent"),
  AIC   = c(AIC(mod1), AIC(mod2), AIC(mod3)),
  BIC   = c(BIC(mod1), BIC(mod2), BIC(mod3))
)

print(model_compare)

anova_compare <- anova(mod1, mod2, mod3)
print(anova_compare)

#Based on model comparisons, mod3 is expected to perform the best. 
#It allows each continent to have it's own intercept and slope over time. 


#plot predictions 
pred_grid <- unicef_tidy %>%
  select(CountryName, Continent, Year) %>%
  distinct() %>%
  arrange(Continent, CountryName, Year)

preds_models <- pred_grid %>%
  mutate(
    pred_mod1 = predict(mod1, newdata = pred_grid),
    pred_mod2 = predict(mod2, newdata = pred_grid),
    pred_mod3 = predict(mod3, newdata = pred_grid)
  ) %>%
  pivot_longer(
    cols = starts_with("pred_mod"),
    names_to = "Model",
    values_to = "pred"
  ) %>%
  mutate(
    Model = recode(
      Model,
      "pred_mod1" = "mod1: Year",
      "pred_mod2" = "mod2: Year + Continent",
      "pred_mod3" = "mod3: Year * Continent"
    )
  )

#plot predicted overtime by continent
# plot predicted values for each model
plot3 <- ggplot(preds_models,
                aes(x = Year, y = pred,
                    group = CountryName,
                    color = Continent)) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ Model, scales = "free_y") +
  labs(
    title = "Predicted U5MR Over Time by Model",
    x = "Year",
    y = "Predicted U5MR",
    color = "Continent"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

ggsave("Sylvester_Plot_3.png", plot = plot3, width = 11, height = 7, dpi = 300)


