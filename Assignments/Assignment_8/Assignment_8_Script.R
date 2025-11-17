library(tidyverse)

# 1. Load the data 
mushroom <- read_csv("Data/mushroom_growth.csv")
glimpse(mushroom)

# Make factors
mushroom <- mushroom %>%
  mutate(
    Species     = factor(Species),
    Humidity    = factor(Humidity),
    Temperature = factor(Temperature)
  )

# 2. Exploratory plots
# A few plots exploring relationships with GrowthRate

# GrowthRate vs Light, colored by Species and faceted by Humidity
ggplot(mushroom, aes(x = Light, y = GrowthRate, color = Species)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ Humidity) +
  labs(title = "GrowthRate vs Light by Species and Humidity") +
  theme_minimal()

# GrowthRate vs Nitrogen, colored by Humidity and faceted by Species
ggplot(mushroom, aes(x = Nitrogen, y = GrowthRate, color = Humidity)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ Species) +
  labs(title = "GrowthRate vs Nitrogen by Species and Humidity") +
  theme_minimal()

# Boxplots for categorical predictors
ggplot(mushroom, aes(x = Humidity, y = GrowthRate)) +
  geom_boxplot() +
  facet_wrap(~ Species) +
  labs(title = "GrowthRate by Humidity and Species") +
  theme_minimal()

ggplot(mushroom, aes(x = Temperature, y = GrowthRate)) +
  geom_boxplot() +
  facet_wrap(~ Species) +
  labs(title = "GrowthRate by Temperature and Species") +
  theme_minimal()

# 3. Define at least 4 models for GrowthRate
# Model 1: simple additive effects of Light and Nitrogen
mod1 <- lm(GrowthRate ~ Light + Nitrogen, data = mushroom)

# Model 2: add Temperature (as a factor) to Model 1
mod2 <- lm(GrowthRate ~ Light + Nitrogen + Temperature, data = mushroom)

# Model 3: allow interaction between Light and Nitrogen + Temperature
mod3 <- lm(GrowthRate ~ Light * Nitrogen + Temperature, data = mushroom)

# Model 4: add Species and allow humidity to interact with temperature
# and still keep Light:Nitrogen interaction
mod4 <- lm(GrowthRate ~ Light * Nitrogen + Temperature * Humidity + Species,
           data = mushroom)

summary(mod1)
summary(mod2)
summary(mod3)
summary(mod4)

# 4. Calculate mean squared error (MSE) for each model

mse <- function(mod) mean(residuals(mod)^2)

mse_tbl <- tibble(
  model = c("mod1", "mod2", "mod3", "mod4"),
  mse   = c(mse(mod1), mse(mod2), mse(mod3), mse(mod4))
)

mse_tbl

# 5. Select the best model (lowest MSE)

best_model_name <- mse_tbl$model[which.min(mse_tbl$mse)]
best_model_name

best_mod <- get(best_model_name)
summary(best_mod)

# 6. Make predictions for new hypothetical values
# Use the variables included in the best model:
# Light, Nitrogen, Temperature, Humidity, Species

newdata <- expand_grid(
  Species     = levels(mushroom$Species),
  Humidity    = levels(mushroom$Humidity),
  Temperature = factor(c(20, 25), levels = levels(mushroom$Temperature)),
  Light       = seq(min(mushroom$Light), max(mushroom$Light), length.out = 3),
  Nitrogen    = seq(min(mushroom$Nitrogen), max(mushroom$Nitrogen), length.out = 5)
)

newdata <- newdata %>%
  mutate(predicted_GrowthRate = predict(best_mod, newdata = newdata))

# Check range of predictions
range(newdata$predicted_GrowthRate)

# 7. Plot predictions alongside real data

pred_plot_data <- newdata %>%
  filter(Light == 20, Temperature == "25")

ggplot() +
  geom_point(
    data = mushroom,
    aes(x = Nitrogen, y = GrowthRate, color = Humidity),
    alpha = 0.5
  ) +
  geom_line(
    data = pred_plot_data,
    aes(x = Nitrogen, y = predicted_GrowthRate,
        color = Humidity, linetype = Species),
    linewidth = 1
  ) +
  labs(
    title = paste("Real vs predicted GrowthRate (best model:", best_model_name, ")"),
    x = "Nitrogen",
    y = "GrowthRate / Predicted GrowthRate"
  ) +
  theme_minimal()

# Save the MSE table (optional)
write_csv(mse_tbl, "mushroom_model_mse.csv")

