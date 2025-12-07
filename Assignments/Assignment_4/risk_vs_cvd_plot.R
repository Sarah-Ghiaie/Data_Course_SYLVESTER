
#Project: Integrative Cardiovascular Disease Risk Prediction
#Purpose: Generate a ggplot showing an expected relationship between genetic risk score and CVD incidence


#Load packages
library(tidyverse)

#Load fake data
df <- read_csv("fake_data.csv")


#Expected Result Visualization: Higher genetic risk scores lead to higher CVD prevalence


#Summarize prevalence of cardio by genetic risk group
plot_df <- df %>%
  group_by(genetic_risk) %>%
  summarise(
    mean_cvd = mean(cardio),
    n = n()
  )

#Print summary table
print(plot_df)


#Bar plot of CVD prevalence by genetic risk score


ggplot(plot_df, aes(x = factor(genetic_risk), y = mean_cvd)) +
  geom_col(fill = "#872bbf") +
  geom_text(aes(label = round(mean_cvd, 2)), vjust = -0.5) +
  labs(
    title = "CVD Prevalence by Genetic Risk Score",
    subtitle = "Expected outcome: Higher genetic risk is associated with higher CVD incidence",
    x = "Genetic Risk Score (0–3)",
    y = "Proportion with Cardiovascular Disease"
  ) +
  theme_minimal(base_size = 14)

