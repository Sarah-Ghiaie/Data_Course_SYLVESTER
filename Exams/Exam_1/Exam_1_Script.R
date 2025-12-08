# Load packages
library(tidyverse)

# I.Read in the data

covid_data <- read_csv("cleaned_covid_data.csv")

covid_data <- covid_data %>%
  mutate(Last_Update = as.Date(Last_Update))


# II. Subset to states that begin with "A" 

A_states <- covid_data %>%
  filter(str_starts(Province_State, "A"))


# III. Plot Deaths over time for A_states

plot_A_states_deaths <- A_states %>%
  ggplot(aes(x = Last_Update, y = Deaths)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ Province_State, scales = "free") +
  labs(
    title = "COVID-19 Deaths Over Time for States Beginning with 'A'",
    x = "Last Update",
    y = "Deaths"
  ) +
  theme_bw()

print(plot_A_states_deaths)


# IV. Find peak Case_Fatality_Ratio for each state

state_max_fatality_rate <- covid_data %>%
  group_by(Province_State) %>%
  summarize(
    Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    Maximum_Fatality_Ratio = ifelse(
      is.infinite(Maximum_Fatality_Ratio),
      NA_real_,
      Maximum_Fatality_Ratio
    )
  ) %>%
  arrange(desc(Maximum_Fatality_Ratio))

print(state_max_fatality_rate)


# V. Bar plot of Maximum_Fatality_Ratio

state_max_fatality_rate <- state_max_fatality_rate %>%
  mutate(
    Province_State = fct_reorder(Province_State, Maximum_Fatality_Ratio, .desc = TRUE)
  )

plot_state_max_fatality <- state_max_fatality_rate %>%
  ggplot(aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col() +
  labs(
    title = "Maximum Case Fatality Ratio by State",
    x = "State",
    y = "Maximum Case Fatality Ratio"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

print(plot_state_max_fatality)


# VI. BONUS: Cumulative deaths for entire US over time

us_cumulative_deaths <- covid_data %>%
  group_by(Last_Update) %>%
  summarize(
    total_deaths = sum(Deaths, na.rm = TRUE)
  ) %>%
  ungroup()

plot_us_cumulative_deaths <- us_cumulative_deaths %>%
  ggplot(aes(x = Last_Update, y = total_deaths)) +
  geom_line() +
  labs(
    title = "Cumulative COVID-19 Deaths for the Entire US Over Time",
    x = "Last Update",
    y = "Cumulative Deaths"
  ) +
  theme_bw()

print(plot_us_cumulative_deaths)

