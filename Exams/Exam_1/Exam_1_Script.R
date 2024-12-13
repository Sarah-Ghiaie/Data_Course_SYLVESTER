library(tidyverse)
library(ggplot2)

# I
covid_data <- read.csv("cleaned_covid_data.csv")

# II
A_states <- covid_data %>% 
  filter(str_detect(Province_State, "^A"))

# III
ggplot(A_states, aes(x = Last_Update, y = Deaths)) +
  labs( x = "Date", y = "Number of Deaths", title = "Deaths Over Time")+
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Province_State, scales = "free")

# IV
state_max_fatality_rate <- covid_data %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

# V
ggplot(state_max_fatality_rate, aes(x = reorder(Province_State, -Maximum_Fatality_Ratio), y = Maximum_Fatality_Ratio, fill = Maximum_Fatality_Ratio)) +
  geom_bar(stat = "identity") +
  labs(x = "Province/State", y = "Max Fatality Ratio", title = "Max Fatality Ratio by State")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# VI
total_deaths_over_time <- covid_data %>%
  group_by(Last_Update) %>%
  summarize(Total_Deaths = sum(Deaths))

ggplot(total_deaths_over_time, aes(x = Last_Update, y = Total_Deaths)) +
  geom_point() +
  labs(x = "Date", y = "Cumulative Deaths", title = "US Deaths Over Time")

