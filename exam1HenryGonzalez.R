#1.

df <- read.csv("cleaned_covid_data.csv")
skimr::skim(df)

#2.

A_states <- df %>%
  +     filter(str_detect(Province_State, "^A"))

#3

ggplot(A_states, aes(x = date(Last_Update), y = Deaths, color = Province_State)) +
  +     geom_point(alpha = 0.5) +  # Scatterplot
  +     geom_smooth(method = "loess", se = FALSE) +  # Loess curves NO shading
  +     facet_wrap(~Province_State, scales = "free") +  # Separate facets, free scales
  +     theme_minimal() +
  +     labs(title = "Deaths Over Time by State",
             +          x = "Date",
             +          y = "Number of Deaths")

#4

state_max_fatality_rate <- df %>%
  +     group_by(Province_State) %>%
  +     summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  +     arrange(desc(Maximum_Fatality_Ratio)) %>%
  +     ungroup()

#5

library(ggplot2)
library(dplyr)

df_sorted <- state_max_fatality_rate %>% arrange(desc(Maximum_Fatality_Ratio))

df_sorted$Province_State <- factor(df_sorted$Province_State, 
                                   levels = unique(df_sorted$Province_State))

ggplot(df_sorted, aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Maximum Fatality Ratio by State",
       x = "Province/State",
       y = "Maximum Fatality Ratio")

