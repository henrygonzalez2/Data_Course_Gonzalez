library(ggplot2)

Unicef <- read.csv("unicef-u5mr.csv")


> u5mr_tibble <- Unicef %>%
  +     pivot_longer(
    +         cols = starts_with("U5MR"),
    +         names_to = "Year",
    +         names_prefix = "U5MR.",
    +         values_to = "U5MR"
    +     ) %>%
  +     mutate(Year = as.integer(Year))
> 
  > # View the resulting tibble
  > view(u5mr_tibble)

#1
ggplot(u5mr_tibble, aes(x = Year, y = U5MR, group = CountryName, color = CountryName)) +
  geom_line() +
  facet_wrap(~ Continent)+
  labs(title = "U5MR Over Time by Country",
       y = "Under-Five Mortality Rate (U5MR)",
       x = "Year") +
  theme_minimal() + 
  theme(legend.position = "none")


#4
mean_u5mr <- u5mr_tibble %>%
  group_by(Year, Continent) %>%
  summarize(mean_U5MR = mean(U5MR, na.rm = TRUE)) %>%
  ungroup()

ggplot(mean_u5mr, aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line(size = 1) +
  labs(title = "Mean U5MR Over Time by Continent",
       y = "Mean Under-Five Mortality Rate (U5MR)",
       x = "Year",
       color = "Continent") +
  theme_minimal()

#6

mod1 <- lm(U5MR ~ Year, data = u5mr_tibble)

mod2 <- lm(U5MR ~ Year + Continent, data = u5mr_tibble)

mod10 <- lm(U5MR ~ Year * Continent, data = u5mr_tibble)

summary(mod1)
summary(mod2)
summary(mod10)

#8
mods <- list(mod1=mod1,mod2=mod2,mod10=mod10)
# apply "performance" function on all in the list and combine 
map(mods,performance) %>% reduce(full_join)

# I believe the third model is the best because it has the highest R^2 value

#9
pred_mod1 <- u5mr_tibble %>%
  filter(!is.na(U5MR)) %>%
  mutate(pred = predict(mod1, newdata = .))

pred_mod2 <- u5mr_tibble %>%
  filter(!is.na(U5MR)) %>%
  mutate(pred = predict(mod2, newdata = .))

pred_mod10 <- u5mr_tibble %>%
  filter(!is.na(U5MR)) %>%
  mutate(pred = predict(mod10, newdata = .))


pred_mod1 <- pred_mod1 %>% mutate(model = "mod1")
pred_mod2 <- pred_mod2 %>% mutate(model = "mod2")
pred_mod10 <- pred_mod3 %>% mutate(model = "mod10")


all_preds <- bind_rows(pred_mod1, pred_mod2, pred_mod10)

ggplot(all_preds, aes(x = Year, y = pred, color = interaction(model, Continent), group = interaction(model, Continent))) +
  geom_line() +
  labs(title = "Predicted U5MR by Model and Continent",
       y = "Predicted U5MR",
       x = "Year",
       color = "Model & Continent") +
  theme_minimal() +
  facet_wrap(~ model, scales = "free_y") +  # separate space for each model
  theme_minimal() +
  labs(title = "U5MR Predictions by Models",
       y = "Under-Five Mortality Rate (U5MR)",
       x = "Year")
  theme(legend.position = "right")
  
 # 10
  newdf2 = data.frame(
    Year = c(2020, 2005, 2020),
    Continent = c("Americas", "Africa", "Europe")
  )
  # making predictions
  pred2 = predict(mod10, newdata = newdf2)
  
  actual_value <- 13
  off_value <- pred2 - actual_value
  
  print(off_value)
  #       1         2         3 
  #-23.58018  91.11857 -15.14478 
  
  #we are off by -23 deaths, which means our prediction isnt great.