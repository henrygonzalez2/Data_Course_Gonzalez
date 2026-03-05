#While using desktop as the working directory
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(dplyr)
dat <- read_csv("./Data_Course/Data/BioLog_Plate_Data.csv")



dat_d02SubWid <- dat_d02 %>% pivot_wider(id_cols = `Sample ID`, values_from = Hr_24)
                                         

dat_d01 <- dat %>% 
  filter(as.numeric(as.character(Dilution)) != "0.010")

dat_d01 <- dat %>%
  filter(Dilution != "0.010")

dat_d01 <- dat %>%
  filter(!Dilution %in% c("0.010", "0.001"))

class(dat)

dat_d02 <- dat_d01 %>%
  filter(!Dilution %in% c("0.01"))


dat_d03 <- dat_d02 %>%
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    values_to = "Absorbance"
  ) %>%
  mutate(
    Time = gsub("Hr_", "", Time),     # remove "Hr_"
    Time = as.numeric(Time)           # convert to numeric
  )

dat_d03 <- dat_d03 %>%
  mutate(Time_hr = factor(Time))

dat_d03$Time <- as.numeric(dat_d03$Time)
dat_d03$Status <- as.factor(dat_d03$Status)
dat_d03$Substrate <- as.factor(dat_d03$Substrate)


dat_d03 <- dat_d03 %>%
  mutate(
    Status = case_when(
      startsWith(`Sample ID`, "W") ~ "Water",
      startsWith(`Sample ID`, "C") ~ "Water",
      startsWith(`Sample ID`, "S") ~ "Soil",
      TRUE ~ "Unknown" # Default for all other cases
    )
  )


ggplot(dat_d03, aes(x = Time, 
                    y = Absorbance, 
                    color = Status, 
                    group = Status)) +
  geom_line(size = 1) +
  geom_smooth(method = "lm", size = 1.0)+
  facet_wrap(~ Substrate) +
  labs(
    x = "Time",
    y = "Absorbance",
    color = "Status"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 8),
    plot.title = element_text(hjust = 0.6)
  )


ggplot(dat_d03,
       aes(x = Time,
           y = Absorbance,
           group = `Sample ID`,
           color = `Sample ID`)) +
  
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  
  facet_wrap(~ Substrate) +
  
  labs(
    title = "Absorbance Over Time: {closest_state}",
    x = "Time",
    y = "Absorbance",
    color = "Sample ID"
  ) +
  
  theme_bw() +
  
  transition_states(Time,
                    transition_length = 2,
                    state_length = 1) +
  ease_aes("linear")


ggplot(
  dat_d03 %>% filter(Substrate == "Itaconic Acid"),
  aes(x = Time,
      y = Absorbance,
      group = `Sample ID`,
      color = `Sample ID`)
) +
  
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  
  labs(
    title = "Average Absorbance Over Time (Itaconic Acid): {closest_state}",
    x = "Time",
    y = "Average Absorbance",
    color = "Sample ID"
  ) +
  
  theme_bw() +
  
  transition_states(Time,
                    transition_length = 2,
                    state_length = 1) +
  ease_aes("linear")


ggplot(
 dat_d03 %>% filter(Substrate == "Itaconic Acid"),
  aes(x = Time,
      y = Absorbance,
      group = `Sample ID`,
      color = `Sample ID`)
) +
  
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  
  labs(
    title = "Average Absorbance Over Time (Itaconic Acid)",
    x = "Time",
    y = "Average Absorbance",
    color = "Sample ID"
  ) +
  
  theme_bw() +
  
  transition_reveal(Time)