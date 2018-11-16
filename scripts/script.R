library(tidyverse)
library(plotly)


# Read in some data
guns <- read_csv("data/guns.csv")
codes <- read_csv("data/cdc_codes.csv", col_names = FALSE)

# Cleaning method 1: McKay
# Format codes data for joining on guns data
codes %<>%
  mutate(code = str_extract(X1, "[0-9]+"), 
         X1 = gsub("[0-9]+\\s", "", X1)) %>% 
  rename(type = X1)

# Cleaning method 2: McKay
# Join codes on guns
guns %<>%
  rename(code = hispanic) %>% 
  merge(codes) %>% 
  mutate(date = zoo::as.yearmon(paste0(year, "-", month)),
         is_minority = case_when(race != "White" ~ "Minority",
                                 race == "White" ~ "White")) %>% 
  mutate(education2 = case_when(education == 1 ~ "Less Than Highschool",
                                education == 2 ~ "Graduated From Highschool or equivalent",
                                education == 3 ~ "Some College",
                                education == 4 ~ "At Least College Graduate",
                                education == 5 ~ "Unknown"))

# Cleaning method 3: Brenden
# Remove bad columns
guns <- guns %>%
  select(-code, -X1)

# Cleaning method 4: Brenden
# Remove NA Values
guns <- guns %>%
  na.omit()

# Profiling method 1: Brenden
# Summary Table
count_gundeath <- guns %>%
  group_by(year, month) %>%
  summarise(n()) %>%
  rename(count = `n()`)

# Profiling method 2: McKay
# Summary of deaths by intent and race (only Suicide and Homicide)
intents <- guns %>% 
  filter(intent %in% c("Homicide", "Suicide")) %>% 
  group_by(intent, race) %>% 
  summarise(count = n())

# Visualization 1: McKay
# Minority vs intent
p1 <- guns %>% 
  group_by(date, is_minority, intent) %>% 
  summarise(count = n()) %>% 
  filter(intent %in% c("Homicide", "Suicide")) %>% 
  ggplot(aes(x = date, y = count, color = intent, group = intent)) +
  geom_point() +
  geom_line() +
  facet_wrap(~is_minority) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  labs(y = "Number of Gun Deaths",
       x = "Date",
       color = "Intent",
       title = "Number of Deaths between Minority and White by Intent")

plotly::ggplotly(p1, tooltip = c("y", "x"))

#Visualization 2: McKay
# Education vs intent

guns %>% 
  group_by(date, education2, intent) %>% 
  filter(intent %in% c("Homicide", "Suicide"),
         education != 5) %>% 
  summarise(count = n()) %>%
  ggplot(aes(x = date, y = count, color = education2, group = education2)) +
  geom_point() +
  geom_line() +
  facet_wrap(~intent) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2") +
  labs(y = "Number of Gun Deaths",
       x = "Date",
       color = "Education Level",
       title = "Number of Deaths by Suicide Proportionately Higher for Higher Educated People")

# Visualization 3: Brenden
# line Charts: Count deaths by Intent

guns %>%
  mutate(date = zoo::as.Date(date)) %>%
  group_by(date, intent) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  group_by(intent) %>% 
  mutate(avg = mean(count)) %>% 
  ggplot(aes(x = date, y = count, color = intent, group = intent)) +
  geom_line() + geom_point() +
  geom_line(aes(y = avg), color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %y",date_breaks = "1 month")

# Visualization 4: Brenden
# Line Chart 2: Count of deaths by race AND pie chart of intent

guns %>%
  mutate(date = zoo::as.Date(date)) %>%
  group_by(date, race) %>%
  summarise(count = n()) %>%
  ungroup() %>% 
  group_by(race) %>% 
  mutate(avg = mean(count)) %>% 
  ggplot(aes(x = date, y = count, color = race, group = race)) +
  geom_line() + geom_point() +
  geom_line(aes(y = avg), color = "black") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_date(date_labels = "%b %y",date_breaks = "1 month")

pie_guns <- guns %>%
  group_by(intent) %>%
  summarise(count = n())
  
pie(pie_guns$count, pie_guns$intent)
