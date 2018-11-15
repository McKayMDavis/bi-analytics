library(tidyverse)


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
  merge(codes)

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
# Pairs Plots

#Visualization 2: McKay
# Box Plots

# Visualization 3: Brenden
# line Charts

guns %>%
  mutate(date = paste0(month, "-", year)) %>%
  group_by(date, intent) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = date, y = count, color = intent, group = intent)) +
  geom_line() + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Visualization 4: Brenden
# Line Chart 2

guns %>%
  mutate(date = paste0(month, "-", year)) %>%
  group_by(date, race) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = date, y = count, color = race, group = race)) +
  geom_line() + geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

