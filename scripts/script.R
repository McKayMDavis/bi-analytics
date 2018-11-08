library(tidyverse)
source("./scripts/write_to_images.R")


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
# 
stuff <- guns %>% 
  filter(intent %in% c("Homicide", "Suicide")) %>% 
  group_by(intent, race) %>% 
  summarise(count = n())







# Write outputs
make_img(count_gundeath, "deaths_by_year_month.png")
make_img(stuff, "deaths_by_intent_race.png")
