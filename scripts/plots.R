library(tidyverse)


# Read in some data
guns <- read_csv("data/guns.csv")
codes <- read_csv("data/cdc_codes.csv", col_names = FALSE)

# Format codes data for joining on guns data
codes %<>%
  mutate(code = str_extract(X1, "[0-9]+"), 
         X1 = gsub("[0-9]+\\s", "", X1)) %>% 
  rename(type = X1)

# Join codes on guns
guns %<>%
  rename(code = hispanic) %>% 
  merge(codes)

# Remove NA values


# Remove bad columns